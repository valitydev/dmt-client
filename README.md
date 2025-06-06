# dmt-client

Кеширующий клиент к [dominant-v2](https://github.com/valitydev/dominant-v2).

## **Проблемы** текущей реализации

### Кеширование

Исторически модель кеширование наследуется от клиента предыдущей реализации для прежнего API доминанты и потому не вполне решает проблемы характерные для нового API.

#### Номер последней версии

Для использования локального кеша каждый вызов клиента с ключевым словом `latest` в качестве версии приводит дополнительному сетевому вызову к бекенду с запросом номера последней версии. Разворачивание этого значения в целочисленный номер версии нужно для дальнейшего пути внутри слоя кеширования и бекенда с непосредственной вычиткой объектов по версиям.

В качестве решения можно сохранять номер последней версии с отметкой времени и добавить в клиент механизм периодического обновления этого значения, например, каждые 5 секунд.

#### Коллекции объектов

На текущий момент кешируются объекты только поштучно, но нужно уметь консистентно кешировать и коллекции объектов. Например упорядоченный список объектов одного типа, как например страны или валюты.

Существующая реализация пообъектного кеша не подходит для коллекций, так как имеет механизм выбывания элемента по времени. В качестве решения можно обобщить поведение кеширующего слоя с упорядоченным запросом целевого объекта и разделить на инстансы-бакеты с разными стратегиями освобождения кеша: для объектов и для коллекций.

#### Версии изменений объектов

Сейчас если мы запрашиваем объект `A` версии `45`, то результат мы сохраняем в кеше с соответственными ключами, даже если объект фактически менялся только в версии `42`. Если далее мы запросим и закешируем версию `50` того же объекта, то фактически мы сохраним то же самое значение что и для версии `45` или `42`.

Если нагромождение кеша одинаковыми объектами станет проблемой, то стоит рассмотреть вариант с заданием отрезков версий в качестве ключей для закешированных значений.

- Если мы запросили объект `A` версии `45`, то в кеш попадёт значение с информацией что этот объект актуален с версии его последнего изменения (`42`) вплоть до запрошенной версии, то есть `42-45`.

- Когда далее мы запросим версии `50`, то не обнаружив попадания версии в хранимый отрезок, получим у бекенда актуальное значние объекта. Увидев что он не обновлялся с версии `42` мы продолжим отрезок в существующем значении до номера последней запрошенной версии, то есть `42-50`.

- При запросе объекта версии, например `48`, мы сможем сразу отдать значние из подпадающего отрезка.

- А если позже с запросом объекта версии `60` мы узнаем что тот менялся в версии `59`, то мы только тогда добавим новое значние в кеш с дискриминирующим отрезком `59-60`.
