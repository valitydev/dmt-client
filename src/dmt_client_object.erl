-module(dmt_client_object).

-export([make_refless/1]).
-export([get_ref/1]).
-export([get_type/1]).

-spec make_refless(dmt_client:domain_object()) -> dmt_client:refless_domain_object().
make_refless({Type, {_Object, _ID, Data}}) ->
    {Type, Data}.

-spec get_ref(dmt_client:domain_object()) -> dmt_client:object_ref().
get_ref({Type, {_Object, ID, _Data}}) ->
    {Type, ID}.

-spec get_type(dmt_client:domain_object()) -> dmt_client:object_type().
get_type({Type, {_Object, _Ref, _Data}}) ->
    Type.
