-module(dmt_client_object).

-export([get_object_ref/1]).
-export([get_object_type/1]).

-spec get_object_ref(dmt_client:domain_object()) -> dmt_client:object_ref().
get_object_ref({Type, {_Object, ID, _Data}}) ->
    {Type, ID}.

-spec get_object_type(dmt_client:domain_object()) -> dmt_client:object_type().
get_object_type({Type, {_Object, _Ref, _Data}}) ->
    Type.
