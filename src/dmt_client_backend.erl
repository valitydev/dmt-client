-module(dmt_client_backend).

-export([commit/4]).
-export([checkout_object/3]).
-export([get_last_version/1]).

%%% Behaviour callbacks

-callback commit(
    dmt_client:vsn(),
    [dmt_client:operation()],
    dmt_client:author_id(),
    dmt_client:opts()
) -> dmt_client:commit_response() | no_return().

-callback checkout_object(dmt_client:object_ref(), dmt_client:vsn(), dmt_client:opts()) ->
    dmt_client:versioned_object() | no_return().

%%% API

-spec commit(
    dmt_client:base_version(),
    [dmt_client:operation()],
    dmt_client:author_id(),
    dmt_client:opts()
) -> dmt_client:commit_response() | no_return().
commit(Version, Operations, AuthorID, Opts) ->
    call(commit, [Version, Operations, AuthorID, Opts]).

-spec checkout_object(dmt_client:object_ref(), dmt_client:vsn(), dmt_client:opts()) ->
    dmt_client:versioned_object() | no_return().
checkout_object(ObjectReference, VersionReference, Opts) ->
    call(checkout_object, [ObjectReference, VersionReference, Opts]).

-spec get_last_version(dmt_client:opts()) -> number() | no_return().
get_last_version(Opts) ->
    call(get_last_version, [Opts]).

%%% Internal functions

-spec get_api_module() -> module().
get_api_module() ->
    genlib_app:env(dmt_client, api_module, dmt_client_api).

-spec call(atom(), list()) -> term() | no_return().
call(Fun, Args) ->
    Module = get_api_module(),
    erlang:apply(Module, Fun, Args).
