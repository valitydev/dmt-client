-module(dmt_client_backend).

-export([search/6]).
-export([commit/4]).
-export([checkout_object/3]).
-export([checkout_object_with_references/3]).
-export([get_latest_version/1]).
-export([get_related_graph/2]).

%%% Behaviour callbacks

-callback search(
    dmt_client:vsn(),
    dmt_client:search_pattern(),
    dmt_client:object_type(),
    dmt_client:limit(),
    dmt_client:continuation_token() | undefined,
    dmt_client:opts()
) -> dmt_client:search_full_response() | no_return().

-callback commit(
    dmt_client:vsn(),
    [dmt_client:operation()],
    dmt_client:author_id(),
    dmt_client:opts()
) -> dmt_client:commit_response() | no_return().

-callback checkout_object(dmt_client:vsn(), dmt_client:object_ref(), dmt_client:opts()) ->
    dmt_client:versioned_object() | no_return().

-callback checkout_object_with_references(
    dmt_client:vsn(), dmt_client:object_ref(), dmt_client:opts()
) ->
    dmt_client:versioned_object_with_references() | no_return().

-callback get_related_graph(dmt_client:related_graph_request(), dmt_client:opts()) ->
    dmt_client:related_graph() | no_return().

%%% API

-spec search(
    dmt_client:vsn(),
    dmt_client:search_pattern(),
    dmt_client:object_type(),
    dmt_client:limit(),
    dmt_client:continuation_token() | undefined,
    dmt_client:opts()
) -> dmt_client:search_full_response() | no_return().
search(Version, Pattern, Type, Limit, Token, Opts) ->
    call(search, [Version, Pattern, Type, Limit, Token, Opts]).

-spec commit(
    dmt_client:vsn(),
    [dmt_client:operation()],
    dmt_client:author_id(),
    dmt_client:opts()
) -> dmt_client:commit_response() | no_return().
commit(Version, Operations, AuthorID, Opts) ->
    call(commit, [Version, Operations, AuthorID, Opts]).

-spec checkout_object(dmt_client:vsn(), dmt_client:object_ref(), dmt_client:opts()) ->
    dmt_client:versioned_object() | no_return().
checkout_object(Version, ObjectReference, Opts) ->
    call(checkout_object, [Version, ObjectReference, Opts]).

-spec checkout_object_with_references(dmt_client:vsn(), dmt_client:object_ref(), dmt_client:opts()) ->
    dmt_client:versioned_object_with_references() | no_return().
checkout_object_with_references(Version, ObjectReference, Opts) ->
    call(checkout_object_with_references, [Version, ObjectReference, Opts]).

-spec get_latest_version(dmt_client:opts()) -> number() | no_return().
get_latest_version(Opts) ->
    call(get_latest_version, [Opts]).

-spec get_related_graph(dmt_client:related_graph_request(), dmt_client:opts()) ->
    dmt_client:related_graph() | no_return().
get_related_graph(Request, Opts) ->
    call(get_related_graph, [Request, Opts]).

%%% Internal functions

-spec get_api_module() -> module().
get_api_module() ->
    genlib_app:env(dmt_client, api_module, dmt_client_api).

-spec call(atom(), list()) -> term() | no_return().
call(Fun, Args) ->
    Module = get_api_module(),
    erlang:apply(Module, Fun, Args).
