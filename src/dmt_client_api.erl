-module(dmt_client_api).

-behaviour(dmt_client_backend).

-export([search/6]).
-export([commit/4]).
-export([checkout_object/3]).
-export([get_latest_version/1]).

-include_lib("damsel/include/dmsl_domain_conf_v2_thrift.hrl").

-spec search(
    dmt_client:vsn(),
    dmt_client:search_pattern(),
    dmt_client:object_type(),
    dmt_client:limit(),
    dmt_client:continuation_token() | undefined,
    dmt_client:opts()
) -> dmt_client:search_full_response() | no_return().
search(Version, Pattern, Type, Limit, Token, Opts) ->
    Params = make_search_params(Pattern, Version, Limit, Type, Token),
    call('Repository', 'SearchFullObjects', {Params}, Opts).

-spec commit(
    dmt_client:vsn(),
    [dmt_client:operation()],
    dmt_client:author_id(),
    dmt_client:opts()
) ->
    dmt_client:commit_response() | no_return().
commit(Version, Operations, AuthorID, Opts) ->
    call('Repository', 'Commit', {Version, Operations, AuthorID}, Opts).

-spec checkout_object(dmt_client:object_ref(), dmt_client:vsn(), dmt_client:opts()) ->
    dmt_client:versioned_object() | no_return().
checkout_object(ObjectReference, VersionRef, Opts) ->
    call('RepositoryClient', 'CheckoutObject', {VersionRef, ObjectReference}, Opts).

-spec get_latest_version(dmt_client:opts()) -> number() | no_return().
get_latest_version(Opts) ->
    call('Repository', 'GetLatestVersion', {}, Opts).

call(ServiceName, Function, Args, Opts) ->
    Url = get_service_url(ServiceName),
    Service = get_service_modname(ServiceName),
    Call = {Service, Function, Args},
    TransportOpts =
        maps:merge(
            #{recv_timeout => 60000, connect_timeout => 1000},
            maps:merge(
                genlib_app:env(dmt_client, transport_opts, #{}),
                maps:get(transport_opts, Opts, #{})
            )
        ),

    CallOpts =
        #{
            url => Url,
            event_handler => get_event_handlers(),
            transport_opts => TransportOpts
        },

    Context = maps:get(woody_context, Opts, woody_context:new()),

    case woody_client:call(Call, CallOpts, Context) of
        {ok, Response} ->
            Response;
        {exception, Exception} ->
            throw(Exception)
    end.

get_service_url(ServiceName) ->
    maps:get(ServiceName, genlib_app:env(dmt_client, service_urls)).

get_service_modname(ServiceName) ->
    {get_service_module(ServiceName), ServiceName}.

get_service_module('Repository') ->
    dmsl_domain_conf_v2_thrift;
get_service_module('RepositoryClient') ->
    dmsl_domain_conf_v2_thrift.

get_event_handlers() ->
    genlib_app:env(dmt_client, woody_event_handlers, []).

make_search_params(Pattern, Version, Limit, Type, Token) ->
    #domain_conf_v2_SearchRequestParams{
        query = Pattern,
        version =
            case Version of
                {version, V} -> V;
                %% Head
                _ -> undefined
            end,
        limit = Limit,
        type = Type,
        continuation_token = Token
    }.
