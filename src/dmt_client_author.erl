-module(dmt_client_author).

-include_lib("damsel/include/dmsl_domain_conf_v2_thrift.hrl").

%% API
-export([
    create/2,
    get/1,
    delete/1
]).

-export_type([author/0]).
-export_type([author_params/0]).
-export_type([author_id/0]).
-export_type([opts/0]).

-type author() :: dmt_client:author().
-type author_params() :: dmt_client:author_params().
-type author_id() :: dmt_client:author_id().
-type opts() :: dmt_client:opts().

-spec create(author_params(), opts()) -> author() | no_return().
create(Params, Opts) ->
    call('AuthorManagement', 'Create', {Params}, Opts).

-spec get(author_id()) -> author() | no_return().
get(ID) ->
    get(ID, #{}).

-spec get(author_id(), opts()) -> author() | no_return().
get(ID, Opts) ->
    call('AuthorManagement', 'Get', {ID}, Opts).

-spec delete(author_id()) -> ok | no_return().
delete(ID) ->
    delete(ID, #{}).

-spec delete(author_id(), opts()) -> ok | no_return().
delete(ID, Opts) ->
    call('AuthorManagement', 'Delete', {ID}, Opts).

%% Internal functions

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
    case genlib_app:env(dmt_client, service_urls) of
        undefined ->
            error({config_missing, service_urls});
        Config when is_map(Config) ->
            case maps:get(ServiceName, Config, undefined) of
                undefined ->
                    error({service_url_missing, ServiceName});
                Url ->
                    Url
            end
    end.

get_service_modname(ServiceName) ->
    {get_service_module(ServiceName), ServiceName}.

get_service_module('AuthorManagement') ->
    dmsl_domain_conf_v2_thrift.

get_event_handlers() ->
    genlib_app:env(dmt_client, woody_event_handlers, [woody_event_handler_default]).
