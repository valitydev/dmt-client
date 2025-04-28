%%% @doc Public API, supervisor and application startup.
%%% @end

-module(dmt_client).

-behaviour(supervisor).
-behaviour(application).

%% API
-export([checkout_object/1]).
-export([checkout_object/2]).
-export([checkout_object/3]).
-export([commit/3]).
-export([commit/4]).
-export([get_latest_version/0]).

% AuthorManagement API

-export([author_create/2]).
-export([author_get/1]).
-export([author_delete/1]).

%% Health check API
-export([health_check/0]).

%% Supervisor callbacks
-export([init/1]).

%% Application callbacks
-export([start/2]).
-export([stop/1]).

-export_type([vsn/0]).
-export_type([vsn_created_at/0]).
-export_type([version/0]).
-export_type([base_version/0]).
-export_type([versioned_object/0]).
-export_type([operation/0]).
-export_type([commit_response/0]).
-export_type([object_ref/0]).
-export_type([domain_object/0]).
-export_type([opts/0]).

-export_type([author_id/0]).
-export_type([author/0]).
-export_type([author_params/0]).

-include_lib("damsel/include/dmsl_domain_conf_v2_thrift.hrl").

-type vsn() :: {version, non_neg_integer()} | {head, dmsl_domain_conf_v2_thrift:'Head'()}.
-type vsn_created_at() :: dmsl_base_thrift:'Timestamp'().
-type version() :: base_version() | latest.
-type base_version() :: non_neg_integer().
-type operation() :: dmsl_domain_conf_v2_thrift:'Operation'().
-type object_ref() :: dmsl_domain_thrift:'Reference'().
-type versioned_object() :: dmsl_domain_conf_v2_thrift:'VersionedObject'().
-type domain_object() :: dmsl_domain_thrift:'ReflessDomainObject'().
-type commit_response() :: dmsl_domain_conf_v2_thrift:'CommitResponse'().
-type opts() :: #{
    transport_opts => woody_client_thrift_http_transport:transport_options(),
    woody_context => woody_context:ctx()
}.

-type author_id() :: dmsl_domain_conf_v2_thrift:'AuthorID'().
-type author() :: dmsl_domain_conf_v2_thrift:'Author'().
-type author_params() :: dmsl_domain_conf_v2_thrift:'AuthorParams'().

%%% API

-spec checkout_object(object_ref()) -> versioned_object() | no_return().
checkout_object(ObjectReference) ->
    checkout_object(ObjectReference, latest).

-spec checkout_object(object_ref(), version()) -> versioned_object() | no_return().
checkout_object(ObjectReference, Reference) ->
    checkout_object(ObjectReference, Reference, #{}).

-spec checkout_object(object_ref(), version(), opts()) -> versioned_object() | no_return().
checkout_object(ObjectReference, Reference, Opts) ->
    unwrap(do_checkout_object(ObjectReference, Reference, Opts)).

do_checkout_object(ObjectReference, Reference, Opts) ->
    Version = ref_to_version(Reference),
    dmt_client_cache:get_object(ObjectReference, Version, Opts).

-spec commit(base_version(), [operation()], author_id()) -> commit_response() | no_return().
commit(Version, Operations, AuthorID) ->
    commit(Version, Operations, AuthorID, #{}).

-spec commit(base_version(), [operation()], author_id(), opts()) -> commit_response() | no_return().
commit(Version, Operations, AuthorID, Opts) ->
    dmt_client_backend:commit(Version, Operations, AuthorID, Opts).

-spec get_latest_version() -> number() | no_return().
get_latest_version() ->
    dmt_client_backend:get_latest_version(#{}).

% AuthorManagement

-spec author_create(author_params(), opts()) -> author().
author_create(Params, Opts) ->
    dmt_client_author:create(Params, Opts).

-spec author_get(author_id()) -> author().
author_get(ID) ->
    dmt_client_author:get(ID).

-spec author_delete(author_id()) -> ok.
author_delete(ID) ->
    dmt_client_author:delete(ID).

%% Health check API

-spec health_check() -> erl_health:result().
health_check() ->
    try
        % TODO Come up with healthcheck
        {passing, #{}}
    catch
        _Class:_Error ->
            {critical, #{last_version => not_found}}
    end.

%%% Supervisor callbacks

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    Cache = #{
        id => dmt_client_cache,
        start => {dmt_client_cache, start_link, []},
        restart => permanent
    },
    {ok,
        {
            #{
                strategy => one_for_one,
                intensity => 10,
                period => 60
            },
            [Cache]
        }}.

%%% Application callbacks

-spec start(normal, any()) -> {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%%% Internal functions

unwrap({ok, Acc}) -> Acc;
unwrap({error, {woody_error, _} = Error}) -> erlang:error(Error);
unwrap({error, version_not_found = Reason}) -> erlang:error(Reason);
unwrap({error, object_not_found}) -> erlang:throw(#domain_conf_v2_ObjectNotFound{}).

-spec ref_to_version(version()) -> vsn().
ref_to_version(Version) when is_integer(Version) ->
    {version, Version};
ref_to_version(latest) ->
    {head, #domain_conf_v2_Head{}}.
