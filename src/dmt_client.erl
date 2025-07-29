%%% @doc Public API, supervisor and application startup.
%%% @end

-module(dmt_client).

-behaviour(supervisor).
-behaviour(application).

%% API
-export([checkout_all/1]).
-export([checkout_all/2]).
-export([checkout_object/1]).
-export([checkout_object/2]).
-export([checkout_object/3]).
-export([checkout_objects_by_type/2]).
-export([checkout_objects_by_type/3]).
-export([commit/3]).
-export([commit/4]).
-export([get_latest_version/0]).
-export([insert/2]).
-export([insert/3]).
-export([insert/4]).
-export([update/2]).
-export([update/3]).
-export([update/4]).
-export([upsert/2]).
-export([upsert/3]).
-export([upsert/4]).
-export([remove/2]).
-export([remove/3]).
-export([remove/4]).

% AuthorManagement API

-export([create_author/2]).
-export([create_author/3]).
-export([get_author/1]).
-export([get_author_by_email/1]).
-export([delete_author/1]).

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
-export_type([ref/0]).
-export_type([versioned_object/0]).
-export_type([operation/0]).
-export_type([commit_response/0]).
-export_type([object_ref/0]).
-export_type([domain_object/0]).
-export_type([refless_domain_object/0]).
-export_type([opts/0]).

-export_type([author_id/0]).
-export_type([author_email/0]).
-export_type([author/0]).
-export_type([author_params/0]).

-export_type([object_type/0]).
-export_type([search_pattern/0]).
-export_type([limit/0]).
-export_type([continuation_token/0]).
-export_type([search_full_response/0]).

-include_lib("damsel/include/dmsl_domain_conf_v2_thrift.hrl").

-type vsn_created_at() :: dmsl_base_thrift:'Timestamp'().
-type version() :: vsn() | latest.
-type vsn() :: dmsl_domain_conf_v2_thrift:'Version'().
-type ref() :: dmsl_domain_conf_v2_thrift:'VersionReference'().
-type operation() :: dmsl_domain_conf_v2_thrift:'Operation'().
-type object_ref() :: dmsl_domain_thrift:'Reference'().
-type versioned_object() :: dmsl_domain_conf_v2_thrift:'VersionedObject'().
-type refless_domain_object() :: dmsl_domain_thrift:'ReflessDomainObject'().
-type domain_object() :: dmsl_domain_thrift:'DomainObject'().
-type commit_response() :: dmsl_domain_conf_v2_thrift:'CommitResponse'().
-type opts() :: #{
    transport_opts => woody_client_thrift_http_transport:transport_options(),
    woody_context => woody_context:ctx()
}.

-type author_id() :: dmsl_domain_conf_v2_thrift:'AuthorID'().
-type author_name() :: dmsl_domain_conf_v2_thrift:'AuthorName'().
-type author_email() :: dmsl_domain_conf_v2_thrift:'AuthorEmail'().
-type author() :: dmsl_domain_conf_v2_thrift:'Author'().
-type author_params() :: dmsl_domain_conf_v2_thrift:'AuthorParams'().

%% TODO Move those to backend behaviour maybe?
-type object_type() :: dmsl_domain_thrift:'DomainObjectType'().
-type search_pattern() :: binary().
-type limit() :: pos_integer().
-type continuation_token() :: dmsl_domain_conf_v2_thrift:'ContinuationToken'().
-type search_full_response() :: dmsl_domain_conf_v2_thrift:'SearchFullResponse'().

%%% API

-spec checkout_all(version()) -> [versioned_object()].
checkout_all(Reference) ->
    checkout_all(Reference, #{}).

-spec checkout_all(version(), opts()) -> [versioned_object()].
checkout_all(Reference, Opts) ->
    do_search(Reference, undefined, Opts).

-spec checkout_object(object_ref()) -> versioned_object() | no_return().
checkout_object(ObjectReference) ->
    checkout_object(latest, ObjectReference).

-spec checkout_object(version(), object_ref()) -> versioned_object() | no_return().
checkout_object(Reference, ObjectReference) ->
    checkout_object(Reference, ObjectReference, #{}).

-spec checkout_object(version(), object_ref(), opts()) -> versioned_object() | no_return().
checkout_object(Reference, ObjectReference, Opts) ->
    unwrap(do_checkout_object(Reference, ObjectReference, Opts)).

-spec checkout_objects_by_type(object_type(), opts()) -> [versioned_object()] | no_return().
checkout_objects_by_type(Type, Opts) ->
    checkout_objects_by_type(latest, Type, Opts).

-spec checkout_objects_by_type(version(), object_type(), opts()) -> [versioned_object()] | no_return().
checkout_objects_by_type(Reference, Type, Opts) ->
    do_search(Reference, Type, Opts).

%% This defines how my objects are retrieved with each search request.
-define(CHUNK_SIZE, 100).
-define(WILDCARD, ~b"*").

do_search(Reference, Type, Opts) ->
    Version = ref_to_version(Reference),
    VersionedObjects = search_and_collect_objects(Version, ?WILDCARD, Type, ?CHUNK_SIZE, Opts),
    ok = dmt_client_cache:update_with_objects(Version, VersionedObjects),
    VersionedObjects.

do_checkout_object(Reference, ObjectReference, Opts) ->
    Version = ref_to_version(Reference),
    case {Version, dmt_client_cache:get_object(ObjectReference, Version, Opts)} of
        %% NOTE Absence of object in virtual version 0 must be
        %% interpreted accordingly.
        {0, {error, version_not_found}} -> {error, object_not_found};
        {_, Result} -> Result
    end.

-spec commit(version(), [operation()], author_id()) -> commit_response() | no_return().
commit(Version, Operations, AuthorID) ->
    commit(Version, Operations, AuthorID, #{}).

-spec commit(version(), [operation()], author_id(), opts()) -> commit_response() | no_return().
commit(Reference, Operations, AuthorID, Opts) ->
    Version = ref_to_version(Reference),
    dmt_client_backend:commit(Version, Operations, AuthorID, Opts).

-spec get_latest_version() -> vsn() | no_return().
get_latest_version() ->
    dmt_client_backend:get_latest_version(#{}).

-spec insert(domain_object() | [domain_object()], author_id()) -> vsn() | no_return().
insert(Objects, AuthorID) ->
    insert(latest, Objects, AuthorID).

-spec insert(version(), domain_object() | [domain_object()], author_id()) -> vsn() | no_return().
insert(Reference, Object, AuthorID) when not is_list(Object) ->
    insert(Reference, [Object], AuthorID);
insert(Reference, Objects, AuthorID) ->
    insert(Reference, Objects, AuthorID, #{}).

-spec insert(version(), [domain_object()], author_id(), opts()) -> vsn() | no_return().
insert(Reference, Objects, AuthorID, Opts) ->
    Operations = [
        {insert, #domain_conf_v2_InsertOp{
            object = dmt_client_object:make_refless(Object),
            force_ref = dmt_client_object:get_ref(Object)
        }}
     || Object <- Objects
    ],
    #domain_conf_v2_CommitResponse{version = NewVersion} = commit(Reference, Operations, AuthorID, Opts),
    %% TODO Update local cache after successful commit
    NewVersion.

-spec update(domain_object() | [domain_object()], author_id()) -> vsn() | no_return().
update(Objects, AuthorID) ->
    update(latest, Objects, AuthorID).

-spec update(version(), domain_object() | [domain_object()], author_id()) -> vsn() | no_return().
update(Reference, Object, AuthorID) when not is_list(Object) ->
    update(Reference, [Object], AuthorID);
update(Reference, Objects, AuthorID) ->
    update(Reference, Objects, AuthorID, #{}).

-spec update(version(), [domain_object()], author_id(), opts()) -> vsn() | no_return().
update(Reference, NewObjects, AuthorID, Opts) ->
    Operations = [
        {update, #domain_conf_v2_UpdateOp{object = NewObject}}
     || NewObject <- NewObjects
    ],
    #domain_conf_v2_CommitResponse{version = NewVersion} = commit(Reference, Operations, AuthorID, Opts),
    %% TODO Update local cache after successful commit
    NewVersion.

-spec upsert(domain_object() | [domain_object()], author_id()) -> vsn() | no_return().
upsert(Objects, AuthorID) ->
    upsert(latest, Objects, AuthorID).

-spec upsert(version(), domain_object() | [domain_object()], author_id()) -> vsn() | no_return().
upsert(Reference, Object, AuthorID) when not is_list(Object) ->
    upsert(Reference, [Object], AuthorID);
upsert(Reference, Objects, AuthorID) ->
    upsert(Reference, Objects, AuthorID, #{}).

-spec upsert(version(), [domain_object()], author_id(), opts()) -> vsn() | no_return().
upsert(Reference, NewObjects, AuthorID, Opts) ->
    Version = ref_to_version(Reference),
    Operations = lists:foldl(
        fun(NewObject, Ops) ->
            ObjectRef = dmt_client_object:get_ref(NewObject),
            case do_checkout_object(Version, ObjectRef, Opts) of
                {error, version_not_found = Reason} ->
                    erlang:error(Reason);
                {ok, #domain_conf_v2_VersionedObject{object = NewObject}} ->
                    Ops;
                {ok, #domain_conf_v2_VersionedObject{object = _OldObject}} ->
                    [
                        {update, #domain_conf_v2_UpdateOp{object = NewObject}}
                        | Ops
                    ];
                {error, object_not_found} ->
                    [
                        {insert, #domain_conf_v2_InsertOp{
                            object = dmt_client_object:make_refless(NewObject),
                            force_ref = ObjectRef
                        }}
                        | Ops
                    ]
            end
        end,
        [],
        NewObjects
    ),
    %% FIXME Race condition:
    %% - If version reference is "latest" then insert operations can
    %% become rancid, which can lead to insertion conflicts.
    %% - If version is specified number then whole commit operations
    %% can become old.
    #domain_conf_v2_CommitResponse{version = NewVersion} = commit(Version, Operations, AuthorID, Opts),
    %% TODO Update local cache after successful commit
    NewVersion.

-spec remove(domain_object() | [domain_object()], author_id()) -> vsn() | no_return().
remove(Objects, AuthorID) ->
    remove(latest, Objects, AuthorID).

-spec remove(version(), domain_object() | [domain_object()], author_id()) -> vsn() | no_return().
remove(Reference, Object, AuthorID) when not is_list(Object) ->
    remove(Reference, [Object], AuthorID);
remove(Reference, Objects, AuthorID) ->
    remove(Reference, Objects, AuthorID, #{}).

-spec remove(version(), [domain_object()], author_id(), opts()) -> vsn() | no_return().
remove(Reference, Objects, AuthorID, Opts) ->
    Operations = [
        {remove, #domain_conf_v2_RemoveOp{ref = dmt_client_object:get_ref(Object)}}
     || Object <- Objects
    ],
    #domain_conf_v2_CommitResponse{version = NewVersion} = commit(Reference, Operations, AuthorID, Opts),
    %% TODO Update local cache after successful commit
    NewVersion.

%% AuthorManagement

-spec create_author(author_name(), author_email()) -> dmt_client_author:author_id() | no_return().
create_author(Name, Email) ->
    create_author(Name, Email, #{}).

-spec create_author(author_name(), author_email(), dmt_client_author:opts()) ->
    dmt_client_author:author_id() | no_return().
create_author(Name, Email, Opts) ->
    Params = #domain_conf_v2_AuthorParams{email = Email, name = Name},
    #domain_conf_v2_Author{id = ID} =
        dmt_client_author:create(Params, Opts),
    ID.

-spec get_author(author_id()) -> author().
get_author(ID) ->
    dmt_client_author:get(ID).

-spec get_author_by_email(author_email()) -> author().
get_author_by_email(Email) ->
    dmt_client_author:get_by_email(Email).

-spec delete_author(author_id()) -> ok.
delete_author(ID) ->
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

ref_to_version(Version) when is_integer(Version) ->
    Version;
ref_to_version(latest) ->
    get_latest_version().

search_and_collect_objects(Version, Pattern, Type, Limit, Opts) ->
    Getter = fun(Token) ->
        dmt_client_backend:search(Version, Pattern, Type, Limit, Token, Opts)
    end,
    collect_objects(Getter(undefined), [], Getter).

-define(SEARCH_RESULT(Chunk, Token), #domain_conf_v2_SearchFullResponse{result = Chunk, continuation_token = Token}).

collect_objects(?SEARCH_RESULT(Chunk, Token), Chunks, _Getter) when Token =:= undefined ->
    lists:flatten(lists:reverse([Chunk | Chunks]));
collect_objects(?SEARCH_RESULT(Chunk, Token), Chunks, Getter) ->
    collect_objects(Getter(Token), [Chunk | Chunks], Getter).
