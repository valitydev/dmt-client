-module(dmt_client_tests_SUITE).

% -include_lib("stdlib/include/assert.hrl").

% -export([all/0]).
% -export([groups/0]).
% -export([init_per_suite/1]).
% -export([end_per_suite/1]).
% -export([insert_and_all_checkouts/1]).
% -export([inserts_updates_upserts_and_removes/1]).
% -export([cached_latest/1]).

% -include_lib("damsel/include/dmsl_domain_conf_thrift.hrl").

% %%
% %% tests descriptions
% %%
% -spec all() -> [term()].
% all() ->
%     [
%         {group, all}
%     ].

% -spec groups() -> [term()].
% groups() ->
%     [
%         {all, [sequence], [
%             insert_and_all_checkouts,
%             inserts_updates_upserts_and_removes,
%             cached_latest
%         ]}
%     ].

% %%
% %% starting/stopping
% -spec init_per_suite(term()) -> term().
% init_per_suite(C) ->
%     Apps = genlib_app:start_application_with(dmt_client, [
%         % milliseconds
%         {cache_update_interval, 5000},
%         {max_cache_size, #{
%             elements => 1,
%             % 2Kb
%             memory => 2048
%         }},
%         {service_urls, #{
%             'Repository' => <<"http://dominant:8022/v1/domain/repository">>,
%             'RepositoryClient' => <<"http://dominant:8022/v1/domain/repository_client">>
%         }}
%     ]),
%     [{apps, Apps} | C].

% -spec end_per_suite(term()) -> term().
% end_per_suite(C) ->
%     genlib_app:stop_unload_applications(proplists:get_value(apps, C)).

% %%
% %% tests
% -spec insert_and_all_checkouts(term()) -> _.
% insert_and_all_checkouts(_C) ->
%     Object = dmt_client_fixtures:fixture_category_object(1, <<"InsertFixture">>),
%     Ref = dmt_client_fixtures:fixture_category_ref(1),
%     #domain_conf_ObjectNotFound{} = (catch dmt_client:checkout_object(Ref)),
%     #domain_conf_Snapshot{version = Version1} = dmt_client:checkout(),
%     Version2 = dmt_client:insert(Object),
%     true = Version1 < Version2,
%     #domain_conf_Snapshot{version = Version2} = dmt_client:checkout(),
%     Object = dmt_client:checkout_object(Ref),
%     #domain_conf_VersionedObject{
%         version = Version2,
%         object = Object
%     } = dmt_client:checkout_versioned_object(latest, Ref).

% -spec inserts_updates_upserts_and_removes(term()) -> _.
% inserts_updates_upserts_and_removes(_C) ->
%     Cat1 = dmt_client_fixtures:fixture_category_object(10, <<"InsertFixture">>),
%     Cat1Modified = dmt_client_fixtures:fixture_category_object(10, <<"Modified">>),
%     Cat1ModifiedAgain = dmt_client_fixtures:fixture_category_object(10, <<"Strike Again">>),
%     Cat1Ref = dmt_client_fixtures:fixture_category_ref(10),

%     Cat2 = dmt_client_fixtures:fixture_category_object(11, <<"Another cat">>),
%     Cat2Ref = dmt_client_fixtures:fixture_category_ref(11),

%     Version1 = dmt_client:get_last_version(),
%     Version2 = dmt_client:insert(Cat1),
%     Cat1 = dmt_client:checkout_object(Cat1Ref),

%     Version3 = dmt_client:update(Cat1Modified),
%     #domain_conf_VersionedObject{
%         version = Version3,
%         object = Cat1Modified
%     } = dmt_client:checkout_versioned_object(Cat1Ref),

%     Version4 = dmt_client:upsert([Cat1ModifiedAgain, Cat2]),
%     Cat1ModifiedAgain = dmt_client:checkout_object(Cat1Ref),
%     Cat2 = dmt_client:checkout_object(Cat2Ref),

%     Version5 = dmt_client:remove(Cat1ModifiedAgain),
%     ?assertThrow(
%         #domain_conf_ObjectNotFound{},
%         dmt_client:checkout_object(Cat1Ref)
%     ),
%     Cat2 = dmt_client:checkout_object(Cat2Ref),

%     %% Check that all versions are strictly greater than previous ones
%     Versions = [
%         Version1,
%         Version2,
%         Version3,
%         Version4,
%         Version5
%     ],

%     ?assertEqual(Versions, ordsets:from_list(Versions)).

% -spec cached_latest(term()) -> _.
% cached_latest(_C) ->
%     Object = dmt_client_fixtures:fixture_category_object(100, <<"UpstreamLatest">>),
%     Commit = #domain_conf_Commit{ops = [{insert, #domain_conf_InsertOp{object = Object}}]},

%     %% Get around library to prevent cache update
%     Version = dmt_client:get_last_version(),
%     NewVersion = dmt_client_backend:commit(Version, Commit, #{}),

%     application:unset_env(dmt_client, use_cached_last_version),
%     Version = dmt_client:get_last_version(),
%     application:set_env(dmt_client, use_cached_last_version, true),
%     Version = dmt_client:get_last_version(),
%     application:set_env(dmt_client, use_cached_last_version, false),
%     NewVersion = dmt_client:get_last_version().
