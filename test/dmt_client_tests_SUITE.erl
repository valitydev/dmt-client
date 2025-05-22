-module(dmt_client_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_domain_conf_v2_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% Common test callbacks
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1
]).

%% Test cases
-export([
    get_latest_version/1,
    checkout_all/1,
    checkout_objects_by_type/1,
    checkout_nonexistent_object/1,
    checkout_object_by_version/1,
    checkout_latest_object/1,
    insert/1,
    update/1,
    upsert/1,
    remove/1,
    commit_insert_object/1,
    commit_update_object/1,
    commit_remove_object/1,
    commit_multiple_operations/1,
    version_sequence_operations/1,
    commit_conflict_handling/1
]).

-type config() :: ct_suite:ct_config().

%% CT callbacks

-spec all() -> [{group, atom()}].
all() ->
    [
        {group, sequential_operations},
        {group, parallel_operations}
    ].

-spec groups() -> [{atom(), list(), [atom()]}].
groups() ->
    [
        {sequential_operations, [], [
            get_latest_version,
            checkout_latest_object,
            version_sequence_operations,
            get_latest_version,
            insert,
            update,
            upsert,
            remove,
            checkout_all,
            checkout_objects_by_type
        ]},
        {parallel_operations, [parallel], [
            checkout_nonexistent_object,
            checkout_object_by_version,
            commit_insert_object,
            commit_update_object,
            commit_remove_object,
            commit_multiple_operations,
            commit_conflict_handling
        ]}
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    Apps = genlib_app:start_application_with(dmt_client, [
        {max_cache_size, #{
            elements => 20,
            % 50MB
            memory => 52428800
        }},
        {service_urls, #{
            'Repository' => <<"http://dmt:8022/v1/domain/repository">>,
            'RepositoryClient' => <<"http://dmt:8022/v1/domain/repository_client">>,
            'AuthorManagement' => <<"http://dmt:8022/v1/domain/author">>
        }}
    ]),
    [{apps, Apps} | Config].

-spec end_per_suite(config()) -> any().
end_per_suite(Config) ->
    genlib_app:stop_unload_applications(?config(apps, Config)).

%% Tests

-spec checkout_nonexistent_object(config()) -> _.
checkout_nonexistent_object(_Config) ->
    Ref = make_domain_ref(),
    ?assertThrow(
        #domain_conf_v2_ObjectNotFound{},
        dmt_client:checkout_object(Ref)
    ).

-spec checkout_object_by_version(config()) -> _.
checkout_object_by_version(_Config) ->
    {Ref, RLObject, Object} = make_test_object(make_domain_ref()),
    #domain_conf_v2_CommitResponse{version = Version} =
        commit_insert(RLObject, Ref),

    % Check we can get object by specific version
    #domain_conf_v2_VersionedObject{
        object = Result
    } = dmt_client:checkout_object(Version, Ref),
    ?assertEqual(Object, Result).

-spec get_latest_version(config()) -> _.
get_latest_version(_Config) ->
    ?assertMatch(Version when is_integer(Version) andalso Version > 0, dmt_client:get_latest_version()).

-spec insert(config()) -> _.
insert(_Config) ->
    ?assertMatch(
        Version when is_integer(Version) andalso Version > 0,
        dmt_client:insert(latest, [make_object()], make_author_id(), #{})
    ).

make_object() ->
    element(3, make_test_object(make_domain_ref())).

-spec update(config()) -> _.
update(_Config) ->
    {Version0, _, [Object]} = setup_existing_for_update(1),
    ?assertMatch(
        Version1 when Version1 > Version0,
        dmt_client:update(latest, [Object], make_author_id(), #{})
    ).

setup_existing_for_update(Count) ->
    InsertedObjects = [make_object() || _ <- lists:seq(1, Count)],
    Version = dmt_client:insert(latest, InsertedObjects, make_author_id(), #{}),
    ModifiedObjects = lists:map(
        fun({Type, {ObjectTag, Ref, _}}) ->
            {Type, {ObjectTag, _, Data}} = make_object(),
            %% Make object with changed data
            {Type, {ObjectTag, Ref, Data}}
        end,
        InsertedObjects
    ),
    {Version, InsertedObjects, ModifiedObjects}.

-spec upsert(config()) -> _.
upsert(_Config) ->
    {Version0, _, Objects0} = setup_existing_for_update(3),
    Objects1 = lists_shuffle([make_object(), make_object() | Objects0]),
    ?assertMatch(
        Version1 when Version1 > Version0,
        dmt_client:upsert(latest, Objects1, make_author_id(), #{})
    ).

lists_shuffle(L) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- L])].

-spec remove(config()) -> _.
remove(_Config) ->
    {Version0, _, Objects} = setup_existing_for_update(3),
    ?assertMatch(
        Version1 when Version1 > Version0,
        dmt_client:remove(latest, Objects, make_author_id(), #{})
    ),
    [
        ?assertThrow(#domain_conf_v2_ObjectNotFound{}, dmt_client:checkout_object(dmt_client_object:get_ref(Object)))
     || Object <- Objects
    ].

-spec checkout_all(config()) -> _.
checkout_all(_Config) ->
    ?assertMatch([_ | _], dmt_client:checkout_all(latest, #{})).

-spec checkout_objects_by_type(config()) -> _.
checkout_objects_by_type(_Config) ->
    Ops = [
        {insert, #domain_conf_v2_InsertOp{object = {'document_type', #domain_DocumentType{name = ~b"document type"}}}}
     || _ <- lists:seq(1, 42)
    ],
    _ = dmt_client:commit(dmt_client:get_latest_version(), Ops, make_author_id()),
    ?assertMatch(
        VersionedObjects when length(VersionedObjects) >= 42,
        dmt_client:checkout_objects_by_type(latest, 'document_type', #{})
    ).

-spec checkout_latest_object(config()) -> _.
checkout_latest_object(_Config) ->
    {Ref, RLObject, Object} = make_test_object(make_domain_ref()),
    _Version = commit_insert(RLObject, Ref),

    % Check we can get latest version
    #domain_conf_v2_VersionedObject{
        object = Result
    } = dmt_client:checkout_object(latest, Ref),
    ?assertEqual(Object, Result).

-spec commit_insert_object(config) -> _.
commit_insert_object(_Config) ->
    {Ref, ROObject, Object} = make_test_object(make_domain_ref()),
    #domain_conf_v2_CommitResponse{version = Version} =
        commit_insert(ROObject, Ref),
    ?assert(is_integer(Version)),

    #domain_conf_v2_VersionedObject{
        object = Result
    } = dmt_client:checkout_object(Ref),
    ?assertEqual(Object, Result).

-spec commit_update_object(config()) -> _.
commit_update_object(_Config) ->
    {Ref, ROObject, _} = make_test_object(make_domain_ref()),
    #domain_conf_v2_CommitResponse{version = Version1} =
        commit_insert(ROObject, Ref),

    {_, _, Object2} = make_test_object(Ref, <<"new_name">>),
    #domain_conf_v2_CommitResponse{version = Version2} =
        commit_update(Version1, Object2),
    ?assert(Version2 > Version1),

    #domain_conf_v2_VersionedObject{
        object = Result
    } = dmt_client:checkout_object(Ref),
    ?assertEqual(Object2, Result).

-spec commit_remove_object(config()) -> _.
commit_remove_object(_Config) ->
    {Ref, ROObject, _} = make_test_object(make_domain_ref()),
    #domain_conf_v2_CommitResponse{version = Version1} =
        commit_insert(ROObject, Ref),

    #domain_conf_v2_CommitResponse{version = Version2} =
        commit_remove(Version1, Ref),
    ?assert(Version2 > Version1),

    ?assertThrow(
        #domain_conf_v2_ObjectNotFound{},
        dmt_client:checkout_object(Version2, Ref)
    ).

-spec commit_multiple_operations(config()) -> _.
commit_multiple_operations(_Config) ->
    {Ref1, _, Object1} = Obj1 = make_test_object(make_domain_ref()),
    {Ref2, _, Object2} = Obj2 = make_test_object(make_domain_ref()),

    #domain_conf_v2_CommitResponse{} =
        commit_batch_insert([Obj1, Obj2]),

    #domain_conf_v2_VersionedObject{
        object = Result1
    } = dmt_client:checkout_object(Ref1),
    #domain_conf_v2_VersionedObject{
        object = Result2
    } = dmt_client:checkout_object(Ref2),
    ?assertEqual(Object1, Result1),
    ?assertEqual(Object2, Result2).

-spec commit_conflict_handling(config()) -> _.
commit_conflict_handling(_Config) ->
    {Ref, RLObject, _} = make_test_object(make_domain_ref()),
    #domain_conf_v2_CommitResponse{version = Version1} =
        commit_insert(RLObject, Ref),

    % Try to insert object with same reference
    ?assertThrow(
        #domain_conf_v2_OperationConflict{
            conflict =
                {object_already_exists, #domain_conf_v2_ObjectAlreadyExistsConflict{
                    object_ref = Ref
                }}
        },
        commit_insert(Version1, RLObject, Ref)
    ).

-spec version_sequence_operations(config()) -> _.
version_sequence_operations(_Config) ->
    {Ref, RLObject, Object1} = make_test_object(make_domain_ref()),

    % First insert

    #domain_conf_v2_CommitResponse{version = Version1} =
        commit_insert(RLObject, Ref),
    #domain_conf_v2_VersionedObject{object = Result1} =
        dmt_client:checkout_object(latest, Ref),
    {category, #domain_CategoryObject{data = Data1}} = Result1,
    ?assertEqual(Object1, Result1),

    % Update
    {category, IRef} = Ref,
    Object2 =
        {category, #domain_CategoryObject{
            ref = IRef,
            data = Data1#domain_Category{
                description = <<"new_desc">>
            }
        }},
    Version2 = commit_update(Version1, Object2),
    ?assert(Version2 > Version1),

    #domain_conf_v2_VersionedObject{object = Result2} =
        dmt_client:checkout_object(latest, Ref),
    ?assertEqual(Object2, Result2),

    % Verify we can still get old version

    #domain_conf_v2_VersionedObject{object = Historical} =
        dmt_client:checkout_object(Version1, Ref),
    ?assertEqual(Object1, Historical).

%% Internal functions

make_domain_ref() ->
    {category, #domain_CategoryRef{id = rand:uniform(10000)}}.

make_test_object({category, #domain_CategoryRef{id = ID}} = Ref) ->
    Name = erlang:integer_to_binary(ID),
    make_test_object(Ref, Name).

make_test_object({category, CategoryRef} = FullRef, Name) ->
    Category = #domain_Category{
        name = Name,
        description = <<"Test category">>
    },
    ReflessObject = {category, Category},
    Object = {category, #domain_CategoryObject{ref = CategoryRef, data = Category}},
    {FullRef, ReflessObject, Object}.

make_author_id() ->
    dmt_client:create_author(genlib:unique(), genlib:unique()).

commit_insert(Object, Ref) ->
    commit_insert(1, Object, Ref).

commit_insert(Version, Object, Ref) ->
    % Get version from any existing object or start with 0
    Op = {insert, #domain_conf_v2_InsertOp{object = Object, force_ref = Ref}},
    AuthorID = make_author_id(),
    dmt_client:commit(Version, [Op], AuthorID).

commit_update(Version, Object) ->
    Op = {update, #domain_conf_v2_UpdateOp{object = Object}},
    AuthorID = make_author_id(),
    dmt_client:commit(Version, [Op], AuthorID).

commit_remove(Version, Ref) ->
    Op = {remove, #domain_conf_v2_RemoveOp{ref = Ref}},
    AuthorID = make_author_id(),
    dmt_client:commit(Version, [Op], AuthorID).

commit_batch_insert(Objects) ->
    Version = 1,
    Ops = [
        {insert, #domain_conf_v2_InsertOp{
            object = Obj,
            force_ref = Ref
        }}
     || {Ref, Obj, _} <- Objects
    ],
    AuthorID = make_author_id(),
    dmt_client:commit(Version, Ops, AuthorID).
