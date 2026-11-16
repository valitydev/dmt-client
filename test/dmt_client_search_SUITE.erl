-module(dmt_client_search_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_domain_conf_v2_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([search_text/1, search_by_type/1, search_by_version/1, search_pagination/1, search_empty/1]).

-type config() :: ct_suite:ct_config().

-spec all() -> [atom()].
all() ->
    [search_text, search_by_type, search_by_version, search_pagination, search_empty].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    Apps = genlib_app:start_application_with(dmt_client, [
        {max_cache_size, #{elements => 20, memory => 52428800}},
        {service_urls, #{
            'Repository' => <<"http://dmt:8022/v1/domain/repository">>,
            'RepositoryClient' => <<"http://dmt:8022/v1/domain/repository_client">>,
            'AuthorManagement' => <<"http://dmt:8022/v1/domain/author">>
        }}
    ]),
    AuthorID = dmt_client:create_author(genlib:unique(), genlib:unique()),
    Query = unique_query(),
    PageQuery = unique_query(),
    Other = unique_query(),
    {Version, Objects} = insert_objects(
        [
            {category, #domain_Category{name = Query, description = <<"Name match">>}},
            {category, #domain_Category{name = Other, description = Query}},
            {document_type, #domain_DocumentType{name = Query}},
            {category, #domain_Category{name = Other, description = <<"No match">>}}
        ],
        AuthorID
    ),
    %% More than two client chunks: search/3,4 must follow continuation tokens.
    {_, PageObjects} = insert_objects(
        [
            {document_type, #domain_DocumentType{name = PageQuery}}
         || _ <- lists:seq(1, 205)
        ],
        AuthorID
    ),
    %% A new matching object must not appear when searching the earlier version.
    {LatestVersion, LaterObjects} = insert_objects(
        [
            {category, #domain_Category{name = Query, description = <<"Later match">>}}
        ],
        AuthorID
    ),
    Matches = [
        Object
     || Object <- Objects,
        case Object of
            {category, #domain_CategoryObject{data = #domain_Category{name = Name, description = Description}}} ->
                Name =:= Query orelse Description =:= Query;
            {document_type, _} ->
                true
        end
    ],
    [
        {apps, Apps},
        {author_id, AuthorID},
        {query, Query},
        {page_query, PageQuery},
        {version, Version},
        {latest_version, LatestVersion},
        {matches, Matches},
        {later_objects, LaterObjects},
        {page_objects, PageObjects},
        {all_objects, Objects ++ PageObjects ++ LaterObjects}
        | Config
    ].

-spec end_per_suite(config()) -> any().
end_per_suite(Config) ->
    try
        %% Remove only this suite's objects; do not reset the shared database.
        dmt_client:remove(latest, ?config(all_objects, Config), ?config(author_id, Config), #{})
    after
        genlib_app:stop_unload_applications(?config(apps, Config))
    end.

-spec search_text(config()) -> _.
search_text(Config) ->
    Expected = ?config(matches, Config) ++ ?config(later_objects, Config),
    %% Matches in both name and description, across different object types.
    Result = dmt_client:search(?config(query, Config), undefined, opts()),
    assert_objects(Expected, Result).

-spec search_by_type(config()) -> _.
search_by_type(Config) ->
    Expected = [Object || {category, _} = Object <- ?config(matches, Config)] ++ ?config(later_objects, Config),
    Result = dmt_client:search(latest, ?config(query, Config), category, opts()),
    assert_objects(Expected, Result).

-spec search_by_version(config()) -> _.
search_by_version(Config) ->
    Version = ?config(version, Config),
    Result = dmt_client:search(Version, ?config(query, Config), undefined, opts()),
    assert_objects(?config(matches, Config), Result),
    lists:foreach(
        fun(#domain_conf_v2_VersionedObject{info = #domain_conf_v2_VersionedObjectInfo{version = ObjectVersion}}) ->
            ?assertEqual(Version, ObjectVersion)
        end,
        Result
    ).

-spec search_pagination(config()) -> _.
search_pagination(Config) ->
    Expected = ?config(page_objects, Config),
    Query = ?config(page_query, Config),
    Version = ?config(latest_version, Config),
    ?assertEqual(205, length(Expected)),
    %% Exercise both public entry points against real paginated responses.
    assert_objects(Expected, dmt_client:search(Query, document_type, opts())),
    assert_objects(Expected, dmt_client:search(Version, Query, document_type, opts())).

-spec search_empty(config()) -> _.
search_empty(Config) ->
    Query = unique_query(),
    ?assertEqual([], dmt_client:search(Query, undefined, opts())),
    ?assertEqual([], dmt_client:search(?config(version, Config), Query, category, opts())).

%% Internal functions

insert_objects(Objects, AuthorID) ->
    Operations = [{insert, #domain_conf_v2_InsertOp{object = Object}} || Object <- Objects],
    #domain_conf_v2_CommitResponse{version = Version, new_objects = NewObjects} =
        dmt_client:commit(latest, Operations, AuthorID),
    {Version, ordsets:to_list(NewObjects)}.

assert_objects(Expected, Result) ->
    Actual = [Object || #domain_conf_v2_VersionedObject{object = Object} <- Result],
    ?assertEqual(length(Expected), length(Result)),
    ?assertEqual(lists:sort(Expected), lists:sort(Actual)).

opts() ->
    #{transport_opts => #{recv_timeout => 60000}, woody_context => woody_context:new()}.

unique_query() ->
    %% One alphanumeric lexeme, isolated from fixtures of other suites/runs.
    <<"search", (integer_to_binary(erlang:system_time(nanosecond)))/binary,
        (integer_to_binary(erlang:unique_integer([positive, monotonic])))/binary>>.
