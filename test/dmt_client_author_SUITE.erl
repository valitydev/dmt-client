-module(dmt_client_author_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_domain_conf_v2_thrift.hrl").

%% Common test callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
%% Test cases
-export([
    create_author/1,
    get_author/1,
    get_author_by_email/1,
    delete_author/1,
    get_nonexistent_author/1,
    delete_nonexistent_author/1,
    create_duplicate_email/1
]).

-type config() :: ct_suite:ct_config().

%% CT callbacks

-spec all() -> [{group, atom()}].
all() ->
    [{group, basic_operations}, {group, error_cases}].

-spec groups() -> [{atom(), list(), [atom()]}].
groups() ->
    [
        {basic_operations, [], [
            create_author,
            get_author,
            get_author_by_email,
            delete_author
        ]},
        {error_cases, [parallel], [
            get_nonexistent_author, delete_nonexistent_author, create_duplicate_email
        ]}
    ].

%% Group callbacks

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    Apps0 = genlib_app:start_application(woody),
    Apps1 = start_dmt_client(),
    % ok = application:get_all_env(dmt_client),
    [{apps, Apps0 ++ Apps1} | Config].

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
    stop_dmt_client(Config),
    ok.

%% Test cases

-spec create_author(config()) -> _.
create_author(_Config) ->
    % ok = dmt_client:checkout_object({category, #domain_CategoryRef{id = 1}}),
    Email = generate_email(),
    Name = generate_name(),
    Params = #domain_conf_v2_AuthorParams{email = Email, name = Name},
    Author = dmt_client_author:create(Params, #{}),
    ?assertMatch(
        #domain_conf_v2_Author{
            id = _,
            email = Email,
            name = Name
        },
        Author
    ).

-spec get_author(config()) -> _.
get_author(_Config) ->
    % Create author first
    Params = #domain_conf_v2_AuthorParams{email = generate_email(), name = generate_name()},
    #domain_conf_v2_Author{id = ID} = dmt_client_author:create(Params, #{}),

    % Get and verify
    Author = dmt_client_author:get(ID),
    ?assertEqual(
        Params#domain_conf_v2_AuthorParams.email,
        Author#domain_conf_v2_Author.email
    ),
    ?assertEqual(Params#domain_conf_v2_AuthorParams.name, Author#domain_conf_v2_Author.name).

-spec get_author_by_email(config()) -> _.
get_author_by_email(_Config) ->
    Email = generate_email(),
    Params = #domain_conf_v2_AuthorParams{email = Email, name = generate_name()},
    #domain_conf_v2_Author{id = ID} = dmt_client_author:create(Params, #{}),

    % Get and verify
    Author = dmt_client_author:get_by_email(Email),
    ?assertEqual(Email, Author#domain_conf_v2_Author.email),
    ?assertEqual(ID, Author#domain_conf_v2_Author.id),
    ?assertEqual(Params#domain_conf_v2_AuthorParams.name, Author#domain_conf_v2_Author.name).

-spec delete_author(config()) -> _.
delete_author(_Config) ->
    % Create author
    Params = #domain_conf_v2_AuthorParams{email = generate_email(), name = generate_name()},
    #domain_conf_v2_Author{id = ID} = dmt_client_author:create(Params, #{}),

    % Delete
    ?assertEqual(ok, dmt_client_author:delete(ID)),

    % Verify it's gone
    ?assertThrow(#domain_conf_v2_AuthorNotFound{}, dmt_client_author:get(ID)).

-spec get_nonexistent_author(config()) -> _.
get_nonexistent_author(_Config) ->
    ?assertThrow(
        #domain_conf_v2_AuthorNotFound{},
        dmt_client_author:get(<<"nonexistent_id">>)
    ).

-spec delete_nonexistent_author(config()) -> _.
delete_nonexistent_author(_Config) ->
    ?assertThrow(
        #domain_conf_v2_AuthorNotFound{},
        dmt_client_author:delete(<<"nonexistent_id">>)
    ).

-spec create_duplicate_email(config()) -> _.
create_duplicate_email(_Config) ->
    Email = generate_email(),
    Params1 = #domain_conf_v2_AuthorParams{email = Email, name = generate_name()},
    _Author1 = dmt_client_author:create(Params1, #{}),

    % Try to create another author with same email
    Params2 = #domain_conf_v2_AuthorParams{email = Email, name = generate_name()},
    ?assertThrow(
        #domain_conf_v2_AuthorAlreadyExists{},
        dmt_client_author:create(Params2, #{})
    ).

%% Internal functions

-spec start_dmt_client() -> [atom()].
start_dmt_client() ->
    genlib_app:start_application_with(dmt_client, [
        {service_urls, #{
            'Repository' => <<"http://dmt:8022/v1/domain/repository">>,
            'RepositoryClient' => <<"http://dmt:8022/v1/domain/repository_client">>,
            'AuthorManagement' => <<"http://dmt:8022/v1/domain/author">>
        }}
    ]).

-spec stop_dmt_client(config()) -> ok.
stop_dmt_client(Config) ->
    genlib_app:stop_unload_applications(?config(apps, Config)).

generate_email() ->
    ID = genlib:unique(),
    erlang:iolist_to_binary([ID, <<"@example.com">>]).

generate_name() ->
    ID = genlib:unique(),
    erlang:iolist_to_binary([<<"Author ">>, ID]).
