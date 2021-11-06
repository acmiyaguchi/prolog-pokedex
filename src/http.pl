:- module(app, [run/2]).

% https://stackoverflow.com/questions/36966024/running-prolog-on-a-server-from-php-or-make-it-daemon
:- use_module(library(http/http_server)).
:- use_module(library(http/http_client)).
:- use_module(reload).

list_pokemon(Data) :- list_pokemon(Data, 20).
list_pokemon(Data, Limit) :-
    % https://www.swi-prolog.org/pldoc/doc/_SWI_/library/http/http_client.pl
    BaseUrl = 'https://pokeapi.co/api/v2/',
    format(atom(Url), '~a/~a?limit=~d', [BaseUrl, 'pokemon', Limit]),
    writeln(user_output, Url),
    http_get(Url, Data, [json_object(dict)]),
    writeln(user_output, Data).

% DCG rules?
% https://stackoverflow.com/questions/24586960/dynamic-html-with-prolog
% https://www.swi-prolog.org/pldoc/man?section=html-write-examples
table_row([]) --> [].
table_row([Row|Rest]) -->
    html([
        tr([
            td(a([href=Row.get(url)], Row.get(name)))
        ])
    ]),
    table_row(Rest).

home_page(_Request) :-
    list_pokemon(Data),
    Results = Data.get(results),
    PageTitle = 'Pokedex served with Prolog',
    reply_html_page(
        title(PageTitle),
        [
            h1(PageTitle),
            p(['This page serves up facts about pokemon using ',
                a([href='https://pokeapi.co'], 'PokeAPI'), '.']),
            p(['There are ', Data.get(count), ' pokemon available.']),
            table([border(1), align(center), width('80%')], [
                thead(tr([th(name)])),
                tbody([\table_row(Results)])
            ])
        ]
    ).

:- http_handler(
    root(.),
    http_redirect(moved, location_by_id(home_page)),
    []
).
:- http_handler(root(home), home_page, []).

run(Port, Dir) :-
    % first load then server on port 8080
    http_server([port(Port)]),
    % watch prolog files in the current directory for whether to restart the server
    reload_source(Dir).
