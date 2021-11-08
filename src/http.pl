:- module(app, [run/1, debug_enabled/0]).

% https://stackoverflow.com/questions/36966024/running-prolog-on-a-server-from-php-or-make-it-daemon
:- use_module(library(http/http_server)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_parameters)).
:- use_module(library(git)).

debug_enabled :-
    (
        getenv('DEBUG', Value) ->
            atom_string(Debug, Value),
            must_be(boolean, Debug);
            Debug = false
    ),
    Debug.

list_pokemon(Data) :- list_pokemon(Data, 20, 0).
list_pokemon(Data, Limit, Offset) :-
    % https://www.swi-prolog.org/pldoc/doc/_SWI_/library/http/http_client.pl
    BaseUrl = 'https://pokeapi.co/api/v2/',
    format(atom(Url), '~a/~a?limit=~d&offset=~d', [BaseUrl, 'pokemon', Limit, Offset]),
    http_get(Url, Data, [json_object(dict)]),

    (
        debug_enabled ->
            writeln(user_output, Url),
            writeln(user_output, Data);
        true
    ).

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

previous_page(CurrentPage) -->
    {
        CurrentPage > 1,
        PreviousPage is CurrentPage - 2
    },
    html([
        a([href='/'+[offset=PreviousPage]], 'prev')
    ]).
previous_page(_) --> html([]).

next_page(CurrentPage, Pages) -->
    {
        CurrentPage < Pages
    },
    html([
        a([href='/'+[offset=CurrentPage]], 'next')
    ]).
next_page(_) --> html([]).


home_page(Request) :-
    http_parameters(Request, [
        offset(Offset, [optional(true), integer, default(0)])
    ]),
    Limit = 20,
    list_pokemon(Data, Limit, Limit*Offset),
    Results = Data.get(results),
    Count = Data.get(count),
    Pages is ceiling(Count / Limit),
    CurrentPage is Offset + 1,
    PageTitle = 'Pokedex served with Prolog',

    reply_html_page(
        title(PageTitle),
        [
            h1(PageTitle),
            p([
                'This page serves up facts about pokemon using ',
                a([href='https://pokeapi.co'], 'PokeAPI'),
                '. Find the source code on ',
                a([href='https://github.com/acmiyaguchi/prolog-pokedex'],
                    'Github at acmiyaguchi/prolog-pokedex'),
                '.'
            ]),
            p(['There are ', Count, ' pokemon available.']),
            div([
                span([
                    \previous_page(CurrentPage),
                    " page ", CurrentPage, " of ", Pages, " ",
                    \next_page(CurrentPage, Pages)
                ]),
                table([border(1), width('100%')], [
                    thead(tr([th(name)])),
                    tbody([\table_row(Results)])
                ])
            ])
        ]
    ).

health(_Request) :-
    git_hash(Hash, []),
    reply_json_dict(_{
        status: "ok",
        git_hash: Hash
    }).

:- http_handler(root(.), home_page, []).
% home is no longer being handled...
:- http_handler(root(home), http_redirect(moved, root(.)), []).
:- http_handler(root(health), health, []).

run(Port) :- http_server([port(Port)]).
