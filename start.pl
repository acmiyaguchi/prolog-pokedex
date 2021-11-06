% https://stackoverflow.com/questions/36966024/running-prolog-on-a-server-from-php-or-make-it-daemon

:- use_module(library(http/http_server)).

home_page(_Request) :-
    PageTitle = 'Pokedex served with Prolog',
    reply_html_page(
        title(PageTitle),
        [
            h1(PageTitle),
            p(['This page serves up facts about pokemon using ', 
                a([href='https://pokeapi.co'], 'PokeAPI'), '.'])
        ]
    ).

:- http_handler(
    root(.),
    http_redirect(moved, location_by_id(home_page)),
    []
).
:- http_handler(root(home), home_page, []).

:- initialization http_server([port(8080)]).
