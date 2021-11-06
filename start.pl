% https://stackoverflow.com/questions/36966024/running-prolog-on-a-server-from-php-or-make-it-daemon
:- use_module(library(http/http_server)).
:- use_module(library(inotify)).

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

% Watch a directory using inotify
% https://www.swi-prolog.org/pack/file_details/inotify/prolog/inotify.pl?show=src
reload_source(Dir) :-
    inotify_init(INotify, []),
    inotify_add_watch(INotify, Dir, [all]),
    repeat,
        (   inotify_read_event(INotify, Ev, [])
        ->
            (
                Ev = modify(file(File)),
                split_string(File, ".", "", L),
                last(L, "pl")
                  ->
                    writeln([File, 'was changed']),
                    make
            ),
            fail
        ;   format('Timeout~n'),
            !
        ).

run :-
    % first load then server on port 8080
    http_server([port(8080)]),
    % watch prolog files in the current directory for whether to restart the server
    reload_source('/app').

:- initialization(run).
