:- module(reload, [reload_source/1]).
:- use_module(library(inotify)).

% Watch a directory using inotify
% https://www.swi-prolog.org/pack/file_details/inotify/prolog/inotify.pl?show=src
reload_source(Dir) :-
    inotify_init(INotify, []),
    inotify_add_watch(INotify, Dir, [modify]),
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