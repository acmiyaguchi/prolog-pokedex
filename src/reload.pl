:- module(reload, [reload_source/1]).
:- use_module(library(inotify)).
:- use_module(library(yall)).

reload_source(Dirs) :-
    % Watch a directory using inotify
    % https://www.swi-prolog.org/pack/file_details/inotify/prolog/inotify.pl?show=src
    inotify_init(INotify, []),
    % https://github.com/dtonhofer/prolog_notes/blob/master/swipl_notes/about_maplist/maplist_3_examples.md
    maplist([Dir]>>inotify_add_watch(INotify, Dir, [modify]), Dirs),
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