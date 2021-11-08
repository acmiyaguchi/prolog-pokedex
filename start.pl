:- use_module(library(http/http_unix_daemon)).
:- use_module(src/http).
:- use_module(src/reload).

:- initialization start.

start:-
    (getenv('PORT', Value) -> atom_number(Value, Port); Port = 8080),
    run(Port),
    % watch prolog files in the current directory hot reloads
    (
        debug_enabled ->
            reload_source(['/app', '/app/src']),
            writeln(user_output, "hot reload enabled");
            true
    ),
    http_daemon.
