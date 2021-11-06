% https://stackoverflow.com/questions/36966024/running-prolog-on-a-server-from-php-or-make-it-daemon

:- use_module(library(http/http_server)).

home_page(_Request) :-
    reply_html_page(
        title('Demo server'),
        [ h1('Hello world!')]).

:- http_handler(
    root(.),
    http_redirect(moved, location_by_id(home_page)),
    []
).
:- http_handler(root(home), home_page, []).
:- initialization http_server([port(8080)]).
