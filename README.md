# pokedex served in prolog

A small project to try running a web service with Prolog. The goal is to deploy
this as web service to Google Cloud Run.

## quickstart

Use `docker compose` to bring the service online. By default, the service will
reload when any prolog files change. Note that if you are running on Windows
via WSL, you will need to run the program from a filesystem native to WSL for
`inotify` to work properly.

```bash
docker compose build
docker compose up
```

If you want to interact with the interpreter, start the service from a shell.

```bash
docker compose run --rm --service-ports app bash
swipl
```

## overview

### prolog web application

I've always thought of [Prolog](https://en.wikipedia.org/wiki/Prolog) as a
unique language and wanted to scratch the itch of building a reasonably-sized
application using [SWI-Prolog](https://www.swi-prolog.org). I've learned quite a
bit through the process of putting together this application, from modularizing
the codebase to language constructs that make expressing ideas reasonably easy.

#### entrypoint and modules

First, I dealt with actually figuring out how to run a basic web service
skeleton. Pouring through the documentation for the HTTP libraries, I built a
better intuition for structuring a larger Prolog codebase. It's a bit like
writing a Flask application (or web microframework of your choice). I started
from a single file that ran the entire web application and then started to break
it down into modules with an entry-point script at the top level.

```bash
├── src
│   └── http.pl
├── start.pl
```

In `http.pl`, I define a module with all the logic necessary for starting the
HTTP server into a `run/1` predicate. Then I can export through the module
interface:

```prolog
% src/http.pl
:- module(app, [run/1]).

run(Port) :- http_server([port(Port)]).
```

Then we call the module from `start.pl` and make it the entry point of our
application:

```prolog
:- use_module(src/http).
:- initialization(run(8080))
```

This structure has served as an excellent backbone for features later down the
line, like live-reloads and conditional logic based on environment variables.

#### live-reload using inotify

Reloading the server on source changes was one of the features that I decided to
tackle early on. The Prolog interpreter has a `make` predicate that reloads
predicates from all sources. I used the [inotify
pack](https://github.com/JanWielemaker/inotify) to run `make` every time a
Prolog file is changed within the source directory, which ends up working very
well. I recommend copying the `reload.pl` module and inserting it into your
application's workflow to use this elsewhere. I learned a few things here: the
use of user-provided "packs" and the `yall` library for lambda functions.

A [pack is a user-contributed module](https://www.swi-prolog.org/pack/list) that
is not shipped with SWI-Prolog by default. I end up using a pattern similar to
`requirements.txt` in Python projects. In a `packages.pl` file at the root, I
add the packs that I needed:

```prolog
:- pack_install(inotify, [interactive(false)]).
```

Then inside of my `Dockerfile`, I add this near to the top of the build:

```dockerfile
COPY packages.pl .
RUN swipl packages.pl
```

While using the inotify library, I realized that `inotify` does not work
recursively. After deciding to list out the directories to watch explicitly, I
needed a way to call the `inotify_add_watch/4` predicate. Instead of building a
separate predicate to walk over the list, I oped to use `yall`'s lambdas with
`maplist` for partial application of the watch predicate to the list of
directories to watch.

```prolog
    maplist([Dir]>>inotify_add_watch(INotify, Dir, [modify]), Dirs),
```

The notation is handy, but I still can't shake the feeling that taking advantage
of the batteries included is cheating a bit.

#### `html_write` and DCGs

There are a [few schools of thought around HTML][youtube-prolog-html] in Prolog:
one based on templates and the other Prolog terms. I decided to go with the
latter, which seems to be a bit more idiomatic. The module utilizes a definite
clause grammar (DCG) to build out an isomorphic representation of HTML
represented as Prolog terms. In an HTTP handler, we might decide to reply with
some HTML:

```Prolog
    % ...
    reply_html_page(
        title(PageTitle),
        [
            h1(PageTitle),
            % ...
            div([
                span([
                    \previous_page(CurrentPage),
                    " page ", CurrentPage, " of ", Pages, " ",
                    \next_page(CurrentPage, Pages)
                ]),
                % ...
            ])
        ]).
```

In this section, I've defined two [DCG
rules](https://www.swi-prolog.org/pldoc/man?section=DCG) called `previous_page`
and `next_page`, which utilize terms in the current scope to spit out tokens for
the HTML DCG. A rule is defined as a normal predicate, but using `-->` instead
of
`:-`.

```prolog
previous_page(CurrentPage) -->
    {
        CurrentPage > 1,
        % CurrentPage is index 1, while the API takes index 0
        PreviousPage is CurrentPage - 2
    },
    html([
        a([href='/'+[offset=PreviousPage]], 'prev')
    ]).
previous_page(_) --> html([]).
```

Note that there are two sections in the main body of the rule. Within the curly
braces, we define ordinary Prolog predicates. Outside of those braces, we define
context-free grammar to generate our HTML. So, for example, this rule will
generate a link to the previous page of the listing if we're not already on the
first page. The idea of separating the logic and the actual rules wasn't
self-evident at first blush, but it's not too tricky to get the hang of once you
learn the syntax.

As mentioned before, the Prolog terms for `html_write` are isomorphic to HTML.
So, in the above function, we'll end up generating the following HTML:

```html
<a href="/?offset=1">prev</a>
```

It includes some nice syntactic sugar, like translating from a list of options
into the appropriate HTML parameters. Overall, it's not an entirely terrible
experience (although I'm far more productive writing web applications
client-side).

[youtube-prolog-html]: https://www.youtube.com/watch?v=-edJid989So&list=PLo2Yjnxu38Q8VI6WVFLlTQOZ9LH9M4wyk&index=4

### cloudbuild and cloudrun

In days not too long ago, a hobbyist like myself would deploy a web application
onto a small server at a premium if we didn't already have one running. Either
you deal with renting a VPS (e.g. DigitalOcean) for $5/month, or you go the
route of dealing with deploying to a platform-as-a-service like Heroku. The
downside of the latter is that you're often limited to the number of
applications you can host, even if you're dealing with low amounts of traffic.

I use [Google Cloud Run](https://cloud.google.com/run) to deploy the dockerized
web application. The Cloud Run service is effectively a massive Kubernetes
cluster managed by Google that charges based on traffic. Given the generous
free-tier (180k vCPU-seconds, 360k GiB-seconds, 2m requests) and estimated low
traffic (hundreds of requests that drops off to zero after sharing with
friends), this service ends up being what I need.

The continuous deployment process looks a bit as follows:

[![](https://mermaid.ink/img/eyJjb2RlIjoiZ3JhcGggTFJcbiAgICBnaXRodWJcbiAgICBidWlsZFtDbG91ZGJ1aWxkXVxuICAgIGdjcltHQ1JdXG4gICAgcnVuW0Nsb3VkIFJ1bl1cblxuICAgIGdpdGh1YiAtLT58dHJpZ2dlciBidWlsZHwgYnVpbGRcbiAgICBidWlsZCAtLT58YnVpbGQgYW5kIHB1c2h8IGdjclxuICAgIGdjciAtLT58aW1hZ2V8IHJ1blxuICAgIGJ1aWxkIC0tPnxkZXBsb3l8IHJ1biIsIm1lcm1haWQiOnsidGhlbWUiOiJkYXJrIn0sInVwZGF0ZUVkaXRvciI6ZmFsc2UsImF1dG9TeW5jIjp0cnVlLCJ1cGRhdGVEaWFncmFtIjpmYWxzZX0)](https://mermaid.live/edit#eyJjb2RlIjoiZ3JhcGggTFJcbiAgICBnaXRodWJcbiAgICBidWlsZFtDbG91ZGJ1aWxkXVxuICAgIGdjcltHQ1JdXG4gICAgcnVuW0Nsb3VkIFJ1bl1cblxuICAgIGdpdGh1YiAtLT58dHJpZ2dlciBidWlsZHwgYnVpbGRcbiAgICBidWlsZCAtLT58YnVpbGQgYW5kIHB1c2h8IGdjclxuICAgIGdjciAtLT58aW1hZ2V8IHJ1blxuICAgIGJ1aWxkIC0tPnxkZXBsb3l8IHJ1biIsIm1lcm1haWQiOiJ7XG4gIFwidGhlbWVcIjogXCJkYXJrXCJcbn0iLCJ1cGRhdGVFZGl0b3IiOmZhbHNlLCJhdXRvU3luYyI6dHJ1ZSwidXBkYXRlRGlhZ3JhbSI6ZmFsc2V9)
