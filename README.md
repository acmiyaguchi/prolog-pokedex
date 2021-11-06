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
docker compose run --rm --service-ports bash
swipl /app/start.pl
```
