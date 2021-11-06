# pokedex served in prolog

A small project to try running a web service with Prolog. The goal is to deploy
this as web service to Google Cloud Run.

## quickstart

```bash
docker compose build
docker compose run --rm --service-ports bash
swipl /app/start.pl
```
