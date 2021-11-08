FROM swipl:latest

RUN apt-get update && apt-get -y install build-essential

WORKDIR /app
COPY packages.pl .
RUN swipl packages.pl
COPY . /app

RUN useradd -ms /bin/bash www
CMD ["swipl", "/app/start.pl", "--user=www", "--no-fork"]
