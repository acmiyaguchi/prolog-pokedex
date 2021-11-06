FROM swipl:latest

RUN apt-get update && apt-get -y install build-essential

WORKDIR /app
COPY packages.pl .
RUN swipl packages.pl
COPY . /app

CMD ["swipl", "/app/start.pl"]
