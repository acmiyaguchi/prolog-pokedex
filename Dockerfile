FROM swipl

WORKDIR /app
RUN apt-get update && apt-get -y install build-essential
COPY packages.pl .
RUN swipl packages.pl

RUN useradd -ms /bin/bash www
USER www
COPY . /app
CMD ["swipl", "/app/start.pl"]
