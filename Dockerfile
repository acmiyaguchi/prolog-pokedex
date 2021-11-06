FROM swipl

RUN useradd -ms /bin/bash www
USER www
COPY . /app
CMD ["swipl", "/app/start.pl"]