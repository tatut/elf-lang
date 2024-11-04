FROM swipl:9.2.8
RUN mkdir /app
COPY elf*.pl /app/
COPY examples.pl /app/
COPY repl.sh /app/
CMD ["sh", "/app/repl.sh"]