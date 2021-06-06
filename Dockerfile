FROM peterbecich/halogen-chess-base:latest AS builder

COPY . /app

# EXPOSE 8080

ENV CLIENT_DIR=/app/static

RUN cd /app \
    && cabal build all \
    && cabal install exe:halogen-chess

RUN cd /app \
    && spago bundle-app --to static/index.js

RUN ls /app
RUN ls /app/static
RUN which halogen-chess

FROM debian:buster
COPY --from=builder /root/.cabal/bin/* /bin/
COPY --from=builder /app/static /app/static

RUN apt update \
    && apt upgrade -y

ENV CLIENT_DIR=/app/static

CMD ["halogen-chess"]
