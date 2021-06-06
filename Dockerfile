FROM peterbecich/halogen-chess-base:latest

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


CMD ["halogen-chess"]
