FROM peterbecich/haskell-chess-base:latest

COPY . /app

EXPOSE 8080

ENV CLIENT_DIR=/app/dist

RUN cd app \
        && cabal build all \
        && cabal install exe:halogen-chess

RUN ls app
RUN ls app/dist

CMD ["halogen-chess"]
