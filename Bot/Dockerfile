FROM haskell:latest

WORKDIR /usr/src/app
COPY stack.yaml Bot.cabal /usr/src/app/

RUN stack build --install-ghc --dependencies-only

COPY . /usr/src/app

RUN stack build

CMD stack exec Bot-exe
