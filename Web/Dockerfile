FROM haskell:latest

WORKDIR /usr/src/app

COPY stack.yaml package.yaml stack.yaml.lock /usr/src/app/

RUN stack build --install-ghc --dependencies-only

COPY . /usr/src/app

RUN stack build

CMD stack exec Web
