FROM debian:bullseye-slim

RUN apt-get update
RUN apt-get install -y \
    cabal-install \
    ghc \
    git

RUN useradd -u 1001 jenkins -m
RUN mkdir /home/jenkins/.cabal

USER jenkins
