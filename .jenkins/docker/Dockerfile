FROM debian:bullseye-slim

ARG JENKINS_UID=1001
ENV DEBIAN_FRONTEND teletype

RUN apt-get update
RUN apt-get install -y \
    cabal-install \
    ghc \
    git

RUN useradd -u $JENKINS_UID jenkins -m
RUN mkdir /home/jenkins/.cabal

USER jenkins
