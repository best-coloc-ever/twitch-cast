FROM ubuntu

RUN apt-get update -qq && apt-get install -yqq \
    vlc \

RUN useradd -u 1000 vlc
USER vlc
