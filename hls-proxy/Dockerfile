FROM ubuntu

RUN apt-get update -qq && apt-get install -yqq \
    vlc

# Hackish but simplest way I found to allow VLC to run as root
RUN sed -i 's/geteuid/getppid/' /usr/bin/vlc

ADD ./vlc_proxy.sh /
ENTRYPOINT ["/vlc_proxy.sh"]
