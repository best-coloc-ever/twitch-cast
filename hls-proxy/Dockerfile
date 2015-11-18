FROM ubuntu

RUN apt-get update -qq && apt-get install -yqq \
    python-pip \
    vlc

# Hackish but simplest way I found to allow VLC to run as root
RUN sed -i 's/geteuid/getppid/' /usr/bin/vlc

RUN pip install \
    Flask

ADD ./ /
ENTRYPOINT ["/api.py"]
