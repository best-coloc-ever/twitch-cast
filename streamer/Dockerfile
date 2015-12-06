FROM ubuntu

RUN apt-get update && apt-get install -y \
    python3-pip \
    unzip \
    vlc

# Hackish but simplest way I found to allow VLC to run as root
RUN sed -i 's/geteuid/getppid/' /usr/bin/vlc

RUN pip3 install \
    Flask \
    jsonschema

# Install my fork of Livestreamer
ENV branch fix-crash-on-httpserver-sigpipe
ADD https://github.com/Globidev/livestreamer/archive/${branch}.zip /tmp/livestreamer.zip
RUN unzip /tmp/livestreamer.zip -d /tmp
RUN cd /tmp/livestreamer-${branch} && python3 setup.py install
RUN rm -rf /tmp/livestreamer.zip /tmp/livestreamer-${branch}

WORKDIR "/streamer"
ENTRYPOINT ["./api.py"]
