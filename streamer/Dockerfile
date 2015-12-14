FROM python:3

# Install vlc
RUN apt-get update && apt-get install -y vlc
# Hackish but simplest way I found to allow VLC to run as root
RUN sed -i 's/geteuid/getppid/' /usr/bin/vlc

RUN pip install \
    Flask \
    jsonschema \
    websockets

# Install my fork of Livestreamer
RUN apt-get install -y unzip
ENV branch fix-crash-on-httpserver-sigpipe
ADD https://github.com/Globidev/livestreamer/archive/${branch}.zip /tmp/livestreamer.zip
RUN unzip /tmp/livestreamer.zip -d /tmp
RUN cd /tmp/livestreamer-${branch} && python3 setup.py install
RUN rm -rf /tmp/livestreamer.zip /tmp/livestreamer-${branch}

# Install netcat
RUN apt-get install -y netcat

WORKDIR "/streamer"
ENTRYPOINT ["./api.py"]
