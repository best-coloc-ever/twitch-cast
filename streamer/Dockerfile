FROM ubuntu

RUN apt-get update -qq && apt-get install -yqq \
    python-pip \
    unzip

RUN pip install \
    Flask \
    jsonschema

# Install my fork of Livestreamer
ENV branch fix-crash-on-httpserver-sigpipe
ADD https://github.com/Globidev/livestreamer/archive/${branch}.zip /tmp/livestreamer.zip
RUN unzip /tmp/livestreamer.zip -d /tmp
RUN cd /tmp/livestreamer-${branch} && python setup.py install
RUN rm -rf /tmp/livestreamer.zip /tmp/livestreamer-${branch}

ADD ./ /
ENTRYPOINT ["/api.py"]
