FROM python:3

RUN pip install \
    Flask \
    jsonschema \
    websockets

# Install netcat and ffmpeg reqs
RUN apt-get update -y && apt-get install -y \
    netcat \
    yasm \
    nasm

# Install my fork of Livestreamer
RUN apt-get install -y unzip
ENV branch fix-crash-on-httpserver-sigpipe
ADD https://github.com/Globidev/livestreamer/archive/${branch}.zip /tmp/livestreamer.zip
RUN unzip /tmp/livestreamer.zip -d /tmp
RUN cd /tmp/livestreamer-${branch} && python3 setup.py install
RUN rm -rf /tmp/livestreamer.zip /tmp/livestreamer-${branch}

ENV FFMPEG_ARCHIVE_URL  http://www.ffmpeg.org/releases/ffmpeg-3.1.tar.gz
ENV FFMPEG_BUILD_DIR    /ffmpeg-build
ENV FFMPEG_DIST_DIR     /ffmpeg
RUN mkdir ${FFMPEG_BUILD_DIR} &&\
    curl -L ${FFMPEG_ARCHIVE_URL} | tar xz -C ${FFMPEG_BUILD_DIR} --strip-components 1 &&\
    cd ${FFMPEG_BUILD_DIR} &&\
    ./configure --prefix=${FFMPEG_DIST_DIR} &&\
    make -j$(nproc) install > build.log &&\
    rm -rf ${FFMPEG_BUILD_DIR}

WORKDIR /streamer
ENTRYPOINT ["./api.py"]
