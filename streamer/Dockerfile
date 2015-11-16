FROM ubuntu

RUN apt-get update -qq && apt-get install -yqq \
    python-pip

RUN pip install \
    livestreamer \
    Flask

ADD ./stream.sh /
ADD ./api.py /
ENTRYPOINT ["/api.py"]
