FROM ubuntu:xenial

RUN apt-get update
RUN apt-get install -yy \
  autoconf \
  bash \
  build-essential \
  ca-certificates \
  curl \
  git \
  golang \
  libcairo2 \
  libcurl4-openssl-dev \
  libfontconfig1 \
  libgmp-dev \
  libjpeg-dev \
  libmpfr-dev \
  libpango1.0-dev \
  libtool \
  nasm \
  sudo \
  time \
  vim \
  wget \
  zlib1g-dev

ENV RACKET_VERSION=7.8
ENV RACKET_DIR=/root/racket
ENV PANDOC=/root/.pandoc
ENV PANDOC_DEV="https://github.com/jgm/pandoc/releases/download/2.10.1/pandoc-2.10.1-1-amd64.deb"

WORKDIR /root/
RUN ["/bin/bash", "-c",\
     "curl https://raw.githubusercontent.com/greghendershott/travis-racket/master/install-racket.sh | bash"]

WORKDIR ${PANDOC}
RUN ["/bin/bash", "-c",\
     "curl -L $PANDOC_DEV > ${PANDOC}/pandoc.deb"]

RUN dpkg -x ${PANDOC}/pandoc.deb ${PANDOC}

WORKDIR /root/cmsc430/
ENV PATH="${PANDOC}/usr/bin:${RACKET_DIR}/bin:${PATH}"
