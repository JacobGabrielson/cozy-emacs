FROM ubuntu:18.04
RUN apt-get update && apt-get install -y --no-install-recommends \
  build-essential \
  ca-certificates \
  curl \
  libncurses5-dev

RUN useradd -ms /bin/bash jacob

# USER jacob

WORKDIR /home/jacob

RUN curl -L https://github.com/emacs-mirror/emacs/archive/emacs-27.0.90.tar.gz --output emacs-src.tar.gz

RUN zcat emacs-src.tar.gz | tar xf -

WORKDIR /home/jacob/emacs-emacs-27.0.90

RUN apt-get install -y --no-install-recommends autoconf
RUN apt-get install -y --no-install-recommends texinfo
RUN apt-get install -y --no-install-recommends libgnutls28-dev gnutls-dev gnutls-bin
RUN apt-get install -y --no-install-recommends emacs25 libx11-dev
RUN apt-get install -y --no-install-recommends libgtk-3-dev
RUN apt-get install -y --no-install-recommends libxpm-dev libjpeg-dev libtiff-dev
RUN apt-get install -y --no-install-recommends libgif-dev
RUN apt-get install -y --no-install-recommends libjansson-dev libsystemd-dev libotf-dev libgpm-dev libxml2-dev libcairo2-dev libmagickcore-dev liblcms2-dev librsvg2-dev libxaw3dxft8-dev
RUN apt-get install -y --no-install-recommends libgconf2-dev

# RUN make -j 16 bootstrap
RUN ./autogen.sh
RUN ./configure --prefix=/home/jacob/.local
