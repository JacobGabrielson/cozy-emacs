FROM ubuntu:18.04

RUN apt-get update && apt-get install -y --no-install-recommends \
	build-essential \
	ca-certificates \
	curl \
	libncurses5-dev \
	autoconf \ 
	texinfo \
	libgnutls28-dev gnutls-dev gnutls-bin \
	emacs25 libx11-dev \
	libgtk-3-dev \
	libxpm-dev libjpeg-dev libtiff-dev \
	libgif-dev \
	libjansson-dev libsystemd-dev libotf-dev libgpm-dev libxml2-dev libcairo2-dev libmagickcore-dev liblcms2-dev librsvg2-dev libxaw3dxft8-dev \
	libgconf2-dev

RUN useradd -ms /bin/bash jacob
WORKDIR /home/jacob

RUN curl -L https://github.com/emacs-mirror/emacs/archive/emacs-27.0.90.tar.gz --output emacs-src.tar.gz
RUN zcat emacs-src.tar.gz | tar xf -
WORKDIR /home/jacob/emacs-emacs-27.0.90

USER jacob

RUN ./autogen.sh
RUN ./configure --prefix=/home/jacob/.local
RUN make -j $(expr $(cat /proc/cpuinfo | grep ^processor | cut -d: -f2 | tail -1) + 1)
