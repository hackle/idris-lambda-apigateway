FROM amazonlinux

RUN yum -y update
RUN yum install -y curl git make ruby sudo which tar zip shadow-utils gcc \
  && yum clean all

RUN useradd -m -s /bin/bash linuxbrew \
	&& echo 'linuxbrew ALL=(ALL) NOPASSWD:ALL' >>/etc/sudoers

USER linuxbrew
WORKDIR /home/linuxbrew

ENV PATH=/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin:$PATH \
	SHELL=/bin/bash \
	USER=linuxbrew

RUN yes | ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install)" \
	&& brew config

RUN brew update && brew install idris
