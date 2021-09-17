# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.191.1/containers/ubuntu/.devcontainer/base.Dockerfile
ARG BASE=gitpod/workspace-full
FROM $BASE

ARG USER=gitpod

ENV DEBIAN_FRONTEND=noninteractive

RUN sudo apt-get update && sudo -E apt-get -y upgrade && \
    sudo -E apt-get install -y build-essential m4 autoconf zlib1g-dev libncurses5-dev libssl-dev libreadline-dev

ADD .tool-versions .tool-versions

RUN if [ ! -f /home/$USER/.tool-versions ]; then \
    mv .tool-versions /home/$USER/ && chown $USER /home/$USER/.tool-versions; fi

## Install asdf
RUN sudo su -l $USER -c 'git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.8.1 && echo ". ~/.asdf/asdf.sh" >> ~/.zshrc && echo ". ~/.asdf/asdf.sh" >> ~/.bashrc'

## Install tools
RUN sudo su -l $USER -c '. ~/.asdf/asdf.sh && cut -d" " -f1 .tool-versions | xargs -i asdf plugin add {} && asdf install' && \
    sudo bash -c "curl https://s3.amazonaws.com/rebar3-nightly/rebar3 > /usr/local/bin/rebar3 && chmod +x /usr/local/bin/rebar3"
