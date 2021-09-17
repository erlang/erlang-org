# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.191.1/containers/ubuntu/.devcontainer/base.Dockerfile
FROM mcr.microsoft.com/vscode/devcontainers/base:focal

ENV INSTALL_LIBS=""

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get -y upgrade && \
    apt-get install -y build-essential m4 autoconf zlib1g-dev libncurses5-dev libssl-dev libreadline-dev

ADD .tool-versions /home/vscode/.tool-versions

## Install asdf
RUN su vscode -c 'git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.8.1 && echo ". ~/.asdf/asdf.sh" >> ~/.zshrc && echo ". ~/.asdf/asdf.sh" >> ~/.bashrc'

## Install tools
RUN su -l vscode -c '. ~/.asdf/asdf.sh && cut -d" " -f1 .tool-versions | xargs -i asdf plugin add {} && asdf install' && \
    sudo bash -c "curl https://s3.amazonaws.com/rebar3-nightly/rebar3 > /usr/local/bin/rebar3 && chmod +x /usr/local/bin/rebar3"