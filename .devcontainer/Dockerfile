# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.191.1/containers/ubuntu/.devcontainer/base.Dockerfile

FROM mcr.microsoft.com/vscode/devcontainers/ruby:3

RUN curl https://raw.githubusercontent.com/kerl/kerl/master/kerl > /usr/local/bin/kerl && chmod +x /usr/local/bin/kerl && \
    curl https://s3.amazonaws.com/rebar3/rebar3 > /usr/bin/rebar3 && chmod +x /usr/bin/rebar3 && \
    su vscode -c "umask 0002 && export MAKEFLAGS=-j8 && RELEASE=`kerl update releases | tail -1` && kerl build \$RELEASE && kerl install \$RELEASE ~/erlang/"

# [Optional] Uncomment this section to install additional OS packages.
# RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
#     && apt-get -y install --no-install-recommends esl-erlang && \

ARG NODE_VERSION="16.9.1"
RUN if [ "${NODE_VERSION}" != "none" ]; then su vscode -c "umask 0002 && . /usr/local/share/nvm/nvm.sh && nvm install ${NODE_VERSION} 2>&1"; fi

ARG USE_RUBY_VERSION="2.6.5"
RUN if [ "${USE_RUBY_VERSION}" != "none" ]; then su -l vscode -c "umask 0002 && rvm install ${USE_RUBY_VERSION} 2>&1 && rvm use ${USE_RUBY_VERSION} --default"; fi