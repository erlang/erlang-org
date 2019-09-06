FROM erlang:18.3.4.11

RUN apt-get update && apt-get -y upgrade

RUN mkdir -p /build/erlorg
WORKDIR /build/erlorg
ADD . /build/erlorg

RUN cp rel/docker.config.template rel/ops.config && make erlang-mk; make distclean; make rel

CMD bash -c "sleep 20 && _rel/erlorg/bin/erlorg foreground"
