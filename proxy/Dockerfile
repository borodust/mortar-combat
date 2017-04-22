FROM ubuntu:16.04

RUN apt-get update && apt-get install -y \
        libuv1


EXPOSE 8778 8222

WORKDIR /mortar-combat-proxy/

ADD * ./

ENTRYPOINT ["./server.bin"]
