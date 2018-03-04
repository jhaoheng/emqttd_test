FROM        ubuntu:16.04

RUN         apt-get update && apt-get install -y wget unzip
RUN         cd /opt && \
            wget http://emqtt.com/downloads/latest/ubuntu16_04 && \
            unzip ubuntu16_04 && \
            rm ubuntu16_04
WORKDIR     /opt/emqttd

COPY        ./start.sh /start.sh
RUN         mv /start.sh /opt/emqttd/start.sh \
            && chmod +x /opt/emqttd/start.sh