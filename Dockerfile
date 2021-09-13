FROM tercen/flowsom:0.1.14

USER root
WORKDIR /operator/flowsom_operator

RUN git checkout master
RUN echo 0.2.3 && git pull
RUN git checkout 0.2.3

ENV TERCEN_SERVICE_URI https://tercen.com

ENTRYPOINT [ "R","--no-save","--no-restore","--no-environ","--slave","-f","main.R", "--args"]
CMD [ "--taskId", "someid", "--serviceUri", "https://tercen.com", "--token", "sometoken"]