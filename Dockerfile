FROM tercen/flowsom:0.1.14

USER root
WORKDIR /operator/flowsom_operator

RUN R -e "if(!require('BiocManager', quietly=TRUE)) install.packages('BiocManager')"
RUN R -e "BiocManager::install('MetaCyto')"

RUN git checkout master
RUN echo 1.2.11 && git pull
RUN git checkout 1.2.11

ENV TERCEN_SERVICE_URI https://tercen.com

ENTRYPOINT [ "R","--no-save","--no-restore","--no-environ","--slave","-f","main.R", "--args"]
CMD [ "--taskId", "someid", "--serviceUri", "https://tercen.com", "--token", "sometoken"]