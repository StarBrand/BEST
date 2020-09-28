FROM rocker/shiny:4.0.2

RUN apt-get update && apt-get install libcurl4-openssl-dev libxml2-dev default-jdk r-cran-rjava r-cran-xml libv8-dev libsasl2-dev libpq-dev libsodium-dev -y && mkdir -p /var/lib/shiny-server/bookmarks/shiny

COPY ./requirements.R /requirements.R

RUN chmod -R 755 /requirements.R

RUN Rscript /requirements.R

COPY ./app /srv/shiny-server/

RUN chmod -R 755 /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
