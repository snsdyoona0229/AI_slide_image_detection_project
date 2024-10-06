FROM rocker/shiny-verse:4.3.1

RUN rm -rf /srv/shiny-server/*

WORKDIR /srv/shiny-server/

FROM bioconductor/ml-verse:devel


ENV WORKON_HOME /opt/virtualenvs
ENV PYTHON_VENV_PATH $WORKON_HOME/r-tensorflow

RUN apt-get update && apt-get install -y --no-install-recommends build-essential libpq-dev python3.10 python3-pip python3-setuptools python3-dev
RUN pip3 install --upgrade pip

ADD requirements.txt .

# installing python libraries
RUN pip3 install -r requirements.txt

RUN install2.r reticulate tensorflow keras Pillow


RUN Rscript -e 'install.packages("reticulate")'
RUN Rscript -e 'install.packages("tensorflow")'
RUN Rscript -e 'install.packages("keras")'

RUN R -e 'library(tensorflow)'
RUN R -e 'library(reticulate)'
RUN R -e 'library(keras)'

RUN Rscript -e 'reticulate::install_miniconda()'

RUN R -e 'reticulate::py_install("Pillow")'
RUN R -e 'tensorflow::install_tensorflow(version = "gpu")'
RUN R -e 'keras::install_keras()'
RUN R -e 'keras::install_keras(tensorflow = "gpu")'

RUN R -e 'reticulate::conda_create("r-py-conda-3.10", python_version = "3.10")'
RUN Rscript -e 'reticulate::conda_install(envname = "r-py-conda-3.10", packages=("tensorflow"))'
RUN Rscript -e 'reticulate::conda_install(envname = "r-py-conda-3.10", packages=("Pillow"))'
RUN Rscript -e 'reticulate::conda_install(envname = "r-py-conda-3.10", packages=("keras"))'

RUN R -e 'install.packages(c("remotes","BiocManager"))' \
 && R -e 'BiocManager::install("EBImage")'

RUN Rscript -e 'install.packages("leaflet")'
RUN Rscript -e  'install.packages("leaflet.extras")'
RUN Rscript -e  'install.packages("leaflet.extras2")'
RUN Rscript -e  'install.packages("devtools")'
RUN Rscript -e 'install.packages("Rcpp")'
RUN Rscript -e 'install.packages("shiny")'
RUN Rscript -e 'install.packages("devtools")'
RUN Rscript -e 'install.packages("shinydashboard")'
RUN Rscript -e 'install.packages("shinydashboardPlus")'
RUN Rscript -e 'install.packages("shinyWidgets")'
RUN Rscript -e 'install.packages("shinyFiles")'
RUN Rscript -e 'install.packages("shinyjqui")'
RUN Rscript -e 'install.packages("htmlwidgets")'
RUN Rscript -e 'install.packages("rhandsontable")'
RUN Rscript -e 'install.packages("rhandsontable")'
RUN Rscript -e 'install.packages("RSQLite")'
RUN Rscript -e 'install.packages("DT")'
RUN Rscript -e 'install.packages("shinyjs")'
RUN Rscript -e 'install.packages("devtools")'
RUN Rscript -e 'install.packages("polyclip")'
RUN Rscript -e 'install.packages("stringr")'
RUN Rscript -e 'install.packages("dplyr")'
RUN Rscript -e 'install.packages("raster")'
RUN Rscript -e 'install.packages("keras")'
RUN Rscript -e 'install.packages("tensorflow")'
RUN Rscript -e 'install.packages("tfaddons")'
RUN Rscript -e 'install.packages("future")'
RUN Rscript -e 'install.packages("future.apply")'
RUN Rscript -e 'install.packages("future.callr")'
RUN Rscript -e 'install.packages("promises")'
RUN Rscript -e 'install.packages("microbenchmark")'
RUN Rscript -e 'install.packages("XML")'
RUN Rscript -e 'install.packages("cowplot")'
RUN Rscript -e 'install.packages("ggplot2")'
RUN Rscript -e 'install.packages("penalized")'
RUN Rscript -e 'install.packages("DBI")'
RUN Rscript -e 'install.packages("rmarkdown")'
RUN Rscript -e 'install.packages("shinyjqui")'
RUN Rscript -e 'install.packages("V8")'
RUN Rscript -e 'install.packages("shinyjs")'
RUN Rscript -e 'install.packages("sf")'
RUN Rscript -e 'install.packages("sp")'
RUN Rscript -e 'install.packages("rgdal")'


RUN mkdir /root/euler
RUN mkdir /usr/local/lib/R/site-library/RNDPI
RUN mkdir /usr/local/lib/R/site-library/lawn

RUN mkdir /usr/local/lib/R/site-library/leafpm
RUN mkdir /usr/local/lib/R/site-library/leafgl


COPY euler /root/euler
COPY RNDPI /usr/local/lib/R/site-library/RNDPI
COPY lawn /usr/local/lib/R/site-library/lawn

COPY leafpm /usr/local/lib/R/site-library/leafpm
COPY leafgl /usr/local/lib/R/site-library/leafgl

COPY Rprofile.site /usr/local/lib/R/etc/


EXPOSE 3838

CMD ["R", "-q", "-e", "shiny::runApp('/root/euler')"]





