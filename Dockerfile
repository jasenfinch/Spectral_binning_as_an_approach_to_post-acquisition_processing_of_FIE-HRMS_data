FROM ghcr.io/jasenfinch/hrm-docker:2021-11-07

RUN apt-get update
RUN apt-get install -y libv8-dev librsvg2-dev

RUN Rscript -e "install.packages(c('renv'), repos = c(CRAN = 'https://cloud.r-project.org'))"

WORKDIR /home/Spectral_binning_as_an_approach_to_post-acquisition_processing_of_FIE-HRMS_data
    
COPY renv.lock renv.lock

RUN Rscript -e "renv::restore()"

ENTRYPOINT ["Rscript","run.R"]
