# Select base image
ARG R_RELEASE="4.3.2"
ARG BASE_NAME="mtpl2"

FROM rocker/r-ver:${R_RELEASE} AS rbase

# System libraries
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libgsl0-dev \
    libnlopt-dev \
    libxt6 \
    ssh

# Setup R packages
ARG NCPUS=1
RUN install2.r --error \
    --skipinstalled \
    --ncpus $NCPUS \
    tinytex \
    remotes \
    markdown \
    mime

FROM ${BASE_NAME} as mtpl2

# Install packages specified in DESCRIPTION
COPY DESCRIPTION* /home/
WORKDIR /home/

RUN if test -f DESCRIPTION ; then \
        install2.r --error \
        --skipinstalled \
        $(Rscript -e "pkg <- remotes:::load_pkg_description('.'); repos <- c('https://cloud.r-project.org', remotes:::parse_additional_repositories(pkg)); deps <- remotes:::local_package_deps(pkgdir = '.', dependencies = NA); write(paste0(deps, collapse = ' '), stdout())"); \
    fi

RUN rm -f DESCRIPTION
RUN rm -rf /tmp/downloaded_packages
