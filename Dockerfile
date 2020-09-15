FROM jrnold/rstan:latest
MAINTAINER Glenn Moncrieff

RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
    lbzip2 \
    libfftw3-dev \
    libgdal-dev \
    libgeos-dev \
    libgsl0-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libhdf4-alt-dev \
    libhdf5-dev \
    libjq-dev \
    liblwgeom-dev \
	libpq-dev \
    libproj-dev \
    libprotobuf-dev \
    libnetcdf-dev \
    libsqlite3-dev \
    libssl-dev \
    libudunits2-dev \
    netcdf-bin \
	postgis \
    protobuf-compiler \
	sqlite3 \
    tk-dev \
    unixodbc-dev \
    cdo \
  && install2.r --error \
    feather \
    tidybayes \
    doParallel \
    dplyr \
    RColorBrewer \
    RandomFields \
    RNetCDF \
    classInt \
    deldir \
    gstat \
    hdf5r \
    lidR \
    mapdata \
    maptools \
    mapview \
    ncdf4 \
    proj4 \
    raster \
    rgdal \
    rgeos \
    rlas \
    sf \
    sp \
    spacetime \
    spatstat \
    spdep \
    geoR \
    geosphere \
    RArcInfo \
    gdalUtils \
    fitdistrplus \
    DBI \
    fasterize \
    ggpubr \
    rasterVis \
    colorspace \
    viridis \
    readxl \
    ggridges \
    mixdist \
    bigstatsr \
    bigrquery \
    DBI \
    googleCloudStorageR \
    ## from bioconductor and devtools
    && R -e "BiocManager::install('rhdf5')" \
    && R -e "devtools::install_github('xiaodaigh/disk.frame')"