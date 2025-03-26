FROM rocker/shiny:latest

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c( \
    'shinyjs', 'tidyr', 'dplyr', 'DT', \
    'ggplot2', 'gridExtra', 'shinycssloaders', \
    'tibble', 'data.table', 'shinythemes', 'httr', 'jsonlite', \
    'markdown','xlsx' \
    ), repos='https://cloud.r-project.org/')"

# Set working directory
WORKDIR /srv/shiny-server

# Copy the Shiny app files into the container
COPY . /srv/shiny-server/

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
CMD ["/usr/bin/shiny-server"]
