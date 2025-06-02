# Use R shiny image
FROM rocker/shiny-verse
LABEL description="Docker image for NBIS 7579 Exposome project"

# Set the working directory
WORKDIR /proj
COPY SumExp_0.1.2.tar.gz /tmp/SumExp_0.1.2.tar.gz

# Install system dependencies for Quarto and other packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    wget \
    curl \
    gdebi-core \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libglpk-dev \
    && rm -rf /var/lib/apt/lists/*

# Install Quarto
RUN wget https://quarto.org/download/latest/quarto-linux-amd64.deb && \
    gdebi --non-interactive quarto-linux-amd64.deb && \
    rm quarto-linux-amd64.deb

# Install R packages
RUN install2.r --error --skipinstalled --ncpus -1 \
    box \
    roxygen2 \
    labelled \
    patchwork \
    ggfortify \
    ggtext \
    plotly \
    ggpubr \
    english \
    kableExtra \
    janitor \
    shinyFiles \
    quarto \
    && rm -rf /tmp/downloaded_packages

# Install a local package
RUN R CMD INSTALL /tmp/SumExp_0.1.2.tar.gz

ENV TZ=EUROPE/STOCKHOLM

# Expose the port for Shiny app
EXPOSE 7579

