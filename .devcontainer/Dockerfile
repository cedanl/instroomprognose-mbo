FROM rocker/r-ver:4.4.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libglpk-dev \
    libgsl-dev \
    libcairo2-dev \
    libbz2-dev \
    libsodium-dev \
    libmagick++-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    python3-full \
    python3-pip \
    wget \
    fonts-firacode \
    && rm -rf /var/lib/apt/lists/*

# Install Quarto
RUN wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.6.39/quarto-1.6.39-linux-amd64.deb \
    && dpkg -i quarto-1.6.39-linux-amd64.deb \
    && rm quarto-1.6.39-linux-amd64.deb

# Set working directory
WORKDIR /workspaces

# Install essential R packages (global installation for all users)
RUN R -e "install.packages('languageserver', repos = 'https://cloud.r-project.org/', lib = '/usr/local/lib/R/site-library')"
RUN R -e "install.packages('httpgd', repos = c('https://cranhaven.r-universe.dev', 'https://cloud.r-project.org'), lib = '/usr/local/lib/R/site-library')"

# Create .Rprofile to load packages on startup
RUN mkdir -p /etc/R && \
    echo 'if(interactive()) { \
    .First <- function() { \
      message("Loading language server and httpgd...") \
      suppressPackageStartupMessages(library(languageserver)) \
      suppressPackageStartupMessages(library(httpgd)) \
    } \
}' > /etc/R/Rprofile.site

# Set up radian (improved R console)
RUN python3 -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
RUN pip install --no-cache-dir radian jupyter

# Create the R library directories with appropriate permissions
RUN mkdir -p /home/vscode/R/library && \
    chmod -R 777 /home/vscode/R && \
    chmod -R 777 /usr/local/lib/R/site-library && \
    # Ensure renv directories have proper permissions
    mkdir -p /workspaces/utils/renv/library && \
    chmod -R 777 /workspaces/utils/renv

# Set R library path environment variables
ENV R_LIBS_USER=/home/vscode/R/library
ENV R_LIBS_SITE=/usr/local/lib/R/site-library

# Set radian as the R terminal
ENV R_TERM="/opt/venv/bin/radian"

CMD ["bash"]
