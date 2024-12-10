# Use the micromamba base image
FROM mambaorg/micromamba:1.5.1
LABEL description="Docker image for the 7579 Exposome project"

# Set the working directory
WORKDIR /project
# Set the user to mamba
COPY --chown=$MAMBA_USER:$MAMBA_USER environment.yml /tmp/environment.yml
COPY --chown=$MAMBA_USER:$MAMBA_USER SumExp_0.1.0.tar.gz /tmp/SumExp_0.1.0.tar.gz

# Create a conda environment and install required packages
RUN micromamba install --yes --name base --file /tmp/environment.yml && \
    micromamba clean --all --yes
# Install a local package
RUN R CMD INSTALL /tmp/SumExp_0.1.0.tar.gz


ENV TZ=EUROPE/STOCKHOLM

# Start bash shell by default
CMD ["/bin/bash"]

