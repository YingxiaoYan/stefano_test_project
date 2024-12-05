# Use the micromamba base image
FROM mambaorg/micromamba:1.5.1
LABEL description="Docker image for the 7579 Exposome project"

# Set the working directory
WORKDIR /project
# Set the user to mamba
COPY --chown=$MAMBA_USER:$MAMBA_USER environment.yml /tmp/environment.yml

# Create a conda environment and install required packages
RUN micromamba install --yes --name base --file /tmp/environment.yml && \
    micromamba clean --all --yes

ENV TZ=EUROPE/STOCKHOLM

# Start bash shell by default
CMD ["/bin/bash"]

