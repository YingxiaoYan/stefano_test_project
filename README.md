This directory contains the code for the program in the support project.
This document is written in Markdown (`.md`) and is best viewed using a Markdown viewer.

# Project information

* Title : Building on open source tools for targeted and untargeted metabolomics and exposomics
* Redmine issue at NBIS: **#7579**  (internal project ID)
* Request by: **Stefano Papazian**
* Principal investigator: **Stefano Papazian** (<stefano.papazian@scilifelab.se>)
* Organisation: **Stockholm University**
* NBIS experts : Mun-Gwan Hong (<mungwan.hong@nbis.se>)
* Git repository : https://github.com/NBISweden/

--------------------------------------------------------------------------------
# Subdirectories

The subdirectories within the `code/` directory are organized as follows.

    ├── code/
    │   ├── envs/             # Software environment
    │   ├── projlib/          # Reusable code modules imported by other scripts
    │   ├── reports/          # Code that generates reports in `<project>/reports/`
    │   └── scripts/          # Scripts (AWK, R, Python, etc.) 

# How to use the program

To mitigate issues arising from software environment inconsistencies, the environment in which the scripts were developed has been saved together with the scripts.
We use a container system called [Docker](https://www.docker.com/) to encapsulate the software environment.
Please ensure that the necessary management software is pre-installed.
For installation questions, please refer to the software's website.

--------------------------------------------------------------------------------

# Individual files under `code/`

Here are some descriptions on individual files.

## In the root of `code`

* `constants.yml` : Contants used in analyses and visualization
* `LICENSE` : License for the code here

## In `reports/` and `scripts/` sub-directories


## R and Rmd scripts

All R functions, except for those from the packages listed below, are primarily called with double colons (`::`) to specify the source package clearly. 

* Packages in `R-core` (e.g. `base`, `stats`, `utils`, ...)
* `ggplot2`  
* Packages in `tidyverse` (e.g. `dplyr`, `tidyr`, `purrr`, ...)
* Packages for S3 methods unable to specify them using `::` (e.g. `predict`, `autoplot`, ...) 


