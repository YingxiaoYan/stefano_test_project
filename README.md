This directory contains the code for the program in the support project.
This document is written in Markdown (`.md`) and is best viewed using a Markdown viewer.

# Project information

* Title : Building on open source tools for targeted and untargeted metabolomics and exposomics
* Redmine issue at NBIS: **#7579**  (internal project ID)
* Request by: **Stefano Papazian**
* Principal investigator: **Stefano Papazian** (<stefano.papazian@scilifelab.se>)
* Organisation: **Stockholm University**
* NBIS experts : Mun-Gwan Hong (<mungwan.hong@nbis.se>)
* Git repository : https://github.com/NBISweden/SMS-7579-23-exposome

--------------------------------------------------------------------------------

# How to use the program

## Copy the code

Copy all the code to the directory `<project>/code/`. 
One easy way to accomplish this is to make a Git clone of the project repository using the following commands.

```sh
cd <project>                    # <project> should be replaced with the appropriate path
git clone <repository> code     # Find the <repository> URL provided above
```

The subdirectories within the `code/` directory should be structured as shown below.

    ├── code/
    │   ├── envs/             # Software environment
    │   ├── projlib/          # Reusable code modules imported by other scripts
    │   ├── R/                # Functions for `app.R` (shiny app)
    │   ├── reports/          # Code that generates reports in `<project>/reports/`
    │   └── scripts/          # Scripts (AWK, R, Python, etc.)

## Software environment

To mitigate issues arising from software environment inconsistencies, the environment in which the scripts were developed has been saved together with the scripts.
We use a popular container system called [Docker](https://wwww.docker.com), which runs on multiple platforms such as Linux, MacOS and Windows.
The Docker image can be built using the `Dockerfile` provided in the `code/envs/` directory if you
want to customize the environment.
It encapsulates the software environment.
Please ensure that the necessary management software is pre-installed and running.
For installation questions, please refer to the software's website.

The commands below will restore the same environment and initiate the Shiny app.
Make sure you are in the `<project>` directory before running the command below.

```sh
docker pull mghong/exposome
docker run -it -v .:/proj -p 7579:7579 --name=expo-docker mghong/exposome expo_app.sh
```

## Shiny app

At the project directory, run the following command to start the Shiny app.

```sh
R -e "shiny::runApp('code')"
```

It will indicate the URL to access the Shiny app.
Copy and paste the URL to a web browser to access the Shiny app.

## Running scripts without the Shiny app

As an alternative way, you can also run the individual scripts in the command line.
First, find the **`params.yml`** file in the `<project>/` directory and modify the parameters as needed.

The scripts are written in R and qmd, and can be run in the following way.

```sh
cd <project>
Rscript code/scripts/read-msdial.R
Rscript code/scripts/process.R
Rscript code/scripts/export_data.R

cd <project>/code/reports
quarto render report-internal.qmd --output <output_report>.html
quarto render report-external.qmd --output <output_report>.pdf
```

--------------------------------------------------------------------------------

# Individual files under `code/`

Here are some descriptions on individual files.

* `constants.yml` : Contants used in analyses and visualization
* `LICENSE` : License for the code here

