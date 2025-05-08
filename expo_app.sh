#! /bin/bash

# This script is used to run the Shiny app

# Note: The working directory has been changed by .Rprofile
R -e "shiny::runApp('code', launch.browser = TRUE)"

