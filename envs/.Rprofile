
# `renv` SETTINGS {{{
# These are to install R packages temporarily before `renv.lock` update. 
# Run R at the current directory with internet connection. Install packages and update `renv.lock`
# using `renv::snapshot()` or `renv::snapshot(type = "all")` to include all installed packages.

Sys.setenv( RENV_PROJECT = getwd() )   # Avoid renv::activate message
warning(
  "As R is running on envs/, ",
  "renv::snapshot will include all installed packages, not just those that are in use."
)

# To install packages temporarily before `renv.lock` update
if(!dir.exists("renv/library")) {
  dir.create("renv/library", recursive = TRUE, showWarnings = FALSE)
  warning("`renv/library` is created.")
}
.libPaths("renv/library")

# Install all packages including the newly installed
options(
  renv.settings = list(
    use.cache = FALSE,
    snapshot.type = "all"
  )
)
# }}} `renv` SETTINGS 

