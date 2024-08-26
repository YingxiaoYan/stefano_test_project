# `renv` SETTINGS {{{
# These are required to use `renv` and update `renv.lock` file.

# Set the `renv` paths
Sys.setenv(
  RENV_PATHS_RENV = "code/envs/renv",
  RENV_PROJECT = getwd()   # Avoid renv::activate message
)
.libPaths(c(.libPaths(), "code/envs/renv/library"))

# For implicit snapshot
options(
  renv.settings = list(
    use.cache = FALSE,
    snapshot.type = "implicit"
  )
)
# }}} `renv` SETTINGS 

