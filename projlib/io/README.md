Here are reusable code modules that are imported by other scripts or functions within the current project.
These modules could contain utility functions, custom classes, or any other pieces of code that are used across multiple parts of the project.

### `__init__.R`

* Used by R `box` package, to load all the modules in the current directory.
* The same file may be in the subdirectories of the current directory for the subgroups of modules.

### `check_io_exist.R`

This has functions to check the availability of necessary input files and/or the directories to which output files are saved.

* `chq_all_files_exist` : checks if all input files exist.
* `mkdir_if_not_exist` : creates a directory if it does not exist.
* `check_io_exist` : checks if all input files exist and creates output directories if they do not exist.
