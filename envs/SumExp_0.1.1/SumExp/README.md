# SumExp

SumExp is a list R package crafted for the management matrix-like data. 
Its design strategically emphasizes compatibility with popular data manipulation and visualization tools in the R ecosystem, namely the `labelled`, `tibble`, and `ggplot2` packages. 
This makes it an excellent package for handling complex data structures in various scientific and analytical contexts.

While the Bioconductor's SummarizedExperiment package is a robust tool for handling matrix-like data in bioinformatics, it comes with a substantial overhead due to numerous dependencies and extensive functionality. 
This complexity might not align with the needs of users who seek simplicity and integration with a few popular tools.

### Key Features

- **Efficiently interacts** with the `labelled` package, allowing for error-less and meaningful data annotation and retrieval.
- **Seamlessly integrates** with `tibble`, ensuring that data remains user-friendly, flexible and powerful.
- **Enhances data visualization capabilities** with direct use as a data for `ggplot2`, enabling the creation of high-quality plots directly from the data structures managed within SumExp.

## Installation

To install the latest development version of SumExp from GitHub, use:

```r
# If devtools is not already installed
# install.packages("devtools")

devtools::install_github("Rundmus/SumExp-R_package")
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

