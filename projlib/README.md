Here are reusable code modules,
which are imported by other scripts or functions within the current project.
These modules could contain utility functions, custom classes, or any other pieces of code that are used across multiple parts of the project.

### `check_io_exist.R`

Provides functions to **validate file** and directory existence before processing:

- `chq_all_files_exist()`: Verifies that all required input files exist
- `mkdir_if_not_exist()`: Creates output directories with proper permissions
- `check_io_exist()`: Main validation function that checks input files and creates output directories

### `msdial_utils.R`

Helper functions to access and **manipulate MS-DIAL data elements**:

- Feature accessors: `std_type()`, `is_targeted_feature()`, `retention_time()`
- Sample accessors: `ctrl_smpl_cat()`, `spiked_conc_pts()`
- Calibration curve utilities: `get_cc_model()`, `set_cc_model()`
- Matrix manipulation: `mat_id_of_blank_subtracted()`, `extract_with_na()`
- Statistical utilities: `.rsd_perc()` for calculating relative standard deviation

### `msdial.R`

Handles **reading and writing** MS-DIAL raw and processed data:

- `get_raw_data_file_name()`: Generates standardized file names for intermediate data
- `get_three_section_indices()`: Identifies the three column sections in MS-DIAL output files
- `fetch_sample_info()`: Extracts sample metadata from MS-DIAL files
- `export_data_with_feature_table_xlsx()`: Exports processed data with feature tables to Excel
- `tbl_chemical_summary()`: Creates summary tables with LOD, LLOQ, detection rates, and calibration curve statistics
- `merge_sumexp()`: Merges multiple SumExp objects with identical features

### `proc.R`

**Core data processing** and normalization functions:

- **Zero imputation**: `impute_zeros_with_mean_of_same_type()` handles missing values
- **Quality control**: `count_outliers_per_sample()` identifies outlying internal standard features
- **Internal standard normalization**: `get_value_idx_of_closest_istd()` finds closest internal standards by retention time
- **LOESS normalization**: `get_loess_fit()` performs retention time-based normalization
- **Blank subtraction**: `add_blank_subtracted_sumexp()` subtracts blank sample averages
- **Calibration curve fitting**: Functions for working range determination, model fitting (linear, quadratic, weighted), and concentration calculation
- **LOD/LLOQ calculation**: Determines limits of detection and quantification

### `show.R`

Functions for generating **plots and tables** for reports:

- **Quality metrics**: `compute_rsd_per_feature()` calculates RSD% across samples
- **Color schemes**: `get_colors_of_classes()` manages consistent color palettes
- **QC visualization**: `extract_quant_qc()` extracts quantitative standards from QC samples
- **Calibration curve plots**: `ggplot_calcurve_samples()` and `ggplot_calcurve_samples_facet()` create calibration curve visualizations with sample points
- **RSD plots**: `ggplot_rsdp_metab()` visualizes RSD% distributions
- Custom ggplot2 coordinate system `CoordZoomInByCC` for zooming into calibration curve ranges
