#' Check if 
.chq_if_a_single_char <- function(x) {
  x_chr <- deparse1(substitute(x))
  if (is.character(x) & length(x) == 1) return(TRUE)
  stop(paste("`", x_chr, "` should be a single character", sep = ""))
}


#' Make a frequency `kable` table by factor level
#'
#' @param x A factor or character variable
#' @param what What to be counted
#' @param exclude Levels to be excluded
#' @param lab The label of the variable
#' @param ... Arguments passed to `janitor::tabyl` 
#'
#' @return A `kable` table with the number of samples and percentage of each level
#' @export
kable_number_of <- function(x, what = "Samples", exclude = NULL, lab, ...) {
  if (missing(lab)) {
    lab <- expss::var_lab(x)
    if (is.null(lab)) lab <- deparse1(substitute(x))
  } else {
    .chq_if_a_single_char(lab)
  }
  tb <- janitor::tabyl(x, ...) |> 
    janitor::adorn_pct_formatting() |> 
    dplyr::filter(!x %in% exclude)
    knitr::kable(
      tb,
      row.names = FALSE,          # To avoid excluded row numbers
      col.names = c(lab, paste("Number of", what), "Percent"),
      align = "lrr"
    ) |> 
    kableExtra::kable_styling(full_width = FALSE)
}

#' Plot the RSD% of various data sets of chemicals
#'
#' @param rsd_df A data frame with columns `chem_id` as well as all in `assay_ids`
#' @param assay_ids A character vector of assay IDs to be plotted in the x-axis
#' @return A ggplot object
#' @export
ggplot_rsdp_metab <- function(rsd_df, assay_ids) {
  nms <- names(assay_ids)
  ids <- unname(assay_ids)    # Duplicated by `dplyr::all_of`
  rsd_df <- rsd_df |> 
    dplyr::mutate(
      # NA was generated when all values were zero
      dplyr::across(
        dplyr::all_of(ids), 
        \(.x) ifelse(is.na(.x), 0, .x)
      ),
    ) |> 
    tidyr::pivot_longer(
      dplyr::all_of(ids), 
      names_to = "Data", 
      values_to = "RSD%"
    ) |> 
    dplyr::mutate(Data = factor(Data, levels = ids, labels = nms))
  ggplot2::ggplot(rsd_df) +
    ggplot2::aes(x = Data, y = `RSD%`) +
    ggplot2::geom_line(ggplot2::aes(group = chem_id), alpha = 0.5)
}

#' Plot the calibration curve for each chemical with the samples in the calibration curve
#'
#' @param data A data frame with columns `conc`, `Class`, `chem_name` and the column given in
#'   `assay_id`
#' @param cc_df A data frame of calibration samples with columns `c_conc`, `chem_name` and the
#'   column given in `assay_id`
#' @param region_df A data frame with columns `lloq`, `max_c_conc` and `chem_name`
#' @param assay_id A character of the assay ID to be plotted in the y-axis
#' @return A ggplot object
#' @export
#' @rdname ggplot_calcurve_samples
ggplot_calcurve_samples <- function(x_se, cc_se, calcurve_models, assay_id) {
  .chq_if_a_single_char(assay_id)
  i_chems <- rownames(x_se)           # Limited to those chemicals in the `x_se`
  cc_se <- cc_se[i_chems, ]
  cc_df <- tibble::as_tibble(cc_se) |> 
    tidyr::drop_na(tidyselect::all_of(assay_id))      # Drop concentrations outside the range
  # LLOQ and LLOD
  region_df <- SummarizedExperiment::rowData(cc_se)[, c("chem_name", "lloq", "llod")]
  # The samples to show with the calibration curve
  x_df <- tibble::as_tibble(x_se) |> 
    tidyr::drop_na(conc)
  
  ggplot2::ggplot(x_df, ggplot2::aes(x = conc, y = .data[[assay_id]])) +
    # Remaining calibration samples
    ggplot2::geom_point(ggplot2::aes(x = c_conc), data = cc_df) + 
    # Samples to measure
    ggplot2::geom_point(ggplot2::aes(color = Class)) +
    # Density of points at the edges of the plot
    ggplot2::geom_rug(color = grDevices::rgb(0.5, 0, 0, alpha = 0.2)) +
    ggplot2::labs(x = "Concentration (ng/ml)") +  
    ggplot2::geom_rect(         # Shade the region below the LLOQ
      ggplot2::aes(xmin = 0, xmax = lloq, ymin = 0, ymax = Inf),
      data = region_df,
      fill = "grey30",
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_rect(         # Shade the region below the LLOD
      ggplot2::aes(xmin = 0, xmax = llod, ymin = 0, ymax = Inf),
      data = region_df,
      fill = "grey25",
      alpha = 0.5,
      inherit.aes = FALSE
    )
}

#' @inheritParams ggplot2::facet_wrap
#' @param ... parameters to be passed to `ggplot2::facet_wrap`
#' @export
#' @rdname ggplot_calcurve_samples
ggplot_calcurve_samples_facet <- function(x_se,
                                          cc_se,
                                          calcurve_models,
                                          assay_id,
                                          scales = "free",
                                          ncol = 3,
                                          ...) {
  ggplot_calcurve_samples(x_se, cc_se, calcurve_models, assay_id) +
    ggplot2::facet_wrap(~ chem_name, scales = scales, ncol = ncol, ...)
}
