

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
#' @param assay_id The assay ID to be plotted in the y-axis
#' @return A ggplot object
#' @export
ggplot_calcurve_samples <- function(x_se, cc_se, calcurve_models, assay_id) {
  i_chems <- rownames(x_se)
  cc_se <- cc_se[i_chems, ]           # Limited to those chemicals in the `x_se`
  cc_df <- tibble::as_tibble(cc_se) |> 
    tidyr::drop_na(tidyselect::all_of(assay_id))      # Drop concentrations outside the range
  
  # The samples to show with the calibration curve
  x_df <- tibble::as_tibble(x_se) |> 
    tidyr::drop_na(conc)
  # LLOQ and LLOD
  region_df <- SummarizedExperiment::rowData(cc_se)[, c("chem_name", "lloq", "llod")]
  
  ggplot2::ggplot(x_df, ggplot2::aes(x = conc, y = .data[[assay_id]])) +
    # Remaining calibration samples
    ggplot2::geom_point(ggplot2::aes(x = c_conc), data = cc_df) + 
    # Samples to measure
    ggplot2::geom_point(ggplot2::aes(color = Class)) +
    # Density of points at the edges of the plot
    ggplot2::geom_rug(color = grDevices::rgb(0.5, 0, 0, alpha = 0.2)) +
    ggplot2::labs(
      x = "Concentration (ng/ml)",
      y = "Peak area (Normalized)"
    ) +  
    ggplot2::facet_wrap(~ chem_name, scales = "free", ncol = 3) +
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
