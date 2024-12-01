#' Check if 
.chq_if_a_single_char <- function(x) {
  x_chr <- deparse1(substitute(x))
  if (is.character(x) & length(x) == 1) return(TRUE)
  stop(paste("`", x_chr, "` should be a single character", sep = ""))
}

#' Extract QC samples into list
#'
#' @param se A `SumExp` object
#'
#' @return A list of SumExp objects divided by QC Classes
#' @export
extract_qc_samples_to_list <- function(se) {
  se <- se[, quote(proc_cat == "QC")]
  qc_id <- SumExp::col_df(se)$Class
  # Column-wise split
  lapply(stats::setNames(nm = unique(qc_id)), \(ii) se[, qc_id == ii])
}

#' Extract quantitative standards in QC samples
#'
#' @param sumexp A `SumExp` object
#' @return A list of SumExp objects with quantitative standards in QC samples
#' @export
extract_quant_qc <- function(sumexp) {
  se_lst <- extract_qc_samples_to_list(sumexp)
  lapply(se_lst, \(se) {
    se[quote(std_type == "Quant"), ]
  })
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
    lab <- labelled::get_label_attribute(x)
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
#' @param x_se A SumExp object with the samples to be plotted
#' @param cc_se A SumExp object with the calibration curve samples 
#' @param calcurve_models A list of the calibration curve models 
#' @param mat_id A character of the assay ID to be plotted in the y-axis
#'
#' @return A ggplot object
#' @export
ggplot_calcurve_samples <- function(x_se, cc_se, calcurve_models, mat_id) {
  .chq_if_a_single_char(mat_id)
  i_chems <- rownames(x_se)           # Limited to those chemicals in the `x_se`
  cc_se <- cc_se[i_chems, ]
  cc_df <- SumExp::as_tibble(cc_se) |> 
    # Drop concentrations outside the range
    tidyr::drop_na(tidyselect::all_of(unname(mat_id)))
  # LLOQ and LLOD
  region_df <- SumExp::row_df(cc_se)[, c("chem_name", "lloq", "llod")]
  # The samples to show with the calibration curve
  x_df <- SumExp::as_tibble(x_se) |> 
    tidyr::drop_na(conc)
  
  ggplot2::ggplot(x_df, ggplot2::aes(x = conc, y = .data[[mat_id]])) +
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
                                          mat_id,
                                          scales = "free",
                                          ncol = 3,
                                          ...) {
  ggplot_calcurve_samples(x_se, cc_se, calcurve_models, mat_id) +
    ggplot2::facet_wrap(~ chem_name, scales = scales, ncol = ncol, ...)
}
