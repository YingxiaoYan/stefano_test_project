#' Check if 
.chq_if_a_single_char <- function(x) {
  x_chr <- deparse1(substitute(x))
  if (is.character(x) & length(x) == 1) return(TRUE)
  stop(paste("`", x_chr, "` should be a single character", sep = ""))
}

#' Extract QC samples into list
#'
#' @param se A [`SumExp`] object
#'
#' @return A list of SumExp objects divided by QC Classes
#' @export
extract_qc_samples_to_list <- function(se) {
  se <- se[, quote(contr_cat == "QC")]
  qc_id <- SumExp::col_df(se)$Class
  # Column-wise split
  lapply(stats::setNames(nm = unique(qc_id)), \(ii) se[, qc_id == ii])
}

#' Extract quantitative standards in QC samples
#'
#' @param sumexp A [`SumExp`] object
#' @return A list of SumExp objects with quantitative standards in QC samples
#' @export
extract_quant_qc <- function(sumexp) {
  se_lst <- extract_qc_samples_to_list(sumexp)
  lapply(se_lst, \(se) {
    se[quote(std_type == "Quant"), ]
  })
}

#' Calculate RSD%
#'
#' @param x A numeric vector
#' @param na.rm A logical value indicating whether to remove NA values
#' @return A numeric value of RSD%
#' @export
rsd_perc <- function(x, na.rm = FALSE) {
  100 * stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

#' Calculate RSD of the quantification standard samples
#'
#' @param qc_se_lst A list of SumExp objects with quantitative standards in QC samples
#' @param mat_ids A character vector of the matrix IDs
#' @return A tibble with RSD% of the quantification standard samples. The first two columns are
#'   `QC` and `feature_id`.
#' @export
calc_rsd_qstd <- function(qc_se_lst, mat_ids) {
  out <- lapply(qc_se_lst, \(qc_se) {
    sapply(stats::setNames(nm = mat_ids), function(nm) {
      apply(qc_se[[nm]], 1, rsd_perc)
    }) |>    # Rows are features, columns are matrix IDs
      tibble::as_tibble(rownames = "feature_id")
  }) |> 
    # `QC` has the name of the QC samples
    dplyr::bind_rows(.id = "QC")
  for(ii in mat_ids) {
    out[[ii]] <- labelled::copy_labels(qc_se_lst[[1]][[ii]], out[[ii]])
  }
  out   
}

#' RSD% across all samples
#'
#' @param mat A numeric matrix
#' @return A numeric vector of RSD% across the samples in columns
#' @export
compute_rsd_per_feature <- function(mat) {
  apply(mat, 1, rsd_perc) |> 
    labelled::set_label_attribute("RSD%")
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

#' Get the colors of classes
#'
#' @param sumexp A [`SumExp`] object
#' @param color_cat A named character vector of colors for each control category
#'
#' @return A named character vector of colors for each class
#' @export
get_colors_of_classes <- function(sumexp, color_cat) {
  df1 <- SumExp::col_df(sumexp)
  by_cat <- split(df1, df1$contr_cat) |> 
    lapply(\(.x) unique(.x$Class))
  # Fix the colors of the classes given in `color_cat`
  color_given <- by_cat[names(by_cat) %in% names(color_cat)] |> 
    purrr::imap(~ stats::setNames(rep(color_cat[[.y]], length(.x)), .x)) |> # Class = "color"
    purrr::flatten()
  # Remaining classes
  rest <- unlist(by_cat[! names(by_cat) %in% names(color_cat)])
  n <- length(rest)
  # Use ggplot2 default colors excluding the first red, which is similar to 'red'
  ggcolor <- grDevices::hcl(seq(15, 375, length.out = n + 2), 100, 65)[2:(n+1)]
  names(ggcolor) <- rest
  c(color_given, ggcolor) 
}


#' Plot the RSD% of various data sets of features
#'
#' @param rsd_df A data frame with columns `feature_id` as well as all in `mat_ids`
#' @param mat_ids A character vector of assay IDs to be plotted in the x-axis
#' @return A ggplot object
#' @export
ggplot_rsdp_metab <- function(rsd_df, mat_ids) {
  nms <- names(mat_ids)
  if (is.null(nms)) {
    nms <- labelled::get_variable_labels(rsd_df[mat_ids])
  }
  ids <- unname(mat_ids)    # Duplicated by `dplyr::all_of`
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
    ggplot2::geom_line(ggplot2::aes(group = feature_id), alpha = 0.5)
}




#' Extract the label attribute of a variable if it has
#'
#' @param x An object that may have a label attribute
#' @param default_lab A default label if the object does not have a label attribute
#'
#' @return A character of the label attribute or the default label
#' @export
label_if_has <- function(x, default_lab) {
  lab <- labelled::get_label_attribute(x)
  if (!is.null(lab)) return(lab)
  default_lab
}

#' Plot the calibration curve with the samples
#'
#' @description
#' Plot the calibration curve for each feature together with the samples. The X-axis is the
#' concentration and the Y-axis is the peak area of the intensity.
#' 
#' @param x_se A SumExp object having the data of the samples to be plotted
#' @param cc_se A SumExp object of the calibration curve samples. The features in `x_se` should
#'   be a subset of `cc_se`.
#' @param cc_models A list of the linear regression models of the calibration curve samples.
#'   The names of the list should be the same as the `feature_id` in `x_se` and `cc_se`.
#' @param mat_id A character of the assay ID to be plotted in the y-axis
#' @param colors_of_classes A named character vector of colors for each class
#'
#' @return A ggplot object. 
#' @export
ggplot_calcurve_samples <- function(x_se, cc_se, cc_models, mat_id, colors_of_classes) {
  .chq_if_a_single_char(mat_id)
  feature_ids <- rownames(x_se)           # Limited to those features in the `x_se`
  cc_se <- cc_se[feature_ids, ]
  cc_df <- SumExp::as_tibble(cc_se) |> 
    # Drop concentrations outside the range
    tidyr::drop_na(tidyselect::all_of(unname(mat_id)))
  # LLOQ, LLOD, max_c_conc of each `feature_id`. `feature_name` is for the output ggplot
  lim_df <- SumExp::row_df(cc_se)[, c("feature_name", "lloq", "llod", "max_c_conc")]
  # The samples to show with the calibration curve
  x_df <- SumExp::as_tibble(x_se) |> 
    tidyr::drop_na(conc)
  
  # Calibration curve lines
  .geom_ccline <- function(feature_ids, lim_df, cc_models) {
    # `feature_name` is added to the data of the output ggplot for further usage
    feature_id_name <- tibble::rownames_to_column(lim_df, "feature_id")
    feature_ids <- stats::setNames(nm = feature_ids)
    conc_df <- sapply(feature_ids, \(feature_id) {
      lims <- lim_df[feature_id, ]
      lloq <- lims$lloq
      max_c_conc <- lims$max_c_conc
      sort(c(
        seq(lloq, max_c_conc, length.out = 100),             # Linear scale
        exp(seq(log(lloq), log(max_c_conc), length.out = 100))[-c(1, 100)]   # Log scale
      ))
    }) |> 
      as.data.frame() |> 
      tidyr::pivot_longer(tidyr::everything(), names_to = "feature_id", values_to = "conc") |> 
      dplyr::left_join(feature_id_name, by = "feature_id")
    
    ccline_df <- conc_df |> 
      dplyr::mutate(
        .y_value = purrr::map2_dbl(
          feature_id, conc, 
          ~stats::predict.lm(cc_models[[.x]], data.frame(conc = .y))
        )
      ) |> 
      dplyr::arrange(feature_id, conc)
    ggplot2::geom_line(
      ggplot2::aes(x = conc, y = .y_value), data = ccline_df,
      color = "cadetblue",
      alpha = 0.7
    )
  }
  
  out <- ggplot2::ggplot(x_df, ggplot2::aes(x = conc, y = .data[[mat_id]])) +
    .geom_ccline(feature_ids, lim_df, cc_models) +
    # Remaining calibration samples
    ggplot2::geom_point(ggplot2::aes(x = c_conc), data = cc_df) + 
    # Samples to measure
    ggplot2::geom_point(ggplot2::aes(color = Class)) +
    # Density of points at the edges of the plot
    ggplot2::geom_rug(color = grDevices::rgb(0.5, 0, 0, alpha = 0.2)) +
    ggplot2::labs(
      x = label_if_has(x_df$conc, "Concentration"),
      y = label_if_has(x_se[[mat_id]], mat_id),
    ) +  
    ggplot2::geom_rect(         # Shade the region below the LLOQ
      ggplot2::aes(xmin = 0, xmax = lloq, ymin = 0, ymax = Inf),
      data = lim_df,
      fill = "grey30",
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_rect(         # Shade the region below the LLOD
      ggplot2::aes(xmin = 0, xmax = llod, ymin = 0, ymax = Inf),
      data = lim_df,
      fill = "grey25",
      alpha = 0.5,
      inherit.aes = FALSE
    )
  if (missing(colors_of_classes)) return(out)
  out + ggplot2::scale_color_manual(values = colors_of_classes)
}

#' @inheritParams ggplot2::facet_wrap
#' @param ... parameters to be passed to `ggplot2::facet_wrap`
#' @export
#' @rdname ggplot_calcurve_samples
ggplot_calcurve_samples_facet <- function(x_se,
                                          cc_se,
                                          cc_models,
                                          mat_id,
                                          colors_of_classes,
                                          scales = "free",
                                          ncol = 3,
                                          ...) {
  ggplot_calcurve_samples(x_se, cc_se, cc_models, mat_id, colors_of_classes) +
    ggplot2::facet_wrap(~ feature_name, scales = scales, ncol = ncol, ...)
}
