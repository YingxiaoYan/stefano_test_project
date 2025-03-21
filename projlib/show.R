#' Check if 
.chq_if_a_single_char <- function(x) {
  x_chr <- deparse1(substitute(x))
  if (is.character(x) & length(x) == 1) return(TRUE)
  stop(paste("`", x_chr, "` should be a single character", sep = ""))
}

#' Extract QC samples into list
#'
#' @param se A [`SumExp::SumExp`] object
#'
#' @return A list of SumExp objects divided by QC Classes
#' @md
#' @export
extract_qc_samples_to_list <- function(se) {
  se <- se[, quote(contr_cat == "QC")]
  qc_id <- SumExp::col_df(se)$Class
  # Column-wise split
  lapply(stats::setNames(nm = unique(qc_id)), \(ii) se[, qc_id == ii])
}

#' Extract quantitative standards in QC samples
#'
#' @param sumexp A [`SumExp::SumExp`] object
#' @return A list of SumExp objects with quantitative standards in QC samples
#' @md
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
#' @param qc_se_lst A list of [`SumExp::SumExp`] objects with quantitative standards in QC samples
#' @param mat_ids A character vector of the matrix IDs
#' @return A tibble with RSD% of the quantification standard samples. The first two columns are
#'   `QC` and `feature_id`.
#' @md
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
#' @param sumexp A [`SumExp::SumExp`] object
#' @param color_cat A named character vector of colors for each control category
#'
#' @return A named character vector of colors for each class
#' @md
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
#' @param default A default label if the object does not have a label attribute
#'
#' @return A character of the label attribute or the default label
#' @export
label_if_has <- function(x, default = deparse1(substitute(x))) {
  lab <- labelled::get_label_attribute(x)
  if (!is.null(lab)) return(lab)
  default
}

#' Calibration curve line
#' 
#' @param lim_df A data frame with columns `lloq`, `max_c_conc`, and `calcurve_model`.
#' @param color,alpha,... Arguments passed to `ggplot2::geom_line`
#' @returns A [`ggplot2::geom_line`] object. 
#' @md
#' @export
geom_calibration_curve_line <- function(lim_df, color = "cadetblue", alpha = 0.7, ...) {
  # Artificial concentration values for smooth calibration curve line
  ccline_df <- purrr::pmap(lim_df, \(lloq, max_c_conc, calcurve_model, ...) {
    conc <- sort(c(
      seq(lloq, max_c_conc, length.out = 100),
      seq(lloq, min(lloq * 10, max_c_conc), length.out = 100)[-c(1, 100)]   # Zoom-in 
    ))
    .y_value <- stats::predict.lm(calcurve_model$inv_model, data.frame(conc = conc))
    tibble::tibble(conc = conc, .y_value = .y_value, ...)
  }) |> 
    dplyr::bind_rows() |> 
    dplyr::arrange(feature_id, conc)      # To make sure
  
  ggplot2::geom_line(
    ggplot2::aes(x = conc, y = .y_value), data = ccline_df,
    color = color, 
    alpha = alpha,
    ...
  )
}

#' Plot the calibration curve with the samples
#'
#' @description
#' Plot the calibration curve for each feature together with the samples. The X-axis is the
#' concentration and the Y-axis is the peak area of the intensity.
#' 
#' @param x_se A [`SumExp::SumExp`] object having the data of the samples to be plotted
#' @param cc_se A [`SumExp::SumExp`] object of the calibration curve samples. The features in
#'   `x_se` should be a subset of `cc_se`.
#' @param mat_id A character of the assay ID to be plotted in the y-axis
#' @param colors_of_classes A named character vector of colors for each class
#'
#' @return A ggplot object. 
#' @md
#' @export
ggplot_calcurve_samples <- function(x_se, cc_se, mat_id, colors_of_classes) {
  .chq_if_a_single_char(mat_id)
  feature_ids <- rownames(x_se)           # Limited to those features in the `x_se`
  cc_se <- cc_se[feature_ids, ]
  cc_df <- SumExp::as_tibble(cc_se) |> 
    # Drop concentrations outside the range
    tidyr::drop_na(tidyselect::all_of(unname(mat_id)))
  # LLOQ, LOD, max_c_conc of each `feature_id`
  lim_df <- SumExp::row_df(x_se) |> 
    # `feature_name` is added for further usage, e.g. `ggplot2::facet_wrap(~ feature_name)`
    dplyr::select(feature_name, lloq, lod, max_c_conc, calcurve_model) |> 
    tibble::rownames_to_column("feature_id") 
  # The samples to show with the calibration curve
  x_df <- SumExp::as_tibble(x_se) |> 
    tidyr::drop_na(conc)
  
  out <- ggplot2::ggplot(x_df, ggplot2::aes(x = conc, y = .data[[mat_id]])) +
    geom_calibration_curve_line(lim_df) +
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
    ggplot2::geom_rect(         # Shade the region below the LOD
      ggplot2::aes(xmin = 0, xmax = lod, ymin = 0, ymax = Inf),
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
                                          mat_id,
                                          colors_of_classes,
                                          scales = "free",
                                          ncol = 3,
                                          ...) {
  ggplot_calcurve_samples(x_se, cc_se, mat_id, colors_of_classes) +
    ggplot2::facet_wrap(~ feature_name, scales = scales, ncol = ncol, ...)
}


#' ggproto for zooming in by the calibration curve
CoordZoomInByCC <- ggplot2::ggproto(
  "CoordZoomInByCC", ggplot2::CoordCartesian, 
  # Data for calibration points. Matched with `ggplot_calcurve_samples`
  i_data = 3,
  n_cc = 4,   # Number of calibration points
  c_conc = "c_conc",       # Concentration column name
  mat_id = NULL,           # Matrix ID column name
  feat_id = ".row_id",     # Feature ID column name
  facet_by = "feature_name",    # Facet by feature name
  
  # Set updated limits by the calibration points
  setup_panel_params = function(self, scale_x, scale_y, params) {
    # Iteratively run for each panel
    self$panel_counter <- self$panel_counter + 1
    df_cc <- self$df_cc
    if (nrow(self$layout) > 1) {   # Multiple panels
      f <- self$layout[[self$facet_by]][self$panel_counter]
      df_cc <- df_cc[df_cc[[self$facet_by]] == f, , drop = FALSE]
    }
    xlim <- c(0, max(df_cc[[self$c_conc]], na.rm = TRUE))
    ylim <- c(0, max(df_cc[[self$mat_id]], na.rm = TRUE))
    # The same format as CoordCartesian$setup_panel_params
    c(ggplot2:::view_scales_from_scale(scale_x, xlim, self$expand), 
      ggplot2:::view_scales_from_scale(scale_y, ylim, self$expand))
  },
  
  # Filter out the data above the upper-bound
  setup_data = function(self, data, params) {
    df_cc <- data[[self$i_data]]    # Data for calibration points
    # Find upper-bound, which is the `n_cc`th lowest concentration
    df_cc_range <- df_cc[, c(self$feat_id, self$c_conc)]
    names(df_cc_range) <- c("feat_id", "c_conc")
    df_cc_range <- dplyr::distinct(df_cc_range) |> 
      dplyr::summarise(.ub = c_conc[order(c_conc)][self$n_cc], .by = feat_id)
    # Filter out the concentration above the bound
    df_cc <- dplyr::right_join(df_cc_range, df_cc, by = c(feat_id = self$feat_id)) |>
      dplyr::filter(c_conc <= .ub) |> 
      dplyr::select(-.ub)
    # Save the filtered data and make it accessible to `setup_panel_params`
    self$df_cc <- df_cc
    data[[self$i_data]] <- df_cc
    data
  },

  # Initialize the panel counter and save the layout for `setup_panel_params`
  setup_layout = function(self, layout, params) {
    self$panel_counter <- 0
    self$layout <- layout
    layout
  }
)

#' Create a ggproto object for zooming in by the calibration curve points
#'
#' @param n_cc Number of calibration points to be presented
#' @param mat_id Matrix ID column name
#' @inheritParams ggplot2::coord_cartesian
#' @export
coord_zoom_in_by_cc <- function(n_cc = 4, 
                                mat_id = NULL, 
                                expand = TRUE, 
                                default = FALSE, 
                                clip = "on") {
  ggplot2::ggproto(
    NULL, CoordZoomInByCC, 
    n_cc = n_cc,
    mat_id = mat_id, 
    expand = expand,
    default = default,
    clip = clip
  )
}


#' [ggplot2::geom_text()] for the best model of the calibration curve
#' 
#' @param x_se A [`SumExp::SumExp`] object
#' @param hjust,vjust,size,color,... Parameters to be passed to `ggplot2::geom_text()`
#' @returns A [`ggplot2::geom_text()`] output
#' @md
#' @seealso [ggplot_calcurve_samples()]
#' @export
geom_best_model_text <- function(x_se, hjust = 1, vjust = 0, size = 4, color = "red3", ...) {
  show_df <- SumExp::row_df(x_se) |> 
    # `feature_name` is added for further usage, e.g. `ggplot2::facet_wrap(~ feature_name)`
    dplyr::select(feature_name, calcurve_model) |> 
    tibble::rownames_to_column("feature_id") 
  # Text for the selected calibration curve models
  best_model_text_df <- show_df |> 
    dplyr::mutate(
      best_model_name = purrr::map_chr(calcurve_model, \(m) m$best_model_name),
      R2 = purrr::map_dbl(calcurve_model, \(m) max(m$R2s)),
      txt = paste0(best_model_name, "\n R2=", round(R2, 3)),
      .keep = "unused"
    ) 
  ggplot2::geom_text(         # Display the best model name
    ggplot2::aes(x = Inf, y = 0, label = txt),
    data = best_model_text_df,
    hjust = hjust,
    vjust = vjust,
    size = size,
    color = color,
    ...
  )
}

#' Create a summary table for the chemicals
#'
#' @param sumexp A [`SumExp::SumExp`] object
#'
#' @returns A tibble with the summary of the chemical. The columns include: `chem_id`,
#'   `chem_name`, `lod`, `lloq`, `n_det`, `perc_detf`, `median`, `mean`, `min`, `max`,
#'   `best_model`, `model_r2`, `n_conc`
#' @md
#' @export
tbl_chemical_summary <- function(sumexp) {
  # Information about the chemicals
  chemicals <- SumExp::row_df(sumexp) |> 
    tibble::as_tibble(rownames = "chem_id") |> 
    dplyr::rename(chem_name = "feature_name")
  # Concentration
  mat <- sumexp[["conc"]]
  # Summary about the concentration ranges
  conc_summary <- chemicals |> 
    dplyr::mutate(
      n_det = sapply(1:nrow(mat), \(i) sum(mat[i, ] >= lloq[i], na.rm = TRUE)),
      n_samples = sapply(1:nrow(mat), \(i) sum(!is.na(mat[i, ]))),
      perc_detf = n_det / n_samples * 100,
      median = apply(mat, 1, stats::median),
      mean = rowMeans(mat),
      min = apply(mat, 1, min),
      max = apply(mat, 1, max),
    )
  
  # Add summary about the calibration curve models
  conc_summary |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      best_model = calcurve_model$best_model_name,
      model_r2 = calcurve_model$R2s[[best_model]],
      n_conc = calcurve_model$n_conc,
      .keep = "unused"     # Remove `calcurve_model`
    ) |> 
    dplyr::ungroup()
}

