source("../../projlib/proc.R", chdir = TRUE)    # Include non-exported functions for testing
to_report <- readRDS("../testdata/testdatato_report.rds")

test_that("add_blank_subtracted_sumexp", {
  sumexp <- to_report[["normalized"]]
  se1 <- add_blank_subtracted_sumexp(
    sumexp = sumexp,
    mat_ids = c("loess_norm"),
    out_mat_ids = c("loess_norm_blk"),
    no_change = SumExp::col_df(sumexp)[[".ctrl_cat"]] == "CalCurve"
  )
  expect_s4_class(se1, "SumExp")
  expect_true("loess_norm_blk" %in% names(se1))
})

test_that("find_calib_lim_pts_and_llox_from_llox_signal", {
  calib_lim_pts <- find_calib_lim_pts_and_llox_from_llox_signal(
    sumexp = to_report[["normalized - blank"]][[1]],
    mat_id = "loess_norm_blk",
    compute_llox_signal_fun = compute_llox_signal_using_mean_times,
    use_rsd20 = TRUE
  )
  expect_s4_class(calib_lim_pts, "SumExp")
})

test_that("make_sure_to_have_enough_calcurve_pts", {
  expect_equal(
    make_sure_to_have_enough_calcurve_pts(
      max_conc = c(50, 20, 10,  5,  2,  1,  0, NA),
      min_conc = c( 1,  1,  1,  1,  1,  1,  1,  1),
      conc_pts = c(0.5, 1, 2, 5, 10, 20, 50),
      min_n = 3,
      enough_n = 4
    ),
    c(50, 20, 10, 10, 10, 10, 10, NA)
  )
  expect_equal(
    make_sure_to_have_enough_calcurve_pts(
      max_conc = c(50, 50, 50, 50, 50, 50, 50, 50),
      min_conc = c( 1,  2,  5, 10, 20, 50,  0, NA),
      conc_pts = c(0.5, 1, 2, 5, 10, 20, 50),
      min_n = 3,
      enough_n = 4
    ),
    c(50, 50, 50, 50, NA, NA, NA, NA)
  )
})

concn_se <- readRDS("../testdata/testdataproc.rds")[["loess_norm_blk"]]

test_that("replace_conc_whose_signal_below_lloq", {
  se1 <- replace_conc_whose_signal_below_lloq(
    sumexp = concn_se,
    signal_mat_id = "loess_norm_blk",
    conc_mat_id = "conc"
  )
  expect_s4_class(se1, "SumExp")
  expect_false(any(is.na(se1[["conc"]])))
})
