box::use(
  ../../projlib/proc,
)
sumexp <- readRDS("testdata-sumexp.rds")

test_that("find_calib_lim_pts_and_llox_from_llox_signal", {
  calib_lim_pts <- proc$find_calib_lim_pts_and_llox_from_llox_signal(
    sumexp = sumexp,
    mat_id = "loess_norm_blk",
    compute_llox_signal_fun = proc$compute_llox_signal_using_mean_times
  )

  expect_s4_class(calib_lim_pts, "SumExp")
})

concn_se <- readRDS("testdata-sumexp-conc.rds")

test_that("replace_conc_whose_signal_below_lloq", {
  se1 <- proc$replace_conc_whose_signal_below_lloq(
    sumexp = concn_se,
    signal_mat_id = "loess_norm_blk",
    conc_mat_id = "conc"
  )
  expect_s4_class(se1, "SumExp")
  expect_false(any(is.na(se1[["conc"]])))
})
