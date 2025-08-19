box::use(
  ../../projlib/proc,
)
sumexp <- readRDS("testdata-sumexp.rds")

test_that("find_calib_lim_pts_and_llox_from_llox_signal", {
  calib_lim_pts <- proc$find_calib_lim_pts_and_llox_from_llox_signal(
    x_se = sumexp,
    mat_id = "loess_norm_blk",
    compute_llox_signal_fun = proc$compute_llox_signal_using_mean_times
  )

  expect_s3_class(calib_lim_pts, "data.frame")
})