
stopifnot(    # Required parameters
  !is.null(params$input_file), 
  !is.null(params$table_dir)
)
FILE <- list(
  i = params$input_file,
  o = local({
    b <- basename(params$input_file) |> 
      tools::file_path_sans_ext()    # Without extension
    rlang::list2(
      conc = rlang::list2(
        loess_norm = file.path(params$table_dir, paste0(b, ".loess.conc.tsv")),
        closest_norm = file.path(params$table_dir, paste0(b, ".closest.conc.tsv")),
      ),
      norm = rlang::list2(
        loess_norm = file.path(params$table_dir, paste0(b, ".loess.norm.tsv")),
        closest_norm = file.path(params$table_dir, paste0(b, ".closest.norm.tsv")),
      )
    )
  })
)



for(assy_id in c("loess_norm", "closest_norm")) {
  msdial$export_data_with_chem_table_tsv(
    sumexp = overall_sumexp, 
    mat_id = assy_id,
    in_file = FILE$i, 
    out_file = FILE$o$norm[[assy_id]]
  )
}



for(assy_id in c("loess_norm", "closest_norm")) {
  msdial$export_concentration_tsv(measurements_se, FILE$o$conc[[assy_id]])
}

