# For Results, write to output/ -------------------------------------------------------------

write_and_return_path <- function(x) {
  path <- paste0("output/", deparse(substitute(x)), ".csv")
  write_csv(x, path)
  return(path)
}

write_lines_and_return_path <- function(x) {
	path <- paste0("output/", deparse(substitute(x)), ".txt")
	write_lines(x, path)
	return(path)
}


# For codes and ids, for extracts and defines, write to sensitive_output -----------------------------

write_delim_and_return_path <- function(x) {
	path <- paste0("sensitive_output/", deparse(substitute(x)), ".txt")
	write_delim(x, path)
	return(path)
}

write_delim_compressed_in_chunks_and_return_paths <- function(x, max) {
	size <- seq_len(nrow(x))
	chunked <- split(x, ceiling(size/max))
	
	paths <- paste0("sensitive_output/", deparse(substitute(x)), seq_len(length(chunked)), ".txt.gz")

	for (i in seq_len(length(chunked))) {
		write_delim(chunked[[i]], file=gzfile(paths[[i]]))
	}
	return(paths)
}

write_gt_and_return_path <- function(x) {
	path <- paste0("sensitive_output/", deparse(substitute(x)), ".html")
	gtsave(x, path)
	return(path)
}

write_svg_and_return_path <- function(x) {
	path <- paste0("sensitive_output/", deparse(substitute(x)), ".svg")
	ggsave(plot=x, filename=path, device="svg")
	return(path)
}

write_png_and_return_path <- function(x) {
	path <- paste0("sensitive_output/", deparse(substitute(x)), ".png")
	ggsave(plot=x, filename=path, device="png")
	return(path)
}

write_ggsurvplot_svg_and_return_path <- function(x) {
	#see https://github.com/kassambara/survminer/issues/152
	path <- paste0("sensitive_output/", deparse(substitute(x)), ".svg")
	ggsave(plot=survminer:::.build_ggsurvplot(x), filename=path, device="svg")
	return(path)
}

write_ggsurvplot_png_and_return_path <- function(x) {
	#see https://github.com/kassambara/survminer/issues/152
	path <- paste0("sensitive_output/", deparse(substitute(x)), ".png")
	ggsave(plot=survminer:::.build_ggsurvplot(x), filename=path, device="png")
	return(path)
}

write_parquet_and_return_path <- function(x) {
	path <- paste0("sensitive_output/", deparse(substitute(x)), ".csv")
	write_parquet(x, path)
	return(path)
}



save_and_return_path <- function(x) {
	path <- paste0("sensitive_output/", deparse(substitute(x)), ".RDS")
	saveRDS(x, file=path)
	return(path)
}

write_xlsx_and_return_path <- function(x) {
	path <- paste0("output/", deparse(substitute(x)), ".xlsx")
	write.xlsx(x, file=path)
	return(path)
}

write_single_line_and_return_path <- function(x) {
	path <- paste0("sensitive_output/", deparse(substitute(x)), ".txt")
	write_lines(paste(x, collapse = ","), file=path)
	return(path)
}

write_single_line_chunked_and_return_path <- function(x, max) {
	size <- seq_along(x)
	chunked <- split(x, ceiling(size/max))
	
	paths <- paste0("sensitive_output/", deparse(substitute(x)), seq_len(length(chunked)), ".txt.gz")
	
	for (i in seq_len(length(chunked))) {
		write_lines(paste(chunked[[i]], collapse = ","), file=gzfile(paths[[i]]))
	}
	return(paths)
}

write_medcodes_chunked_by_observations_and_return_path <- function(codelists, maxfiles) {
	chunked <- codelists |> 
		filter(codevar=="MedCodeId") |> 
		pull(full) |> 
		map(\(x) select(x, MedCodeId, Observations)) |> 
		bind_rows() |> 
		mutate(cumsum=cumsum(Observations),
					 chunk=ceiling(cumsum/(max(cumsum)/(maxfiles+1)))) |> 
		group_split(chunk) |> 
		map(\(x) pull(x, MedCodeId))
	
	paths <- paste0("sensitive_output/", "medcodes_chunked", seq_len(length(chunked)), ".txt.gz")
	
	for (i in seq_len(length(chunked))) {
		write_lines(paste(chunked[[i]], collapse = ","), file=gzfile(paths[[i]]))
	}
	return(paths)
}


write_delim_chunked_and_return_path <- function(x, max) {
	
	size <- 1:nrow(x)
	chunked <- split(x, ceiling(size/max))
	
	paths <- paste0("sensitive_output/", deparse(substitute(x)), seq_len(length(chunked)), ".txt")
	
	for (i in seq_along(chunked)) {
		write_delim(chunked[[i]], paths[[i]])
	}
	return(paths)
}

format_icd <- function(x, leading = 2) {
  fmt <- paste0("%0", leading, "d")
  s1 <- sprintf(fmt, as.integer(sub("\\..*", "", x)))
  s2 <- sprintf("%s", sub(".*\\.{-1}", "", x))
  sprintf("%s%s%s", s1, ifelse(s2 == "", "", "."), s2)
}
