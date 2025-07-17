remove_handout <- function(url) {
	x <- readLines(url)
	i_co <- grep("^classoption", x)
	if(length(i_co) > 0){
		x[i_co[1]] <- gsub(",handout", "", x[i_co[1]])
	}
	x
	writeLines(x, url)
}

add_handout <- function(url) {
	x <- readLines(url)
	i_co <- grep("^classoption", x)
	if(length(i_co) > 0 && length(grep("handout", x[i_co[1]])) == 0){
		x[i_co[1]] <- gsub("'$", ",handout'", x[i_co[1]])
	}
	x
	writeLines(x, url)
}
enable_render <- function(url = "_quarto.yml") {
	x <- readLines(url)
	i_co <- grep('- "!Cours"$', x)
	if(length(i_co) > 0){
		x[i_co[1]] <- paste("#", x[i_co[1]])
	}
	x
	writeLines(x, url)
}
disable_render <- function(url = "_quarto.yml") {
	x <- readLines(url)
	i_co <- grep('- "!Cours"$', x)
	if(length(i_co) > 0){
		x[i_co[1]] <- gsub("# ", "", x[i_co[1]])
	}
	x
	writeLines(x, url)
}
create_cmd <- function(dir){
	f <- list.files(dir, pattern = "qmd$", full.names = TRUE)
	site_dir <- paste0("_site/", gsub("qmd$", "pdf", f))
	cat(c(
		sprintf('remove_handout("%s")', file.path(dir, "_metadata.yml")),
		sprintf("quarto::quarto_render('%s')\nfile.copy('%s', '%s', overwrite = TRUE)",
				f,
				site_dir,
				gsub("qmd$", "pdf", file.path(dirname(f), "pres", basename(f)))
		),
		sprintf('add_handout("%s")', file.path(dir, "_metadata.yml")),
		'',
		sprintf("quarto::quarto_render('%s')\nfile.copy('%s', '%s', overwrite = TRUE)",
				f,
				site_dir,
				gsub("qmd$", "pdf", f)
		),
		'',
		sprintf('unlink("%s", recursive = TRUE)', unique(dirname(site_dir)))
	), sep = "\n")
}
