library(readr)
library(stringr)


exclude_file_from_copy <- function(file) {
    for (p in copy_exclude_patterns) {
        if (str_detect(file, p)) {
            return(TRUE)
        }
    }
    FALSE
}


copy_dir <- function(dir, from_basedir, to_basedir) {
    from_dir <- file.path(from_basedir, dir)
    to_dir <- file.path(to_basedir, dir)

    if (dir.exists(from_dir)) {
        if (!dir.exists((to_dir))) dir.create((to_dir))
        for (file in list.files(path = from_dir)) {
            if (!exclude_file_from_copy(file)) {
                from_file <- file.path(from_dir, file)
                to_file <- file.path(to_dir, file)

                # list.files lists folders
                if (!dir.exists(from_file)) file.copy(from_file, to_file, overwrite = FALSE)
            }
        }
    }
}


insert_index <- function(string, idx) str_replace(string, "\\$\\{idx\\}", toString(idx))


print_header <- function(title, subtitle, idx) {
    cat("---\n")
    cat(paste0("title: \"", insert_index(title, idx), "\"\n"))
    cat(paste0("subtitle: \"", subtitle, "\"\n"))
    cat("---\n")
}


cat_file <- function(file) {
    cat("\n")
    for (l in read_lines(file)) cat(l, "\n")
}


cat_files <- function(dir, pattern, with_solution = FALSE) {
    #
    # process files in include pattern
    for (file in list.files(path = dir, pattern = pattern, full.names = TRUE)) {
        #
        # Is the current qmd file a solution?
        is_solution <- str_ends(file, ".sol.qmd")

        # This is an assignment
        if (!is_solution) {
            have_solution <- file.exists(str_replace(file, ".qmd", ".sol.qmd"))
            cat_file(file)
            if (with_solution) cat("\n### Lösung {-}\n")
            if (!have_solution) cat("\nKeine Lösung\n")
        }

        # This is a solution
        if (is_solution && with_solution) cat_file(file)
    }
}


fname <- function(basename, idx, extension = ".qmd") {
    idx <- str_pad(idx, 2, side = "left", pad = "0")
    paste0(insert_index(basename, idx), extension)
}
