library(purrr)

process_assignments <- function(assignments, dir_in_base, dir_out, subdirs) {
    #
    # Loop over assignments
    idx <- 1
    for (entry in assignments$content) {
        #
        # Input directory and existence check
        dir_in <- file.path(dir_in_base, entry$root, "aufgaben")
        if (!dir.exists(dir_in)) stop("Folder ", dir_in, " does not exist")

        # Copy subdirectories
        walk(subdirs, \(dir) copy_dir(dir, dir_in, dir_out))

        # Assignment
        sink(file.path(dir_out, fname("aufgabenblatt-${idx}", idx)))
        print_header(assignments$title, entry$subtitle, idx)
        walk(entry$include, \(inc) cat_files(dir_in, inc, with_solution = FALSE))
        sink()

        # Solution
        if (assignments$with_solution) {
            sink(file.path(dir_out, fname(paste0("aufgabenblatt-${idx}-loesung-", entry$sol), idx)))
            print_header(paste0("Lösungen zu ", assignments$title), entry$subtitle, idx)
            walk(entry$include, \(inc) cat_files(dir_in, inc, with_solution = TRUE))
            sink()
        }

        # Loop
        idx <- idx + 1
    }
}