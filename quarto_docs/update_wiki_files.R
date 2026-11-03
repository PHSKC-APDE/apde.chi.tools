# Render every QMD in quarto_docs/ to GitHub flavored markdown and push the
# results (plus their image folders) to https://github.com/PHSKC-APDE/apde.chi.tools.wiki
#
# Run from the apde.chi.tools repo root. Some vignettes query PHExtractStore,
# so you must be on the KC network / VPN.

library('quarto')

# Start in apde.chi.tools directory
start_dir <- getwd()

# Find all QMD files
qmds <- normalizePath(list.files('quarto_docs', pattern = '\\.qmd$', full.names = TRUE, ignore.case = TRUE))

# Set up temporary directory and clone wiki repo
td <- tempdir()
setwd(td)
wiki_dir <- file.path(td, 'apde.chi.tools.wiki')
unlink(wiki_dir, recursive = TRUE, force = TRUE)
system('git clone https://github.com/PHSKC-APDE/apde.chi.tools.wiki.git')
setwd(wiki_dir)

# Render each file
for(qmd_file in qmds) {
  cat("Processing:", qmd_file, "\n")
  qmd_name <- tools::file_path_sans_ext(basename(qmd_file))

  # Render github flavored markdown ----
  out_file <- paste0(qmd_name, '.md')

  quarto::quarto_render(
    input = qmd_file,
    output_format = "gfm",
    output_file = out_file
  )

  # Move the rendered file from quarto_docs/ into wiki repo ----
  rendered_path <- file.path(dirname(qmd_file), out_file)
  wiki_out_file <- file.path(wiki_dir, out_file)

  file.copy(rendered_path, wiki_out_file, overwrite = TRUE)
  file.remove(rendered_path)

  if (!file.exists(wiki_out_file)) {
    warning(paste0("\U26A0\UFE0F Failed to move '", out_file, "' into wiki repo:", wiki_out_file))
  }

  # Move support directories into wiki repo ----
  # <name>_files  = figures Quarto/knitr creates itself
  # <name>_images = images a vignette writes on purpose (e.g., chi_plot_demographics)
  # The wiki copy is deleted first so images from removed or renamed examples
  # don't linger. Unchanged images are re-added identically, so git sees no change.
  for (support_dir_name in paste0(qmd_name, c('_files', '_images'))) {
    rendered_support_dir <- file.path(dirname(qmd_file), support_dir_name)
    wiki_support_dir <- file.path(wiki_dir, support_dir_name)

    if (dir.exists(rendered_support_dir)) {
      unlink(wiki_support_dir, recursive = TRUE, force = TRUE)
      file.copy(rendered_support_dir, wiki_dir, recursive = TRUE, overwrite = TRUE)
      unlink(rendered_support_dir, recursive = TRUE, force = TRUE)

      if (!dir.exists(wiki_support_dir)) {
        warning(paste0("\U26A0\UFE0F Failed to move support directory '", support_dir_name, "' into wiki repo"))
      }
    }
  }
}

# Push changes to https://github.com/PHSKC-APDE/apde.chi.tools.wiki.git
system('git add -A')
system('git commit -m "Update wiki from automated process"')
system('git push origin master')

# Clean up
setwd(start_dir)
gc()
unlink(wiki_dir, recursive = TRUE, force = TRUE)
rm(wiki_dir)
