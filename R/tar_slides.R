slides <- tibble::tibble(
  path = list.files(here_rel("slides"), pattern = "\\.Rmd", full.names = TRUE)
) %>%
  mutate(
    name = tools::file_path_sans_ext(basename(path)),
    sym = syms(janitor::make_clean_names(paste0("slide_rmd_", name))),
    sym_html = syms(janitor::make_clean_names(paste0("slide_html_", name))),
    sym_pdf = syms(janitor::make_clean_names(paste0("slide_pdf_", name)))
  )

build_slides <- list(
  tar_eval(
    tar_files_input(target_name, rmd_file),
    values = list(
      target_name = slides$sym,
      rmd_file = slides$path
    )
  ),

  tar_eval(
    tar_target(target_name, render_xaringan(rmd_file), format = "file"),
    values = list(
      target_name = slides$sym_html,
      rmd_file = slides$sym
    )
  ),

  tar_eval(
    tar_target(target_name, xaringan_to_pdf(html_file), format = "file"),
    values = list(
      target_name = slides$sym_pdf,
      html_file = slides$sym_html
    )
  )
)


# We need to return the path to the rendered HTML file. In this case,
# rmarkdown::render() *does* return a path, but it returns an absolute path,
# which makes the targets pipeline less portable. So we return our own path to
# the HTML file instead.
render_xaringan <- function(slide_path) {
  # crayon does weird things to R Markdown and xaringan output, so we need to
  # disable it here. This is the same thing that tarchetypes::tar_render() does
  # behind the scenes too.
  withr::local_options(list(crayon.enabled = NULL))
  withr::with_dir(here::here(), {
    rmarkdown::render(slide_path, quiet = TRUE)
  })
  paste0(tools::file_path_sans_ext(slide_path), ".html")
}


# Use pagedown to convert xaringan HTML slides to PDF. Return a relative path to
# the PDF to keep targets happy.
#
# Some slides are too large and cause renderthis::to_pdf() to timeout. For these
# slides (specified in skip_pdf_slides in _targets.R), we skip automatic PDF
# generation and expect the PDF to already exist (generated manually).
#
# Manual PDF generation workaround using decktape via Docker:
# 1. Serve slides locally (e.g., with VS Code Live Server on port 5500)
# 2. Run: docker run --rm -t --net=host -v `pwd`:/slides astefanutti/decktape \
#         http://host.docker.internal:5500/slides/10-slides.html 10-slides.pdf
#
# Note: complex_slides parameter in renderthis is temporarily broken in early
# 2025 due to changes in headless Chrome (https://github.com/rstudio/chromote/issues/193)

xaringan_to_pdf <- function(slide_path, skip_list = skip_pdf_slides) {
  path_sans_ext <- tools::file_path_sans_ext(slide_path)
  pdf_path <- paste0(path_sans_ext, ".pdf")

  # Skip PDF generation for slides in the skip list - assume they already exist
  if (path_sans_ext %in% skip_list) {
    if (!file.exists(pdf_path)) {
      warning(
        "PDF does not exist for skipped slide: ", path_sans_ext, "\n",
        "Expected at: ", pdf_path, "\n",
        "Generate manually with decktape (see comments in tar_slides.R)"
      )
    }
    return(pdf_path)
  }

  # Generate PDF for all other slides
  # Note: delay parameter only works with complex_slides = TRUE, which is
  # currently broken due to chromote issues. Keeping it here for when it's fixed.
  withr::with_dir(here::here(), {
    renderthis::to_pdf(
      slide_path,
      to = pdf_path#,
      # Uncomment when chromote is fixed:
      # complex_slides = FALSE
    )
  })

  return(pdf_path)
}
