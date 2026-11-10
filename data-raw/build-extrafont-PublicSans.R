# Build Type 1 Public Sans resources for extrafont.
#
# Run this from the nswtheme package root during development.
# Do not run it when nswtheme is loaded by users.

if (!requireNamespace("Rttf2pt1", quietly = TRUE)) {
  stop(
    "Install Rttf2pt1 before building the extrafont resources:\n",
    "install.packages(\"Rttf2pt1\")",
    call. = FALSE
  )
}

ttf_dir <- file.path("inst", "fonts")
afm_dir <- file.path(ttf_dir, "metrics")
pfb_dir <- file.path(ttf_dir, "outlines")

dir.create(afm_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pfb_dir, recursive = TRUE, showWarnings = FALSE)

ttf_files <- c(
  "PublicSans-Regular.ttf",
  "PublicSans-Bold.ttf",
  "PublicSans-Italic.ttf",
  "PublicSans-BoldItalic.ttf",
  "PublicSans-SemiBold.ttf",
  "PublicSans-SemiBoldItalic.ttf"
)

ttf_paths <- file.path(ttf_dir, ttf_files)

missing <- ttf_paths[!file.exists(ttf_paths)]

if (length(missing)) {
  stop(
    "Could not find:\n",
    paste0("  - ", missing, collapse = "\n"),
    call. = FALSE
  )
}

ttf2pt1 <- Rttf2pt1::which_ttf2pt1()

if (!nzchar(ttf2pt1) || !file.exists(ttf2pt1)) {
  stop("Could not locate the ttf2pt1 executable.", call. = FALSE)
}

convert_to_type1 <- function(ttf) {
  
  basename_no_ext <- tools::file_path_sans_ext(basename(ttf))
  
  # ttf2pt1 writes both .afm and .pfb using this output stem.
  output_stem <- file.path(tempdir(), basename_no_ext)
  
  status <- system2(
    command = ttf2pt1,
    args = c(
      "-a",  # produce AFM metrics
      "-b",  # produce binary Type 1 PFB outlines
      shQuote(normalizePath(ttf, winslash = "/", mustWork = TRUE)),
      shQuote(output_stem)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  
  exit_status <- attr(status, "status")
  
  if (!is.null(exit_status) && exit_status != 0L) {
    stop(
      "ttf2pt1 failed for ", basename(ttf), ":\n",
      paste(status, collapse = "\n"),
      call. = FALSE
    )
  }
  
  generated_afm <- paste0(output_stem, ".afm")
  generated_pfb <- paste0(output_stem, ".pfb")
  
  if (!file.exists(generated_afm) || !file.exists(generated_pfb)) {
    stop(
      "Expected AFM/PFB files were not generated for ",
      basename(ttf),
      ".\nttf2pt1 output:\n",
      paste(status, collapse = "\n"),
      call. = FALSE
    )
  }
  
  file.copy(
    generated_afm,
    file.path(afm_dir, basename(generated_afm)),
    overwrite = TRUE
  )
  
  file.copy(
    generated_pfb,
    file.path(pfb_dir, basename(generated_pfb)),
    overwrite = TRUE
  )
  
  message("Generated Type 1 resources for ", basename(ttf))
}

invisible(lapply(ttf_paths, convert_to_type1))
