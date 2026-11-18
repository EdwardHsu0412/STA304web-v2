# Local launcher. The Shiny app itself remains the standard app.R entry point.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
app_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")) else getwd()
local_library <- file.path(app_dir, "..", "..", "work", "r-library")
if (dir.exists(local_library)) .libPaths(c(normalizePath(local_library, winslash = "/"), .libPaths()))
options(timeout = 2)
existing <- tryCatch(suppressWarnings(readLines("http://127.0.0.1:3847/", warn = FALSE)), error = function(e) character())
if (any(grepl("STA304 Group 18", existing, fixed = TRUE))) {
  browseURL("http://127.0.0.1:3847/")
} else {
  options(timeout = 60)
  shiny::runApp(app_dir, host = "127.0.0.1", port = 3847, launch.browser = TRUE)
}
