# Package loading for real subprocess tests

# `pkgload::load_all()` is appropriate when tests run from a source checkout,
# but the relative source path does not exist when `R CMD check` runs tests
# against the installed package. Carry the parent process library paths into
# callr and select the matching loading strategy there.
callr_dsprrr_context <- function() {
  package_path <- normalizePath(
    getNamespaceInfo(asNamespace("dsprrr"), "path"),
    mustWork = TRUE
  )

  list(
    package_path = package_path,
    lib_paths = .libPaths()
  )
}

callr_load_dsprrr <- function(context) {
  source_marker <- file.path(context$package_path, "R", "optimizer-core.R")
  if (file.exists(source_marker)) {
    pkgload::load_all(context$package_path, quiet = TRUE)
  } else {
    library_path <- dirname(context$package_path)
    .libPaths(unique(c(library_path, context$lib_paths)))
    loadNamespace("dsprrr", lib.loc = library_path)
  }
  asNamespace("dsprrr")
}
