file_identity_exact <- function(path) {
  .Call(C_file_identity_exact, path)
}
