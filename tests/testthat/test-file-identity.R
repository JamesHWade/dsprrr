file_identity_key <- function(identity) {
  paste(identity$device_id, identity$inode, sep = "/")
}

test_that("file identities stay stable across renames and hard links", {
  directory <- tempfile("file-identity-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)

  original_path <- file.path(directory, "original.txt")
  renamed_path <- file.path(directory, "renamed.txt")
  hardlink_path <- file.path(directory, "hardlink.txt")
  writeLines("same bytes", original_path, useBytes = TRUE)

  original_identity <- dsprrr:::file_identity_exact(original_path)
  expect_type(original_identity$device_id, "character")
  expect_length(original_identity$device_id, 1)
  expect_type(original_identity$inode, "character")
  expect_length(original_identity$inode, 1)
  expect_match(
    original_identity$device_id,
    "^(posix-device:[0-9]+|win-volume:[0-9a-f]{16})$"
  )
  expect_match(
    original_identity$inode,
    "^(posix-inode:[0-9]+|win-file-id:[0-9a-f]{32})$"
  )

  expect_true(file.rename(original_path, renamed_path))
  expect_identical(
    dsprrr:::file_identity_exact(renamed_path),
    original_identity
  )

  linked <- suppressWarnings(file.link(renamed_path, hardlink_path))
  if (!isTRUE(linked)) {
    skip("This filesystem does not support hard links")
  }
  expect_identical(
    dsprrr:::file_identity_exact(hardlink_path),
    original_identity
  )
})

test_that("identical-content files have distinct exact identities", {
  directory <- tempfile("file-identity-distinct-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)

  paths <- file.path(directory, sprintf("copy-%02d.txt", seq_len(32)))
  for (path in paths) {
    writeLines("identical content", path, useBytes = TRUE)
  }

  identities <- lapply(paths, dsprrr:::file_identity_exact)
  expect_true(all(vapply(identities, Negate(is.null), logical(1))))
  keys <- vapply(identities, file_identity_key, character(1))
  expect_length(unique(keys), length(paths))
})

test_that("UTF-8 paths and directories have identities", {
  directory <- tempfile("file-identity-unicode-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)

  unicode_directory <- file.path(directory, "\u65e5\u672c\u8a9e-\u03bb")
  expect_true(dir.create(unicode_directory))
  file_path <- file.path(unicode_directory, "caf\u00e9-\u6771\u4eac.txt")
  writeLines("unicode path", file_path, useBytes = TRUE)

  expect_type(dsprrr:::file_identity_exact(unicode_directory), "list")
  expect_type(dsprrr:::file_identity_exact(file_path), "list")
  expect_identical(
    dsprrr:::file_identity_exact(file_path),
    dsprrr:::file_identity_exact(file_path)
  )
})

test_that("missing paths, symlinks, and unsupported path inputs fail closed", {
  directory <- tempfile("file-identity-fail-closed-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)

  target <- file.path(directory, "target.txt")
  link <- file.path(directory, "link.txt")
  writeLines("target", target, useBytes = TRUE)

  expect_null(dsprrr:::file_identity_exact(file.path(directory, "missing.txt")))
  expect_null(dsprrr:::file_identity_exact(character()))
  expect_null(dsprrr:::file_identity_exact(NA_character_))
  expect_null(dsprrr:::file_identity_exact(1))
  if (.Platform$OS.type != "windows") {
    expect_null(dsprrr:::file_identity_exact(paste0(target, "/")))
  }

  created <- suppressWarnings(file.symlink(target, link))
  if (!isTRUE(created)) {
    skip("This environment does not permit symbolic links")
  }
  expect_null(dsprrr:::file_identity_exact(link))
  if (.Platform$OS.type != "windows") {
    expect_null(dsprrr:::file_identity_exact(paste0(link, "/")))
  }
})

test_that("directory symlinks with trailing separators fail closed", {
  directory <- tempfile("file-identity-directory-link-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)

  target <- file.path(directory, "target-directory")
  link <- file.path(directory, "directory-link")
  dir.create(target)
  created <- suppressWarnings(file.symlink(target, link))
  if (!isTRUE(created)) {
    skip("This environment does not permit symbolic links")
  }

  expect_null(dsprrr:::file_identity_exact(link))
  expect_null(dsprrr:::file_identity_exact(paste0(link, "/")))
})

test_that("long Windows paths can be identified when the filesystem permits them", {
  skip_if(.Platform$OS.type != "windows")

  root <- tempfile("file-identity-long-")
  if (!dir.create(root)) {
    skip("Could not create a temporary directory")
  }
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  current <- root
  for (index in seq_len(5)) {
    current <- file.path(current, paste0("segment-", index, strrep("x", 45)))
    if (!dir.create(current, showWarnings = FALSE)) {
      skip("This Windows environment does not permit long paths")
    }
  }
  file_path <- file.path(current, "long-path-file.txt")
  skip_if(nchar(enc2utf8(file_path), type = "bytes") <= 260)
  written <- tryCatch(
    {
      writeLines("long path", file_path, useBytes = TRUE)
      TRUE
    },
    error = function(condition) FALSE
  )
  if (!written) {
    skip("This Windows environment does not permit long files")
  }

  expect_type(dsprrr:::file_identity_exact(file_path), "list")
})
