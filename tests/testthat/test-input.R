test_that("input() accepts ellmer types", {
  # Basic ellmer types
  inp1 <- input("text", ellmer::type_string())
  expect_true(is_dsprrr_input(inp1))
  expect_equal(inp1$name, "text")
  expect_true(inherits(inp1$type, "ellmer::Type"))

  # Number type
  inp2 <- input("age", ellmer::type_number())
  expect_true(inherits(inp2$type, "ellmer::TypeBasic"))

  # Boolean
  inp3 <- input("active", ellmer::type_boolean())
  expect_true(inherits(inp3$type, "ellmer::TypeBasic"))
})

test_that("input() accepts exact canonical type labels", {
  basic <- c(
    string = "string",
    number = "number",
    integer = "integer",
    boolean = "boolean"
  )
  for (label in names(basic)) {
    expect_identical(input(label, label)$type@type, basic[[label]])
  }

  expect_s7_class(input("items", "array")$type, ellmer::TypeArray)
  expect_s7_class(input("record", "object")$type, ellmer::TypeObject)
})

test_that("input() defaults to string when type is NULL", {
  inp1 <- input("name")
  expect_true(inherits(inp1$type, "ellmer::TypeBasic"))
  expect_equal(inp1$type@type, "string")

  inp2 <- input("name", description = "User's name")
  expect_true(inherits(inp2$type, "ellmer::TypeBasic"))
  expect_equal(inp2$type@type, "string")
})

test_that("input() rejects legacy and unknown type specifications", {
  expect_snapshot(input("text", "str"), error = TRUE)
  expect_snapshot(input("text", " String "), error = TRUE)
  expect_snapshot(input("text", S7::class_character), error = TRUE)
  expect_snapshot(
    input("text", class = S7::class_character),
    error = TRUE
  )
})

test_that("normalize_input_type handles various inputs", {
  # NULL -> string
  type1 <- normalize_input_type(NULL)
  expect_true(inherits(type1, "ellmer::TypeBasic"))
  expect_equal(type1@type, "string")

  # Canonical string label
  type2 <- normalize_input_type("number")
  expect_true(inherits(type2, "ellmer::TypeBasic"))
  expect_equal(type2@type, "number")

  # Ellmer type passes through
  type3 <- normalize_input_type(ellmer::type_boolean())
  expect_true(inherits(type3, "ellmer::TypeBasic"))
  expect_equal(type3@type, "boolean")
})

test_that("format_ellmer_type formats basic types", {
  # String
  expect_equal(format_ellmer_type(ellmer::type_string()), "string")

  # Number
  expect_equal(format_ellmer_type(ellmer::type_number()), "number")

  # Integer
  expect_equal(format_ellmer_type(ellmer::type_integer()), "integer")

  # Boolean
  expect_equal(format_ellmer_type(ellmer::type_boolean()), "boolean")

  # NULL
  expect_equal(format_ellmer_type(NULL), "any")
})

test_that("format_ellmer_type formats enum types", {
  # Simple enum
  enum_type <- ellmer::type_enum(values = c("a", "b", "c"))
  expect_equal(format_ellmer_type(enum_type), "enum(a, b, c)")

  # Enum with more values (truncates in non-verbose mode)
  long_enum <- ellmer::type_enum(values = c("a", "b", "c", "d", "e", "f", "g"))
  result <- format_ellmer_type(long_enum, verbose = FALSE)
  expect_match(result, "enum\\(a, b, c, \\.\\.\\. \\+4 more\\)")

  # Verbose shows all
  result_verbose <- format_ellmer_type(long_enum, verbose = TRUE)
  expect_equal(result_verbose, "enum(a, b, c, d, e, f, g)")
})

test_that("format_ellmer_type formats array types", {
  # Array of strings
  arr_type <- ellmer::type_array(items = ellmer::type_string())
  expect_equal(format_ellmer_type(arr_type), "array(string)")

  # Array of numbers
  arr_num <- ellmer::type_array(items = ellmer::type_number())
  expect_equal(format_ellmer_type(arr_num), "array(number)")
})

test_that("format_ellmer_type formats object types", {
  # Empty object
  empty_obj <- ellmer::type_object()
  expect_equal(format_ellmer_type(empty_obj), "object")

  # Object with fields (non-verbose)
  obj_type <- ellmer::type_object(
    name = ellmer::type_string(),
    age = ellmer::type_number()
  )
  expect_equal(
    format_ellmer_type(obj_type, verbose = FALSE),
    "object(2 fields)"
  )

  # Object with fields (verbose)
  result <- format_ellmer_type(obj_type, verbose = TRUE)
  expect_match(result, "object\\(")
  expect_match(result, "name: string")
  expect_match(result, "age: number")
})

test_that("input() works in signature creation", {
  sig <- signature(
    inputs = list(
      input("text"), # defaults to string
      input("count", "integer"), # canonical type label
      input("score", ellmer::type_number()) # ellmer type
    ),
    output_type = ellmer::type_string(),
    instructions = "Process inputs"
  )

  expect_s7_class(sig, Signature)
  expect_length(sig@inputs, 3)
})
