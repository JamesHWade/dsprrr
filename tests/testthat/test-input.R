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

test_that("input() accepts string shortcuts", {
  # String types
  inp1 <- input("name", "string")
  expect_true(inherits(inp1$type, "ellmer::TypeBasic"))

  inp2 <- input("text", "str")
  expect_true(inherits(inp2$type, "ellmer::TypeBasic"))

  # Numeric types
  inp3 <- input("score", "number")
  expect_true(inherits(inp3$type, "ellmer::TypeBasic"))

  inp4 <- input("count", "integer")
  expect_true(inherits(inp4$type, "ellmer::TypeBasic"))

  # Boolean
  inp5 <- input("flag", "boolean")
  expect_true(inherits(inp5$type, "ellmer::TypeBasic"))

  inp6 <- input("active", "bool")
  expect_true(inherits(inp6$type, "ellmer::TypeBasic"))
})

test_that("input() defaults to string when type is NULL", {
  inp1 <- input("name")
  expect_true(inherits(inp1$type, "ellmer::TypeBasic"))
  expect_equal(inp1$type@type, "string")

  inp2 <- input("name", description = "User's name")
  expect_true(inherits(inp2$type, "ellmer::TypeBasic"))
  expect_equal(inp2$type@type, "string")
})

test_that("input() converts S7 classes to ellmer types", {
  # S7 class inputs should be converted to ellmer types
  inp1 <- input("text", S7::class_character)
  expect_true(is_dsprrr_input(inp1))
  expect_true(inherits(inp1$type, "ellmer::TypeBasic"))
  expect_equal(inp1$type@type, "string")

  inp2 <- input("count", S7::class_integer)
  expect_true(inherits(inp2$type, "ellmer::TypeBasic"))
  expect_equal(inp2$type@type, "integer")

  inp3 <- input("value", S7::class_double)
  expect_true(inherits(inp3$type, "ellmer::TypeBasic"))
  expect_equal(inp3$type@type, "number")

  inp4 <- input("flag", S7::class_logical)
  expect_true(inherits(inp4$type, "ellmer::TypeBasic"))
  expect_equal(inp4$type@type, "boolean")
})

test_that("typed input helpers work correctly", {
  # String helper
  inp1 <- input_string("text", description = "Some text")
  expect_equal(inp1$name, "text")
  expect_equal(inp1$description, "Some text")
  expect_true(inherits(inp1$type, "ellmer::TypeBasic"))

  # Number helper
  inp2 <- input_number("age")
  expect_true(inherits(inp2$type, "ellmer::TypeBasic"))

  # Boolean helper
  inp3 <- input_boolean("active")
  expect_true(inherits(inp3$type, "ellmer::TypeBasic"))

  # Integer helper
  inp4 <- input_integer("count")
  expect_true(inherits(inp4$type, "ellmer::TypeBasic"))

  # Enum helper
  inp5 <- input_enum("status", c("pending", "active", "done"))
  expect_true(inherits(inp5$type, "ellmer::TypeEnum"))

  # Array helper
  inp6 <- input_array("tags", item_type = ellmer::type_string())
  expect_true(inherits(inp6$type, "ellmer::TypeArray"))

  # Object helper
  inp7 <- input_object("data")
  expect_true(inherits(inp7$type, "ellmer::TypeObject"))
})

test_that("normalize_input_type handles various inputs", {
  # NULL -> string
  type1 <- normalize_input_type(NULL)
  expect_true(inherits(type1, "ellmer::TypeBasic"))
  expect_equal(type1@type, "string")

  # String shortcut
  type2 <- normalize_input_type("number")
  expect_true(inherits(type2, "ellmer::TypeBasic"))
  expect_equal(type2@type, "number")

  # Ellmer type passes through
  type3 <- normalize_input_type(ellmer::type_boolean())
  expect_true(inherits(type3, "ellmer::TypeBasic"))
  expect_equal(type3@type, "boolean")

  # S7 class conversion
  type4 <- normalize_input_type(S7::class_character)
  expect_true(inherits(type4, "ellmer::TypeBasic"))
  expect_equal(type4@type, "string")
})

test_that("type_to_s7_class converts correctly", {
  # String -> character
  s7_1 <- type_to_s7_class(ellmer::type_string())
  expect_identical(s7_1, S7::class_character)

  # Number -> double
  s7_2 <- type_to_s7_class(ellmer::type_number())
  expect_identical(s7_2, S7::class_double)

  # Integer -> integer
  s7_3 <- type_to_s7_class(ellmer::type_integer())
  expect_identical(s7_3, S7::class_integer)

  # Boolean -> logical
  s7_4 <- type_to_s7_class(ellmer::type_boolean())
  expect_identical(s7_4, S7::class_logical)

  # Array -> list
  s7_5 <- type_to_s7_class(ellmer::type_array(items = ellmer::type_string()))
  expect_identical(s7_5, S7::class_list)

  # Enum -> character
  s7_6 <- type_to_s7_class(ellmer::type_enum(values = c("a", "b")))
  expect_identical(s7_6, S7::class_character)

  # Object -> list
  s7_7 <- type_to_s7_class(ellmer::type_object())
  expect_identical(s7_7, S7::class_list)
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
  # Using new flexible input syntax
  sig1 <- Signature(
    inputs = list(
      input("text"), # defaults to string
      input("count", "integer"), # string shortcut
      input("score", ellmer::type_number()) # ellmer type
    ),
    output_type = ellmer::type_string(),
    instructions = "Process inputs"
  )

  expect_s7_class(sig1, Signature)
  expect_length(sig1@inputs, 3)

  # Test backward compatibility
  sig2 <- Signature(
    inputs = list(
      input("text", S7::class_character, "Text input")
    ),
    output_type = ellmer::type_string()
  )

  expect_s7_class(sig2, Signature)
  expect_length(sig2@inputs, 1)
})
