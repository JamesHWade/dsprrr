test_that("is_ellmer_type identifies ellmer types correctly", {
  # Test with actual ellmer types
  expect_true(is_ellmer_type(ellmer::type_string()))
  expect_true(is_ellmer_type(ellmer::type_boolean()))
  expect_true(is_ellmer_type(ellmer::type_integer()))
  expect_true(is_ellmer_type(ellmer::type_number()))
  expect_true(is_ellmer_type(ellmer::type_array(items = ellmer::type_string())))
  expect_true(is_ellmer_type(ellmer::type_object(
    a = ellmer::type_string(),
    b = ellmer::type_number()
  )))
  expect_true(is_ellmer_type(ellmer::type_enum(values = c("a", "b", "c"))))

  # Test with non-ellmer types
  expect_false(is_ellmer_type("string"))
  expect_false(is_ellmer_type(123))
  expect_false(is_ellmer_type(list()))
  expect_false(is_ellmer_type(NULL))
  expect_false(is_ellmer_type(data.frame()))
})
