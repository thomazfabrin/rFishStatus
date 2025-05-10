test_that("mock_function returns expected output", {
  result <- rfs_mock_function("test input")
  expect_equal(result, "This is a mock output")
})
