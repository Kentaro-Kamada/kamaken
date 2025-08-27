test_that("my_cross handles data without NA values correctly", {
  # Create test data without NA values
  test_data <- data.frame(
    cut = c("Fair", "Good", "Fair", "Good", "Fair"),
    clarity = c("SI1", "VS1", "SI1", "VS1", "SI1")
  )
  
  # This should not throw an error about column 'NA_' not existing
  expect_no_error({
    result <- kamaken::my_cross(test_data, cut, clarity)
  })
})

test_that("my_cross handles data with NA values correctly", {
  # Create test data with NA values
  test_data <- data.frame(
    cut = c("Fair", "Good", NA, "Good", "Fair"),
    clarity = c("SI1", "VS1", "SI1", NA, "SI1")
  )
  
  # This should work as before
  expect_no_error({
    result <- kamaken::my_cross(test_data, cut, clarity)
  })
})

test_that("my_cross handles factor data without NA values correctly", {
  # Create test data with factors but no NA values
  test_data <- data.frame(
    cut = factor(c("Fair", "Good", "Fair", "Good", "Fair")),
    clarity = factor(c("SI1", "VS1", "SI1", "VS1", "SI1"))
  )
  
  # This should not throw an error about column 'NA_' not existing
  expect_no_error({
    result <- kamaken::my_cross(test_data, cut, clarity)
  })
})