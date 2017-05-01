#-----------code to test tripgen() function-------------#
test_that("tripgen('sample_data.csv', 'OD600', 0.3, time.col = 'Time')", {
  expect_equal(tripgen("sample_data.csv", "OD600", 0.3, time.col = "Time"))
})
