#-----------code to test singen() function-------------#
test_that("singen('sample_data.csv', 'OD600', 0.3)", {
  expect_equal(singen("sample_data.csv", "OD600", 0.3), 43.54039)
})
