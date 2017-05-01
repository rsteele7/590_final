#-----------code to test tripgen() function-------------#
test_that("tripgen('sample_data_trip.csv', 'OD600_1', 'OD600_2', 'OD600_3', 0.3)", {
  expect_equal(tripgen("sample_data_trip.csv", "OD600_1", "OD600_2", "OD600_3", 0.3), 43.8523)
})
