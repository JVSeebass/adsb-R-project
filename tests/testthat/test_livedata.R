
test_that("point and radius are valid", {
  expect_equal(class(GetLiveData("Berlin")), "data.frame")
  expect_equal(class(GetLiveData(point = c(52.5243, 12.0001))), "data.frame")
  expect_equal(class(GetLiveData(("Berlin"), radius = 100)), "data.frame")
  expect_equal(class(GetLiveData(c(58,13), radius = 500)), "data.frame")
})


test_that("country is valid", {
  expect_equal(class(GetLiveData(country = "Denmark")), "data.frame")
})

test_that("type is valid", {
  expect_equal(class(GetLiveData(type = "A320")), "data.frame")
})


# Errors can occur if too many parameters are selected and no matches could be found.
test_that("mixtrues are valid", {
  expect_equal(class(GetLiveData(point = "Copenhagen", country = "Denmark")), "data.frame")
  expect_equal(class(GetLiveData(point = "Copenhagen", radius = 200, country = "Denmark", type = "A320")), "data.frame")
  expect_equal(class(GetLiveData(point = "London", radius = 200, type = "A319")), "data.frame")
})

test_that("error messages are correct", {
  expect_error(GetLiveData(point = c(1, 5, 7)), "Point has to be a city or of length 2!")
  expect_error(GetLiveData(point = c(-100, 5)), "Latitude has to be between -90 and 90!")
  expect_error(GetLiveData(point = c(10, 1000)), "Longitude has to be between -180 and 180!")
  expect_error(GetLiveData(point = "Errortown"), "Point has to be a city or of length 2!")
  expect_error(GetLiveData(country = "Failstate"), "No matches found! Try another input.")
  expect_error(GetLiveData(type = "Mercedes"), "No matches found! Try another input.")
  expect_error(GetLiveData(country = 10), "No matches found! Try another input.")
})

test_that("warnings are correct", {
  expect_warning(GetLiveData(),
                 "No selection done! If unintended, please check your function input.")
  expect_warning(GetLiveData(),
                 "You did not choose any selection parameter.")
  expect_warning(GetLiveData(point = c(0, 0), radius = 1000000),
                 "No selection done! If unintended, please check your function input.")
  expect_warning(GetLiveData(type = ""),
                 "No selection done! If unintended, please check your function input.")
})
