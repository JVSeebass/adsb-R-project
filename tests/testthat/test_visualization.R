

test_that("NoPlanes works", {
  expect_message(NoPlanes("Gottingen"), paste("There are currently ", length(GetLiveData("Gottingen")[, 1]),
                                              " airplanes in the area 100 km around Gottingen.", sep = ""))
  expect_message(NoPlanes("Berlin", radius = 300),
                 paste("There are currently ", length(GetLiveData("Berlin", radius = 300)[,1]),
                       " airplanes in the area ", 300," km around Berlin.",sep = ""))
})


test_that("NoPlanes error is correct", {
  expect_error(NoPlanes(c(52, 12)), "Please take a city as function input.")
})
