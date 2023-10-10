test_that("kolada rejects errounous input", {
  expect_error(kolada_mod <- kolada$new('stockholm'))
  expect_error(kolada_mod <- kolada$new('Lumd'))
  expect_error(kolada_mod <- kolada$new(Linkoping))
})

test_that("getonecitydata() method works", {
  kolada_mod <- kolada$new('Linkoping')
  kolada_data <- kolada_mod$getonecitydata()
  expect_true(is.data.frame(kolada_data))
  expect_true(names(kolada_data)[2]=='Linkoping')
  expect_equal(round(kolada_data[,2][c(3,17)],3),c(34.155,38.775))
})

test_that("getallcitydata() method works", {
  kolada_mod <- kolada$new('Uppsala')
  kolada_data <- kolada_mod$getallcitydata()
  expect_true(is.data.frame(kolada_data))
  expect_true(all(names(kolada_data)==c("period","Stockholm","Gothenburg","Lund","Uppsala","Linkoping")))
  expect_equal(round(kolada_data[,5][c(3,17)],3),c(33.619,37.325))
})

test_that("meandata() method works", {
  kolada_mod <- kolada$new('Gothenburg')
  expect_output(kolada_mod$meandata(),'The average value of percentage in Gothenburg is 36.7%.')
})

