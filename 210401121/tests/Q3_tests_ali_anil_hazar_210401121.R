library(testthat)


#Q3 TESTLERI

# Test 3.1
test_that("Veri seti basariyla yuklendi - mathDF", {
  expect_is(mathDF, "data.frame")
  expect_gt(nrow(mathDF), 0)
  expect_gt(ncol(mathDF), 0)
})

# Test 3.2
test_that("Veri seti basariyla yuklendi - portugueseDF", {
  expect_is(portugueseDF, "data.frame")
  expect_gt(nrow(portugueseDF), 0)
  expect_gt(ncol(portugueseDF), 0)
})


# Test 3.3
test_that("mathDF ve portugueseDF'nin sutun sayilari dogru", {
  expect_equal(ncol(mathDF), ncol(portugueseDF))
})