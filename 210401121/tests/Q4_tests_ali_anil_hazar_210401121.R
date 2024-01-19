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

# Q4 Testleri

# Test 4.1: 'mathDF' ve 'portugueseDF' tibble'lar olusturuldu mu?
test_that("Math ve Portuguese tibble'lar olusturuldu mu?", {
  expect_is(tibblemathDF, "tbl_df")
  expect_is(tibbleportugueseDF, "tbl_df")
})

# Test 4.2: 'address_type' sutunlari faktor mu
test_that("'address_type' sutunlari faktor mu", {
  expect_is(tibblemathDF$address_type, "factor")
  expect_is(tibbleportugueseDF$address_type, "factor")
})

# Test 4.3: 'travel_time' sutunlari sayisal mi
test_that("'travel_time' sutunlari sayisal mi", {
  expect_is(tibblemathDF$travel_time, "numeric")
  expect_is(tibbleportugueseDF$travel_time, "numeric")
})

# Test 4.4: 'study_time' sutunlari sayisal mi
test_that("'study_time' sutunlari sayisal mi", {
  expect_is(tibblemathDF$study_time, "numeric")
  expect_is(tibbleportugueseDF$study_time, "numeric")
})

# Test 4.5: Eksik degerler dogru bir sekilde islendi mi?
test_that("Eksik degerler dogru bir sekilde islendi mi?", {
  expect_true(all(!is.na(tibblemathDF$grade_1)))
  expect_true(all(!is.na(tibblemathDF$grade_2)))
  expect_true(all(!is.na(tibblemathDF$final_grade)))
  
  expect_true(all(!is.na(tibbleportugueseDF$grade_1)))
  expect_true(all(!is.na(tibbleportugueseDF$grade_2)))
  expect_true(all(!is.na(tibbleportugueseDF$final_grade)))
})