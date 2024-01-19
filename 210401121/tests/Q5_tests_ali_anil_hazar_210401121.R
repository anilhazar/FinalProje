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

#Q5 Testleri

# Test 5.1: merged_data veri cercevesinin beklenen sutunlari iceriyor mu?
test_that("Merged Data Columns Test", {
  expect_true("student_id" %in% colnames(merged_data))
  expect_true("grade_1.x" %in% colnames(merged_data))
  expect_true("grade_2.x" %in% colnames(merged_data))
})

# Test 5.2: weighted_average sutunu dogru bir sekilde hesaplanmis mi?
test_that("Weighted Average Test", {
  expect_true("weighted_average_grade" %in% colnames(merged_data))
})

# Test 5.3: avg_travel_time, avg_internet_access, avg_study_time sutunlari dogru bir sekilde hesaplanmis mi?
test_that("Address Type Summary Test", {
  expect_true("avg_travel_time" %in% colnames(address_type_summary))
  expect_true("avg_internet_access" %in% colnames(address_type_summary))
  expect_true("avg_study_time" %in% colnames(address_type_summary))
})

# Test 5.4: rural_students ve urban_students veri cerceveleri beklenen sutunlari iceriyor mu?
test_that("Rural and Urban Students Test", {
  expect_true("student_id" %in% colnames(rural_students))
  expect_true("weighted_average_grade" %in% colnames(rural_students))
  expect_true("travel_time.x" %in% colnames(rural_students))
})