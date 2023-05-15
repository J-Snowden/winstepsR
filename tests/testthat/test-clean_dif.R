test_that("filename is a character", {
 expect_error(clean_dif(3, 4))
})

test_that("filename is a character", {
  expect_no_error(clean_dif("asdf", "asdf"))
})


