test_that("vennItem works for 2 sets", {
  sets <- list(A = c("apple", "banana"), B = c("banana", "kiwi"))
  p <- vennItem(sets)
  expect_s3_class(p, "ggplot")
})

test_that("vennItem works for 3 sets", {
  sets <- list(A = c("apple", "banana"), B = c("banana", "kiwi"),
               C = c("banana", "kiwi", "apple", "pear"))
  p <- vennItem(sets)
  expect_s3_class(p, "ggplot")
})
