test_that("combine_words() combines words", {
  expect_equal(combine_words("a"), "a")
  expect_equal(combine_words(c("a", "b")), "a and b")
  expect_equal(combine_words(c("a", "b", "c")), "a, b, and c")
  expect_equal(combine_words(c(1, 2, 3)), "1, 2, and 3")
  expect_equal(combine_words(c(TRUE, FALSE, TRUE)), "TRUE, FALSE, and TRUE")

  expect_error(combine_words())
  expect_equal(combine_words(""), "")
})
