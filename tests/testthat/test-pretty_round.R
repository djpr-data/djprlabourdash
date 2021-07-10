test_that("pretty_round() returns expected results", {
  expect_equal(
    pretty_round(c(10, 110, 10050, 20500, 2000100, 3445123)),
    c("0", "100", "10,100", "20,500", "2m", "3.445m")
  )
})
