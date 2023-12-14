test_that("mediascraping works", {
  expect_equal(mediascraper(outlets = c("Watson", "20 Minuten")))
})
