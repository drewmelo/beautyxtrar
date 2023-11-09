test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

ggplot(cars,
       aes(speed, dist)) +
  geom_point() +
  theme_xtra()
