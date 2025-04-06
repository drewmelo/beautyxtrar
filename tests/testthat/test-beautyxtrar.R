test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

ggplot(cars,
       aes(speed, dist)) +
  geom_point() +
  theme_xtra() 
  

ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species)) +
  geom_point() +
  facet_wrap(~ Species) +
  theme_xtra()

ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species)) +
  geom_point() +
  dark_theme_xtra()

ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species)) +
  geom_point() +
  theme_xtra()

ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species)) +
  geom_point() +
  theme_academic()