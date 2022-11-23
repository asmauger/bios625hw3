test_that('number of points is correct', {
  require(ggplot2)
  p = elbowplot(pca(iris[,-5]))
  data = ggplot_build(p)$data[[1]]
  expect_equal(nrow(data), ncol(iris[,-5]))
})

test_that('number of points is correct 2', {
  require(ggplot2)
  set.seed(304232)
  x=rnorm(10000, 50, 5)
  dim(x) = c(100, 100)
  p = elbowplot(pca(x), n=50)
  data = ggplot_build(p)$data[[1]]
  expect_equal(nrow(data), 50)
})

test_that('error is given if n is non-numeric', {
  expect_error(elbowplot(pca(iris[,-5]), n='a'))
})

test_that('y values are correct', {
  require(ggplot2)
  p = elbowplot(pca(~carat + depth + table + price, data=diamonds))
  data = ggplot_build(p)$data[[1]]
  p2 = prcomp(~carat + depth + table + price, data=diamonds, scale=T)
  p2 = p2$sdev^2/sum(p2$sdev^2)
  expect_equal(data$y, p2, tolerance = .001)
})

test_that('pca class warning works', {
  expect_error(elbowplot(c(1:50)))
})

