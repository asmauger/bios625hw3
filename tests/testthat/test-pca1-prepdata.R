test_that('error for responses works', {
  expect_error(pca(Petal.Width~Petal.Length, data=iris))
})

test_that('data must be specified if formula is used', {
  expect_error(pca(~Petal.Length + Petal.Width))
})

test_that('formula method works', {
  expect_equal(pca(~mpg + hp^2 + wt, data=mtcars)$x, prcomp(~mpg + hp^2 + wt, data=mtcars, scale=T)$x)
})

test_that('subsetting works', {
  expect_equal(
    pca(~Petal.Length + Petal.Width, data=iris, subset=c(1:10))$x,
    prcomp(~Petal.Length + Petal.Width, data=iris, subset=c(1:10), scale=T)$x
  )
})

test_that('subsetting works 2', {
  expect_equal(
    dim(pca(iris[,-5], subset=c(5:15, 25:130))$x)[1],
    length(c(5:15, 25:130))
  )
})

test_that('subsetting works 3',{
  expect_warning(pca(~Petal.Width + Sepal.Length, data=iris, subset=c(148:160))
  )
})

test_that('na omit works', {
  set.seed(34982)
  x = rnorm(1000, 50, 3)
  dim(x) = c(100, 10)
  x[50, 8] = NA
  x[84, 6] = NA
  expect_equal(pca(x)$x, prcomp(~x, data=data.frame(x), scale=T)$x)
  expect_equal(
    dim(pca(x)$x)[1],
    98
  )
  expect_warning(pca(x))
})

test_that('error for non-numeric variables', {
  expect_error(pca(iris))
})

test_that('not scaling works', {
  set.seed(359382)
  x = rnorm(1000, 50, 3)
  dim(x) = c(100, 10)
  expect_equal(pca(x, scale=F)$x, prcomp(~x, data=data.frame(x), center=F)$x)
})

test_that('retx=F works', {
  expect_equal(pca(~mpg + cyl + qsec, data=mtcars, retx=F)$x, NULL)
})

test_that('print.pca works', {
  p = pca(~Petal.Width + Sepal.Length + Petal.Length, data=iris)
  expect_equal(print(p), print.default(p[3:4]))
})

test_that('summary.pca works', {
  p = pca(~Petal.Width + Sepal.Length + Petal.Length, data=iris)
  expect_equal(summary(p), print.default(p[6]))
})
