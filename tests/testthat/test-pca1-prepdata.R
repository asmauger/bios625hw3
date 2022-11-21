test_that('error for responses works', {
  expect_error(pca(Petal.Width~Petal.Length, data=iris))
})

test_that('data must be specified if formula is used', {
  expect_error(pca(~Petal.Length + Petal.Width))
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
