test_that('pca check works', {
  expect_error(scoreplot('a'))
})

test_that('pc check works', {
  p1=pca(iris[,-5])
  expect_error(scoreplot(p1, pc=c(1,2,3)))
  expect_error(scoreplot(p1, pc=c('a', 'b')))
})

test_that('scale check works', {
  p1 = pca(iris[,-5])
  expect_error(scoreplot(p1, pc=c(1,2), score='a'))
  expect_error(scoreplot(p1, pc=c(1,2), score=c(1,0)))
})

test_that('grouping check works', {
  p1 = pca(iris[,-5])
  expect_error(scoreplot(p1, pc=c(1,2), grouping='a'))
  expect_error(scoreplot(p1, pc=c(1,2), grouping=iris[1:149, 5]))
  expect_error(scoreplot(p1, pc=c(1,2), grouping=iris))
})

test_that('scale works', {
  require(ggfortify)
  require(ggplot2)
  data(diamonds)
  p1 = pca(~carat + depth + table, data=diamonds)
  p2 = prcomp(~carat + depth + table, data=diamonds, scale=T)
  for(i in 1:4) {
  scale = c(0, .33, .5, 1)[i]
  plot1 = scoreplot(p1, pc=c(1,2), scale=i)
  plot2 = autoplot(p2, scale=i)
  data1 = ggplot_build(plot1)$data[[1]][,1:2]
  data2 = ggplot_build(plot2)$data[[1]][,1:2]
  expect_equal(data1, data2)
  }
})

test_that('grouping works', {
  require(ggplot2)
  require(dplyr)
  p1 = pca(~Petal.Width + Sepal.Length + Petal.Length, data=iris)
  plot = scoreplot(p1, pc=c(2,3), grouping=iris[,5])
  groups = ggplot_build(plot)$data[[1]]$group
  expect_equal(
    count(as.data.frame(groups), groups)$n,
    count(as.data.frame(iris[,5]), iris[,5])$n
               )
})
