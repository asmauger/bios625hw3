#' Elbow (Scree) Plot
#' @description
#' Plots the percent variance explained by each principal component from a `pca` object.
#' As a rule of thumb, points above the 'elbow' of the plot should be retained in downstream analyses.
#' @param object an object of class `pca` generated using the `pca()` function.
#' @param n a single numeric value indicating the number of principal components to include in the plot.
#'
#' @return
#' Returns a ggplot. This plot can be modified by adding additional ggplot functions.
#' @export
#'
#' @examples
#' require(ggplot2)
#' data(diamonds)
#' p1 = pca(~carat + depth + table + price+ x + y + z, data=diamonds)
#' elbowplot(p1)
#' # modify with additional ggplot functions:
#' elbowplot(p1) + geom_hline(aes(yintercept=.05), col='red')
#'
#' elbowplot(p1, n=4) # only plot the first 4 PCs

elbowplot <- function(object, n='all') {
  # check that object is class pca
  if(class(object)!='pca') stop('object must be of class pca')
  summary = object[[6]] # extract summary from PCA object
  # n specifies the number of PCs to include in the plot:
  if(n=='all') {
    summary = summary
  } else if (class(n)!='numeric' & class(n)!='integer') stop('n should be numeric')
    else {
    summary = summary[, 1:n]
  }
  plotmatrix = t(summary) # transpose
  plotmatrix = as.data.frame(plotmatrix) # convert to dataframe
  plotmatrix$pc = 1:nrow(plotmatrix) # add variable for PC ids
  plot = ggplot2::ggplot(data=plotmatrix, ggplot2::aes(x=plotmatrix$pc, y=plotmatrix$`Proportion of Variance`)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(group='none') +
    ggplot2::theme_classic() +
    ggplot2::labs(x='Principal Components', y='Percent Variance Explained')
  return(plot)
  }


