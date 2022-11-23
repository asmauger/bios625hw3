#' Principal Components Analysis
#'
#' @description
#' Performs a principal component analysis on input data and returns the principal components and related quantities in an object of class `pca`.
#' @param x a numeric matrix or data frame containing data for the PCA.
#' @param formula optional; instead of `x`, specify a formula containing numeric variables from `data`. Do not specify a response variable.
#' @param data a matrix or data frame containing the variables specified in `formula`. Only required if `formula` is used.
#' @param subset an optional vector containing indices for rows of `x` or of `data` to be used for the PCA.
#' @param scale a logical value indicating whether the variables should be mean-centered and scaled to unit variance.
#' @param retx a logical value indicating whether the rotated matrix should be returned.
#'
#' @return
#' Returns a list of class `pca` containing the following elements: \describe{
#'     \item{center}{the means of the input variables used for centering if `scale=T`.}
#'     \item{scale}{the standard deviations of the variables used for scaling if `scale=T`.}
#'     \item{sdev}{the standard deviations of the principal components.}
#'     \item{rotation}{a matrix of the principal components. More specifically, the eigenvectors of the covariance matrix of the input data.}
#'     \item{x}{a matrix containing the rotated data if `retx=T`. That is, the scaled data multiplied by `rotation`.}
#'     \item{summary}{a matrix containing `sdev`, the proportion of variance associated with each PC, and the cumulative proportion of variance associated with each PC. Can be accessed with `summary()`.}
#' }
#' @export
#'
#' @examples
#' data(iris)
#' # Note: cannot include non-numeric variables.
#' p1 = pca(iris[, -5]) # Input matrix method.
#' summary(p1)
#' p2 = pca(~Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris) # Formula method.
#' summary(p2)
#' all.equal(summary(p1), summary(p2)) # both methods yield same results
#' # Scaling is recommended, but can be skipped.
#' p3 = pca(~Sepal.Length + Sepal.Width, data=iris, subset=c(1,50), scale=FALSE)
#' summary(p3)

pca = function(x, formula=NULL, data=NULL, subset=NULL, scale=T, retx=T) {
  # allow user to avoid specifying 'formula=' in function call
  if(class(x)[1]=='formula' & is.null(formula)) {
    formula = x
  }
  # generate data frame from formula & data or from x
  .prepdata = function() {
    # if formula is specified:
  if(is.null(formula)==F){
    # check if formula specifies a response
    if(attr(stats::terms(formula), 'response') == 1) stop('error in formula; formula should not have response')
    # check if data were specified
    if(is.null(data)) stop('must specify dataframe for formula')
    df = as.data.frame(data)
    # subset dataframe rows if subset is specified
    if(is.null(subset)) {
      df = df
    } else {
      df = df[subset,]
    }
    # apply formula to dataframe
    x1 = stats::model.frame(formula, df)
  } else { # if formula was not specified:
    x1 = as.data.frame(x)
    # subset dataframe rows if subset is specified
    if(is.null(subset)) {
      x1 = x1
    } else {
      x1 = x1[subset,]
    }
    # drop rows containing NA values
    x1 = stats::na.omit(x1)
  }
    # warn user if NAs were removed
  if(length(attr(x1, 'na.action'))!=0) warning(paste(length(attr(x1, 'na.action')), 'rows were removed due to missing values'))
    # check for non-numeric variables
    classes = c(1:ncol(x1))
    for(i in 1:ncol(x1)) {
      classes[i] = class(x1[,i])
    }
    if(sum(classes!='numeric' & classes!= 'integer')!=0) stop('variables should be numeric')
  return(x1)
  }
  x1 = .prepdata()
  # save extras (mean, scale) for the return list
  center = colMeans(x1)
  scalefactor = apply(x1, 2, stats::sd)
  # scaling to 0 mean and unit variance
  .scale = function(x) {
    # mean center and divide by sdev
    x1 = dplyr::mutate(x, dplyr::across(dplyr::everything(),~(.x-mean(.x))/sd(.x)))
    return(x1)
  }
  if(scale) x1 = .scale(x=x1)
  # singular value decomposition
  x1 = as.matrix(x1)
  svdresult = svd(x1)
  eigenvalues = (svdresult$d)^2 # singular values are sqrt of eigenvalues of t(X)%*%X (proportional to covariance matrix of X)
  rotation = svdresult$v # right singular vectors are eigenvectors
  colnames(rotation) = paste0('PC', 1:ncol(rotation))
  rownames(rotation) = colnames(x1)
  sdev = sqrt(eigenvalues/(nrow(x1)-1)) # st dev of principal components (sqrt of the eigenvalues of the cov matrix)
  names(sdev) = paste0('PC', 1:length(sdev))
  if(retx) {
    x = x1%*%rotation
    colnames(x) = paste0('PC', 1:ncol(x))
  } else x = NULL
  # summary (sdev, proportion of variance, and cumulative proportion)
  sdev_round = round(sdev, 4)
  pvar = round(sdev^2/sum(sdev^2), 4)
  cumvar = round(cumsum(sdev^2)/sum(sdev^2), 4)
  summary = cbind(sdev_round, pvar, cumvar)
  colnames(summary) = c('Standard deviation', 'Proportion of Variance', "Cumulative Proportion")
  summary = t(summary)
  result = list(center, scalefactor, sdev, rotation, x, summary)
  names(result) = c('center', 'scale', 'sdev', 'rotation', 'x', 'summary')
  class(result) = 'pca'

  return(result)
}
#'@export
print.pca = function(x, ...) print(x[c(3, 4)]) # define custom print to avoid printing the entire rotated data matrix
#'@export
summary.pca = function(object, ...) print(object[[6]]) # define custom summary

