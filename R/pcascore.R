#' PCA Score Plot
#' @description
#' Plots the rotated data from a `pca` object.
#' It represents the projection of the original data into the space spanned by the two specified principal components.
#' @param object an object of class `pca` generated using the `pca()` function.
#' @param pc a vector indicating the two principal components that should be plotted.
#' @param scale a number between 0 and 1 to scale the rotated data stored in `object`. The scaling procedure is the same as used in `autoplot()` for `prcomp` objects in `ggplot2`.
#' @param grouping a vector of the same length and in the same order as the data used in `pca()` that corresponds to categories of the original data.
#'
#' @return
#' Returns a ggplot. This plot can be modified by adding additional ggplot functions.
#' @export
#'
#' @examples
#' require(ggplot2)
#' require(ggfortify)
#' require(patchwork)
#' # Compare to score plot from ggplot2/ggfortify
#' data(iris)
#' p1 = pca(iris[,-5]) # pca from pca package
#' p2 = prcomp(iris[,-5], scale=TRUE) # pca from stats package
#' plot1 = scoreplot(p1, pc=c(1,2), grouping=iris[,5])
#' plot2 = autoplot(p2, data=iris, colour='Species') # from ggfortify
#' plot1 + plot2
#'
#' # Plot all combinations of PCs
#' data(swiss)
#' q = pca(~Fertility + Agriculture + Catholic + Infant.Mortality, data=swiss)
#' pcs = combn(x=1:4, m=2)
#' plot_list = vector("list", ncol(pcs))
#' for(i in 1:ncol(pcs)) {
#'     plot_list[[i]] = scoreplot(q, pc=pcs[,i])
#' }
#' wrap_plots(plot_list)
#'
#' # Set scale to 0 to remove scaling
#' data(diamonds)
#' r = pca(~carat+depth+table+x+y+z, data=diamonds)
#' plot_scaled = scoreplot(r, scale=1)
#' plot_unscaled = scoreplot(r, scale=0)
#' plot_scaled + plot_unscaled # compare scaled and unscaled plots

scoreplot = function(object, pc=c(1,2), scale=1, grouping=NULL) {
  # check that object is class pca
  if(class(object)!='pca') stop('object must be of class pca')
  # check that pc is a vector of length 2
  if(length(pc)!=2 | (class(pc)!= 'numeric' & class(pc)!= 'integer')) stop('pc must be a numeric vector of length 2')
  # check that scale is numeric or integer
  if(length(scale)!=1 | class(scale)!= 'numeric' & class(scale)!= 'integer')stop('scale must a single number')
  x = object[[5]][,pc] # rotated x

  # scale x as done in biplot.princomp and ggbiplot (ggfortify package)
  lambda = object[[3]] # lambda are the singular values from svd (or sdev)
  lambda = lambda[pc] # only use 2 pcs
  n = nrow(x)
  x = apply(x, 1, function(z)(z*1/(lambda*sqrt(n))^(scale)))
  x=t(x)
  xdf = as.data.frame(x)

  # add grouping to dataframe if it is not null:
  if(is.null(grouping)==F){
  # check that grouping is a vector
  if(is.null(dim(grouping))==F) stop('grouping should be a vector')
  # check that grouping is of same length as x
  if(length(grouping)!= nrow(x)) stop('grouping must be same length as the number of observations in data used for pca')
  grouping = as.factor(grouping) # coerce to factor
  xdf$group = grouping # add grouping to xdf
  }
  # put together plot
  pclabels = colnames(xdf)
  varexplained = (object[[3]]^2/sum(object[[3]]^2))[pc]
  varexplained = round(varexplained, 4)*100 # percentage
  if(is.null(grouping)) { # only use colors if grouping is not null
    geo_point = ggplot2::geom_point(data=xdf, ggplot2::aes(x=xdf[,1], y=xdf[,2]))
    labs = ggplot2::labs(x=paste0(pclabels[1], '(', varexplained[1], '%)'), y=paste0(pclabels[2], '(', varexplained[2], '%)'))
    } else {
      geo_point = ggplot2::geom_point(data=xdf, ggplot2::aes(x=xdf[,1], y=xdf[,2], color=xdf[,3]))
      labs = ggplot2::labs(x=paste0(pclabels[1], '(', varexplained[1], '%)'), y=paste0(pclabels[2], '(', varexplained[2], '%)'), col=pclabels[3])
    }
  plot = ggplot2::ggplot() +
    geo_point + # pc plot
    ggplot2::theme_classic() +
    labs
  return(plot)
}


