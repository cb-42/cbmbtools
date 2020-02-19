#' Generate a list of randomForest models.
#'
#' @param df Dataframe that contains the variables to be included in the model.
#' @param y The response vector.
#' @param nforest Number of random forests to create, 100 by default.
#' @param seed Numeric value passed to \code{\link{set.seed}}, for ensuring reproducibility. Although the default is 1234, best practice would be to vary the seed, perhaps by project, or even by model.
#' @param ... Optional parameters to pass to \code{\link[randomForest]{randomForest}}.
#' @return A list object containing \code{nforest randomForest} models.
#' @export
#' @examples
#' many_rf(data = vfd_fam.norm[, -length(vfd_fam.norm)], y = vfd_fam.norm[, "VFDandAlive"], importance = TRUE)

many_rf <- function(df, y, nforest = 100, seed = 1234, ...) {
  set.seed(seed)
  rflist <- vector("list", nforest)  # creating list vector ahead of time is much faster than during loop
  for (i in seq_along(1:nforest)) {
    rflist[[i]] <- randomForest::randomForest(y ~ ., data = df, ...)
  }
  return(rflist)
}
