#' Function to fit a gbm model
#'
#' This wraps up my most common options when building a gradient boosted random forest model with GBM
#'
#' @param target name of the column that is being predicted
#' @param data `data.frame` of predictors and the response variable
#' @param n.trees how many trees in the random forest
#' @param shrinkage learning rate
#' @param interaction.depth how many branches per tree
#' @param distribution statistical distribution of the response variable
#'
#' @return a fitted GBM model
#'
#' @importFrom gbm gbm gbm.perf
#' @export
gbm_wrapper = function( target, data, n.trees=5000, shrinkage=0.0025, interaction.depth=5, distribution ="gaussian", ... ) {
  # drop rows with NAs
  data = data[ complete.cases(data), ]

  # get the variables with the most explanatory power

  ## first, set up a formula to run all predictors through the gbm algo
  # target = as.character(formula)[[2]]
  f1 = formula( paste0( target, "~." ))

  minobs = min( 4, floor(nrow(data) / 10) )

  ## now fit a gbm model to all the data and find the optimal number of trees
  screening_model = gbm( f1, data=data, cv.folds=10, n.trees=n.trees, interaction.depth = interaction.depth, n.minobsinnode=minobs, shrinkage=shrinkage, distribution=distribution, ...)
  ntree_screen = gbm.perf( screening_model, plot.it=FALSE )

  ## get the top predictors
  sorted_vars = summary( screening_model, n.trees=ntree_screen, plotit=FALSE )$var
  k = min( length(sorted_vars), ceiling( nrow(data) / 20 ))
  f2 = formula(paste0( target, "~", paste(sorted_vars[seq_len(k)], collapse='+')))


  # fit the model
  cvk = ifelse( k==1, 0, 10 )
  model = gbm( f2, data=data, cv.folds=cvk, n.trees=n.trees, interaction.depth = interaction.depth, n.minobsinnode=minobs, shrinkage=shrinkage, distribution=distribution, ...)

  # return the fitted model
  model
}
