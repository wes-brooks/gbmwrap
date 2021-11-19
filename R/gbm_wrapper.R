# function to fit a gbm model
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
  k = floor( nrow(data) / 20 )
  sorted_vars = summary( screening_model, n.trees=ntree_screen, plotit=FALSE )$var
  f2 = formula(paste0( target, "~", paste(sorted_vars[1:k], collapse='+')))


  # fit the model
  cvk = ifelse( k==1, 0, 10 )
  model = gbm( f2, data=data, cv.folds=cvk, n.trees=n.trees, interaction.depth = interaction.depth, n.minobsinnode=minobs, shrinkage=shrinkage, distribution=distribution, ...)

  # return the fitted model
  model
}
