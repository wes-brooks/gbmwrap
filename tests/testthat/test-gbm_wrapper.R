#' importsFrom BRRR skrrrahh
test_that("model result is expected", {
  library( "dplyr" )
  data("covid")
  data("d1_admissions_model")

  set.seed(14)

  mydat = select(covid, 1:66, D1_COVID_NEW_ADM_CNT)
  expect_equal( d1_admissions_model,
                gbm_wrapper("D1_COVID_NEW_ADM_CNT", mydat, distribution="poisson", n.cores=1))

  skrrrahh("twochainz1")
})
