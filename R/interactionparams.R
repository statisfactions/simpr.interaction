#' Extract simulation parameters for power analysis of regression interaction models.
#'
#' Extract useful simulation parameters for regression.interaction models,
#' computed via change in R2. This function is largely a wrapper for
#' \code{\link[InteractionPoweR:power_interaction_r2]{power_interaction_r2()}}
#' from the \code{InteractionPoweR} package which calculates useful quantities based
#' on correlations and reliabilities. This function then massages the output into
#' a more convenient format for simpr as well as calculating R2 and sigma.
#'
#' @param r.x1.y Pearson's correlation between x1 and y. Must be between -1 and 1. Has no default value. Can be a single value or a vector of values.
#' @param r.x2.y Pearson's correlation between x2 and y. Must be between -1 and 1. Assumed to be the 'moderator' in some functions. Has no default value. Can be a single value or a vector of values.
#' @param r.x1x2.y Pearson's correlation between the interaction term x1x2 (x1 * x2) and y. Must be between -1 and 1. Has no default value. Can be a single value or a vector of values.
#' @param r.x1.x2 Pearson's correlation between x1 and x2. Must be between -1 and 1. Has no default value. Can be a single value or a vector of values.
#' @param rel.x1 Reliability of x1 (e.g. test-retest reliability, ICC, Cronbach's alpha). Default is 1 (perfect reliability). Must be greater than 0 and less than or equal to 1.
#' @param rel.x2 Reliability of x2 (e.g. test-retest reliability, ICC, Cronbach's alpha). Default is 1 (perfect reliability). Must be greater than 0 and less than or equal to 1.
#' @param rel.y Reliability of y (e.g. test-retest reliability, ICC, Cronbach's alpha). Default is 1 (perfect reliability). Must be greater than 0 and less than or equal to 1.
#'
#' @return A single-row dataframe with the following
#'
#' @importFrom InteractionPoweR "power_interaction_r2"
#'
#' @export
#' @examples
#' params_all = interaction_params(r.x1.y = -0.18,
#'  r.x2.y = -0.03,
#'  r.x1.x2 = 0.64,
#'  r.x1x2.y = 0.1,
#'  rel.y = 0.79,
#'  rel.x1 = 0.89,
#'  rel.x2 = 0.95)

interaction_params = function (r.x1.y, r.x2.y, r.x1x2.y, r.x1.x2, rel.x1 = 1, rel.x2 = 1,
                       rel.y = 1) {

  params_all = InteractionPoweR::power_interaction_r2(
    N = 500, # This number does not affect the remainder of the calculations and is a placeholder
    r.x1.y = r.x1.y,
    r.x2.y = r.x2.y,
    r.x1x2.y = r.x1x2.y,
    r.x1.x2 = r.x1.x2,
    rel.x1 = rel.x1,
    rel.x2 = rel.x2,
    rel.y = rel.y,
    alpha = 0.05, # also a placeholder for the purpose of these calculations
    detailed_results = TRUE
  )

  params = as.list(params_all[,c("b1", "b2", "b3", "obs.r.x1.x2", "obs.r.x1.y", "obs.r.x2.y",
                        "obs.r.x1x2.y")])
  params$r2 = params$b1*params$obs.r.x1.y + (params$b2*params$obs.r.x2.y) + (params$b3*params$obs.r.x1x2.y)
  params$sigma = sqrt(1-params$r2)

  return(params)
}
