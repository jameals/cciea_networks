#' Standardize GLM 
#'
#' Standardize the input variables for a GLM
#'
#' @param m the unstandardized model from the "glm" function
#' @param name the network metric being modeled (as a character string)
#' @param season whether the input data are for the early or late season ["Early Season" / "Late Season"]
#' @return a data frame with the standardized model's coefficients
#' @examples
#' ed_early.glm <- glm(ed ~ D*R + N + pcgroup, data = edata, family = quasibinomial('logit'))
#' ed_early.sglm <- get_sc_coef(ed.glm, name="Edge Density",season="Early Season")
#' @export
get_sc_coef <- function(m,name,season){
  display(arm::standardize(m))
  tmpdat <- as.data.frame(arm::standardize(m)$coefficients)
  colnames(tmpdat) <- c("coefficients")
  tmpdat <- mutate(tmpdat, variable=rownames(tmpdat),
                   metric=name, season=season)
  return(tmpdat)
}