#' @title Validation metrics and data creation
#' @param data is the validation data
#' @param yi is the actual measured value
#' @param yhat is the fitted value
#' @param grade_filter a numeric value to filter certain observations by grade
#' @param model_name this text input will be added as a new column titled model
#' @param n defaults to `NULL`, If `NULL` then calculated from `data` after `grade_filter` is applied. If numeric, it is used as the number of observations
#' @param params is the input for the number of parameters in the model used for argument `yhat`
#' @param resid_out defaults to `FALSE`. If `FALSE` then the model metrics are returned. If `TRUE` the residuals are output.

valid_detail <- function(data, yi = c("Actual kj/min", "Actual vo2/kg/ml")[1], yhat, grade_filter = NULL, model_name = "", 
                         n = NULL, params, resid_out = FALSE){
  if (is.null(grade_filter)) {
    dat <- data
  } else {
    dat <- filter(data, grade > grade_filter)
  }
  
  if (is.null(n)) n <- nrow(dat)
  
  y_i <- pull(dat,yi)
  y_bar <-  mean(y_i)
  y_hat <- pull(dat, yhat)
  resid <- y_i - y_hat
  sse <- sum((resid)^2)
  mse <- sse/(n - params)
  ssto <- sum((y_i - y_bar)^2)
  adjusted <- 1 - (((n - 1)/(n - params)) * (sse/ssto))
  # https://newonlinecourses.science.psu.edu/stat462/node/131/
  standard_error <- sqrt(mse)

  out <- tibble(adj_r_sqr = adjusted, mse = mse, model = model_name, standard_error)
  if (resid_out) out <- resid
  out
}


# mydata2 <- bind_rows(
#   valid_detail(data = dat_validate, yi = "actual_vo2kgml", yhat = "byui_vo2kgml", model_name = "BYUI_vo2kgml", params = filter(ct, model == "byui")$params),
#   valid_detail(data = dat_validate, yi = "actual_vo2kgml", yhat = "acsm_vo2kgml", grade_filter = 0, model_name = "ACSM_vo2kgml", 
#                params = filter(ct, model == "acsm")$params),
#   valid_detail(data = dat_validate, yi = "actual_vo2kgml", yhat = "lcda_vo2kgml", model_name = "LCDA_vo2kgml", params = filter(ct, model == "lcda")$params))

