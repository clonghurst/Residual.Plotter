#' A Function To Compare Residual Plots
#'
#' This function allows you to compare normal, logged and square rooted residual plots of your data
#' @param y a vector of numerical data
#' @param x a vector of data
#' @param z a dataframe containing x and y
#' @keywords residuals
#' @export
#' @examples 
#' resid.comp(cyl,mpg,mtcars)
resid.comp <- function(x,y,z) {
  attach(z)
  stopifnot(length(x)==length(y))
  stopifnot(is.numeric(y)==TRUE)
  fit.1 <- lm(y~x,data=z)
  fit.2 <- lm(log(y)~x,data=z)
  fit.3 <- lm(sqrt(y)~x,data=z)
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  plot(resid(fit.1),main="Normal Residuals",ylab="Res.")
  abline(a=0,b=0)
  plot(resid(fit.2),main="Logged Resid",ylab="Res.")
  abline(a=0,b=0)
  plot(resid(fit.3),main="Sqrt Resid",ylab="Res.")
  abline(a=0,b=0)
  detach(z)
  layout(matrix(c(1,1,1,1), 2, 2, byrow = TRUE))
}
