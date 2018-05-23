# RMSE root mean squared error
rmse <- function(error)
{
  sqrt(mean(error^2))
}
# coefficient of correlation, also known as r squared value
rsquared <- function(a,b)
{
  (var(a)-(var(a)-var(b))) / var(a)
}
