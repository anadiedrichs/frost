#TODO requires or import packages


#'
#' Predict the minimum temperature using the recommended FAO equation
#'
#' Predict the minimum temperature using the recommended FAO equation, which can be applied to
#' nights with radiative frost (wind less than 2 m/s^2, no clouds, no rain).
#' For more details please check: TODO
#' This method is a multiple linear regression of the following form:
#' Tmim = a * temp + b * dw + i
#' where *temp*  and *dw* are the temperature and dew point respectively (chequear spelling)
#' two hours after sunset
#' This function resolve the equation and find the coefficients
#' @param temp: [°C] an array of ambient temperature, two hours after sunset.
#' @param dw: [°C] an array of dew points, two hours after sunset.
#' @param tmin: [°C] minimum temperature
#' @return a, b, and i values
#' @export
#' @examples
#' x1 <- rnorm(100,mean=2,sd=5)
#' x2 <- rnorm(100,mean=2,sd=5)
#' y <- rnorm(100,mean=0,sd=2)
#' predFAO(x1,x2,y)

predFAO <- function(dw,temp,tmin)
{
  # TODO chequear valores validos en temp, tmin, dw
  # chequear que tienen la misma longitud los arrays
  # chequear que no haya valores nulos

  if(checkTemp(dw) && checkTemp(temp) && checkTemp(tmin)
     && checkLenght(dw,tmin) && checkLenght(temp,tmin))
  {
    data = as.data.frame(y = tmin, x1 = temp, x2= dw)
    fit <- lm(y ~ x1 + x2 , data=data)
    return(c = coefficients(fit), fit = fit)
  }else(return(NULL))
#https://www.statmethods.net/stats/regression.html
}

#' Predict the trend of the temperature during a frost nigth
#' @param temp Temperature in °C, 2 hours after sunset
#' @param Tmin predicted minimum temperature
#' @param t2 temperature 2 hours after sunset
#' @param n how many hours for sunrise
#' @param i value which must be i < n, i hours value after sunset
#' @return
#' @export
#' @examples
#'
# from uc davis formula http://biomet.ucdavis.edu/frostprotection/fp002.htm
predTrend <- function(Tmin, t2, i, n)
{
  b = ((Tmin - t2)/sqrt(n - 2))
  ti = t2 + b * sqrt(i-2)
  return(ti)
}

#TODO doc
#TODO improve plot
plotTrend <- function(Tmin, t2, n)
{
  v <- c(3:n)

  for(i in 1:(n-2)){
    v[i] <- predTrend(Tmin,t2,i,n)
  }
  print(length(v))
  print(length(seq(3,n,1)))
  plot(x = c(3:n), y = v, type = "l", xlab= "Hours after sunset", ylab= "Temperature trend")
}


# consultar a darksky la temperatura de dicho sitio + punto de rocio actual
# tambien obtener su pronostico
# compararlo con el pronostico de alguna formula u otro modelo
