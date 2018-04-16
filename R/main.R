
#'
#' Predict the minimum temperature using the recommended FAO equation
#'
#' Predict the minimum temperature using the recommended FAO equation, which can be applied to
#' nights with radiative frost (wind less than 2 m/s^2, no clouds, no rain).
#' For more details please check: TODO
#'
#' Tmim = a * temp + b * dw + c
#' where *temp*  and *dw* are the temperature and dew point respectively (chequear spelling)
#' two hours after sunset
#' This function resolve the equation and find the coefficients
#' @param temp [°C]: an array of ambient temperature, two hours after sunset.
#' @param dw [°C]: an array of dew points, two hours after sunset.
#' @param tmin [°C]: minimum temperature
#' @return a, b, and c values
#' @export
#' @examples
#' x1 <- rnorm(100,mean=2,sd=5)
#' x2 <- rnorm(100,mean=1,sd=3)
#' y <- rnorm(100,mean=0,sd=2)
#' predFAO(dw = x2,temp=x1,tmin=y)

predFAO <- function(dw,temp,tmin)
{
  a = 0; b = 0; c= 0
  # chequear que no haya valores nulos

  if(checkTemp(dw) && checkTemp(temp) && checkTemp(tmin)
     && checkLenght(dw,tmin) && checkLenght(temp,tmin))
  {
    # primera formula
    data = as.data.frame(cbind(y = tmin, x1 = temp, x2= dw))
    fit1 <- lm(y~x1, data=data)
    a <- fit1$coefficients[[2]]
    w <-  fit1$coefficients[[1]]
    #Tp prima = a t_0 + b
    tp_1 <- a * temp + w
    residual1 <- tmin - tp_1
    # segunda formula
    fit2 <- lm(dw ~ residual1 , data=as.data.frame(cbind(dw = dw, residual1 = residual1)))
    b <- fit2$coefficients[[2]]
    c <- w + fit2$coefficients[[1]]

    # analisis de la prediccion
    Tp <- a * temp + b * dw + c
    Rp <- tmin - Tp
    r2 <- (var(tmin)-(var(tmin)-var(Tp))) / var(tmin)
    return(list(a = a, b= b, c= c, Tp = Tp, Rp = Rp, r2 = r2))

  }else(return(NULL))
#https://www.statmethods.net/stats/regression.html
}


# predTrend <- function(Tmin, t2, i, n)
# {
#   b = ((Tmin - t2)/sqrt(n - 2))
#   ti = t2 + b * sqrt(i-2)
#   return(ti)
# }
#' Predict the trend of the temperature during a frost nigth.
#' This equation has been taken
#' from UC Davis formula http://biomet.ucdavis.edu/frostprotection/fp002.htm
#' @param Tmin predicted minimum temperature
#' @param t2 temperature 2 hours after sunset
#' @param n how many hours between sunset and sunrise
#' @return A vector with the n-2 values of estimated temperatures
#' @export
#' @examples
#' plotTrend(Tmin = 22.2,t2 = 33.7,n = 15) # in °F degress
#' plotTrend(Tmin = -5.45,t2 = 0.95,n = 15) # in °C degress
#
plotTrend <- function(Tmin, t2, n)
{
  #TODO check Tmin << a t2
  #TODO check n sea mayor a 2
  v <- vector(mode = "numeric",length = n-2)
  b = ((Tmin - t2)/sqrt(n - 2))

  for(i in 3:n)
  {
    ti = t2 + b * sqrt(i-2)
    v[i-2] <- ti
  }
  #print(length(v))
  #print(length(seq(3,n,1)))
  plot(x = c(3:n), y = v, type = "l", xlab= "Hours after sunset", ylab= "Temperature trend")
  return(v)
}


# consultar a darksky la temperatura de dicho sitio + punto de rocio actual
# tambien obtener su pronostico
# compararlo con el pronostico de alguna formula u otro modelo
