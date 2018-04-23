
#' @title Predict the minimum temperature using the recommended FAO equation
#' @description
#' Predict the minimum temperature using the recommended FAO equation, which
#' can be applied to
#' nights with radiative frost (wind less than 2 m/s^2, no clouds, no fog, no rain).
#' @details The method was extracted from the documentation and previous implementation in FFST.xls file,
#' which is name in the book
#' "Frost Protection: fundamentals, practice, and economics. Volume 1.
#' Authors: Richard L Snyder, J. Paulo de Melo-Abreu.
#' Food and Agriculture Organization of the United Nations. 2005"
#'
#' This function implements the method, resolve the equation and find the coefficients.
#' Equation: Tmim = a * temp + b * dw + c
#' where "temp"  and "dw" are the temperature and dew point respectively, which must be taken
#' two hours after sunset
#'
#' For more details please check:
#' http://www.fao.org/docrep/008/y7223e/y7223e0b.htm#bm11.8
#' http://www.fao.org/docrep/008/y7223e/y7223e0b.htm
#' FFST spreasheet http://biomet.ucdavis.edu/frostprotection/FTrend/FFST_FTrend.htm
#'
#' @param temp [°C]: an array of ambient temperature, two hours after sunset.
#' @param dw [°C]: an array of dew points, two hours after sunset.
#' @param tmin [°C]: minimum temperature
#' @return a, b, and c values, which can be used to estimate minimum temperature (Tmin) using temp
#' and dw. This function also returns Tp (predicted temperature using the equation), Rp (residuals, the
#' difference between tmin given and Tp) and r2 which is the coefficient of correlation (R squared).
#' @export
#' @examples
#' x1 <- rnorm(100,mean=2,sd=5)
#' x2 <- rnorm(100,mean=1,sd=3)
#' y <- rnorm(100,mean=0,sd=2)
#' predFAO(dw = x2,temp=x1,tmin=y)
#' #data example taken from FAO Book
#' t0 <- c(3.2,0.8,0.2,2.6,4.4,5.2,2.7,1.2,4.5,5.6) # temperature 2 hours after sunset
#' td <- c(-4.2,-8.8,-6.5,-6.2,-6.1,2.6,-0.7,-1.7,-1.2,0.1) # dew point 2 hours after sunset
#' tn <- c(-3.1,-5,-6.3,-5.4,-4,-2.5,-4.8,-5,-4.4,-3.3)
#' predFAO(dw = td,temp=t0,tmin=tn)
predFAO <- function(dw,temp,tmin)
{
  a = 0; b = 0; c= 0; d = 0
  # chequear que no haya valores nulos

  if(checkTemp(dw) && checkTemp(temp) && checkTemp(tmin)
     && checkLenght(dw,tmin) && checkLenght(temp,tmin)
    && checkNoNA(dw) && checkNoNA(tmin) && checkNoNA(temp))
  {
    # first formula
    data = as.data.frame(cbind(y = tmin, x1 = temp, x2= dw))
    fit1 <- lm(y~x1, data=data)
    a <- fit1$coefficients[[2]] #slope
    w <-  fit1$coefficients[[1]] #intercept
    #Tp prima = a t_0 + b
    tp_1 <- a * temp + w
    residual1 <- tmin - tp_1
    # segunda formula
    fit2 <- lm(res1 ~ dw , data=as.data.frame(cbind(dw = dw, res1 = residual1)))
    b <- fit2$coefficients[[2]] # slope
    c <- w + fit2$coefficients[[1]] # intercept

    # analisis de la prediccion
    Tp <- a * temp + b * dw + c
    Rp <- tmin - Tp
    r2 <- rsquared(tmin, Tp) #(var(tmin)-(var(tmin)-var(Tp))) / var(tmin)
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
#' which was also published in the book
#' @param Tmin predicted minimum temperature.
#' @param t2 temperature 2 hours after sunset, where t2 > Tmin
#' @param n how many hours between sunset and sunrise, an integer value where n > 2
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
  v= NULL
  if(checkTemp(Tmin) && checkTemp(t2) && is.numeric(n))
  {
    if (n <= 2) stop("argument n value must be major than two (2)")
    if (Tmin > t2) stop("Tmin argument must be less than t2")
    v <- vector(mode = "numeric",length = n-2)
    b = ((Tmin - t2)/sqrt(n - 2))

    for(i in 3:n)
    {
      ti = t2 + b * sqrt(i-2)
      v[i-2] <- ti
    }
    #print(length(v))
    #print(length(seq(3,n,1)))
    plot(x = c(3:n), y = v, type = "l", xlab= "Hours after sunset", ylab= "Temperature")
    return(v)
  }else stop("Check valid values for the function arguments")


}
#'
#'@title empiric equation for minimum temperature used in Mendoza
#'@description
#' According to Maldonado (see [1]), the empirical equation used in Mendoza to estimate the minimum
#' temperature in the night is:
#'
#' Tmin = ((Tmax + dew)/2)) - K
#'
#' , where K is a constant calculated for each place, Tmax: maximum temperature of previous day, dew: dew point
#' in °C, Tmin: is the forecaste minimum temperature.
#' Given an array of the information of dw, tempMax and tmin,
#' this function calculates K constant using linear regression.
#' [1] Ortiz Maldonado, Alberto. Adversidades agrometeorológicas de Mendoza. 1991.
#'@param dw Dew Point in °C
#'@param tempMax Maximum temperature of the previous day
#'@param tmin Minimum temperature measure that day.
#'@return a list with:
#'* K constant value, which could be used in predMza function
#'* model: an object of class lm (see ?lm)
#'@examples
#' # just a random example
#' dw <- c(-2,-5,2,6,8)
#' tempMax <- c(10,20,30,25,29)
#' tmin <- c(-1,-2,3,5,10)
#' predMzaCalculateEquation(dw,tempMax,tmin)
predMzaCalculateEquation <- function(dw,tempMax,tmin)
{
  k = vector(mode = "logical", length = length(dw))
  # chequear que no haya valores nulos

  if(checkTemp(dw) && checkTemp(tempMax) && checkTemp(tmin)
     && checkLenght(dw,tmin) && checkLenght(tempMax,tmin))
  {
    # k <- ((tempMax + dw)/2)-tmin
    dd <- as.data.frame(cbind(dw,tempMax,tmin))
    model <- lm(tmin~.,as.data.frame(dd))
    # TODO check that model is ok or not null before return
  }
  return(list(model=model,k = model$coefficients[[1]]))
}

#'@title empiric equation for minimum temperature used in Mendoza
#'@description
#' According to Maldonado (see [1]), the empirical equation used in Mendoza to estimate the minimum
#' temperature in the night is:
#'
#' Tmin = ((Tmax + dew)/2)) - K
#'@param dw Dew Point in °C
#'@param tempMax Maximum temperature of the previous day
#'@param K numeric constant
#'#'@return predicted minimum temperature
#'@examples
#' # just an example
#' dw <- c(-2,-5,2,6,8)
#' tempMax <- c(10,20,30,25,29)
#' tmin <- c(-1,-2,3,5,10)
#' out <- predMzaCalculateEquation(dw,tempMax,tmin)
#' predMza(dw = -3, tempMax = 15, K=out$K)
#'
predMza <- function(dw,tempMax,K)
{
  tmin <- NULL

  if(checkTemp(dw) && checkTemp(tempMax) && checkLenght(dw,tempMax) )
  {
    tmin <- ((tempMax + dw)/2) - K
  }

  return(tmin)
}


# consultar a darksky la temperatura de dicho sitio + punto de rocio actual
# tambien obtener su pronostico
# compararlo con el pronostico de alguna formula u otro modelo
