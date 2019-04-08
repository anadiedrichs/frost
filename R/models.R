
setClass("FAOFrostModel",
         representation(a = "numeric", b = "numeric", c= "numeric", Tp="numeric",Rp="numeric",r2="numeric"))
setClass("MdzFrostModel",
         representation(k = "numeric", kvector="numeric"))

#' @title Estimate the coefficients for the recommended FAO equation
#' @description
#' Estimate the coefficients for the recommended FAO equation using: ambient temperature
#' and dew point taken two hours after sunset, and minimum  daily temperature, which
#' were taken in nights with radiative frost
#' (wind less than 2 m/s^2, no clouds, no fog, no rain).
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
#' <http://www.fao.org/docrep/008/y7223e/y7223e0b.htm#bm11.8>
#' <http://www.fao.org/docrep/008/y7223e/y7223e0b.htm>
#' FFST spreasheet <http://biomet.ucdavis.edu/frostprotection/FTrend/FFST_FTrend.htm>
#'
#' @param temp [°C]: an array of ambient temperature, two hours after sunset.
#' @param dw [°C]: an array of dew points, two hours after sunset.
#' @param tmin [°C]: minimum temperature
#' @return A FAOFrostModel object with a, b, and c values, which can be used to estimate minimum temperature (Tmin) using temp
#' and dw. This function also returns Tp (predicted temperature using the equation), Rp (residuals, the
#' difference between tmin given and Tp) and r2 which is the coefficient of correlation (R squared).
#' @export
#' @examples
#' x1 <- rnorm(100,mean=2,sd=5)
#' x2 <- rnorm(100,mean=1,sd=3)
#' y <- rnorm(100,mean=0,sd=2)
#' buildFAO(dw = x2,temp=x1,tmin=y)
#' #data example taken from FAO Book
#' t0 <- c(3.2,0.8,0.2,2.6,4.4,5.2,2.7,1.2,4.5,5.6) # temperature 2 hours after sunset
#' td <- c(-4.2,-8.8,-6.5,-6.2,-6.1,2.6,-0.7,-1.7,-1.2,0.1) # dew point 2 hours after sunset
#' tn <- c(-3.1,-5,-6.3,-5.4,-4,-2.5,-4.8,-5,-4.4,-3.3)
#' buildFAO(dw = td,temp=t0,tmin=tn)
buildFAO <- function(dw,temp,tmin)
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

    model <- new("FAOFrostModel",a = a, b= b, c= c, Tp = Tp, Rp = Rp, r2 = r2)

    # return(list(a = a, b= NULL, c= w, Tp = Tp, Rp = Rp, r2 = r2))
    return(model)

  }else(return(NULL))
#https://www.statmethods.net/stats/regression.html
}

#' @title Estimate the coefficients for the recommended FAO equation
#' @description
#'
#' Estimate the coefficients for the recommended FAO equation using
#' temperature two hours after sunset and minimum temperature.
#' @details The method was extracted from the documentation and previous implementation in FFST.xls file,
#' which is name in the book
#' "Frost Protection: fundamentals, practice, and economics. Volume 1.
#' Authors: Richard L Snyder, J. Paulo de Melo-Abreu.
#' Food and Agriculture Organization of the United Nations. 2005"
#'
#' This function implements the method, resolve the equation and find the coefficients.
#' Equation: Tmim = a * temp + c
#' where "temp"  is the temperature which must be taken two hours after sunset
#'
#' For more details please check:
#' <http://www.fao.org/docrep/008/y7223e/y7223e0b.htm#bm11.8>
#' <http://www.fao.org/docrep/008/y7223e/y7223e0b.htm>
#' FFST spreasheet <http://biomet.ucdavis.edu/frostprotection/FTrend/FFST_FTrend.htm>
#'
#' @param temp [°C]: an array of ambient temperature, two hours after sunset.
#' @param tmin [°C]: minimum temperature
#' @return A FAOFrostModel object with a, b, and c values, which can be used to estimate minimum temperature (Tmin) using temp
#' and dw. This function also returns Tp (predicted temperature using the equation), Rp (residuals, the
#' difference between tmin given and Tp) and r2 which is the coefficient of correlation (R squared).
#' @examples
#' x1 <- rnorm(100,mean=2,sd=5)
#' x2 <- rnorm(100,mean=1,sd=3)
#' y <- rnorm(100,mean=0,sd=2)
#' buildFAO(dw = x2,temp=x1,tmin=y)
#' #data example taken from FAO Book
#' t0 <- c(3.2,0.8,0.2,2.6,4.4,5.2,2.7,1.2,4.5,5.6) # temperature 2 hours after sunset
#' tn <- c(-3.1,-5,-6.3,-5.4,-4,-2.5,-4.8,-5,-4.4,-3.3)
#' buildFAOTemp(temp=t0,tmin=tn)
buildFAOTemp <- function(temp,tmin)
{
  a = 0; b = 0; c= 0; d = 0
  # chequear que no haya valores nulos

  if( checkTemp(temp) && checkTemp(tmin)
      && checkLenght(temp,tmin)
     && checkNoNA(tmin) && checkNoNA(temp))
  {
    # first formula
    data = as.data.frame(cbind(y = tmin, x1 = temp))
    fit1 <- lm(y~x1, data=data)
    a <- fit1$coefficients[[2]] #slope
    w <-  fit1$coefficients[[1]] #intercept

    # analisis de la prediccion
    Tp <- a * temp + w
    Rp <- tmin - Tp
    r2 <- rsquared(tmin, Tp)

    model <- new("FAOFrostModel",a = a, b= b, c= c, Tp = Tp, Rp = Rp, r2 = r2)
   # return(list(a = a, b= NULL, c= w, Tp = Tp, Rp = Rp, r2 = r2))
   return(model)
  }else(return(NULL))
  #https://www.statmethods.net/stats/regression.html
}

#' @title Predict the minimum temperature using the recommended FAO equation
#' @description
#'
#' Predict the minimum temperature using the recommended FAO equation, which
#' can be applied to nights with radiative frost (wind less than 2 m/s^2, no clouds, no fog, no rain).
#' @details The method was extracted from the documentation and previous implementation in FFST.xls file,
#' which is name in the book
#' "Frost Protection: fundamentals, practice, and economics. Volume 1.
#' Authors: Richard L Snyder, J. Paulo de Melo-Abreu.
#' Food and Agriculture Organization of the United Nations. 2005"
#'
#' This function returns Tmim = a * temp + b * dew + c
#' if dew argument is not null.
#' Otherwise, return Tmin = a * temp + c, if dew is NULL.
#'
#' For more details please check:
#' <http://www.fao.org/docrep/008/y7223e/y7223e0b.htm#bm11.8>
#' <http://www.fao.org/docrep/008/y7223e/y7223e0b.htm>
#' FFST spreasheet <http://biomet.ucdavis.edu/frostprotection/FTrend/FFST_FTrend.htm>
#'
#' @param model : a FAOFrostModel object
#' @param t [°C]: an array of ambient temperature, two hours after sunset.
#' @param dw [°C]: an array of dew points, two hours after sunset, by default is NULL.
#' @return  tmin [°C]: minimum temperature
#' @examples
#' t0 <- c(3.2,0.8,0.2,2.6,4.4,5.2,2.7,1.2,4.5,5.6) # temperature 2 hours after sunset
#' td <- c(-4.2,-8.8,-6.5,-6.2,-6.1,2.6,-0.7,-1.7,-1.2,0.1) # dew point 2 hours after sunset
#' tn <- c(-3.1,-5,-6.3,-5.4,-4,-2.5,-4.8,-5,-4.4,-3.3)
#' out <- buildFAO(dw = td,temp=t0,tmin=tn)
#' current_temp <- 10
#' current_dw <- 2
#' ptmin <- predFAO(out,current_temp,current_dw)
#' cat("The predicte minimum temperature is ",ptmin," °C")
#'
predFAO <- function(model,t,dw=NULL){

  tmin <- NULL
  if(class(model)!="FAOFrostModel"){stop("model should be an object of type FAOFrostModel")}
  if(checkTemp(t))
  {
    if(is.null(dw))
      tmin <- (model@a*t) + model@c
    else tmin <- (model@a*t) + (model@b*dw) +model@c

  }
  return(tmin)
  }

#' @title Temperature trend during a frost night.
#' @description
#' Predict the trend of the temperature during a frost night.
#' This equation has been taken
#' from UC Davis formula [1] which was also published in the FAO book mentioned in predFAO function.
#'
#' [1] <http://biomet.ucdavis.edu/frostprotection/fp002.htm>
#' @param Tmin predicted minimum temperature.
#' @param t2 temperature 2 hours after sunset, where t2 > Tmin
#' @param n how many hours between sunset and sunrise, an integer value where n > 2
#' @param plot TRUE if you want to see the trend plot, otherwise FALSE. Default value: FALSE
#' @return A data frame with the (x,y) points plotted, where y values are the n-2 values of estimated temperatures
#' @export
#' @examples
#' getTrend(Tmin = 22.2,t2 = 33.7,n = 15) # in °F degress
#' getTrend(Tmin = -5.45,t2 = 0.95,n = 15,plot=TRUE) # in °C degress
#
getTrend <- function(Tmin, t2, n, plot=FALSE)
{
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
    plot(x = c(3:n), y = v, type = "l", xlab= "Hours after sunset", ylab= "Temperature",col="red")
    return(data.frame(x= c(3:n),y=v))

  }else stop("Check valid values for the function arguments")
}
#'
#'@title Empiric equation for minimum temperature used in Mendoza
#'@description
#' According to Maldonado (see [1]), the empirical equation used in Mendoza
#' to estimate the minimum
#' temperature in the night is:
#'
#' Tmin = ((Tmax + dew)/2)) - K
#'
#' , where K is a constant calculated for each place,
#' Tmax: maximum temperature of previous day, dew: dew point
#' in °C, Tmin: is the forecaste minimum temperature.
#' Given an array of the information of dw, tempMax and tmin,
#' this function calculates K constant using linear regression.
#' [1] Ortiz Maldonado, Alberto. Adversidades agrometeorológicas de Mendoza. 1991.
#'@param dw [°C] Dew Point in °C
#'@param tempMax [°C] Maximum temperature of the previous day
#'@param tmin [°C] Minimum temperature measure that day.
#'@return an object of class MdzFrostModel
#'@examples
#' # just a random example
#' dw <- c(-2,-5,2,6,8)
#' tempMax <- c(10,20,30,25,29)
#' tmin <- c(-1,-2,3,5,10)
#' buildMdz(dw,tempMax,tmin)


# buildMdz <- function(dw,tempMax,tmin)
# {
#   k = vector(mode = "logical", length = length(dw))
#
#   #TODO check, como calcular constante K, el approach
#   # k <- ((tempMax + dw)/2)-tmin
#   dd <- as.data.frame(cbind(dw,tempMax,tmin))
#   model <- lm(tmin~.,as.data.frame(dd))
#   # TODO check that model is ok or not null before return
#   k = tmin - ((tempMax+dw)/2)
#   m = (tempMax + dw)/2
#   dd2 = data.frame(tmin,dw=(0.5*dw),tempMax=(0.5*tempMax))
#   lm2 = lm(tmin~dw+tempMax, dd2)
#   return(list(model=model,k = mean(k), kmedian = median(k), kvector=k,lm2 = lm2))
#
# }

buildMdz <- function(dw,tempMax,tmin)
{
  k = vector(mode = "logical", length = length(dw))

  if(checkTemp(dw) && checkTemp(tempMax) && checkTemp(tmin)
     && checkLenght(dw,tmin) && checkLenght(tempMax,tmin))
  {
    #TODO check, como calcular constante K, el approach
    # k <- ((tempMax + dw)/2)-tmin
    dd <- as.data.frame(cbind(dw,tempMax,tmin))
    model <- lm(tmin~.,as.data.frame(dd))
    # TODO check that model is ok or not null before return
    k = tmin - ((tempMax+dw)/2)
    model <- new("MdzFrostModel",k = mean(k), kvector=k)

  }
  # return(list(model=model,k = model$coefficients[[1]]))

  return(model=model)

}

#'@title empiric equation for minimum temperature used in Mendoza
#'@description
#' According to Maldonado (see [1]), the empirical equation used in Mendoza to estimate the minimum
#' temperature in the night is:
#'
#' Tmin = ((Tmax + dew)/2)) - K
#'@param dw Dew Point in °C
#'@param tempMax Maximum temperature of the previous day
#'@param model an object of class MdzFrostModel, returned by buildMdz
#'@return predicted minimum temperature
#'@examples
#' # just an example
#' dw <- c(-2,-5,2,6,8)
#' tempMax <- c(10,20,30,25,29)
#' tmin <- c(-1,-2,3,5,10)
#' out <- buildMdz(dw,tempMax,tmin)
#' predMdz(dw = -3, tempMax = 15, out)
#'
predMdz <- function(dw,tempMax,model)
{
  tmin <- NULL

  if(class(model)!="MdzFrostModel"){stop("model should be an object of type MdzFrostModel")}

  if(checkTemp(dw) && checkTemp(tempMax) && checkLenght(dw,tempMax) &&
     !is.null(model) && !is.na(model@k))
  {
    tmin <- ((tempMax + dw)/2) - model@k
  }else{stop("Check the arguments of predMdz, they shouldn't be null, NA or empty")}

  return(tmin)
}


# consultar a darksky la temperatura de dicho sitio + punto de rocio actual
# tambien obtener su pronostico
# compararlo con el pronostico de alguna formula u otro modelo
