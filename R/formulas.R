# functions to validate inputs values

checkRH <- function(x){
  if((is.integer(x) || is.double(x)) && x <= 100 && x >0) return(TRUE)
  else{ stop("Relative humidity values must double and range between 1 and 100")}
}

checkTemp <- function(x){

  if((is.integer(x) || is.double(x)) && x <= 60 && x >-20) return(TRUE)
  else{
    stop("Temperature and dew point values must be doubles and range from -20 to 60")
  }
}

checkLenght <- function(a,b)
{
  if(length(a)!= length(b))stop("Array arguments must have the same length")
  return(TRUE)
}


###############################################################################33
#' @title DEW POINT estimation given relative humidity and temperature
#' @description
#' Mode "A" : calls calcDewPoint.A function
#' Mode "B" : calls calcDewPoint.B function
#' Mode "C":  calls calcDewPoint.C function
#'
#' @param temp [°C] an integer or double value between -20 and 60 °C.
#' @param RH [in percentage] an integer or double value between 0 and 100.
#' @param mode string values "A", "B" or "C". Default "A".
#' @return dew point value (double)
#' @export
#' @examples

calcDewPoint <- function(RH,temp,mode = "A")
{
  if(checkRH(RH) && checkTemp(temp) && mode %in% c("A","B","C"))
  {

    if(mode =="A") return(calcDewPoint.A(RH,temp))
    else if(mode == "B")return(calcDewPoint.B(RH,temp))
    else if(mode =="C") return(calcDewPoint.C(RH,temp))

  }else return(NULL)
}

#' @title  Calculates dew point from ambient temperature and relative humidity.
#'
#' @description
#' The following formula is used for dew point estimation:
#' $$ (RH/100 )^(1/8) * (110+temp) -110) $$
#' ,where RH is relative humidity and temp is ambient temperature.
#' The formula was taken from this wikipedia page:
#' https://es.wikipedia.org/wiki/Punto_de_roc%C3%ADo
#'
#' @param temp [°C] environmental temperature, an integer or double value between -20 and 60 °C
#' @param RH [in percentage] relative humidity, an integer or double value between 0 and 100.
#' @return dew point value (double)
#' @export
#' @example
#' temp <- 25
#' rh <- 54
#' calcDewPoint.A(rh,temp)
calcDewPoint.A <- function(RH,temp) {return((RH/100 )^(1/8) * (110+temp) -110)}

#' @title Calculates dew point from ambient temperature and relative humidity.
#' @description
#' Calculation of dew point using the Mark G. Lawrence approach given in the following paper:
#' * "The Relationship between Relative Humidity and the Dewpoint Temperature in Moist Air:
#' A Simple Conversion and Applications", DOI: https://doi.org/10.1175/BAMS-86-2-225 ,
#' URL: https://journals.ametsoc.org/doi/pdf/10.1175/BAMS-86-2-225
#' @param temp [°C] environmental temperature, an integer or double value between -20 and 60 °C
#' @param RH [in percentage] relative humidity, an integer or double value between 0 and 100.
#' @return dew point value (double)
#' @export
#' @example
#' temp <- 25
#' rh <- 54
#' calcDewPoint.B(rh,temp)
calcDewPoint.B <- function(RH,temp) {
  dw <- 0
  if(RH < 50){ dw <- (0.198 + 0.0017*temp) * RH + (0.84*temp) - 19.2}
  else{ dw <- temp - ( ((100-RH)/5) * (temp/300)^2 ) - (0.00135 * (RH - 84)^2 )+ 0.35}
  return(dw)
}
#' @title Calculates dew point from ambient temperature and relative humidity.
#' @description
#' Calculation of dew point using the approach given in the following paper:
#' Alduchov and Eskridge (1996),
#' Improved Magnus' form approximation of saturation vapor pressure. J. Appl. Meteor., 35, 601–609.
#' @param temp [°C] environmental temperature, an integer or double value between -20 and 60 °C
#' @param RH [in percentage] relative humidity, an integer or double value between 0 and 100.
#' @return dew point value (double)
#' @export
#' @example
#' temp <- 25
#' rh <- 54
#' calcDewPoint.C(rh,temp)
calcDewPoint.C <- function(RH,temp){
  return (243.04*(log(RH/100)+((17.625*temp)/(243.04+temp)))/(17.625-log(RH/100)-((17.625*temp)/(243.04+temp))))
}
######################################################################################

