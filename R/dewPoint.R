###############################################################################33
#' @title  Calculates dew point from ambient temperature and relative humidity.
#'
#' @description
#' The following formula is used for dew point estimation:
#' $$ (RH/100 )^(1/8) * (110+temp) -110) $$
#' ,where RH is relative humidity and temp is ambient temperature.
#' The formula was taken from this wikipedia page:
#' <https://es.wikipedia.org/wiki/Punto_de_roc%C3%ADo>
#'
#' @param temp [°C] environmental temperature, an integer or double value between -20 and 60 °C
#' @param RH [in percentage] relative humidity, an integer or double value between 0 and 100.
#' @return dew point value (double)
#' @export
#' @examples
#' library(frost)
#' temp <- 25
#' rh <- 54
#' calcDewPoint(rh,temp,mode="A")
#'
calcDewPoint.A <- function(RH,temp) {return((RH/100 )^(1/8) * (110+temp) -110)}

#' @title Calculates dew point from ambient temperature and relative humidity.
#' @description
#' Calculation of dew point using the Mark G. Lawrence approach given in the following paper:
#' * "The Relationship between Relative Humidity and the Dewpoint Temperature in Moist Air:
#' A Simple Conversion and Applications", DOI: <https://doi.org/10.1175/BAMS-86-2-225>,
#' URL: <https://journals.ametsoc.org/doi/pdf/10.1175/BAMS-86-2-225>
#' @param temp [°C] environmental temperature, an integer or double value between -20 and 60 °C
#' @param RH [in percentage] relative humidity, an integer or double value between 0 and 100.
#' @return dew point value (double)
#' @export
#' @examples
#' library(frost)
#' temp <- 25
#' rh <- 54
#' calcDewPoint(rh,temp,mode="B")
#'
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
#' @examples
#' library(frost)
#' temp <- 25
#' rh <- 54
#' calcDewPoint(rh,temp,mode="C")
#'
calcDewPoint.C <- function(RH,temp){
  return (243.04*(log(RH/100)+((17.625*temp)/(243.04+temp)))/(17.625-log(RH/100)-((17.625*temp)/(243.04+temp))))
}
#' @title Dew point estimation given relative humidity and temperature
#' @description
#' This function is a wrapper to access to one of the dew point calculation methods
#' offered in this package.
#' Read more about the method in calcDewPoint.A, calcDewPoint.B,calcDewPoint.C functions.
#' @param temp [°C] an integer or double value between -20 and 60 °C.
#' @param RH [in percentage] an integer or double value between 0 and 100.
#' @param mode string values "A", "B" or "C". Default "A".
#' * Mode "A" : calls calcDewPoint.A function
#' * Mode "B" : calls calcDewPoint.B function
#' * Mode "C":  calls calcDewPoint.C function
#' @return dew point value (double)
#' @export
#' @examples
#' temp <- 25
#' rh <- 54
#' calcDewPoint(rh,temp) # it takes mode = "A" by default
#' calcDewPoint(rh,temp,mode="B")
calcDewPoint <- function(RH,temp,mode = "A")
{
  if(checkRH(RH) && checkTemp(temp) && mode %in% c("A","B","C"))
  {

    if(mode =="A") return(calcDewPoint.A(RH,temp))
    else if(mode == "B")return(calcDewPoint.B(RH,temp))
    else if(mode =="C") return(calcDewPoint.C(RH,temp))

  }else return(NULL)
}

######################################################################################
