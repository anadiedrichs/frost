#' @title  Temperature conversion
#'
#' @description
#' Temperature conversion from/to Fahrenheit (°F), degress Celsius (°C) and Kelvin (K)
#'
#' @param from possible values, "F" for Fahrenheit, "C" degress Celsius and "K" Kelvin.
#' @param to possible values, "F" for Fahrenheit, "C" degress Celsius and "K" Kelvin.
#' @param values can be an vector, array, or a numeric single value.
#' @return value (double)
#' @export
#' @examples
#' library(frost)
#' convert.temperature(from="K", to="C",350)
#' cels <- convert.temperature(from="F",to="C",c(120,80,134,110))
#' k <- convert.temperature(from="C", to="K",cels)
#'
convert.temperature <- function(from="F",to="C",values)
{
  units <- c("F","C","K")

  if(is.null(values) || !checkNoNA(values) || !is.numeric(values)){
    stop("Values must be numeric and not NA")
    }
  if(!(from %in% units) || !(to %in% units) ){
    stop("The valid values of from and to argument are only \"F\", \"C\" or \"K\" ")
  }

  if(from=="F" & to=="C") return(5/9*(values-32))
  if(from=="C" & to=="F") return(9/5*values+32)
  if(from=="K" & to=="F") return((9/5 * values)-459.67)
  if(from=="F" & to=="K") return(5/9*(values+459.67))
  if(from=="K" & to=="C") return(values-273.15) # from Kelvin to Celsius
  if(from=="C" & to=="K") return(values+273.15) # from Celsius to Kelvin

}
