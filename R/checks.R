# functions to validate inputs values

checkRH <- function(x){
  if((is.integer(x) || is.double(x)) && any(x <= 100) && any(x >0)) return(TRUE)
  else{ stop("Relative humidity values must double and range between 1 and 100")}
}

checkTemp <- function(x){

  if((is.integer(x) || is.double(x)) && any(x <= 60) && any(x > -20)) return(TRUE)
  else{
    stop("Temperature and dew point values must be doubles and range from -20 to 60")
  }
}

checkLenght <- function(a,b)
{
  if(length(a)!= length(b))stop("Array arguments must have the same length")
  return(TRUE)
}
checkNoNA <- function(anArray)
{
  if(!is.null(anArray) && length(which(complete.cases(anArray)==FALSE)) < 1)
    return(TRUE)
  else
    stop("NAs or NULL values could be present in the array")

}

