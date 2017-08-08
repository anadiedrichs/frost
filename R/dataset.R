
library(readr)


#-----------------
#ESCALAR LOS DATOS, Z NORMALIZATION
#--------------------
#escalar los datos entre -1 y 1, o Z normalization
my.scale.fn <- function(col){ 
  #es lo mismo que correr o llamar al a funcion scale(col, scale=TRUE, center = TRUE)
  return( (col - mean(col))/sd(col))
}

#' es la inversa de my.scale.fn, para regresar de dato normalizado a datos en grados o temp, 
#' es decir, dataset original
#' ejemplo: para tener en grados la primer columna 
#' CC <- convert.fn(scaled.dataset[,1],colnames(scaled.dataset)[1])
#' 
convert.fn <- function(c,col.name)
{
  return((c * data.sd[col.name]) + data.mean[col.name]) 
}
# FIN ESCALAR DATOS
#' Funcion que carga dataset de Javier Chaar, 20 sensores ibutton de temperatura, mas la info
#' en el mismo tiempo de la estacion meteorologica de INTA EEA Junin
load.data.chaar <- function()
{
  RF.GINI.FILES <<- paste(ROOT,"/datasets/junin-chaar/rf-fs/",sep="")
  MI.FILES <<- paste(ROOT,"/datasets/junin-chaar/mutual-info/",sep="")
  
  #periodicity <<- 1 # datos diarios 
  t_names <<- c("1","2")
  t_period <<- c(1,2) # uno, dos dias, numero de filas por las que debo desplazar el dataset
  #t_period <- c((periodicity*12)) # correr solo 12 hs con svm*
  DATA <<- read.csv(paste(PATH,"sensores.csv",sep="")) #<- no usado a nivel global
  minimas <<- read.csv(paste(PATH,"minimo.csv",sep="")) #<- no usado a nivel global
  sensors <<- colnames(minimas)[-1] # quito primer columna de time o tiempo o timestamp, MUY USADO
  #columnas estacion meteorologica junin 
  colEstacion <- c("Est.humedad_min","Est.humedad_med","Est.humedad_max","Est.temp_min","Est.temp_max","Est.temp_med")   
  sensors <<- c(sensors,"Est.temp_min")
  # IMPORTANTE para solo entrenar/usar un sensor, x tiempos computo
  #sensors <<- c("Est.temp_min")
  sensors <<- c("S8.min","S9.min","Est.temp_min") # "S5.min","S6.min","S7.min",
  data.mean <<- sapply(DATA[-1],mean)
  data.sd <<- sapply(DATA[-1],sd)
  scaled.dataset <- sapply(DATA[-1],my.scale.fn) # quito primer columna de time o tiempo o timestamp, MUY USADO
  train.data <- as.data.frame(scaled.dataset[1:323,])
  test.data <- as.data.frame(scaled.dataset[324:465,])
  return(list(data=DATA,train=train.data,test=test.data))
  
}
# tiene que estar train.data cargado!!!, no nulo
# las columnas del dataset deben ser nombradas bajo la convención:
# nombreSensorOEstacion.nombreVariable
vars.del.sensor <- function(sensor.name)
{
  si <- unlist(strsplit(sensor.name,"[.]"))[1] # extraigo nombre del sensor, por ej de S15.min queda S15
  ##solo info del mismo sensor si (su minima maxima y media) OJO DEPENDIENTE DATASET
  vars <- colnames(train.data[,grepl(si,names(train.data))])
  return(vars)
}
#' dataset estaciones-dacc-diarios.csv 
#' 
load.data.estaciones <- function()
{
  RF.GINI.FILES <<- paste(ROOT,"/datasets/DACC-estaciones/rf-fs/",sep="")
  MI.FILES <<- paste(ROOT,"/datasets/DACC-estaciones/mutual-info/",sep="")
  
  #periodicity <<- 1 # datos diarios 
  t_names <<- c("2") #"1","2")
  t_period <<- c(2) #1,2) # uno, dos dias, numero de filas por las que debo desplazar el dataset
  #t_period <- c((periodicity*12)) # correr solo 12 hs con svm*
  #~/phd-repos/tmin/tmin/datasets/DACC-estaciones
  DATA <- read.csv(paste(PATH,"estaciones-dacc-diarios.csv",sep=""))
  # arranco desde el índice 3, para descartar columna X y date
  nombres <- colnames(DATA[,3:ncol(DATA)])
  #variables que deseo que se predigan, el "Y" en cada uno de los modelos
  #"las_paredes.Tmin" "la_llave.Tmin"    "tunuyan.Tmin"     "junin.Tmin"       "agua_amarga.Tmin"
  #[6] "el_marcado.Tmin" 
  #sensors <<- nombres[grepl("Tmin",nombres)]
  #sensors <<- c("junin.Tmin")
  sensors <<- c("agua_amarga.Tmin","el_marcado.Tmin","junin.Tmin","la_llave.Tmin","tunuyan.Tmin") #para una estacion especifica
  data.mean <<- sapply(DATA[,3:ncol(DATA)],mean)
  data.sd <<- sapply(DATA[,3:ncol(DATA)],sd)
  scaled.dataset <- sapply(DATA[,3:ncol(DATA)],my.scale.fn) # quito primer columna de time o tiempo o timestamp, MUY USADO
  train.data <- as.data.frame(scaled.dataset[1:1152,]) # 70%
  test.data <- as.data.frame(scaled.dataset[1153:1645,]) # el 30% restante
  
  return(list(data=DATA[3:ncol(DATA)],train=train.data,test=test.data,predvars = sensors))
  
}

load.data.ur.steinfeld <- function()
{
  
  
}
