#!/usr/bin/Rscript
#clear worksace
rm(list = ls(all = TRUE))

#set working directory, just in case I run it as script from anywhere
# setwd("/home/dharma/anita/phd-repos/sensor-analysis/scripts")

library(xts)
library(timeDate)
library(caret)
library(foreach)
#libraries used by caret in training phase
library("nnet")
#library("kernlab")

source("dataset.R")
source("svm-lib.R")
# args = commandArgs()
# print(args)
# ROOT <- args[4]
# PATH <- args[5]
# PATH.RESULTS.MODELS <-args[6] #paste(ROOT,"sensor-analysis/results-chaar/regressionTrainControl/",sep="")
# PATH.PLOTS <- args[7] #paste(ROOT,"sensor-analysis/results-chaar/plotsTrainControl-t/",sep="")
# PATH.SUMMARY.REGRESSION<- args[8] #paste(ROOT,"sensor-analysis/results-chaar/regressionSummary-trainControl.csv",sep="")
# FUNCTION <- args[9] #"test" #
# CORES <- args[11]
VERBOSE <- TRUE #args[11]

ROOT <- "/home/ana/phd-repos/tmin/tmin"# "/home/anadiedrichs/phd-repos/tmin"
PATH <- paste(ROOT,"/datasets/DACC-estaciones/",sep="")
PATH.RESULTS.MODELS <-paste(ROOT,"/models-estaciones/",sep="")
#PATH.PLOTS <-paste(ROOT,"",sep="/chaar-results/rf-fs/")
PATH.PLOTS <-paste(ROOT,"","/plots-estaciones/",sep="") #plotsModel-2017-04-27/
PATH.SUMMARY.REGRESSION<- paste(ROOT,"/rmse-estaciones-",Sys.time(),".csv",sep="")
CORES<-2
FUNCTION<-"test"

#use both cores from the processor
#OJO! WARNING! error en server UDP. 
#OJO! En laptop, ralentiza el sistema GUI, puede colgarse para CORES >2
library(doMC)
registerDoMC(cores = as.integer(CORES))

#asegurar reproducibilidad de los experimento
set.seed(998)

#### OJO DEPENDIENTE DEL DATASET del dataset a analizar ######################

# #inicializo variables globales
#inicializo variables globales
N.MEJORES.DEL.RF <- 20
RF.GINI.FILES=NULL;MI.FILES=NULL;periodicity=NULL;t_names=NULL;t_period=NULL;DATA=NULL;data.mean=NULL;data.sd=NULL 
#cargar el dataset con el que se desee trabajar
dataset <- load.data.estaciones() #load.data.chaar()

source("utils.R")

#'----
#' TRAIN Y TEST SET DIVISION
train.data <- dataset$train
test.data <- dataset$test

## FIN DEPENDIENTE DEL DATASET

#'----------------------
#' REGRESSION
#' ------------------------------

# function train.models.regression
# el dataset debe contener columna y, la que se desea predecir (regression)
# dataset: X + y
# name: string
# horizon: es el t lag para usar en timeSlices
train.models.regression <- function(dataset,x,y,t=1,name,train.control)
{
  #which models do we want to run?  
  models <- c("svmLinear","svmRadial","lm","nnet") #"svmLinear","svmRadial","lm")   #,"lm","nnet"
  
  nnet.tune.grid <-  as.data.frame(expand.grid(size=seq(3,20,by=1), decay=0.001))
  my.metric <- "RMSE"
  names.models <- list()
  svmLinearTuneGrid <- expand.grid(C=2^(-2:7))
  svmRadialTuneGrid <- expand.grid(C=2^(-2:7), gamma = seq(from=0.5,to=0.1,by=-0.1) ) #change gamma by sigma to run with caret
  
  #TODO omito pre process por bug al pasar center o scale
  # error es Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10,  :   Std. deviations could not be computed for:
  
  #my.preProcess <- c("scale","center")
  #dataset.prep <- preProcess(dataset, method = c("center", "scale"))
  
  #TODO
  #si trainControl es none, no hay que pasar el gridTune, sino Error
  #train.control <- trainControl(method="none")

  
  
  for(m in 1:length(models))
#  foreach(m=1:length(models)) %dopar%
  {  
    model.name <- models[m]
    print(model.name)
    head(dataset)
    model <- NULL
    
    if(model.name=="nnet"){
      
      if(VERBOSE) cat("model nnet training")
      
      #tControl <- trainControl(method="timeslice",initialWindow = 1, horizon = t, fixedWindow = TRUE)
      #tControl <- trainControl(method="none", search = "grid")
      # resampling method used: bagging (boot option by default in caret )
      model<- train(y~.,data=dataset,method="nnet",#preProcess=my.preProcess, 
                    maxit=1000,
                    tuneGrid=nnet.tune.grid, trace=TRUE # ,trControl=tControl  #TODO
                    ,metric=my.metric,linout = 1)
    }
    else if(model.name %in% c("svmRadial","svmLinear")){
      grid <- NULL
      if(model.name %in% c("svmLinear")){
        grid <- svmLinearTuneGrid
        if(VERBOSE) cat("model svmLinear training")
        model <- train.svm(x,y,model = "linear",grid = grid, file = paste(PATH.RESULTS.MODELS,name,model.name,sep=""))
      }else{
        grid <- svmRadialTuneGrid
        if(VERBOSE) cat("model svmRadial training")
        model <- train.svm(x,y,model = "radial",grid = grid, file = paste(PATH.RESULTS.MODELS,name,model.name,sep=""))
        
      }
      # 
      # model <- train.svm(x,y,model = "radial",grid = NULL, file = NULL)
      # 
      # model<- train(y~.,data=dataset,method=model.name,# preProcess=my.preProcess,
      #              metric=my.metric,
      #              trControl=train.control,
      #              tuneGrid=grid)

    }else{
        #next
        if(VERBOSE) cat("model lm (linear model) training")
        model<- train(y~.,data=dataset,method=model.name,
                      #preProcess=my.preProcess,
                      metric=my.metric
                      ,trace=TRUE,    #TODO
                      trControl=train.control
                      )
    }
    
    if(VERBOSE) print(model)  
    if(!is.null(model))
    {
      whereToSave <- paste(PATH.RESULTS.MODELS,paste(name,model.name,sep="--"),"--",Sys.time(),".RData",sep="")
      save(model,file=whereToSave)
      #solo para chusmear resultados via print 
      summary(model)
    }
    
  }
}

#' Experimentos iniciales para cumplir la consigna de Facundo
#' El objetivo es primero entrenar con todas las variables de entrada (input 127...)
#' Con las mas importantes segun random forest 
#' Con las mas importantes segun random forest + Sit
#' Con las mas informativas segun MI + Sit
#' 
train.main <- function(){
  
  #avance <- 0
  #total <- 9 * length(t_period) * length(sensors)

  # sensors <<- c("S10.min") # <- para correr solo S10
  
  for(tt in 1:length(t_period))
  #foreach(tt =1:length(t_period)) %dopar% 
  {
    
    if(VERBOSE) cat("tt slice number ",t_period[tt],"\n")
    
  #initialWindows: 24 hs of data
  #horizon= t hs to predict of hour interest
  # timeSlices <- createTimeSlices(1:nrow(DATA),initialWindow = 24*periodicity, horizon = horizon_t, fixedWindow = TRUE)
  #initialWindow: cuantas filas anteriores tomar
  #horizon: a cuantas filas leugo quiero predecir, desfazar el dataset 
  #train.control <- trainControl(method="timeslice",initialWindow = 1, horizon = t_period[t], fixedWindow = TRUE)
   train.control <- trainControl(method="none") 
   
   for(s in 1:length(sensors))
   #foreach(s =1:length(sensors)) %dopar%
    {
      print(sensors[s])
      #USAR todos los S presentes y Si+T como salida
      l <- nrow(train.data )
      
      # por defecto inicializamos el input y output
      x1 <- train.data
      y <- train.data[,sensors[s]] #nombre sensor a utilizar como Y
      
      #siguientes dos lineas en caso de train.control$method es none
      if(train.control$method=="none")
      {
        x1 <- train.data[1:(l-t_period[tt]),]
        y <- train.data[(t_period[tt]+1):(l),sensors[s]] #nombre sensor a utilizar como Y 
      }
      y <- as.data.frame(y,col.names=c("y"))
      
      #pasamos todas las variables x para entrenar
      #train.models.regression(dataset = d,name=paste(sensors[s],"all",t_period[t],sep = "-"), train.control)
      #los mejores N vecinos segun random forest
      #
      mejores <- rf.most.important(N.MEJORES.DEL.RF,RF.GINI.FILES,paste(sensors[s],tt,sep="-"))
      mejores.mi <- get.most.informative(sensor.name=sensors[s],time=tt,N.MEJORES.DEL.RF)
      #si <- unlist(strsplit(sensors[s],"[.]"))[1] # extraigo nombre del sensor, por ej de S15.min queda S15
      
      ##solo info del mismo sensor si (su minima maxima y media) OJO DEPENDIENTE DATASET
      #vars.del.sensor <- colnames(x1[,grepl(si,names(train.data))])
      vars.del.sensor <- vars.del.sensor(sensors[s])
      # if(VERBOSE) print("MEJORES")
      for(v in 1:9)
      #foreach(v = 1:10) %dopar%
      {
        #entrenamos RF config
        u <- union(vars.del.sensor,mejores$X[1:v])
        xr <- x1[,which(colnames(train.data) %in% u)]
        d <- data.frame(xr,y)
        train.models.regression(dataset = d,x=xr,y=y,t_period[tt],name=paste(sensors[s],t_period[tt],"RF",v,sep = "-"),train.control)

        # entrenamos MI config
        u <- union(vars.del.sensor,mejores.mi[1:v])
        x.mi <- x1[,which(colnames(train.data) %in% u)]
        #y <- train.data[,sensors[s]] #nombre sensor a utilizar como Y
        #y <- as.data.frame(y,col.names=c("y"))
        #pasamos todas las variables x para entrenar
        d <- data.frame(x.mi,y)
        #pasamos todas las variables x para entrenar
        train.models.regression(dataset = d,x=x.mi,y=y,t_period[tt],name=paste(sensors[s],t_period[tt],"MI",v,sep = "-"),train.control)

      }
      
      ### solo info del mismo sensor si (su minima maxima y media) OJO DEPENDIENTE DATASET
      x <- x1[,vars.del.sensor]
      d <- data.frame(x,y)
      train.models.regression(dataset = d,x,y,t_period[tt],name=paste(sensors[s],t_period[tt],"solo",sep = "-"),train.control)
      ### solo con la info de su temperatura minima anterior
      x <- x1[,sensors[s]]
      d <- data.frame(x,y)
      train.models.regression(dataset = d,x,y,t_period[tt],name=paste(sensors[s],t_period[tt],"minsolo",sep = "-"),train.control)


    }
  }
}

#' Use predict method to plot ŷ-y
#' and then the both functions F(t,ŷ) F(t,y)
#' lag: cuantas filas desfazar el dataset
#' y.name string de la forma "S20"
#' config metodo de feature selection u otra caracteristica dle experimento
validate.model<- function(model=NULL, name=NULL, data=NULL, lag=2,y.name,config)
{ 
  if(is.null(data)|| is.null(model)||is.null(name)) stop("Debe pasar todos los parámetros a la funcion validate.model")
  
  set.seed(998)
  t <- lag 
  #   #USAR todos los S presentes y Si+T como salida
  l <- nrow(data )
  x <- as.data.frame(data[1:(l-t),-which(names(data)%in% c("y"))])
  y <- data[(t+1):(l),c("y")] #nombre sensor a utilizar como Y
  if(ncol(x)==1){ 
    if(length(grep(pattern = "MI-1",name,value = TRUE))==1) 
      colnames(x) <- "x.mi"
    else if(length(grep(pattern = "RF-1",name,value = TRUE))==1) 
      colnames(x) <- "xr"
    else
      colnames(x) <- "x"
  }
  limit <- l
  index.date <- index(x)
  y.column <- y.name
  
  pred <- predict(model,newdata=x,type="raw")
  
  ###########################
  #   diff <- (pred - y)^2 
  #   pdf(file = paste(PATH.PLOTS,name,"-diff-Rplot.pdf",sep = ""))
  #   p <- plot(diff)
  #   print(p)
  #   dev.off()
  ####################
  y.temp <- convert.fn(y,y.name)
  pred.temp <- convert.fn(pred,y.name)
  cc <- cummulative.error(y.temp,pred.temp)

  ########## save .csv file with y and y_pred
  dd <- as.data.frame(cbind(y.temp,pred.temp))
  colnames(dd)<- c("y_real","y_pred")
  write.csv(x = dd,file = paste(PATH.PLOTS,name,"-Y-vs-Y_pred.csv",sep = ""))
  ### plot previous file
  png(file = paste(PATH.PLOTS,name,"-pred-vs-real.png",sep = ""))
  p <- plot(y.temp,pred.temp)
  print(p)
  dev.off()
  
  # PLOT EN GPLOT!! #### 
#      df <- data.frame(x=index.date[(t):(l-1)], val = as.numeric(y),fun="real")
#      df2  <- data.frame(x=index.date[(t):(l-1)], val = as.numeric(pred),fun="pred")
    # df <- data.frame(x=index.date, val = as.numeric(convert.fn(y,y.name)),fun="real")
    # df2  <- data.frame(x=index.date, val = as.numeric(convert.fn(pred,y.name)),fun="pred")
     df <- data.frame(x=index.date, val = as.numeric(y.temp),fun="real")
     
     df2  <- data.frame(x=index.date, val = as.numeric(pred.temp),fun="pred")
     
     x <- rbind(df,df2)
     png(file = paste(PATH.PLOTS,name,"-curvesT-Rplot.png",sep = ""))
     p <- ggplot(x, aes(x=x, y=val, group=fun)) + geom_line(aes(color=fun))+
       xlab("") +
       theme(text = element_text(size=12),
             axis.text.x = element_text(angle=90, vjust=1),
             legend.position="bottom")
     print(p)
     dev.off()
  
  ##########variable importance 
  # The function automatically scales the importance scores 
  # to be between 0 and 100. Using scale = FALSE avoids this normalization step. 
  # For regression, the relationship between each predictor and the outcome is evaluated. 
  # An argument, nonpara, is used to pick the model fitting technique. When nonpara = FALSE, 
  # a linear model is fit and the absolute value of the t-value for the slope of the predictor 
  # is used. Otherwise, a loess smoother is fit between the outcome and the predictor. 
  # The R2 statistic is calculated for this model against the intercept only null model. 
  # This number is returned as a relative measure of variable importance.
  
  # pdf(file = paste(PATH.PLOTS,name,"-varImp-Rplot.pdf",sep = ""))
  # p <- plot(varImp(model, scale=TRUE))
  # print(p)
  # dev.off()
  ##########regresa por ejemplo solo dos valores
  ## RMSE  Rsquared 
  ### 0.7059448 0.3658964 
  return(list(ps = postResample(pred.temp,y.temp), obs =y, pred = pred, cum = cc))
}

#' TESTING THE MODELS CHOSEN IN TRAINING PHASE WITH REAL DATA
#' 1°) load testing/validation data
#' 2°) load model type train from *.RData file 
#' save results (rmse) in variable
#' show all models results 
testing <- function()
{
  
  list.of.models <- list()
  names.models <- character(0)
  #/results-chaar/regression$ ls -l | wc -l
  r.columns <- c("model","sensor","t","config","rmse","r2","vecino")
  max.models <- length(load.files(base=PATH.RESULTS.MODELS,pat="*.RData"))
  i.model <- 1
  #guarda archivo resumen de resultado final, rmse por cada modelo
  results <- data.frame(matrix(0,nrow = max.models,ncol=length(r.columns),dimnames = list(seq(1:max.models),r.columns)))
  
  
  for(j in 1:length(sensors))#por cada sensor
  {
    #nombre del directorio donde está .Rdata models
    sensor.name <- sensors[j]
    if(sensor.name == "agua_amarga.Tmin") next;
    
    r.data.models <- load.files(base=PATH.RESULTS.MODELS,pat=glob2rx(paste(sensors[j],"*",".RData",sep="")))
    res.rows.names <- c()
    #guardo error acumulativo
    result.error.cum <- data.frame(matrix(0,nrow = 6,ncol=length(r.data.models),dimnames = list(seq(1:6),seq(1:length(r.data.models)))))
    vars.del.sensor <- vars.del.sensor(sensors[j])
    
    for(i in 1:length(r.data.models))#por cada modelo de dicho sensor
    {
      file <- r.data.models[[i]] #cargo path completo archivo
      load(file); print(file) # cargo a memoria modelo .RData
      if(is.null(model)) stop("LOS DATOS NO SE CARGAN EN VARIABLE model")
      s <- strsplit(basename(file),"-")
      #WARNING!! si el nombre de archivo no tiene el formato correcto lo siguiente fallara!!!
      method <-s[[1]][3]
      #EXTRAER NOMBRE sensor
      sensor.name <- s[[1]][1]
      t <- as.integer(s[[1]][2]) #
      l <- nrow(test.data)
      vv <- NULL
      
      if(method=="RF") #random forest
      {
        vv <- as.integer(s[[1]][4])
        mejores <- rf.most.important(vv,RF.GINI.FILES,paste(sensor.name,t,sep="-"))
        u <- union(vars.del.sensor,mejores$X[1:vv])
        #ARMAR dataset validacion
        x <- test.data[,which(colnames(test.data) %in% u)]
        alg <- s[[1]][6] 
        n <- paste(sensor.name,method,vv,t,alg,sep="-")
        
      }else if(method=="MI") #mutual information
      {
        vv <- as.integer(s[[1]][4])
        mejores <- get.most.informative(sensor.name=sensor.name,time=t,vv)
        u <- union(vars.del.sensor,mejores)
        #ARMAR dataset validacion
        x <- test.data[,which(colnames(test.data) %in% u)]
        alg <- s[[1]][6] 
        n <- paste(sensor.name,method,vv,t,alg,sep="-")
        
      }else if(method=="solo") #ok testeado
      {
        #CONFIGURACION solo son los vecinos Si (su minima, maxima y media nomas)
        #si <- unlist(strsplit(sensor.name,"[.]")) # extraigo nombre del sensor, por ej de S15.min queda S15
        x <- test.data[,vars.del.sensor]
        alg <- s[[1]][5] 
        n <- paste(sensor.name,method,t,alg,sep="-")
        
      }else if(method=="minsolo") #TESTEAR Y CHEQUEAR, no funciona, error al validate
      {
        #CONFIGURACION solo con info S??.min pasada
        x <- as.data.frame(test.data[,sensor.name])
        colnames(x) <- sensor.name
        alg <- s[[1]][5] 
        n <- paste(sensor.name,method,t,alg,sep="-")
        
      }else 
      {
        x <- test.data
        #TODO chequear para otras configs
        alg <- s[[1]][5] 
        n <- paste(sensor.name,method,t,alg,sep="-")
        
      }
      
      y <- test.data[,sensor.name] #nombre sensor a utilizar como Y
      y <- as.data.frame(y,col.names=c("y"))
      val.data <- data.frame(x,y)
      
      #EXTRAER N periodos de t tiempo DESPUES
      names.models <- append(names.models,n)
      # results[i.model,"model"]<-alg
      results[i.model,"sensor"]<-sensor.name
      results[i.model,"t"]<-t
      results[i.model,"config"]<-method
      if(!is.null(vv)) results[i.model,"nroVar"]<-vv
      if(alg == "svmRadial" && method=="RF" && t == 1 && vv == 6 && sensor.name == "S8.min")
      {
        #S8.min-1-RF-6--svmRadial
        print("quiero breakpoint aqui :-)")
      }
      #################
      #PLOT MODEL
      print(summary(model))
      if(!is.null(model$results))
      {
        write.csv(model$results,file= paste(PATH.PLOTS,n,"-modelResults.csv",sep = ""))
        print(model$results) 
      }
      # if(has.tune.parameters(model))
      # {
      #   pdf(file = paste(PATH.PLOTS,n,"-train-Rplot.pdf",sep = ""))
      #   p <- plot.train(model)
      #   print(p)
      #   dev.off()
      # }
      ##############
      v <- validate.model(model=model, name=n,val.data,t,sensor.name,method)
      results[i.model,"rmse"]<-v$ps[[1]]
      results[i.model,"r2"]<-v$ps[[2]]
      res.rows.names <- c(res.rows.names,n)
      result.error.cum[,i] <- v$cum$error
      ###################
      # call other plo ts
      i.model <- i.model+1
    }
    # plot cumulative error
    colnames(result.error.cum) <- res.rows.names
    # write csv cummulative error
    write.csv(result.error.cum,file = paste(PATH.PLOTS,sensor.name,"-cumm-error.csv",sep = ""))
  }
  
  write.csv(results,file=PATH.SUMMARY.REGRESSION)
  
  #al finalizar realizar un resample de todos los modelos
  #names.models <- apply(expand.grid(days.slices,"nnet",s.data.name),1,function(x) paste(x,collapse="-"))
    # r <- resamples(list.of.models,modelNames=names.models)
    # print(summary(r))
    # pdf(file = paste(PATH.PLOTS,"BWPLOT.pdf",sep = ""))
    # p<- bwplot(r, metric = "RMSE")
    # print(p)
    # dev.off()
    # 
    # pdf(file = paste(PATH.PLOTS,"DOTPLOT.pdf",sep = ""))
    # p <- dotplot(r, scales =list(x = list(relation = "free")),between = list(x = 2))
    # print(p)
    # dev.off()
}


if(FUNCTION=='train') train.main() 
if(FUNCTION=='test') testing()

