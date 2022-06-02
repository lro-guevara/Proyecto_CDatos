# Exploración de datos
install.packages("qdap")
library(qdap)
library(ggplot2)

# Leemos el archivo que contiene los datos
disease <- read.csv("wcgs.csv")

#Observamos los primeros registros
head(disease, n=5)
# Observamos dimensiones de nuestro documento
dim(disease)

# Se observa un total de 3154 registros con 22 características

# Generamos dos conjuntos de datos dependiendo del arco 
arcus.yes<-disease[disease$arcus=="1",]
arcus.no<-disease[disease$arcus=="0",]

# Eliminamos los valores nulos
arcus.yes<-na.omit(arcus.yes)
arcus.no<-na.omit(arcus.no)

# Dimensiones
dim(arcus.no)
dim(arcus.yes)

# MMM

medidas <- function(medida){
  
  media <- mean(medida)
  moda <- as.numeric(names(which.max(table(medida))))
  mediana <- median(medida)
  
  MMM<-c(Media=media, Moda=moda, Mediana=mediana)
  
  dt_MMM <-data.frame(MMM)
  
  return(dt_MMM)
}


#Colesterol
colesterol <- data.frame(medidas(arcus.yes$chol), medidas(arcus.no$chol))
colnames(colesterol) <- c("Arco", "No arco")
colesterol


# No. cigarros 

nocig <- data.frame(medidas(arcus.yes$ncigs),
                    medidas(arcus.no$ncigs))
colnames(nocig) <- c("Arco", "No arco")
nocig

cigarros <- data.frame(NoCigarros_Arco=mean(arcus.yes$ncigs),
                       NoCigarros_noarco=mean(arcus.no$ncigs))
cigarros


table(arcus.no$smoke)
table(arcus.yes$smoke)




#### Cuartiles

# Colesterol
par(mfrow=c(2, 2))
plot(quantile(arcus.no$chol, prob=seq(0, 1, 1/100)),xlab = "Porcentaje" ,
     ylab = "Colesterol",main="No Arco")
plot(quantile(arcus.yes$chol, prob=seq(0, 1, 1/100)),xlab = "Porcentaje" ,
     ylab = "Colesterol",main="Arco")
boxplot(arcus.yes$chol, main="Arco")
boxplot(arcus.no$chol, main="No Arco")


# Ncigs
par(mfrow=c(2, 2))
plot(quantile(arcus.no$ncigs, prob=seq(0, 1, 1/100)),xlab = "Porcentaje" ,
     ylab = "No cigs",main="No Arco")
plot(quantile(arcus.yes$ncigs, prob=seq(0, 1, 1/100)),xlab = "Porcentaje" ,
     ylab = "No cigs",main="Arco")
boxplot(arcus.yes$ncigs, main="No. Cigs Arco")
boxplot(arcus.no$ncigs, main="No. Cigs No Arco")

#fumadores
arcus.yes_smokers<-arcus.yes[arcus.yes$smoke=="Yes",]
arcus.no_smokers<-arcus.no[arcus.no$smoke=="Yes",]

par(mfrow=c(2, 2))
plot(quantile(arcus.no_smokers$ncigs, prob=seq(0, 1, 1/100)),
     xlab = "Porcentaje" ,ylab = "No. cigs",main="No Arco")
plot(quantile(arcus.yes_smokers$ncigs, prob=seq(0, 1, 1/100)),
     xlab = "Porcentaje" ,ylab = "No. cigs",main="Arco")
boxplot(arcus.yes_smokers$ncigs, main="Arco")
boxplot(arcus.no_smokers$ncigs, main="No Arco")

## Comparación
summary_arcus.col<-summary(arcus.yes$chol)
summary_noarcus.col<-summary(arcus.no$chol)
summary_arcus.ncigs<-summary(arcus.yes$ncigs)
summary_noarcus.ncigs<-summary(arcus.no$ncigs)
summary_d<-data.frame(c(summary_arcus.ncigs),c(summary_noarcus.ncigs),
                      c(summary_arcus.col),c(summary_noarcus.col))
colnames(summary_d) <- c("Arco cigarros", "No arco cigarros", "Arco colesterol",
                         "No Arco colesterol")
summary_d


## Correlación

DF_arco <- data.frame(colesterol = arcus.yes$chol, cigarros=arcus.yes$ncigs)
DF_noarco <- data.frame(colesterol = arcus.no$chol, cigarros=arcus.no$ncigs)

#Calculamos correlación

par(mfrow=c(2,2))
cor(DF_arco)
pairs(DF_arco, main="Arco")

cor(DF_noarco)
pairs(DF_noarco,main="No Arco")

## Var SD

##Colesterol
varianza<-var(arcus.yes$chol)
desEst<-sd(arcus.yes$chol)
var_sd_yes<-c(var=varianza,des=desEst)

varianza<-var(arcus.no$chol)
desEst<-sd(arcus.no$chol)
var_sd_no<-c(var=varianza,des=desEst)

var_sd<-data.frame(var_sd_yes,var_sd_no)
var_sd

## No. cigarros
varianza<-var(arcus.yes$ncigs)
desEst<-sd(arcus.yes$ncigs)
var_sd_yes<-c(var=varianza,des=desEst)

varianza<-var(arcus.no$ncigs)
desEst<-sd(arcus.no$ncigs)
var_sd_no<-c(var=varianza,des=desEst)

var_sd<-data.frame(var_sd_yes,var_sd_no)
var_sd


## Distribución de datos

# Colesterol Arco
MediaCol<-summary_d$`Arco colesterol`[4]
SdCol<-var_sd$var_sd_yes[2]
#Calcular los cuartiles
QuarNorm<-qnorm(p=seq(0, 1, 1/4),mean = MediaCol,sd = SdCol)
#Calcula la probabilidad de variables aleatorias en arco
Quar_DD<-quantile(arcus.yes$chol, prob=seq(0, 1, 1/4))
ProbNorm_datos<-pnorm(q=Quar_DD,mean = MediaCol,sd = SdCol)

#pnorm al ingresar quantiles generados con los mismos parametros como resultado el valor p de qnorm
ProbNorm_normal<-pnorm(q=QuarNorm,mean = MediaCol,sd = SdCol)
#comparemos con nuestros datos

dt_norm_Vs_Datos<-data.frame(Cuartil_Arco=Quar_DD,Cuartil_Normal=QuarNorm,
                             Prob_Arco=ProbNorm_datos,Prob_Normal=ProbNorm_normal)
dt_norm_Vs_Datos

#Colesterol No Arco

MediaCol<-summary_d$`No Arco colesterol`[4]
SdCol<-var_sd$var_sd_no[2]

# No Arco
Quar_DDNA<-quantile(arcus.no$chol, prob=seq(0, 1, 1/4))
ProbNorm_datosNA<-pnorm(q=Quar_DDNA,mean = MediaCol,sd = SdCol)

#Calcular los cuartiles
QuarNorm<-qnorm(p=seq(0, 1, 1/4),mean = MediaCol,sd = SdCol)
#Calcula la probabilidad de variables aleatorias en no arco
Quar_DD<-quantile(arcus.no$chol, prob=seq(0, 1, 1/4))
ProbNorm_datos<-pnorm(q=Quar_DD,mean = MediaCol,sd = SdCol)


#pnorm al ingresar quantiles generados con los mismos parametros como resultado el valor p de qnorm
ProbNorm_normal<-pnorm(q=QuarNorm,mean = MediaCol,sd = SdCol)
#comparemos con nuestros datos

dt_norm_Vs_Datos<-data.frame(Cuartil_NoArco=Quar_DDNA,Cuartil_Normal=QuarNorm,
                             Prob_NoArco=ProbNorm_datosNA,Prob_Normal=ProbNorm_normal)
dt_norm_Vs_Datos

#Número de cigarros Arco
MediaCigs<-summary_d$`Arco cigarros`[4]
SdCigs<-var_sd$var_sd_yes[2]
#Calcular los cuartiles
QuarNorm<-qnorm(p=seq(0, 1, 1/4),mean = MediaCigs,sd = SdCigs)
#Calcula la probabilidad de variables aleatorias en arco
Quar_DD<-quantile(arcus.yes$ncigs, prob=seq(0, 1, 1/4))
ProbNorm_datos<-pnorm(q=Quar_DD,mean = MediaCigs,sd = SdCigs)


#pnorm al ingresar quantiles generados con los mismos parametros como resultado el valor p de qnorm
ProbNorm_normal<-pnorm(q=QuarNorm,mean = MediaCigs,sd = SdCigs)
#comparemos con nuestros datos

dt_norm_Vs_Datos<-data.frame(Cuartil_Arco=Quar_DD,Cuartil_Normal=QuarNorm,
                             Prob_Arco=ProbNorm_datos,Prob_Normal=ProbNorm_normal)
dt_norm_Vs_Datos

# Número de cigarros No Arco
MediaCigs<-summary_d$`No Arco cigarros`[4]
SdCigs<-var_sd$var_sd_no[2]

# No Arco
Quar_DD<-quantile(arcus.no$ncigs, prob=seq(0, 1, 1/4))
ProbNorm_datosNA<-pnorm(q=Quar_DD,mean = MediaCigs,sd = SdCigs)

#Calcular los cuartiles
QuarNorm<-qnorm(p=seq(0, 1, 1/4),mean = MediaCigs,sd = SdCigs)
#Calcula la probabilidad de variables aleatorias en no arco
Quar_DD<-quantile(arcus.no$ncigs, prob=seq(0, 1, 1/4))
ProbNorm_datos<-pnorm(q=Quar_DD,mean = MediaCigs,sd = SdCigs)

#pnorm al ingresar quantiles generados con los mismos parametros como resultado el valor p de qnorm
ProbNorm_normal<-pnorm(q=QuarNorm,mean = MediaCigs,sd = SdCigs)
#comparemos con nuestros datos

dt_norm_Vs_Datos<-data.frame(Cuartil_NoArco=Quar_DD,Cuartil_Normal=QuarNorm,
                             Prob_NoArco=ProbNorm_datosNA,Prob_Normal=ProbNorm_normal)
dt_norm_Vs_Datos

## Grafica

Dnormal<-rnorm(n=1020,mean = MediaCigs,sd = SdCigs)
DatosCigs<-arcus.yes$ncigs
#calcula la funcion de densidad
x<-seq(0, 1020, length = 1020)
distNorm<-dnorm(x=x,mean = MediaCigs,sd = SdCigs)
#imprimamos los datos
par(mfrow=c(1,2))
hist(Dnormal,main = "normal",freq = F,ylim =  c(0,0.030))
lines(x,distNorm,col = "blue", lty = 1, lwd = 2)
hist(DatosCigs,main = "Datos",freq = F,ylim =  c(0,0.06))
lines(x,distNorm,col = "blue", lty = 1, lwd = 2)


Dnormal<-rnorm(n=1020,mean = MediaCigs,sd = SdCigs)
DatosCigs<-arcus.no$ncigs
#calcula la funcion de densidad
x<-seq(0, 1020, length = 1020)
distNorm<-dnorm(x=x,mean = MediaCigs,sd = SdCigs)
#imprimamos los datos
par(mfrow=c(1,2))
hist(Dnormal,main = "normal",freq = F,ylim =  c(0,0.03))
lines(x,distNorm,col = "blue", lty = 1, lwd = 2)
hist(DatosCigs,main = "Datos",freq = F,ylim =  c(0,0.12))
lines(x,distNorm,col = "blue", lty = 1, lwd = 2)

