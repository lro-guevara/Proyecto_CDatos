library(e1071)
require(nortest)
library(lsr)


# Parte 1. Preparacion de datos
## Importar los datos conservando el header
inputdata<-read.csv("wcgs.csv", header = TRUE)
## Revisar los indices correspondientes a los nombres de las 
## columnas/caracteristicas
colnames(inputdata)

## Dividir los datos en dos conjuntos dada la presencia del arco senil, 
## tomando los valores correspondientes a las columnas ncigs y chol
data_arcus<-inputdata[inputdata$arcus==1,c(6,13)]
data_no_arcus<-inputdata[inputdata$arcus==0,c(6,13)]

# Obtener las dimensiones
dim(data_arcus)
dim(data_no_arcus)

## Eliminar posibles valores nulos para no tener errores
data_arcus<-na.omit(data_arcus)
data_no_arcus<-na.omit(data_no_arcus)

# Obtener las nuevas dimensiones
dim(data_arcus)
dim(data_no_arcus)

## Buscar elementos vacios
empty_values<-function(data){
  sapply(data, function(x){
    empty_val<-which(x=="")
    if(length(empty_val)>0){
      empty_val<-"yes"
      return(empty_val)
    }
    empty_val<-"no"
    return(empty_val)
  })}

empty_values(data_arcus)
empty_values(data_no_arcus)


# Parte 2. Comparar medias de niveles de colesterol entre
#         individuos con arco senil y sin arco senil

## Evaluar Normalidad
### Generar gráficos de cuantiles
par(mfrow = c(1,2))
qqnorm(data_arcus$chol, main = "Arcus senilis")
qqline(data_arcus$chol)
qqnorm(data_no_arcus$chol, main = "No arcus senilis")
qqline(data_no_arcus$chol)

### Obtener asimetría de los datos
skewness(data_arcus$chol)
skewness(data_no_arcus$chol)

### Los datos presentan asimetría positiva

### Realizar test Lilliefors
lillie.test(data_arcus$chol)
lillie.test(data_no_arcus$chol)

### Se elige este test por tener tamaños superiores a 50 en los grupos
### Se rechaza la hipotesis nula para ambos grupos, no siguen
### distribucion normal

## Evaluar Homocedasticidad
### Realizar test de Fligner-Killeen 
fligner.test(list(data_arcus$chol, data_no_arcus$chol))

### Se elige este test por no tener datos normales
### Se acepta la hipotesis nula, los grupos tienen la misma varianza

## Realizar analisis de la diferencia entre las medias
### Realizar prueba T para dos conjuntos
df<-data.frame(cholesterol=as.numeric(c(data_arcus$chol,data_no_arcus$chol)), 
                arcus=as.factor(c(rep("arcus", nrow(data_arcus)), 
                         rep("no arcus", nrow(data_no_arcus)))))

independentSamplesTTest(formula=cholesterol~arcus,data=df,var.equal=T)
independentSamplesTTest(formula=cholesterol~arcus,data=df,var.equal=T)$p.value

### Se elige t test por el gran tamaño de los grupos y porque los grados de
### libertad son mayores a 30, y el hecho de no ser normales no se influencia
### con un valor altor de df
### Se rechaza la hipotesis nula de que no hay diferencia entre las medias,
### y el efecto es pequeño, diferencia neta observada entre grupos pequeña


# Parte 3. Comparar medias de numero de cigarros fumados por dia
#          entre individuos con arco senil y sin arco senil

## Evaluar Normalidad
### Generar gráficos de cuantiles
par(mfrow = c(1,2))
qqnorm(data_arcus$ncigs, main = "Arcus senilis")
qqline(data_arcus$ncigs)
qqnorm(data_no_arcus$ncigs, main = "No arcus senilis")
qqline(data_no_arcus$ncigs)

### Obtener asimetría de los datos
skewness(data_arcus$ncigs)
skewness(data_no_arcus$ncigs)

### Los datos presentan asimetría positiva

### Realizar test Lilliefors
lillie.test(data_arcus$ncigs)
lillie.test(data_no_arcus$ncigs)

### Se rechaza la hipotesis nula para ambos grupos, no siguen
### distribucion normal

## Evaluar Homocedasticidad
fligner.test(list(data_arcus$ncigs, data_no_arcus$ncigs))

### Se elige este test por no tener datos normales
### Se rechaza la hipotesis nula, los grupos no tienen la misma varianza

## Realizar analisis de la diferencia entre las medias
### Realizar prueba T para dos conjuntos
df<-data.frame(num_cigarettes=as.numeric(c(data_arcus$ncigs,data_no_arcus$ncigs)), 
               arcus=as.factor(c(rep("arcus", nrow(data_arcus)), 
                                 rep("no arcus", nrow(data_no_arcus)))))

independentSamplesTTest(formula=num_cigarettes~arcus,data=df,var.equal=F)
independentSamplesTTest(formula=num_cigarettes~arcus,data=df,var.equal=F)$p.value


### Se rechaza la hipotesis nula de que no hay diferencia entre las medias,
### por lo que la diferencia es estadisticamente significativa, aunque
### dado que el tamaño de efecto es menor a 0.2, esta diferencia termina
### siendo despreciable


# Parte 4. Comparar medias de niveles de colesterol 
#          entre individuos fumadores y no fumadores
## Preparar datos
### Dividir los datos en dos conjuntos dado si fuman, 
### tomando los valores correspondientes a chol
data_smoke<-inputdata[inputdata$smoke=="Yes",6]
data_no_smoke<-inputdata[inputdata$smoke=="No",6]

### Obtener tamaños
length(data_smoke)
length(data_no_smoke)

### Eliminar posibles valores nulos para no tener errores
data_smoke<-na.omit(data_smoke)
data_no_smoke<-na.omit(data_no_smoke)

### Obtener nuevos tamaños
length(data_smoke)
length(data_no_smoke)

## Evaluar Normalidad
###Generar gráficos de cuantiles
par(mfrow = c(1,2))
qqnorm(data_smoke, main = "Smoker")
qqline(data_smoke)
qqnorm(data_no_smoke, main = "Non smoker")
qqline(data_no_smoke)

### Obtener asimetría de los datos
skewness(data_smoke)
skewness(data_no_smoke)

### Los datos presentan asimetría positiva

## Realizar test Lilliefors
lillie.test(data_smoke)
lillie.test(data_no_smoke)

### Se elige este test por tener tamaños superiores a 50 en los grupos
### Se rechaza la hipotesis nula para ambos grupos, no siguen
### distribucion normal

## Evaluar Homocedasticidad
fligner.test(list(data_smoke, data_no_smoke))
### Se elige este test por no tener datos normales
### Se rechaza la hipotesis nula, los grupos no tienen la misma varianza

## Realizar analisis de la diferencia entre las medias
### Realizar prueba de Welch para los dos conjuntos
df<-data.frame(cholesterol=as.numeric(c(data_smoke,data_no_smoke)), 
               smoke=c(rep("smoke", length(data_smoke)), 
                                 rep("no smoke", length(data_no_smoke))))
df$smoke<-factor(df$smoke,levels=c("smoke","no smoke"))

independentSamplesTTest(formula=cholesterol~smoke,data=df,var.equal=F)
independentSamplesTTest(formula=cholesterol~smoke,data=df,var.equal=F)$p.value

### Se elige la prueba de Welch por no cumplirse la homocedasticidad,
### por el gran tamaño de los grupos y  porque los grados de libertad 
### son mayores a 30, y el hecho de no ser normales no se influencia 
### con un valor altor de df

### Se rechaza la hipotesis nula de que no hay diferencia entre las medias,
### por lo que la diferencia es estadisticamente significativa, aunque
### dado que el tamaño de efecto es menor a 0.2, esta diferencia termina
### siendo despreciable
