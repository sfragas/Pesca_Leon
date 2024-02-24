#Analisis de biomasa de trucha en CyL despues de la entrada en vigor de la ley
file.edit(".Rprofile")
rm(list=ls())
#1. PREPARAMOS LOS DATOS DE castilla
setwd("~/Master_R/Temas_R/Pesca/CyL/Pesca_Leon")
biom<-read.csv("Biomasa.csv",check.names = F)
dens<-read.csv("densidades.csv",check.names = F)
summary(biom)
summary(dens)
#Tenemos que eliminar los valores de 0 en densidad cuando haya valor de biomasa

#El año de comienzo es
colnames(biom)[grep(pattern = '2',colnames(biom))][1]
#El último año será el
a<-length(colnames(biom)[grep(pattern = '2',colnames(biom))])
colnames(biom)[grep(pattern = '2',colnames(biom))][a]
#Cambiamos de formato de los datos
library(tidyr)
biom_long<-pivot_longer(biom,cols=3:10,names_to="año",values_to = "Biomasa")
dens_long<-pivot_longer(dens,cols=3:10,names_to = "año",values_to = "Densidad")
#biom_long<-melt(biom,id=c("Estacion","Provincia","Gestion","Nivel"),measure.vars = c("2014","2015","2016","2017","2018","2019","2020","2021"),variable_name = "año")
#dens_long<-melt(dens,id=c("Estacion","Provincia","Gestion","Nivel"),measure.vars = c("2014","2015","2016","2017","2018","2019","2020","2021"),variable_name = "año")
#1.1 Unimos las 2 tablas
#colnames(biom_long)[6]<-c("Biomasa")
#colnames(dens_long)[6]<-c("Densidad")
datos<-cbind(biom_long,dens_long[,6])
colnames(datos)[7]<-c("Densidad")
#Comprobamos la tabla de datos
summary(datos)
#Eliminamos los registros con densidad =0
datos<-datos[!datos$Densidad==0,]
#1.2 Añadimos el peso medio dividiendo Biomas entre Densidad
datos<-(datos%>%mutate(Peso_medio=Biomasa/Densidad))
#1.3 Redondeamos los castilla numericos para ajustarlos con 2 decimales
datos$Peso_medio<-round(datos$Peso_medio,2)
datos$Biomasa<-round(datos$Biomasa,2)
datos$Densidad<-round(datos$Densidad,4)
#1.4 Indicamos factores en las variables nivel,gestion,provincia, Estacion,año
datos$Estacion<-as.factor(datos$Estacion)
datos$Provincia<-as.factor(datos$Provincia)
datos$Gestion<-as.factor(datos$Gestion)
datos$Nivel<-as.factor(datos$Nivel)
datos$año<-as.factor(datos$año)

#Hacemos una tabla exclusiva para Leon.
leon<-droplevels.data.frame(datos[datos$Provincia=="Leon",])
#leon_1<-leon[leon$Nivel==1,]
#leon_2<-leon[leon$Nivel==2,]
leon_l<-subset(leon,leon$Gestion=="L")
leon_CSM<-subset(leon,leon$Gestion=="CSM")
leon_CCM<-subset(leon,leon$Gestion=="CCM")
leon_CM<-subset(leon,leon$Gestion=="CM")
leon_l<-droplevels.data.frame(leon_l)
#2. Graficamos
#2.1 Para Leon
#Graficamos las variables

 
#Valores medios de Biomasa y densidad para León 
#Para os valores de Biomasa
longitud<-function(x){
  length(which(!is.na(x)))
}
#tamaño_muestral<-data.frame(numero=tapply(leon_l[,6],leon_l[,5],longitud),año=c("2014","2015","2016","2017","2018","2019","2020","2021"))
tamaño_muestral<-data.frame(numero=tapply(leon_l[,6],leon_l[,5],longitud),año=unique(leon_l$año))
sd<-function(x){  sqrt(var(x,na.rm=T))
}
Media<-tapply(leon_l[,6],leon_l[,5],mean,na.rm=T)
medias<-data.frame(Año=sort(unique(leon_l$año)),Medias=Media)
Desv<-tapply(leon_l[,6],leon_l[,5],sd)
tabla<-rbind(Media,Desv)
knitr::kable(tabla, caption = 'Valores medios y desviaciones estandar')
B<-ggplot(leon_l,aes(x=año,y=Biomasa))+ylab("gr/m2")+geom_bar(stat="summary",fun="mean")+ggtitle("Biomasas de trucha en los libres sm (provincia de León)")

#B+annotate("text",x=1:5,y=c(3.67,6.2,7.08,8.51,6.83)+.3,label=c("3.67","6.2","7.08","8.51","6.83"),col="blue")
c<-B+geom_text(data=medias,aes(x=as.factor(Año),y=Medias,label=paste(round(Medias,2))),nudge_y=.4,nudge_x=-.3)+stat_summary(fun.data = mean_se,geom="errorbar",linewidth = 1,width = 0.2)
c+geom_text(data=tamaño_muestral,aes(x=as.factor(año),y=rep(-0.3,length(unique(leon_l$año))),label=paste("n= ",numero)))

#Para los valores de densidad
longitud<-function(x){
  length(which(!is.na(x)))
}
tamaño_muestral<-data.frame(numero=tapply(leon_l[,7],leon_l[,5],longitud),año=unique(leon_l$año))
Media_d<-tapply(leon_l[,7],leon_l[,5],mean,na.rm=T)
medias_d<-data.frame(Año=sort(unique(leon_l$año)),Medias=Media_d)
B<-ggplot(leon_l,aes(x=año,y=Densidad))+ylab("Indv/m2")+geom_bar(stat="summary",fun="mean")+ggtitle("Densidad de trucha en los libres sm (provincia de León)")
c<-B+geom_text(data=medias_d,aes(x=as.factor(Año),y=Medias,label=paste(round(Medias,2))),nudge_y=.02,nudge_x=-.3)+stat_summary(fun.data = mean_se,geom="errorbar",col="blue",width=.2,linewidth=1)
c+geom_text(data=tamaño_muestral,aes(x=as.factor(año),y=rep(-0.02,length(unique(leon_l$año))),label=paste("n= ",numero)))


library(gplots)

#Ploteamos medias y desviaciones para Leon
plotmeans(Biomasa~año,data=leon_l,ci.label=F,mean.labels=T,digits=2,adj=1,cex=.9,pty=3)
plotmeans(Densidad~año,data=leon_l,ci.label=F,mean.labels=F,digits=2,barwidth=2)
plotmeans(Peso_medio~año,data=leon_l[is.finite(leon_l$Peso_medio),],ci.label=F,mean.labels=T,digits=2,barwidth=2)


# VALORES DE BIOMASA -------------------------------------

#Valores medios y desviaciones por año
sd<-function(x)sqrt(var(x,na.rm=T))
Media<-tapply(subset(leon,leon$Gestion=="L")[,6],subset(leon,leon$Gestion=="L")[,5],mean,na.rm=T)
Desv<-tapply(subset(leon,leon$Gestion=="L")[,6],subset(leon,leon$Gestion=="L")[,5],sd)
rbind(Media,Desv)


# NORMALIDAD Y OUTLIERS ---------------------------------------------------

hist(leon_l$Biomasa)
boxplot(leon_l$Biomasa~leon_l$año)#Hay outliers y no se puede presumir normalidad.

#Transformamos los datos
library(MASS)
leon_tb_l<-droplevels.data.frame(subset(leon_l,leon_l$Gestion=="L"))
(b<-boxcox(lm(leon_tb_l$Biomasa~1)))
(c<-b$x[which.max(b$y)])
#Transformamos los datos
leon_tb_l$Biomasa<-(leon_tb_l$Biomasa)^c
#Visulaizamos los datos transformados
hist(leon_tb_l$Biomasa)
ggplot(data = leon_tb_l,aes(x=año,y=Biomasa))+geom_boxplot()+ggtitle("Boxplot de las biomasas transformadas")+labs(y="Biomasa transformada")

#Comprobamos la normalidad
shapiro.test(leon_tb_l$Biomasa)#Aceptamos la normalidad
#Testa de homogeneidad de varianzas
leveneTest(leon_tb_l$Biomasa,leon_tb_l$año)#Aceptamos la homocedasticidad


# COMPARAMOS LA BIOMASA POR FRIEDMAN --------------------------------------
y<-reshape(leon_l[,1:6],v.names = "Biomasa",idvar="Estacion",timevar="año",direction="wide")
boxplot(y[,-c(1:4)])
friedman.test(as.matrix(y[,-c(1:4)]))#No muestra diferencias significativas
#Hacemos las pruebas post hoc
pairwise.wilcox.test(leon_l$Biomasa, leon_l$año, p.adj="bonferroni", exact=F, paired=T)


# MODELOS MIXTOS --------------------------------------------------------
library(nlme)
####PROCEDIMIENTO DE ZUUR PARA LA SELECCION DEL MODELO
##1. Determinamos los efectos variables y para ello saturamos los efectos fijos
leon_tb_l<-leon_tb_l[!is.na(leon_tb_l$Biomasa),]
m1a<-gls(Biomasa~1+año,data=leon_tb_l,method="REML")
m1b<-lme(Biomasa~1+año,random=~1|Estacion,data=leon_tb_l,method="REML")
anova(m1a,m1b)#Efectivamente al año parece afectar a la VD, seleccionamos como variable aleatoria la estación.
##2. Seleccionamos las variables de la estructura fija.
m1c<-lme(Biomasa~1,random=~1|Estacion,data=leon_tb_l,method="ML")
m1d<-lme(Biomasa~1+año,random=~1|Estacion,data=leon_tb_l,method="ML")
anova(m1c,m1d)#El modelo m1d es el elegido
##3. Ajuste del modelo final con el método REML
m1d_final<-lme(Biomasa~1+año,random=~1|Estacion,method="REML",data=leon_tb_l)
summary(m1d_final)#Diferencias entre 2017 y 2016 respecto de 2014
#Hacemos comparaciones multiples
library(multcomp)
MC<-glht(m1d_final,linfct=mcp(año="Tukey"))
summary(MC)
#Se observan diferencias significativas entre 2014 y 2017,2019, 2020 y 2021.
# Ploteamos los datos del modelo
plot(m1d_final)
Res<-residuals(m1d_final, type="normalized")
Fit<-fitted(m1d_final) #level=1
# Residuos vs. predicciones
op<-par(mfrow=c(3,2))
plot(Res ~ Fit, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
# Residuos vs. predictores (variables explicativas)
plot(Res~leon_tb_l$Estacion, xlab="Estacion", ylab="Residuals")
abline(h=0)
plot(Res~leon_tb_l$año, xlab="Exposure", ylab="Residuals")
abline(h=0)

# Normalidad de los residuos
hist(Res)
qqnorm(Res)
qqline(Res)
par(op)


# 2. VALORES DE DENSIDAD --------------------------------------------------

#Valores medios y desviaciones por año
Media<-tapply(subset(leon,leon$Gestion=="L")[,7],subset(leon,leon$Gestion=="L")[,5],mean,na.rm=T)
Desv<-tapply(subset(leon,leon$Gestion=="L")[,7],subset(leon,leon$Gestion=="L")[,5],sd)
rbind(Media,Desv)
# NORMALIDAD Y OUTLIERS ---------------------------------------------------

hist(leon_l$Densidad)
boxplot(leon_l$Densidad~leon_l$año)#Hay outliers y no se puede presumir normalidad.

#Transformamos los datos
library(MASS)
leon_tb_l<-droplevels.data.frame(subset(leon_l,leon_l$Gestion=="L"))
summary(leon_tb_l$Densidad)
leon_tb_l<-leon_tb_l[which(leon_tb_l$Densidad>0),]#Retiramos el máximo por ser un error
leon_tb_l<-leon_tb_l[-which.min(leon_tb_l$Densidad),]#Retiramos el valor de 0 por ser un error

(b<-boxcox(lm((leon_tb_l$Densidad)~1)))
(c<-b$x[which.max(b$y)])
#Transformamos los datos
leon_tb_l$Densidad<-(leon_tb_l$Densidad)^c
#Visulaizamos los datos transformados
hist(leon_tb_l$Densidad)
ggplot(data = leon_tb_l,aes(x=año,y=Densidad))+geom_boxplot()+ggtitle("Boxplot de las densidades transformadas")+labs(y="Densidad transformada")
#Comprobamos la normalidad
shapiro.test(leon_tb_l$Densidad)#Aceptamos la normalidad
#Testa de homogeneidad de varianzas
leveneTest(leon_tb_l$Densidad,leon_tb_l$año)#Aceptamos la homocedasticidad


# COMPARAMOS LA DENSIDAD POR FRIEDMAN --------------------------------------
y<-reshape(leon_l[,C(1:7)],v.names = "Densidad",idvar="Estacion",timevar="año",direction="wide")
y<-y[,-5]
boxplot(y[,-c(1:4)])
friedman.test(as.matrix(y[,-c(1:4)]))#No muestra diferencias significativas
#Hacemos las pruebas post hoc
pairwise.wilcox.test(leon_l$Biomasa, leon_l$año, p.adj="bonferroni", exact=F, paired=T)


# MODELOS MIXTOS --------------------------------------------------------
library(nlme)
####PROCEDIMIENTO DE ZUUR PARA LA SELECCION DEL MODELO
##1. Determinamos los efectos variables y para ello saturamos los efectos fijos
leon_tb_l<-leon_tb_l[!is.na(leon_tb_l$Biomasa),]
m1a<-gls(Biomasa~1+año,data=leon_tb_l,method="REML")
m1b<-lme(Biomasa~1+año,random=~1|Estacion,data=leon_tb_l,method="REML")
anova(m1a,m1b)#Efectivamente al año parece afectar a la VD, seleccionamos como variable aleatoria la estación.
##2. Seleccionamos las variables de la estructura fija.
m1c<-lme(Biomasa~1,random=~1|Estacion,data=leon_tb_l,method="ML")
m1d<-lme(Biomasa~1+año,random=~1|Estacion,data=leon_tb_l,method="ML")
anova(m1c,m1d)#El modelo m1d es el elegido
##3. Ajuste del modelo final con el método REML
m1d_final<-lme(Biomasa~1+año,random=~1|Estacion,method="REML",data=leon_tb_l)
summary(m1d_final)#Diferencias entre 2017 y 2016 respecto de 2014
#Hacemos comparaciones multiples
library(multcomp)
MC<-glht(m1d_final,linfct=mcp(año="Tukey"))
summary(MC)
#Se observan diferencias significativas entre 2014 y 2017,2019, 2020 y 2021.
# Ploteamos los datos del modelo
plot(m1d_final)
Res<-residuals(m1d_final, type="normalized")
Fit<-fitted(m1d_final) #level=1
# Residuos vs. predicciones
op<-par(mfrow=c(3,2))
plot(Res ~ Fit, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
# Residuos vs. predictores (variables explicativas)
plot(Res~leon_tb_l$Estacion, xlab="Estacion", ylab="Residuals")
abline(h=0)
plot(Res~leon_tb_l$año, xlab="Exposure", ylab="Residuals")
abline(h=0)

# Normalidad de los residuos
hist(Res)
qqnorm(Res)
qqline(Res)
par(op)

#Valores de peso medio
Media<-tapply(leon_l[-which(is.infinite(leon_l$Peso_medio)),8],leon_l[-which(is.infinite(leon_l$Peso_medio)),5],mean,na.rm=T)
Desv<-tapply(leon_l[-which(is.infinite(leon_l$Peso_medio)),8],leon_l[-which(is.infinite(leon_l$Peso_medio)),5],sd)
rbind(Media,Desv)




leon_r<-subset(leon,leon$Biomasa!=is.na(leon$Biomasa))
leon_r<-subset(leon_r,leon_r$Densidad!=is.na(leon_r$Densidad))



#Para la pesca sin muerte en León
#leon_l<-subset(leon,leon$Gestion=="L")
#Representamos las 3 variables para Leon

ggplot(leon_r_l,aes(x=año,y=Biomasa,fill=Gestion))+geom_boxplot()+ggtitle("Boxplot de la biomasa de trucha en libres sin muerte anualmente en León")
ggplot(leon_r_l,aes(x=año,y=Densidad,fill=Gestion))+geom_boxplot()+ggtitle("Boxplot de la densidad de trucha en libres sin muerte anualmente en León")
ggplot(leon_r_l,aes(x=año,y=Peso_medio,fill=Gestion))+geom_boxplot()+ggtitle("Boxplot del peso medio de trucha en libres sin muerte anualmente en León")
#Comparamos los grupos
#Para el valor de Biomasa
kruskal.test (leon_r$Biomasa~leon_r$año)#Se rechaza la hipostesis nula
kwAllPairsNemenyiTest(Biomasa~año,data=leon_r,dist="Chisquare")#Solo se muestran diferencias significativas entre 2014 y 2017
#Para el valor de densidad
kruskal.test (leon_r_l$Densidad~leon_r_l$año)#Se rechaza la hipostesis nula
kwAllPairsNemenyiTest(Densidad~año,data=leon_r_l,dist="Chisquare")#Se muestran diferencias significativas entre 2014 y 2017
#Para la variable peso medio
kruskal.test (leon_r_l$Peso_medio~leon_r_l$año)#Se rechaza la hipostesis nula
kwAllPairsNemenyiTest(Peso_medio~año,data=leon_r_l,dist="Chisquare")#Se muestran diferencias significativas entre 2014 y 2015 y 2017


#3. PARA LA VARIABLE PESO MEDIO -----------------------------------------

# 3. ANÁLISIS DE LA VARIABLE PESO MEDIO
#Otra variable determinada de las 2 anteriores por simple cocientre entre la biomasa y la densidad es el peso medio (gr/ind). Con ella podemos comprobar si desde 2014 ha habido variaciones significativas en la misma, también centrándonos en los tramos libres, desde la entrada de la normativa que exige la pesca sin muerte.

  
  ## 3.1 VISUALIZACIÓN DE LOS DATOS
  
#Vemos los datos
leon<-droplevels.data.frame(datos[datos$Provincia=="Leon",])
leon_l<-subset(leon,leon$Gestion=="L")
leon_CSM<-subset(leon,leon$Gestion=="CSM")
leon_CCM<-subset(leon,leon$Gestion=="CCM")
leon_CM<-subset(leon,leon$Gestion=="CM")
leon_l<-droplevels.data.frame(leon_l)
leon_l<-leon_l[which(is.finite(leon_l$Peso_medio)),]
#Retiramos el outlier de densidad por ser claros errores de codificación
leon_l<-leon_l[-which.max(leon_l$Densidad),]#Retiramos el valor maximo por ser un error
summary(leon_l)
#Variable densidad
ggplot(leon,aes(x=año,y=Peso_medio,fill=Gestion))+ylab("gr/ind")+geom_boxplot()+facet_wrap(~Gestion,ncol=2)+ggtitle("Boxplot del peso en gr/ind para cada año y tipo de gestión en León")

#Representamos las variables para los tramos libres, donde es previsible que se puedan observar con los efectos que se quieren comprobar por la introducción de la modalidad de sin muerte de forma general.


#Variable densidad
ggplot(leon_l,aes(x=año,y=Peso_medio,fill=Gestion))+geom_boxplot()+ggtitle("Boxplot de la variable peso medio por individuo de trucha en libres sin muerte")+theme(plot.title = element_text(size = rel(1.1)))

# Se observa la existencia de outliers que para la comprobación de las comparaciones y realizar las comprobaciones de hipótesis deberá ser tenida en cuenta para elegir la tecnica estadística.
#Determinamos a continuación los valores medios para cada año para los tramos de pesca libre, recogiéndose igualmente las desviaciones estandar de cada grupo.
## 3.2 MEDIAS Y DESVIACIONES DE LA VARIABLE peso medio
#Para los tramos libres de León, comprobamos los valores medios del peso medio de trucha (en gr/ind) en los diferentes años, así como los valores de la desviación estándar.
#Para los valores de densidad
#Retiramos el outlier de densidad por ser claros errores de codificación
longitud<-function(x){
  length(which(!is.na(x)))
}
tamaño_muestral<-data.frame(numero=tapply(leon_l[,8],leon_l[,5],longitud),año=unique(leon_l$año))
sd<-function(x){sqrt(var(x,na.rm=T))
}
Media<-tapply(leon_l[,8],leon_l[,5],mean,na.rm=T)
medias<-data.frame(Año=levels(leon_l$año),Medias=Media)
Desv<-tapply(leon_l[,8],leon_l[,5],sd)
knitr::kable(rbind(Media,Desv), caption = 'Valores medios y desviaciones estandar')
#Se observa un aumento de los valores medios respecto a 2014.
#Representamos la distribución mediante histograma de la variable densidad

B<-ggplot(leon_l,aes(x=año,y=Peso_medio))+ylab("ind/m2")+geom_bar(stat="summary",fun="mean")+ggtitle("Peso medio de trucha en los libres sm (provincia de León)")
c<-B+geom_text(data=medias,aes(x=as.factor(Año),y=Medias,label=paste(round(Medias,2))),nudge_y=2,nudge_x=-.3)+stat_summary(fun.data = mean_se,geom="errorbar",linewidth = 1,width = 0.2)
c+geom_text(data=tamaño_muestral,aes(x=as.factor(año),y=rep(-1.5,length(unique(leon_l$año))),label=paste("n= ",numero)))


## 2.3 MODELO PARA LA COMPARACIÓN DEL PESO MEDIO
#Para la comparación de los grupos optamos en primer lugar por observar los datos y ver los supuestos para adaptarlo a un modelo lineal.
#Comprobamos la normalidad y la homocedasticidad. 
#Primero los visualizamos

hist(leon_l$Peso_medio)
boxplot(leon_l$Peso_medio~leon_l$año)#Hay outliers y no se puede presumir normalidad.


#Comprobamos la normalidad y homocedasticidad aunque es visible que no parecen cumplirse ninguno de los 2 criterios
shapiro.test(leon_l$Peso_medio)#No se acepta la normalidad

#No se puede aceptar la la distribución normal de los datos
#Test de homogeneidad de varianzas
leveneTest(leon_l$Peso_medio,leon_l$año)#Se acpta la homocedasticidad

#Por ello es preciso transformar los datos y ver si con ello podemos asumir un modelo lineal. Para ello determinaremos los valores de \lambda mediante la aplicación de una transformación de Box Cox

library(MASS)
leon_tb_l<-droplevels.data.frame(leon_l)
(b<-boxcox(lm(leon_tb_l$Peso_medio~1)))
(c<-b$x[which.max(b$y)])
#Transformamos los datos
leon_tb_l$Peso_medio<-(leon_tb_l$Peso_medio)^c
#Una vez transformado los datos visualizamos nuevamente

hist(leon_tb_l$Peso_medio)
ggplot(data = leon_tb_l,aes(x=año,y=Peso_medio))+geom_boxplot()+ggtitle("Boxplot del peso medio transformado")+labs(y="Peso medio transformadO")
#Hay outliers y no se puede presumir normalidad.
#Visualmente ahora parece que los datos pueden cumplir alos criterios de normalidad y homocedasticidad, pese a ello realizamos los test.
shapiro.test(leon_tb_l$Peso_medio)#Se acepta la normalidad
#Se acepta la la distribución normal de los datos
#Test de homogeneidad de varianzas
leveneTest(leon_tb_l$Peso_medio,leon_tb_l$año)#Se acepta la homocedasticidad
#Se acepta la homocedasticidad
#Planteamos ahora un modelo lineal mixto. Esto es conveniente en nuestro caso ya que podemos establecer una parte aleatoria del modelo con los datos anidados por la variable Estación. Como término fijo utilizaríamos la variable año, ya que se trata del factor principal que nos explica precisamente si existen diferencias en los valores de la densidad considerando el año 2014 como el año que se produce el cambio de política de gestión al establecer los libres como libres sin muerte.
#Para la construcción del modelo seguimos el procedimiento que recomienda Zuur et al. (2009) y que se secuenciaría en fases:
#1) Estructura aleatoria óptima. Usando un modelo saturado (beyond optimal model), se determina la estructura óptima del componente aleatorio, la cual no debe contener información que esté en la componente fija. Debemos:
#• construir un modelo saturado.
#• comparar modelos con distinta estructura aleatoria, mediante máxima verosimilitud restringida (REML).
#2) Estructura fija óptima. Una vez encontramos la estructura aleatoria óptima, podemos encontrar la estructura fija óptima. Comparamos los modelos anidados mediante máxima verosimilitud (ML), manteniendo la misma estructura aleatoria.
#3) Ajuste del modelo final con REML.  
library(nlme)
####PROCEDIMIENTO DE ZUUR PARA LA SELECCION DEL MODELO
##1. Determinamos los efectos variables y para ello saturamos los efectos fijos
leon_tb_l<-leon_tb_l[!is.na(leon_tb_l$Peso_medio),]
m1a<-gls(Peso_medio~1+año,data=leon_tb_l,method="REML")
m1b<-lme(Peso_medio~1+año,random=~1|Estacion,data=leon_tb_l,method="REML")
anova(m1a,m1b)#Efectivamente al año parece afectar a la VD, seleccionamos como variable aleatoria la estación.
##2. Seleccionamos las variables de la estructura fija.
m1c<-lme(Peso_medio~1,random=~1|Estacion,data=leon_tb_l,method="ML")
m1d<-lme(Peso_medio~1+año,random=~1|Estacion,data=leon_tb_l,method="ML")
anova(m1c,m1d)#El modelo m1d es el elegido
##3. Ajuste del modelo final con el método REML
m1d_final<-lme(Peso_medio~1+año,random=~1|Estacion,method="REML",data=leon_tb_l)
summary(m1d_final)#Diferencias entre 2017 y 2016 respecto de 2014
#El modelo final lo utilizamos patra contrastar los diferentes niveles de la variable predictora, en este caso los diferentes años mediante comparaciones múltiples mediante eltest de Tukey.
#Comrobamos el modelo de forma gráfica

# Ploteamos los datos del modelo
plot(m1d_final)
Res<-residuals(m1d_final, type="normalized")
Fit<-fitted(m1d_final) #level=1
# Residuos vs. predicciones
op<-par(mfrow=c(2,2))
plot(Res ~ Fit, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
# Residuos vs. predictores (variables explicativas)
plot(Res~leon_tb_l$Estacion, xlab="Estacion", ylab="Residuals")
abline(h=0)
plot(Res~leon_tb_l$año, xlab="Exposure", ylab="Residuals")
abline(h=0)

# Normalidad de los residuos
par(op)
hist(Res)
qqnorm(Res)
qqline(Res)
library(multcomp)
MC<-glht(m1d_final,linfct=mcp(año="Tukey"))
summary(MC)
#Podemos ver en la tabla de contrastes que para los datos de biomasa existen diferencias significativas de los datos del año 2014 respecto a los de 2017 en adelante. 
#Aplicamos modelos robustos
#Comparamos los valores de peso medio para los diferentes años
rmanova(y=leon_tb_l$Peso_medio,groups=leon_tb_l$año,block=leon_tb_l$Estacion)
rmmcp(y=leon_tb_l$Peso_medio,groups=leon_tb_l$año,block=leon_tb_l$Estacion)
library(robustlmm)
library(Matrix)
library(lme4)
lmer(Peso_medio~1+año+(1|Estacion),data=leon_tb_l)   
robust_model<-rlmer(Peso_medio~1+año+(1|Estacion),data=leon_tb_l)
summary(robust_model) 
aov(robust_model)
#Comparamos los diferentes años
library(multcomp)
MC<-glht(robust_model,linfct=mcp(año="Tukey"))
summary(MC)
