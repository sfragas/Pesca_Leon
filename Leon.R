#Analisis de biomasa de trucha en CyL despues de la entrada en vigor de la ley
file.edit(".Rprofile")
rm(list=ls())
#1. PREPARAMOS LOS DATOS DE castilla
setwd("~/Master_R/Temas_R/Pesca/CyL/Pesca_Leon")
biom<-read.csv("Biomasa.csv",check.names = F)
dens<-read.csv("densidades.csv",check.names = F)
summary(biom)
#Cambiamos de formato de los datos

biom_long<-pivot_longer(biom,cols=3:10,names_to="año",values_to = "Biomasa")
dens_long<-pivot_longer(dens,cols=3:10,names_to = "año",values_to = "Densidad")
#biom_long<-melt(biom,id=c("Estacion","Provincia","Gestion","Nivel"),measure.vars = c("2014","2015","2016","2017","2018","2019","2020","2021"),variable_name = "año")
#dens_long<-melt(dens,id=c("Estacion","Provincia","Gestion","Nivel"),measure.vars = c("2014","2015","2016","2017","2018","2019","2020","2021"),variable_name = "año")
#1.1 Unimos las 2 tablas
#colnames(biom_long)[6]<-c("Biomasa")
#colnames(dens_long)[6]<-c("Densidad")
datos<-cbind(biom_long,dens_long[,6])
colnames(datos)[7]<-c("Densidad")
#1.2 Añadimos el peso medio dividiendo Biomas entre Densidad
datos<-(datos%>%mutate(Peso_medio=Biomasa/Densidad))
#1.3 Redondeamos los castilla numericos para ajustarlos con 2 decimales
datos$Peso_medio<-round(datos$Peso_medio,2)
datos$Biomasa<-round(datos$Biomasa,2)
datos$Densidad<-round(datos$Densidad,2)
#1.4 Indicamos factores en las variables nivel,gestion,provincia, Estacion,año
datos$Estacion<-as.factor(datos$Estacion)
datos$Provincia<-as.factor(datos$Provincia)
datos$Gestion<-as.factor(datos$Gestion)
datos$Nivel<-as.factor(datos$Nivel)
datos$año<-as.factor(datos$año)

#Hacemos una tabla exclusiva para Leon.
leon<-datos[datos$Provincia=="Leon",]
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
sd<-function(x){
  sqrt(var(x,na.rm=T))
}
Media<-tapply(leon_l[,6],leon_l[,5],mean,na.rm=T)
medias<-data.frame(Año=unique(leon_l$año),Medias=Media)
Desv<-tapply(leon_l[,6],leon_l[,5],sd)
knitr::kable(rbind(Media,Desv), caption = 'Valores medios y desviaciones estandar')
B<-ggplot(leon_l,aes(x=año,y=Biomasa))+ylab("gr/m2")+geom_bar(stat="summary",fun="mean")+ggtitle("Biomasas de trucha en los libres sm (provincia de León)")

#B+annotate("text",x=1:5,y=c(3.67,6.2,7.08,8.51,6.83)+.3,label=c("3.67","6.2","7.08","8.51","6.83"),col="blue")
c<-B+geom_text(data=medias,aes(x=as.factor(Año),y=Medias,label=paste(round(Medias,2))),nudge_y=.4,nudge_x=-.2)+stat_summary(fun.data = mean_se,geom="errorbar",linewidth = 1,width = 0.2)
c+geom_text(data=tamaño_muestral,aes(x=as.factor(año),y=rep(-0.3,length(unique(leon_l$año))),label=paste("n= ",numero)))

#Para los valores de densidad
longitud<-function(x){
  length(which(!is.na(x)))
}
tamaño_muestral<-data.frame(numero=tapply(leon_l[,7],leon_l[,5],longitud),año=unique(leon_l$año))
Media_d<-tapply(leon_l[,7],leon_l[,5],mean,na.rm=T)
medias_d<-data.frame(Año=unique(leon_l$año),Medias=Media_d)
B<-ggplot(leon_l,aes(x=año,y=Densidad))+ylab("Indv/m2")+geom_bar(stat="summary",fun="mean")+ggtitle("Densidad de trucha en los libres sm (provincia de León)")
c<-B+geom_text(data=medias_d,aes(x=as.factor(Año),y=Medias,label=paste(round(Medias,2))),nudge_y=.02,nudge_x=-.3)+stat_summary(fun.data = mean_se,geom="errorbar",col="blue",width=.2,linewidth=1)
c+geom_text(data=tamaño_muestral,aes(x=as.factor(año),y=rep(-0.02,length(unique(leon_l$año))),label=paste("n= ",numero)))


library(gplots)

#Ploteamos medias y desviaciones para Leon
plotmeans(Biomasa~año,data=leon_l,ci.label=F,mean.labels=T,digits=2,adj=1,cex=.9,pty=3)
plotmeans(Densidad~año,data=leon_l,ci.label=F,mean.labels=F,digits=2,barwidth=2)
plotmeans(Peso_medio~año,data=leon_l[is.finite(leon_l$Peso_medio),],ci.label=F,mean.labels=T,digits=2,barwidth=2)


# VEMOS VALORES MEDIOS Y DESVIACIONES -------------------------------------

#Valores de Biomasa
sd<-function(x)sqrt(var(x,na.rm=T))
Media<-tapply(subset(leon,leon$Gestion=="L")[,6],subset(leon,leon$Gestion=="L")[,5],mean,na.rm=T)
Desv<-tapply(subset(leon,leon$Gestion=="L")[,6],subset(leon,leon$Gestion=="L")[,5],sd)
rbind(Media,Desv)
#Valores de densidad
Media<-tapply(subset(leon,leon$Gestion=="L")[,7],subset(leon,leon$Gestion=="L")[,5],mean,na.rm=T)
Desv<-tapply(subset(leon,leon$Gestion=="L")[,7],subset(leon,leon$Gestion=="L")[,5],sd)
rbind(Media,Desv)
#Valores de peso medio
Media<-tapply(leon_l[-which(is.infinite(leon_l$Peso_medio)),8],leon_l[-which(is.infinite(leon_l$Peso_medio)),5],mean,na.rm=T)
Desv<-tapply(leon_l[-which(is.infinite(leon_l$Peso_medio)),8],leon_l[-which(is.infinite(leon_l$Peso_medio)),5],sd)
rbind(Media,Desv)


# NORMALIDAD Y OUTLIERS ---------------------------------------------------

hist(leon_l$Biomasa)
boxplot(leon_l$Biomasa~leon_l$año)#Hay outliers y no se puede presumir normalidad. 

# COMPARAMOS LA BIOMASA POR FRIEDMAN --------------------------------------
y<-reshape(leon_l[,1:6],v.names = "Biomasa",idvar="Estacion",timevar="año",direction="wide")
boxplot(y[,-c(1:4)])
friedman.test(as.matrix(y[,-c(1:4)]))#No muestra diferencias significativas
#Hacemos las pruebas post hoc
pairwise.wilcox.test(leon_l$Biomasa, leon_l$año, p.adj="bonferroni", exact=F, paired=T)


# PRUEBAS ROBUSTAS --------------------------------------------------------




#leon_r<-subset(leon,leon$Biomasa<40)
leon_tb_l<-droplevels.data.frame(subset(leon_l,leon_l$Gestion=="L"))
hist(leon_tb_l$Biomasa)
boxplot(leon_tb_l$Biomasa~leon_tb_l$año)
#Normalizamos los datos
(b<-boxcox(lm(leon_tb_l$Biomasa~1)))#muestra valores proximos a 0,3
(c<-b$x[which.max(b$y)])# Da un valor de 0,34
#Transformamos los datos de Biomasa
leon_tb_l$Biomasa<-(leon_tb_l$Biomasa)^c
hist((leon_tb_l$Biomasa))
shapiro.test((leon_tb_l$Biomasa))#Se acepta la normalidad
#Transformamos los datos y en este caso se acepta la normalidad
leveneTest((leon_tb_l$Biomasa),leon_tb_l$año)#Se acepta la homocedasticidad
#Comprobamos el anova de medias relacionadas
leon_tb_l<-droplevels.data.frame(leon_tb_l[!is.na(leon_tb_l$Biomasa),])#Retiramos los NA´s
ezANOVA(leon_tb_l,dv=Biomasa,wid=Estacion,within=año)
ezDesign(leon_tb_l,x=año,y=Estacion)#Ver el diseño de los datos

modelo_b<-aov(Biomasa~año, data=leon_tb_l)
summary(modelo_b)
TukeyHSD(modelo_b)
par(mfrow=c(2,2))
plot(modelo_b)












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




##################ALETERNATIVA AL MODELO LINEAL MEDIANTE MODELOS MIXTOS (MM)
CyL<-castilla_long[castilla_long$value<25&!is.na(castilla_long$value),]
CyL$Estacion<-factor(CyL$Estacion)
#Observamos los valores
ggplot(CyL,aes(value))+geom_histogram()+facet_wrap(.~año)
boxplot(CyL$value~CyL$año,xlab="Año",ylab="Densidad (g/m2)",main="castilla biomasa de trucha en CyL")
library(nlme)
####PROCEDIMIENTO DE ZUUR PARA LA SELECCION DEL MODELO
##1. Determinamos los efectos variables y para ello saturamos los efectos fijos
m1a<-gls(value~1+año,data=CyL,method="REML")
m1b<-lme(value~1+año,random=~1|Estacion,data=CyL,method="REML")
anova(m1a,m1b)#Efectivamente al año parece afectar a la VD, seleccionamos como variable añeatoria la estación.
##2. Seleccionamos las variables de la estructura fija.
m1c<-lme(value~1,random=~1|Estacion,data=CyL,method="ML")
m1d<-lme(value~1+año,random=~1|Estacion,data=CyL,method="ML")
anova(m1c,m1d)#El modelo m1d es el elegido
##3. Ajuste del modelo final con el método REML
m1d_final<-lme(value~1+año,random=~1|Estacion,method="REML",data=CyL)
summary(m1d_final)#Diferencias entre 2017 y 2016 respecto de 2014
#Hacemos comparaciones multiples
library(multcomp)
MC<-glht(m1d_final,linfct=mcp(año="Tukey"))
summary(MC)
#Se observan diferencias significativas entre 2014 y 2017 y 2017 con 2015 y2018

CyL$año<-relevel(CyL$año,ref="X2017")#Comparamos con 2017 el año con media mas elevada
m1d_final<-lme(value~1+año,random=~1|Estacion,method="REML",data=CyL)
summary(m1d_final)#Es significativamente superior a todos excepto a 2016




#Ploteamos los resultados
Res <- residuals(m1d_final, type = "normalized")
Fit <- fitted(m1d_final)
par(mfrow = c(2, 2))
plot(Res ~ Fit, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs. fitted")
abline(h = 0)
boxplot(Res ~ CyL$año, ylab = "Residuals", main = "Treatment")
abline(h = 0, lty = 3)
hist(Res, main = "Histogram of residuals", xlab = "Residuals")
qqnorm(Res)
qqline(Res)
library(lme4)
ezmodel<-ezMixed(CyL,dv=.(value),random = .(Estacion),fixed = .(año))
summary(ezmodel)
ezmodel$models
##################castilla de Leon#########################################################

par(mfrow=c(1,1))
#leon<-castilla_long[castilla_long$Provincia=="Leon",]
#eliminamos los NA´s
leon<-leon[!is.na(leon$Biomasa),]
#Comprobamos supuestos para modelos lineales
shapiro.test(leon$Biomasa)#No se puede presuponer normalidad
leveneTest(leon$Biomasa,leon$año)#Presumimos homogeneidad de varianzas
#Pese a no cumplir supuestos para un modelo lineal hacemos el anova
lm(leon$Biomasa~leon$año)
boxplot(leon$Biomasa~leon$año,xlab="Año",ylab="Densidad (g/m2)",main="Biomasas de trucha en León")
ml1<-aov(leon$Biomasa~leon$año)
summary(ml1)#Significación marginal
TukeyHSD(ml1)#Hay diferencia marginal entre el año 2014 y 2017 el resto no muestra diferencias significativas

#Consideramos medias repetidas
library(ez)
library(WRS2)
#Eliminamos los casos faltantes para balancear los castilla
remover<-function(data,dv,wid,with) {
  dato<-data[!is.na(data[,dv]),]
  tabla<-table(dato[,wid])
  completo<-labels(which(tabla==length(unique(dato[,with]))))
  leon<<-dato[dato[,wid]%in%completo,]
}
remover(leon,4,1,3)
ezANOVA(leon,dv=Biomasa,wid=Estacion,within=año)#Se muestran diferencias significativas asumiendo la corrección de la esfericidad.
#Hacemos comparaciones posthoc
with(leon,pairwise.t.test(value,año,p.adjust.method = "bonferroni",paired=T))#No se aprecian diferencias significativas
#Ploteamos las medias anuales
library(gplots)
plotmeans(value~año,xlab="Año",ylab="g/m2", main="Mean Plot\nwith 95% CI",data=leon)

#sin eliminar las estaciones con castilla faltantes analizamos el modelo mixto
leon<-castilla_long[castilla_long$Provincia=="Leon",]
#eliminamos los NA´s
leon<-leon[!is.na(leon$value),]
#Observamos los valores
ggplot(leon,aes(value))+geom_histogram()+facet_wrap(.~año)
boxplot(leon$value~leon$año,xlab="Año",ylab="Densidad (g/m2)",main="castilla biomasa de trucha en CyL")
library(nlme)
ml2<-lme(value~año,random=~1|Estacion,data=leon)
summary(ml2)
#Representamos los valores medios

plotmeans(value~año,xlab="Año",ylab="g/m2", main="Mean Plot\nwith 95% CI",data=leon)






