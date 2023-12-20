######Ahora si este script es para modelar la severidad de los datos, monto de cada siniestro
library(dplyr)
library(ggplot2)
library(rriskDistributions)
library(fitdistrplus)
library(actuar)
library(fitur)
library(MASS)
library(moments)
library(nortest)
library(survival)
library(momentfit)
library(vcd)
library(fdth)
library(ExtDist)
library(statmod)
setwd("/Users/mariamielvs/Library/CloudStorage/OneDrive-UniversidadAutónomadelEstadodeMéxico/Actuaria/9no/Materias/Mod_Actuariales/Proyecto_Martha/Bases_Datos")
bd_t <- read.csv("bd_terrestres_ANIO_22.csv")
####################################################################################################################################################################

sini <- bd_t$SINIESTRO
frec <- bd_t$FRECUENCIA

k_sini<-kurtosis(sini) # 151.4806 kurtosis sin tantos valores extremos
s_sini<-skewness(sini) # 9.960233 existe asimetris de los datos
summary(sini) # mu = 648025 
sqrt(var(sini)) # sigma = 1616571
plot(density(sini), col='orange', xlab="Monto siniestros", ylab="Densidad", main="Densidad del Monto de Siniestros")

### se realizaran pruebas con distribuciones de colas pesadas
fit_we <- fitdist(sini,"weibull",method="mle")
fit_lnorm <- fitdist(sini,"lnorm",method="mle")
fit_pareto<- fitdist(sini,"pareto",method="mle")
fit_llog<-fitdist(sini,"llogis",method = "mle")

###### INVERSA GAUSSIANAN PARAMETROS######

#1) Dado que E(x) = Mu, entonces por mme Mu= Xbarra

mu5<- mean(sini)
#2) dado que Var (x) = [mu5^3]/theta, igual estimamos el parametros por metodo de lomentos
sum_cuad <- sum(sini^2) #Caulcula sumas cuadradas
n5<- length(sini)
tetha5<- (n5*(mu5)^3)/(sum_cuad)

fit_invgauss<-fitdist(sini,"invgauss",start = list(mean = 600000,shape=2000 ,dispersion = 1/89758.76))


######GAMMA### PARAMETROS####
fit_gamma <- fitdist(sini,"gamma",method="mle")### code 100

length(sini)
sumlog <- sum(log(sini))
media<-mean(sini)
fitgamma <- fitdist(sini,"gamma",method="mme")
alpha1<-fitgamma$estimate[1] #para theta es el 2
print(alpha1)
fg <- function(x){ 
  -((1800*x[1]*(log(x[1])-log(media)-1))-1800*log(gamma(x[1]))+sumlog*(x[1]-1))
 
}
#aplicar el nlm, no linial inixilization
funciong <- nlm(fg,alpha1)
alphagam<-funciong$estimate[1]
thetagam <- media/alphagam #4263.856

fit_gamma2 <- fitdist(sini,"gamma", start=list(shape=alphagam,scale=thetagam))
fit_gamma2$estimate[1]
fit_gamma2$estimate[2]
summary(fit_gamma2)

### PARA LOS IC (Intervalos de Confianza)
ic_w<-confint(fit_we) ### inversa de la matriz de informacion
ic_lnorm<-confint(fit_lnorm) 
ic_p<-confint(fit_pareto) 
ic_llog<-confint(fit_llog) 
ic_invg<-confint(fit_invgauss) 
ic_gam<-confint(fit_gamma2) 

####Prueba de bondad de ajuste
### AIC & BIC

sum_we <- summary(fit_we) ### Valores de AIC  y BIC mas bajos
sum_lnorm <- summary(fit_lnorm)
sum_pareto <- summary(fit_pareto)
sum_llog <- summary(fit_llog)
sum_invgauss <- summary(fit_invgauss)
sum_gamma2<-summary(fit_gamma2)

ks_w<-ks.test(sini,"pweibull", shape=sum_we$estimate[1], scale=sum_we$estimate[2] )
ks_lnorm<-ks.test(sini,"plnorm",meanlog=sum_lnorm$estimate[1],sdlog=sum_lnorm$estimate[2])
ks_p<-ks.test(sini,"ppareto",shape=sum_pareto$estimate[1],scale=sum_pareto$estimate[2]) ##n No aprueba, por las variaciones en la cola derecha
ks_llogis<-ks.test(sini,"pllogis",shape=sum_llog$estimate[1],scale=sum_llog$estimate[2])
ks_invgauss<-ks.test(sini,"pinvgauss",mean=sum_invgauss$estimate[1],shape=sum_invgauss$estimate[2],dispersion=sum_invgauss$estimate[3] )
ks_gamma<-ks.test(sini,"pgamma",shape=sum_gamma2$estimate[1],scale=sum_gamma2$estimate[2]) ##n No aprueba, por las variaciones en la cola derecha


ad_w<-ad.test(pweibull(sini, shape=sum_we$estimate[1], scale=sum_we$estimate[2]))
ad_lnorm<-ad.test(plnorm(sini,meanlog=sum_lnorm$estimate[1],sdlog=sum_lnorm$estimate[2]))
ad_p<-ad.test(ppareto(sini,shape=sum_pareto$estimate[1],scale=sum_pareto$estimate[2])) ##n No aprueba, por las variaciones en la cola derecha
ad_llogis<-ad.test(pllogis(sini,shape=sum_llog$estimate[1],scale=sum_llog$estimate[2]))
ad_invgauss<-ad.test(pinvgauss(sini,mean=sum_invgauss$estimate[1],shape=sum_invgauss$estimate[2],dispersion=sum_invgauss$estimate[3] ))
ad_gamma<-ad.test(pgamma(sini,shape=sum_gamma2$estimate[1],scale=sum_gamma2$estimate[2])) ##n No aprueba, por las variaciones en la cola derecha

##### Construccion de un dataframe #### o tabla
###columnas
DISTRIBUCION <- c("Weibull","LogNormal","Pareto","Loglogistic","Gaussiana-Inversa","Gamma")
PARAMETROS <- c(list(fit_we$estimate),list(fit_lnorm$estimate),list(fit_pareto$estimate),list(fit_llog$estimate),list(fit_invgauss$estimate),list(fit_gamma2$estimate) )
IC <- c(list(ic_w),list(ic_lnorm),list(ic_p),list(ic_llog),"Na",list(ic_gam))
KS_D <- c(ks_w$statistic,ks_lnorm$statistic,ks_p$statistic,ks_llogis$statistic,ks_invgauss$statistic,ks_gamma$statistic)
KS_P_value <- c(ks_w$p.value,ks_lnorm$p.value,ks_p$p.value,ks_llogis$p.value,ks_invgauss$p.value,ks_gamma$p.value)
AD_D<- c(ad_w$statistic,ad_lnorm$statistic,ad_p$statistic,ad_llogis$statistic,ad_invgauss$statistic,ad_gamma$statistic)
AD_P_value <- c(ad_w$p.value,ad_lnorm$p.value,ad_p$p.value,ad_llogis$p.value,ad_invgauss$p.value,ad_gamma$p.value)
AIC <- c(fit_we$aic,fit_lnorm$aic,fit_pareto$aic,fit_llog$aic,fit_invgauss$aic,fit_gamma2$aic )
BIC<-c(fit_we$bic,fit_lnorm$bic,fit_pareto$bic,fit_llog$bic,fit_invgauss$bic,fit_gamma2$bic )
LOKLIKE <- c(fit_we$loglik,fit_lnorm$loglik,fit_pareto$loglik,fit_llog$loglik,fit_invgauss$loglik,fit_gamma2$loglik )

resultados_sini<- data.frame(DISTRIBUCION,KS_D,KS_P_value,AD_D,AD_P_value,AIC,BIC,LOKLIKE)

cdfcomp(list(fit_we,fit_lnorm,fit_pareto,fit_llog,fit_invgauss,fit_gamma2))
ppcomp(list(fit_we,fit_lnorm,fit_pareto,fit_llog,fit_invgauss,fit_gamma2))

plot(density(sini), col='orange')

write.csv(resultados_sini,"resultados_siniestros_1.csv")


