######Ahora si este script es para modelar la severidad de los datos
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

#siniestros_repetidos <- rep(bd_t$SINIESTRO, bd_t$FRECUENCIA)
sini <- bd_t$SINIESTRO
frec <- bd_t$FRECUENCIA
# Crear un data frame con este vector
#df_repetido <- data.frame(Siniestro = siniestros_repetidos)

k_sini<-kurtosis(frec) # 722.0938 kurtosis sin tantos valores extremos
s_sini<-skewness(frec) # 24.34668
summary(frec) # mu = 648025 
sqrt(var(frec)) # sigma = 1616571
plot(density(frec), col='orange',xlab="Frecuencia", ylab="Densidad", main="Densidad de la frecuencia")

### se realizaran pruebas con distribuciones de colas pesadas

fit_nbinom <- fitdist(frec,"nbinom",method="mle")
fit_pois<- fitdist(frec,"pois",method="mle")
fit_geom<- fitdist(frec,"geom",method="mle")


### PARA LOS IC (Intervalos de Confianza)
ic_nbinom<-confint(fit_nbinom) ### inversa de la matriz de informacion
ic_pois<-confint(fit_pois) 
ic_geom<-confint(fit_geom) 


####Prueba de bondad de ajuste
### AIC & BIC

sum_nbinom<- summary(fit_nbinom) ### Valores de AIC  y BIC mas bajos
sum_pois<- summary(fit_pois)
sum_geom <- summary(fit_geom)

##### PRUEBAS DE BONDAD DE AJUSTE CON CHI-CUADRADA
frec_esperadas <- dnbinom(frec, size=sum_nbinom$estimate[1], mu=sum_nbinom$estimate[2]) * length(frec)
chisq.test(x=frec, p=frec_esperadas/sum(frec_esperadas))


#  Crear tabla de frecuencias observadas
frec_obs <- table(frec)

#  Calcular frecuencias esperadas para cada modelo
# Para la distribución negativa binomial
frec_esp_nbinom <- length(frec) * pnbinom(as.numeric(names(frec_obs)), size = fit_nbinom$estimate["size"], mu = fit_nbinom$estimate["mu"])

# Para la distribución de Poisson
frec_esp_pois <- length(frec) * ppois(as.numeric(names(frec_obs)), lambda = fit_pois$estimate["lambda"])
dnbinom(frec, size=sum_nbinom$estimate[1], mu=sum_nbinom$estimate[2]) * length(frec)
# Para la distribución geométrica
frec_esp_geom <- length(frec) * pgeom(as.numeric(names(frec_obs)), prob = fit_geom$estimate["prob"])

#  Aplicar la prueba chi-cuadrada
chisq_test_nbinom <- chisq.test(frec_obs, p = frec_esp_nbinom/sum(frec_esp_nbinom))
chisq_test_pois <- chisq.test(frec_obs, p = frec_esp_pois/sum(frec_esp_pois))
chisq_test_geom <- chisq.test(frec_obs, p = frec_esp_geom/sum(frec_esp_geom))

# Mostrar resultados
chisq_test_nbinom
chisq_test_pois
chisq_test_geom

##### Construccion de un dataframe #### o tabla
###columnas
DISTRIBUCIONd <- c("Binomia-Neg","Poisson","Geometrica")
PARAMETROSd <- c(list(fit_nbinom$estimate),list(fit_pois$estimate),list(fit_geom$estimate) )
ICd <- c(list(ic_nbinom),list(ic_pois),list(ic_geom))
ChiSq_D <- c(chisq_test_nbinom$statistic,chisq_test_pois$statistic,chisq_test_geom$statistic)
ChiSq_P_value <- c(chisq_test_nbinom$p.value,chisq_test_pois$p.value,chisq_test_geom$p.value)
AICd <- c(sum_nbinom$aic,sum_pois$aic,sum_geom$aic)
BICd<-c(sum_nbinom$bic,sum_pois$bic,sum_geom$bic)
LOKLIKEd <- c(sum_nbinom$loglik,sum_pois$loglik,sum_geom$loglik)

resultados_frec<- data.frame(DISTRIBUCIONd,ChiSq_D,ChiSq_P_value,AICd,BICd,LOKLIKEd)

cdfcomp(list(fit_nbinom,fit_pois,fit_geom))
ppcomp(list(fit_nbinom,fit_pois,fit_geom))

write.csv(resultados_frec,"resultados_frec_1.csv")

