#### AJUSTE NO PARAMETRICOS A LOS DATOS KERNEL

library(ks)
library(kdensity)

setwd("/Users/mariamielvs/Library/CloudStorage/OneDrive-UniversidadAutónomadelEstadodeMéxico/Actuaria/9no/Materias/Mod_Actuariales/Proyecto_Martha/Bases_Datos")
bd_t <- read.csv("bd_terrestres_ANIO_22.csv")
####################################################################################################################################################################

pagos_repetidos <- rep(bd_t$PAGO, bd_t$FRECUENCIA)
siniestros_repetidos <- rep(bd_t$SINIESTRO, bd_t$FRECUENCIA)
siniestro<- bd_t$SINIESTRO
# Crear un data frame con este vector
df_repetido <- data.frame(Siniestro = siniestros_repetidos)
df_pago <- data.frame(Pago = pagos_repetidos)
# Grafico de densidad
plot(density(siniestros_repetidos), col='orange', main="Monto de Siniestro x Frecuencia", xlab="Monto", ylab="Frecuencia/Densidad")



k_uniforme<-density(df_repetido$Siniestro, bw="ucv",
                    kernel = "rectangular")
k_triangular<-density(df_repetido$Siniestro, bw="ucv",
                      kernel = "triangular")
k_gamma<-kdensity(df_repetido$Siniestro,bw="RHE",kernel = "gamma")
coef(k_gamma)


#0.019652 es un coeficiente muy pequeño
plot(k_gamma,col="blue")
lines(k_uniforme,col="green")
lines(k_triangular,col="red")
lines(density(df_repetido$Siniestro),col="purple")

legend("topright", # posiciona la leyenda en la esquina superior derecha
       legend=c("Gamma", "Uniforme", "Triangular", "Densidad Siniestro"), 
       col=c("blue", "green", "red", "purple"), 
       lty=1, # tipo de línea, 1 = línea sólida
       cex=0.8) # tamaño del texto en la leyenda
##PARA LOS KERNEL ACUMULADOS:

# Función para calcular la CDF a partir de un objeto de densidad
calc_cdf <- function(density) {
  y_cdf <- cumsum(density$y) * diff(density$x[1:2])
  y_cdf <- y_cdf / max(y_cdf) # Normalizar
  list(x = density$x, y = y_cdf)
}

##calculando para cada distribucion
cdf_uniforme <- calc_cdf(k_uniforme)
cdf_triangular <- calc_cdf(k_triangular)
cdf_gamma <- calc_cdf(k_gamma)

###Grafica
plot(cdf_uniforme$x, cdf_uniforme$y, type="l", col="blue", ylim=c(0, 1), xlab="Siniestro", ylab="CDF", main="CDFs Comparadas")
lines(cdf_triangular$x, cdf_triangular$y, col="red")
lines(cdf_gamma$x, cdf_gamma$y, col="green")
legend("topright", legend=c("Uniforme", "Triangular", "Gamma"), col=c("blue", "red", "green"), lty=1)


