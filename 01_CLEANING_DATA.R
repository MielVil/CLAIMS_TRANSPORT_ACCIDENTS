### El proposito de este script es el de analisar la estructura de la BD
### A demas de limpiar la base

library(dplyr)
library(ggplot2)

setwd("/Users/mariamielvs/Library/CloudStorage/OneDrive-UniversidadAutónomadelEstadodeMéxico/Actuaria/9no/Materias/Mod_Actuariales/Proyecto_Martha/Bases_Datos")
db <- read.csv("maritimo_transportes.csv")

###Nos enforcaremos en el medio terrestre

db_filtrado <- db %>% 
  select(ANIO,MEDIO.DE.TRANSPORTE,NUMERO.DE.SINIESTROS,MONTO.DEL.SINIESTRO,MONTO.PAGADO)
### Se observan valores negativos en la base de datos
### estos valores negativos seran eliminados:
## Se condiciona a que los siniestros y montos de pago sean positivos

df <- subset(db_filtrado, MONTO.DEL.SINIESTRO > 0 & MONTO.PAGADO > 0)

## para facilitar el manojo de la base de datos se cambiara el nombre de las columnas

df <- df %>% 
  rename(TRANSPORTE=MEDIO.DE.TRANSPORTE,SINIESTRO=MONTO.DEL.SINIESTRO, FRECUENCIA=NUMERO.DE.SINIESTROS, PAGO=MONTO.PAGADO )
## verificaremos si existen valores NA en el dataframe

# Contar el número de NA en la columna "SINIESTRO"
numero_de_na <- sum(is.na(df$PAGO))
# Imprimir el resultado
print(numero_de_na)
## AL PARECER NO HAY VALORES NA EN EL DATA FRAME

### filtramos por medio de transporte Terrestre
bd_ter <- df %>% 
  filter(TRANSPORTE=="Terrestre" & ANIO==2020 )
table(bd_ter$TRANSPORTE) # Verificamos que solo se cuente con el tipo de transporte terrestre

## Se procede a guardar la base de datos en formato .csv 
write.csv(bd_ter, "bd_terrestres_ANIO_20.csv")

