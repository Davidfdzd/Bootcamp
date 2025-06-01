# Cargar del dataset
data("AirPassengers")

# Inspeccionar la estructura
print(class(AirPassengers))     # Debe ser "ts"
print(summary(AirPassengers))   # Estadísticas descriptivas
print(start(AirPassengers))     # Inicio de la serie temporal
print(end(AirPassengers))       # Fin de la serie temporal
print(frequency(AirPassengers)) # Frecuencia mensual (12)

#Exploración inicial

##Gráfico de la serie temporal

plot(AirPassengers)

##Estadísticas descriptivas

mean(AirPassengers)
sd(AirPassengers)

#Análisis de tendencia y estacionalidad

descomposicion = decompose(AirPassengers)
plot(descomposicion)

#Análisis de estacionariedad

library(tseries)

##Autocorrelación y autocorrelación parcial

acf(AirPassengers)
pacf(AirPassengers)

##Prueba de Dickey-Fuller aumentada
adf.test(AirPassengers)

##Diferenciación si no es estacionaria
serie_diff = diff(AirPassengers)
plot(serie_diff)

adf.test(serie_diff)


#Detección de valores atípicos

boxplot(AirPassengers)

##Marcar picos visualmente
plot(AirPassengers)
abline(h = quantile(AirPassengers, 0.95), col = "red", lty = 2)


#Interpretación
##Respecto a la tendencia se aprecia un fuerte crecimiento a lo largo de los años.
##Estacionalidad: Hay picos que se repiten cada año sobre los meses de verano
##Estacionariedad: la serie original no es estacionaria
##Valores atípicos no encontramos en el boxplot, pero en el autocoorelacion parcial si