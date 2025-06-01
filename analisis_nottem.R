# Cargar del dataset y librerías
library(tseries)
library(ggplot2)

data("nottem")

##Inspeccionar la estructura
print(class(nottem))
print(summary(nottem))

##Graficar la serie temporal
plot(nottem, main = "Temperaturas Mensuales en Nottingham (1920-1939)",
     xlab = "Año", ylab = "Temperatura", col = "blue")

#Exploración y preparación de datos

descomposicion = decompose(nottem)
plot(descomposicion)

#Análisis de estacionaridad
##ACF y PACF

acf(nottem)
pacf(nottem)

##Prueba Dickey-Fuller
adf.test(nottem)

nottem_diff = diff(nottem)
plot(nottem_diff)
adf.test(nottem_diff)

##ACF y PACF después de diferenciar

acf(nottem_diff)
pacf(nottem_diff)

#Detección de outliers
##boxplot

boxplot((nottem))

##Valores extremos verlos visualmente en la serie

plot(nottem)
abline(h = boxplot.stats(nottem)$stats[c(1,5)], col = "red", lty = 2)

#INTERPRETACIÓN
# - La serie es estacional, se repite cada 12 meses
# - Probablemente no sea estacionaria al inicio, pero sí tras diferenciar
# - Valores extremos en verano/invierno
# - Tendencia plana a lo largo del tiempo
# - No hay outliers en el boxplot