# Función para leer los números desde un archivo

leer_numeros <- function(nombre_archivo) {
  if (!file.exists(nombre_archivo)) {
    stop("El archivo no existe.")
  }
  numeros <- as.integer(readLines(nombre_archivo, warn = -1))
  return(numeros)
}

# Leer los números desde el archivo
numeros <- leer_numeros("numeros.txt")

# Calcular media, mediana y desviación estándar
media <- mean(numeros)
mediana <- median(numeros)
desviacion_estandar <- sd(numeros)

# Detectar alta variabilidad
if (desviacion_estandar > 10) {
  print("Alta variabilidad en los datos.")
}

# Calcular el cuadrado de cada número usando sapply()
cuadrados <- sapply(numeros, function(x) x^2)

# Crear el contenido del archivo de resultados
resultados <- c(
  "Estadísticos calculados:",
  paste("Media:", media),
  paste("Mediana:", mediana),
  paste("Desviación estándar:", desviacion_estandar),
  "Cuadrados de los números:",
  paste(cuadrados, collapse = ", ")
)

# Guardar los resultados en un archivo de texto
writeLines(resultados, "resultados.txt")

