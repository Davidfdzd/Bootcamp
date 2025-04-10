# PASO 1: configuración inicial

energia <- c(rep("Renovable", 10), rep("No Renovable", 10))
consumo <-  c(12, 15, NA, 14, 16, 13, 11, NA, 18, 17, 20, 19, 13, NA, 16, 14, 18, 19, 20, 16)
costo_kwh <- c(rep(0.10, 10), rep(0.15, 10))

# PASO 2: limpieza de los datos

consumo[is.na(consumo) & energia == "Renovable"] <- median(consumo[energia == "Renovable"], na.rm = TRUE)
consumo[is.na(consumo) & energia == "No Renovable"] <- median(consumo[energia == "No Renovable"], na.rm = TRUE)

# PASO 3: creación del dataframe

df_consumo <- data.frame(energia = energia, consumo = consumo, costo_kwh = costo_kwh)

# PASO 4: cálculos

df_consumo$costo_total <- df_consumo$consumo * df_consumo$costo_kwh

total_consumo_renovable <- sum(subset(df_consumo,energia == "Renovable")$consumo)
total_consumo_no_renovable <- sum(subset(df_consumo,energia == "No Renovable")$consumo)
total_costo_renovable <- sum(subset(df_consumo,energia == "Renovable")$costo_total)
total_costo_no_renovable <- sum(subset(df_consumo,energia == "No Renovable")$costo_total)

# PASO 5: resumen

df_costo_ordenado <- df_consumo[order(df_consumo$costo_total, decreasing =TRUE), ]

top_3_costos <- head(df_costo_ordenado, 3)

resumen_energia <- list(df = df_costo_ordenado, 
                        Total_consumo = c(Renovable = total_consumo_renovable, No_Renovable = total_consumo_no_renovable), 
                        Total_costo = c(Renovable = total_costo_renovable, No_Renovable = total_costo_no_renovable), 
                        top_3_costos = top_3_costos)
resumen_energia
