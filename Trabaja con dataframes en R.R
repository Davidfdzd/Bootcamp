#Cargar las librerias y los datos
library(dplyr)
library(tidyr)

data(mtcars)
df <- as.data.frame(mtcars)

print(df)

#Selección de columnas y filtrado de filas
 df_filter <- df %>% 
   select(mpg, cyl, hp, gear) %>% 
   filter(cyl > 4)

 print(df_filter)
 
 #Ordenación y renombrado de columnas
 df_ordenado <- df_filter %>% 
   arrange(desc(hp)) %>% 
   rename(consumo = mpg, potencia = hp)
 
 print(df_ordenado)
 
 #Creación de nuevas columnas y agregación de datos
 df_eficiencia <- df_ordenado %>% 
   mutate(eficiencia = consumo / potencia)
 
 print(df_eficiencia)
 
 df_agrupado <- df_eficiencia %>% 
   group_by(cyl) %>% 
   summarise(consumo_medio = mean(consumo),
             potencia_maxima = max(potencia),
             .groups = 'drop')
 
 print(df_agrupado)
 
 #Creación del segundo dataframe y unión de dataframes
 transmisiones <- data.frame(gear = c(3, 4, 5),
   tipo_transmision = c("Manual", "Automática", "Semiautomática"))
 
 df_join <- df_eficiencia %>% 
   left_join(transmisiones, by = "gear")
 
 print(df_join)
 
 #Trasformación de los datos
 formato_largo <- df_join %>% 
   pivot_longer(cols = c(consumo, potencia, eficiencia),
                names_to = "medida", values_to = "valor")
 
 print(formato_largo)
 
 df_duplicados <- formato_largo %>% 
   group_by(cyl, gear, tipo_transmision, medida) %>% 
   summarise(n = n(), .groups = "drop") %>% 
   filter(n > 1)
 
 print(df_duplicados)
 
 df_ancho <- formato_largo %>% 
   group_by(cyl, gear, tipo_transmision, medida) %>% 
   summarise(valor = mean(valor), .groups = "drop") %>% 
   pivot_wider(names_from = medida, values_from = valor)
 
 print(df_ancho)
 