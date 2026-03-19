# ============================================================
# PARTE 1 - Carga, limpieza y estadísticas de resumen
# Taller Análisis Descriptivo - Coffee Quality
# ============================================================

# --- 1. Cargar librerías necesarias ---
library(dplyr)
library(ggplot2)

# --- 2. Cargar el dataset ---
datos <- read.csv("datos.csv", header = TRUE, stringsAsFactors = FALSE)

# --- 3. Exploración inicial ---
dim(datos)          # Número de filas y columnas
str(datos)          # Estructura y tipos de variables
head(datos, 10)     # Primeras 10 filas

# --- 4. Revisión de valores nulos ---
cat("Valores nulos por columna:\n")
colSums(is.na(datos))

# --- 5. Limpieza: Moisture_Percentage = 0 (dato atípico) ---
cat("\nMuestras con Moisture_Percentage = 0:\n")
print(datos[datos$Moisture_Percentage == 0, ])

# Se reemplaza el 0 por NA para excluirlo de los cálculos
datos$Moisture_Percentage[datos$Moisture_Percentage == 0] <- NA

# --- 6. Limpieza: estandarizar columna Color ---
# Revisar categorías actuales
cat("\nCategorías originales de Color:\n")
table(datos$Color)

datos$Color <- tolower(trimws(datos$Color))  # minúsculas y sin espacios

# Unificar variantes del mismo color
datos$Color[datos$Color %in% c("yellow-green", "yellow green", 
                               "yello-green", "yellow- green")] <- "yellow-green"
datos$Color[datos$Color %in% c("bluish-green", "blue-green")]    <- "blue-green"
datos$Color[datos$Color %in% c("browish-green")]                 <- "brownish-green"

cat("\nCategorías de Color después de limpiar:\n")
table(datos$Color)

# --- 7. Convertir Country_of_Origin y Color a factor ---
datos$Country_of_Origin <- as.factor(datos$Country_of_Origin)
datos$Color             <- as.factor(datos$Color)

# --- 8. Definir variables sensoriales relevantes ---
# Clean_Cup y Sweetness se excluyen (stdev = 0, no aportan información)
# Uniformity se excluye (stdev ≈ 0, casi constante)
vars_sensoriales <- c("Aroma", "Flavor", "Aftertaste", "Acidity",
                      "Body", "Balance", "Overall", "Total_Cup_Points")

# --- 9. Estadísticas de resumen generales ---
cat("\n--- RESUMEN GENERAL DEL DATASET ---\n")
summary(datos)

# --- 10. Estadísticas detalladas para variables sensoriales ---
cat("\n--- ESTADÍSTICAS DETALLADAS (variables sensoriales) ---\n")

resumen_sensorial <- datos %>%
  select(all_of(vars_sensoriales)) %>%
  summarise(across(everything(), list(
    Media    = ~round(mean(., na.rm = TRUE), 3),
    Mediana  = ~round(median(., na.rm = TRUE), 3),
    SD       = ~round(sd(., na.rm = TRUE), 3),
    Min      = ~round(min(., na.rm = TRUE), 3),
    Max      = ~round(max(., na.rm = TRUE), 3),
    Q1       = ~round(quantile(., 0.25, na.rm = TRUE), 3),
    Q3       = ~round(quantile(., 0.75, na.rm = TRUE), 3)
  )))

# Reorganizar en formato legible (una fila por variable)
resumen_largo <- data.frame(
  Variable = vars_sensoriales,
  Media    = sapply(vars_sensoriales, function(v) round(mean(datos[[v]], na.rm=TRUE), 3)),
  Mediana  = sapply(vars_sensoriales, function(v) round(median(datos[[v]], na.rm=TRUE), 3)),
  SD       = sapply(vars_sensoriales, function(v) round(sd(datos[[v]], na.rm=TRUE), 3)),
  Min      = sapply(vars_sensoriales, function(v) round(min(datos[[v]], na.rm=TRUE), 3)),
  Max      = sapply(vars_sensoriales, function(v) round(max(datos[[v]], na.rm=TRUE), 3)),
  Q1       = sapply(vars_sensoriales, function(v) round(quantile(datos[[v]], 0.25, na.rm=TRUE), 3)),
  Q3       = sapply(vars_sensoriales, function(v) round(quantile(datos[[v]], 0.75, na.rm=TRUE), 3))
)

print(resumen_largo)

# --- 11. Nota sobre variables excluidas ---
cat("\n--- VARIABLES EXCLUIDAS DEL ANÁLISIS ---\n")
cat("Clean_Cup  → SD =", round(sd(datos$Clean_Cup), 4),
    "| Todos los valores son 10. No aporta variabilidad.\n")
cat("Sweetness  → SD =", round(sd(datos$Sweetness), 4),
    "| Todos los valores son 10. No aporta variabilidad.\n")
cat("Uniformity → SD =", round(sd(datos$Uniformity), 4),
    "| Casi constante, no aporta información útil.\n")

# --- 12. Conteo de muestras por país ---
cat("\n--- MUESTRAS POR PAÍS ---\n")
conteo_pais <- datos %>%
  count(Country_of_Origin, name = "Muestras") %>%
  arrange(desc(Muestras))
print(conteo_pais)

# --- 13. Guardar dataset limpio para compartir con el grupo ---
write.csv(datos, "datos_limpios.csv", row.names = FALSE)

# ============================================================
# PARTE 4 - MATRIZ DE CORRELACIÓN Y CONCLUSIONES
# ============================================================

# --- 14. Seleccionar solo variables numéricas relevantes ---
datos_numericos <- datos %>%
  select(all_of(vars_sensoriales))
# Nos aseguramos de usar solo variables cuantitativas limpias

# --- 15. Calcular matriz de correlación ---
correlacion <- cor(datos_numericos, use = "complete.obs")
# use = "complete.obs" evita errores por NA

cat("\n--- MATRIZ DE CORRELACIÓN ---\n")
print(round(correlacion, 3))
# Redondeamos para mejor lectura

# --- 16. Relación con Total Cup Points ---
cat("\n--- CORRELACIÓN CON TOTAL CUP POINTS ---\n")

cor_total <- correlacion["Total_Cup_Points", ]
print(round(cor_total, 3))
# Extraemos solo la fila de interés

# --- 17. Identificar variables más influyentes ---
cor_total_sin_total <- cor_total[names(cor_total) != "Total_Cup_Points"]
# Quitamos la autocorrelación (1)

mayor_influencia <- sort(cor_total_sin_total, decreasing = TRUE)

cat("\nVariables más relacionadas con el puntaje total:\n")
print(round(mayor_influencia, 3))

# --- 18. Interpretación automática (TEXTO) ---
cat("\n--- INTERPRETACIÓN ---\n")

cat("Las variables con mayor correlación positiva con el puntaje total son:\n")
print(head(round(mayor_influencia, 3), 3))

cat("\nEsto indica que estas características sensoriales son las que más influyen en la calidad del café.\n")

# --- 19. (Opcional pero recomendado) Visualización ---
if (!"corrplot" %in% installed.packages()) {
  install.packages("corrplot")
}

library(corrplot)

corrplot(correlacion, method = "color", type = "upper", tl.cex = 0.8)
# Muestra mapa de calor de correlaciones

# --- 20. Conclusiones generales ---
cat("\n--- CONCLUSIONES GENERALES ---\n")

cat("
1. El análisis descriptivo permitió identificar la distribución de las variables sensoriales del café.
2. Se observó que algunas variables como Clean Cup y Sweetness no aportan variabilidad.
3. La matriz de correlación mostró relaciones fuertes entre varias características sensoriales.
4. Variables como Aroma, Flavor y Balance presentan alta correlación con el puntaje total.
5. Esto indica que estas dimensiones son determinantes en la calidad del café evaluado.
6. Los resultados son coherentes con el análisis previo por país y gráficos realizados.
7. En general, el dataset presenta consistencia y permite identificar patrones claros de calidad.
")

# --- 21. Nota final ---
cat("\nInforme listo para exportar a PDF.\n")

