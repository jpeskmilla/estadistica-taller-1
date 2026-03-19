# ============================================================
# PARTE 1 - Carga, limpieza y estadísticas de resumen
# Taller Análisis Descriptivo - Coffee Quality
# ============================================================
install.packages("dplyr")
install.packages("moments")
install.packages("e1071")
install.packages("corrplot")
# --- 1. Cargar librerías necesarias ---
library(dplyr)
library(ggplot2)
library(tidyr)

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
cat("\nArchivo 'datos_limpios.csv' guardado exitosamente.\n")
cat("Comparte este archivo con tus compañeros para que todos trabajen con los mismos datos.\n")

# --- 14. GRÁFICOS ---

#HISTOGRAMA
print(
  ggplot(datos, aes(x = Total_Cup_Points)) +
    geom_histogram(fill = "skyblue", color = "black", bins = 20) +
    ggtitle("Distribución del puntaje total del café")
) 

#DIAGRAMA DE BARRAS
print(
  ggplot(conteo_pais, aes(x = reorder(Country_of_Origin, -Muestras), y = Muestras)) +
    geom_bar(stat = "identity", fill = "orange") +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle("Número de muestras por país")
)

#BOXBLOT
print(
  ggplot(datos, aes(x = Country_of_Origin, y = Total_Cup_Points)) +
    geom_boxplot(fill = "lightgreen") +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle("Distribución del puntaje por país")
)

#DIAGRAMA DE TORTA
color_count <- datos %>%
  filter(!is.na(Color)) %>%
  count(Color)

p <- ggplot(color_count, aes(x = factor(1), y = n, fill = Color)) +
  geom_bar(stat = "identity") +
  coord_polar("y")

print(p)

#MATRIZ DE CORRELACION
library(corrplot)

correlacion <- cor(datos[, vars_sensoriales], use = "complete.obs")

corrplot(correlacion, method = "color", type = "upper")
