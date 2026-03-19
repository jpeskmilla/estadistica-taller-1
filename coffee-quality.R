#----------------------------------------------------------------------#
#                  Curso: Probabilidad y Estadística                   #
#                        Universidad del Valle                         #
#               Taller - Análisis Descriptivo de Datos                 #
#                  Coffee Quality Institute (CQI)                      #
#----------------------------------------------------------------------#

### Instalación y carga de paquetes
library(dplyr)
library(ggplot2)
library(tidyr)
library(moments)
library(corrplot)

#----------------------------------------------------------------------#
#                          CARGA DE DATOS                              #
#----------------------------------------------------------------------#

setwd("~/Estadística/Taller1/")  # Cambiar por la ruta del archivo
datos <- read.csv("datos.csv", header = TRUE, stringsAsFactors = FALSE)

dim(datos)
str(datos)
head(datos, 10)

#----------------------------------------------------------------------#
#                        LIMPIEZA DE DATOS                            #
#----------------------------------------------------------------------#

# Valores nulos y duplicados
colSums(is.na(datos))
sum(duplicated(datos))

# Moisture_Percentage = 0 es un dato inválido, se reemplaza por NA
datos$Moisture_Percentage[datos$Moisture_Percentage == 0] <- NA

# Estandarizar categorías de Color
datos$Color <- tolower(trimws(datos$Color))
datos$Color[datos$Color %in% c("yellow-green", "yellow green",
                               "yello-green", "yellow- green")] <- "yellow-green"
datos$Color[datos$Color %in% c("bluish-green", "blue-green")]    <- "blue-green"
datos$Color[datos$Color %in% c("browish-green")]                 <- "brownish-green"

# Convertir a factor
# Convertir a factor
datos$Country_of_Origin <- as.factor(datos$Country_of_Origin)
datos$Color             <- as.factor(datos$Color)
attach(datos)  

# Variables a analizar
# Clean_Cup y Sweetness tienen SD = 0, Uniformity SD ≈ 0 → se excluyen
vars_sensoriales <- c("Aroma", "Flavor", "Aftertaste", "Acidity",
                      "Body", "Balance", "Overall", "Total_Cup_Points")

# Verificar exclusión
sd(datos$Clean_Cup)
sd(datos$Sweetness)
sd(datos$Uniformity)

# Guardar dataset limpio
write.csv(datos, "datos_limpios.csv", row.names = FALSE)

#----------------------------------------------------------------------#
#                     ESTADÍSTICAS DE RESUMEN                         #
#----------------------------------------------------------------------#

summary(datos)

# Indicadores para Total_Cup_Points
mean(Total_Cup_Points)
median(Total_Cup_Points)
sd(Total_Cup_Points)
max(Total_Cup_Points) - min(Total_Cup_Points)
sd(Total_Cup_Points) / mean(Total_Cup_Points) * 100
quantile(Total_Cup_Points)
quantile(Total_Cup_Points, prob = seq(0, 1, length = 11))
skewness(Total_Cup_Points)
kurtosis(Total_Cup_Points)

# Tabla resumen para todas las variables sensoriales
resumen_largo <- data.frame(
  Variable  = vars_sensoriales,
  Media     = sapply(vars_sensoriales, function(v) round(mean(datos[[v]], na.rm = TRUE), 3)),
  Mediana   = sapply(vars_sensoriales, function(v) round(median(datos[[v]], na.rm = TRUE), 3)),
  SD        = sapply(vars_sensoriales, function(v) round(sd(datos[[v]], na.rm = TRUE), 3)),
  CV        = sapply(vars_sensoriales, function(v) round(sd(datos[[v]], na.rm = TRUE) /
                                                           mean(datos[[v]], na.rm = TRUE) * 100, 3)),
  Min       = sapply(vars_sensoriales, function(v) round(min(datos[[v]], na.rm = TRUE), 3)),
  Max       = sapply(vars_sensoriales, function(v) round(max(datos[[v]], na.rm = TRUE), 3)),
  Q1        = sapply(vars_sensoriales, function(v) round(quantile(datos[[v]], 0.25, na.rm = TRUE), 3)),
  Q3        = sapply(vars_sensoriales, function(v) round(quantile(datos[[v]], 0.75, na.rm = TRUE), 3)),
  IQR       = sapply(vars_sensoriales, function(v) round(IQR(datos[[v]], na.rm = TRUE), 3)),
  Asimetria = sapply(vars_sensoriales, function(v) round(skewness(datos[[v]], na.rm = TRUE), 3)),
  Curtosis  = sapply(vars_sensoriales, function(v) round(kurtosis(datos[[v]], na.rm = TRUE), 3))
)
print(resumen_largo)

# Conteo por país
conteo_pais <- datos %>%
  count(Country_of_Origin, name = "Muestras") %>%
  arrange(desc(Muestras))
print(conteo_pais)

#----------------------------------------------------------------------#
#                         GRÁFICOS                                     #
#----------------------------------------------------------------------#

# Histograma - Total Cup Points
g1 <- ggplot(datos, aes(x = Total_Cup_Points)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  theme_minimal() +
  labs(title = "Distribución del puntaje total del café",
       x = "Total Cup Points", y = "Frecuencia")

# Barras - Muestras por país
g2 <- ggplot(conteo_pais, aes(x = reorder(Country_of_Origin, -Muestras), y = Muestras)) +
  geom_bar(stat = "identity", fill = "orange") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Número de muestras por país", x = "", y = "Muestras")

# Torta - Color del grano
color_count <- datos %>%
  filter(!is.na(Color)) %>%
  count(Color)

g3 <- ggplot(color_count, aes(x = factor(1), y = n, fill = Color)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  theme_minimal() +
  labs(title = "Distribución por color del grano", x = "", y = "")

# Boxplot - Atributos sensoriales
datos_sensorial <- datos %>%
  select(Aroma, Flavor, Aftertaste, Acidity, Body, Balance) %>%
  pivot_longer(cols = everything(), names_to = "Atributo", values_to = "Puntaje")

g4 <- ggplot(datos_sensorial, aes(x = Atributo, y = Puntaje, fill = Atributo)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  theme_minimal() +
  labs(title = "Distribución de Atributos Sensoriales del Café",
       x = "Atributos", y = "Puntaje")

# Boxplot - Puntaje total por país
g5 <- ggplot(datos, aes(x = reorder(Country_of_Origin, Total_Cup_Points, FUN = median),
                        y = Total_Cup_Points, fill = Country_of_Origin)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Comparativa de Puntaje Total por País",
       x = "País de Origen", y = "Total Cup Points")

print(g1)
print(g2)
print(g3)
print(g4)
print(g5)

#----------------------------------------------------------------------#
#                      MATRIZ DE CORRELACIÓN                          #
#----------------------------------------------------------------------#

datos_numericos <- datos %>% select(all_of(vars_sensoriales))
correlacion     <- cor(datos_numericos, use = "complete.obs")

print(round(correlacion, 3))

# Correlación con Total_Cup_Points
cor_total <- correlacion["Total_Cup_Points", ]
cor_total <- sort(cor_total[names(cor_total) != "Total_Cup_Points"], decreasing = TRUE)
print(round(cor_total, 3))

corrplot(correlacion, method = "color", type = "upper", tl.cex = 0.8)

#----------------------------------------------------------------------#
#                    ANÁLISIS POR NIVEL DE CALIDAD                    #
#----------------------------------------------------------------------#

alta  <- datos[datos$Total_Cup_Points >= 85, ]
media <- datos[datos$Total_Cup_Points <  83, ]

for (v in c("Aroma", "Flavor", "Aftertaste", "Acidity", "Body", "Balance", "Overall")) {
  cat(v, "| Alta:", round(mean(alta[[v]], na.rm = TRUE), 3),
      " Media:", round(mean(media[[v]], na.rm = TRUE), 3), "\n")
}

# Ranking de países
ranking_paises <- datos %>%
  group_by(Country_of_Origin) %>%
  summarise(Promedio = round(mean(Total_Cup_Points, na.rm = TRUE), 3),
            Muestras = n()) %>%
  arrange(desc(Promedio))
print(ranking_paises)