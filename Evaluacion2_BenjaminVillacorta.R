# Cargar las librerías necesarias

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)


# Obtener la ruta del archivo
#ruta_file <- paste(getwd(),"/Python/CursoML/")
ruta_file <- getwd()
print(paste("La ruta del archivo :", ruta_file))
dt <- read_csv(file.path(ruta_file, "ObesityDataSet_raw_and_data_sinthetic.csv"))

# Modificar la columna Gender
dt <- dt %>%
  mutate(Gender = ifelse(Age >= 19 & Age <= 23, 'NB', Gender))

# Modificar las columnas family_history_with_overweight, MTRANS y NObeyesdad
dt <- dt %>%
  mutate(family_history_with_overweight = toupper(gsub("_", " ", family_history_with_overweight)),
         MTRANS = toupper(gsub("_", " ", MTRANS)),
         NObeyesdad = toupper(gsub("_", " ", NObeyesdad)))

# Definir bins para las edades
bins <- c(0, 18, 30, 50, 70, 100)
labels <- c('0-18', '19-30', '31-50', '51-69', '70-100')
dt$AgeGroup <- cut(dt$Age, breaks = bins, labels = labels, right = FALSE)

# Crear una clasificación para hacer un grupo más pequeño de tipos de sobrepeso
clasificaciones <- c('NORMAL WEIGHT' = 'PESO NORMAL',
                     'INSUFFICIENT WEIGHT' = 'PESO INSUFICIENTE',
                     'OBESITY TYPE I' = 'OBESIDAD',
                     'OBESITY TYPE II' = 'OBESIDAD',
                     'OBESITY TYPE III' = 'OBESIDAD',
                     'OVERWEIGHT LEVEL I' = 'SOBREPESO',
                     'OVERWEIGHT LEVEL II' = 'SOBREPESO')

# Aplicar clasificación
dt$TipoPeso <- recode(dt$NObeyesdad, !!!clasificaciones)

# Agrupar y calcular promedio y cantidad de registros
dt_ag <- dt %>%
  group_by(family_history_with_overweight, Gender, AgeGroup, TipoPeso) %>%
  summarise(promedio_Weight = mean(Weight, na.rm = TRUE),
            cantidad_registros = n()) %>%
  ungroup()

# Crear tabla pivote
pivot_df <- dt_ag %>%
  pivot_wider(names_from = TipoPeso, values_from = cantidad_registros, values_fill = 0)

# Crear el gráfico de barras apiladas
pivot_df %>%
  gather(key = "TipoPeso", value = "cantidad_registros", -family_history_with_overweight, -Gender) %>%
  ggplot(aes(x = interaction(family_history_with_overweight, Gender), y = cantidad_registros, fill = TipoPeso)) +
  geom_bar(stat = "identity") +
  labs(x = "Tiene antecedentes familiares con sobrepeso", y = "Cantidad de casos", title = "Relación de antecedentes familiares de personas con sobrepeso") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Agrupar por clasificación y género
dt_ag2 <- dt_ag %>%
  group_by(TipoPeso) %>%
  summarise(cantidad_registros = sum(cantidad_registros))

dt_ag3 <- dt_ag %>%
  group_by(TipoPeso, Gender, AgeGroup) %>%
  summarise(cantidad_registros = sum(cantidad_registros))

# Crear gráfico de pastel
ggplot(dt_ag2, aes(x = "", y = cantidad_registros, fill = TipoPeso)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Distribución de pesos de la población") +
  theme_minimal()

# Crear gráfico de dispersión
colors <- c('Female' = 'red', 'Male' = 'blue', 'NB' = 'green')
ggplot(dt_ag3, aes(x = cantidad_registros, y = TipoPeso, color = Gender)) +
  geom_point() +
  scale_color_manual(values = colors) +
  labs(title = "Relación entre Cantidad y Tipo de Obesidad", x = "Cantidad", y = "Tipo de Obesidad") +
  theme_minimal()

# Crear gráfico de barras agrupadas por rango de edad, tipo de peso y género
ggplot(dt_ag3, aes(x = AgeGroup, y = cantidad_registros, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ TipoPeso) +
  labs(title = "Distribución de Cantidad por rango de edad, tipo de peso y Género", x = "Rango de edad y tipo peso", y = "Cantidad") +
  theme_minimal()

# Crear gráfico de barras agrupadas por promedio de peso
ggplot(dt_ag, aes(x = AgeGroup, y = promedio_Weight, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ TipoPeso) +
  labs(title = "Distribución de Promedio de Peso por rango de edad, tipo de peso y Género", x = "Rango de edad y tipo peso", y = "Promedio") +
  theme_minimal()

