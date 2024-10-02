library(reticulate)
library(tidyverse)

# Definir el código Python como una cadena de texto

codigo_python <- "
import os
import pandas as pd

# Obtener la ruta del archivo

ruta_file = os.getcwd()
print(f'La ruta del archivo: {ruta_file}')
dt = pd.read_csv(os.path.join(ruta_file, 'ObesityDataSet_raw_and_data_sinthetic.csv'))

# Modificar la columna Gender
dt['Gender'] = dt.apply(lambda row: 'NB' if 19 <= row['Age'] <= 23 else row['Gender'], axis=1)

# Modificar las columnas family_history_with_overweight, MTRANS y NObeyesdad
dt['family_history_with_overweight'] = dt['family_history_with_overweight'].str.replace('_', ' ').str.upper()
dt['MTRANS'] = dt['MTRANS'].str.replace('_', ' ').str.upper()
dt['NObeyesdad'] = dt['NObeyesdad'].str.replace('_', ' ').str.upper()

# Definir bins para las edades
bins = [0, 18, 30, 50, 70, 100]
labels = ['0-18', '19-30', '31-50', '51-69', '70-100']
dt['AgeGroup'] = pd.cut(dt['Age'], bins=bins, labels=labels, right=False)

# Crear una clasificación para hacer un grupo más pequeño de tipos de sobrepeso
clasificaciones = {
    'NORMAL WEIGHT': 'PESO NORMAL',
    'INSUFFICIENT WEIGHT': 'PESO INSUFICIENTE',
    'OBESITY TYPE I': 'OBESIDAD',
    'OBESITY TYPE II': 'OBESIDAD',
    'OBESITY TYPE III': 'OBESIDAD',
    'OVERWEIGHT LEVEL I': 'SOBREPESO',
    'OVERWEIGHT LEVEL II': 'SOBREPESO'
}

# Aplicar clasificación

dt['TipoPeso'] = dt['NObeyesdad'].map(clasificaciones)

# Agrupar y calcular promedio y cantidad de registros
dt_ag = dt.groupby(['family_history_with_overweight', 'Gender', 'AgeGroup', 'TipoPeso']).agg(
    promedio_Weight=('Weight', 'mean'),
    cantidad_registros=('Weight', 'size')
).reset_index()

# Crear tabla pivote
pivot_df = dt_ag.pivot_table(index=['family_history_with_overweight', 'Gender', 'AgeGroup'],
                             columns='TipoPeso',
                             values='cantidad_registros',
                             fill_value=0).reset_index()

print(pivot_df)
"

# Ejecutar el código Python
py_run_string(codigo_python)

# Convertir el DataFrame de Python a R
pivot_df <- py$pivot_df
dt_ag <- py$dt_ag



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

