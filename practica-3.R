---
title: "MPF: Trabajo Práctico 2"
author: "Gonzalo Barrera Borla"
date: "Marzo 31, 2017"
output: 
  html_document
---

```{r knitr_options, include=FALSE}
library(knitr)
opts_chunk$set(fig.width=12, fig.height=6, message=FALSE, comment=NA)
```

# Acondicionamiento Inicial
## Carga de librerías

```{r librerias, warning=FALSE}
library(tidyverse)
```

## Carga de datos
```{r carga_datos}
autos <- read_delim(file = "../Datos/autos.txt", delim = " ", 
                    col_types =  cols(precio = col_double(), calidad = col_double()))
```

## Diagrama de dispersión precio-calidad
```{r dispersion_precio_calidad}
autos %>% 
  ggplot(mapping = aes(x = calidad, y = precio)) +
  geom_point() +
  scale_y_continuous(labels = dollar) -> fig1
ggsave("fig1_dispersion_precio_calidad.png", plot = fig1, width = 12, height = 8)
fig1
```

```{r declaracion_modelos}
# Formulas por modelo
# Solo ordenada
f <- formula(calidad ~ 1)
# Lineal en precio sin ordenada
g <- formula(calidad ~ 0 + precio)
# Lineal en precio con ordenada
h <- formula(calidad ~ precio)
formulas_modelos <- list(f, g ,h)
ajustar_reglin <- function(datos, formula) { lm(formula, datos) }

# No sé como llamar a esta variable expresivamente
tibble(id = 1:length(formulas_modelos),
       formula = formulas_modelos,
       name = list("Y = b0", "Y = b1 * precio", "Y = b0 + b1 * precio"),
       datos = rep(list(autos), 3)) %>%
  # Ajusto una regresion lineal sobre `datos` segun `formula` y guardo el objeto resultado en `resultados`.
  mutate(resultados = map2(formula, datos, lm)) %>%
  # Utilizo `predict` para predecir sobre un dataset con una sola fila de precio = 50000
  mutate(prediccion_50k = map_dbl(resultados, ~predict(., tibble(precio = 50000)))) -> df
```

Divido el conjunto de datos en train y test y ajusto el modelo nuevamente

```{r split_train_test}
test_frac <- 0.3
n <- dim(autos)[1]
df %>%
  # n sera igual a la cantidad de filas del dataframe en la columna `datos`
  mutate(idx_test = replicate(3, list(sample(1:n, test_frac*n)))) %>%
  mutate(idx_train = map(idx_test, ~setdiff(1:100, .))) %>%
  mutate(datos_test = map2(datos, idx_test, slice),
         datos_train = map2(datos, idx_train, slice)) -> df

df %>%
  # Entreno el modelo sobre los datos de entrenamiento
  mutate(resultados_train = map2(formula, datos_train, lm)) %>%
  # Genero predicciones para los datos de prueba
  mutate(datos_test = map2(resultados_train, datos_test, ~broom::augment(.x, newdata = .y))) -> df

# Calculo RSE por modelo
calcular_rse <- function(df) { mean((df$calidad-df$.fitted)^2) }
df %>% mutate(rse_test = map_dbl(datos_test, calcular_rse)) -> df


```
