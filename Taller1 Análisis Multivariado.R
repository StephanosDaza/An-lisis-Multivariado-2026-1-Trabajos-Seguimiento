if(!require(readxl)) install.packages("readxl")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(writexl)) install.packages("writexl")

library(readxl)
library(tidyverse)
library(writexl)

#FILTRADO Y TRATAMIENTO DE LA BASE DE DATOS

ruta_archivo <- file.choose()
datos_crudos <- read_excel(ruta_archivo)

datos_casanare <- datos_crudos %>%
  mutate(DEPARTAMENTO = toupper(DEPARTAMENTO)) %>%
  filter(DEPARTAMENTO == "CASANARE") %>%
  select(
    NATURALEZA,
    JORNADA,
    NOMBREMUNICIPIO,
    PROMLECTURACRITICA,
    PROMMATEMATICA,
    PROMSOCIALESYCIUDADANAS,
    PROMCIENCIASNATURALES,
    PROMINGLES
  ) %>%
  na.omit() 

write_xlsx(datos_casanare, "Base_Casanare_Final.xlsx")

#ENCABEZADO Y COLA DE LA MATRIZ DE LOS DATOS

str(datos_casanare)
head(datos_casanare)
tail(datos_casanare)


#PUNTO ESTADÍSTICAS DESCRIPTIVAS VARIABLES NUMÍERICAS INVOLUCRADAS: (VECTOR MEDIAS - MATRIZ(S) Y MATRIZ(R))

datos_numericos <- datos_casanare %>%
  select(where(is.numeric))

vector_medias <- colMeans(datos_numericos)
"Vector de Medias"
(round(vector_medias, 3))

matriz_cov <- cov(datos_numericos)
"Matriz de Covarianzas (S)"
(round(matriz_cov, 3))

matriz_cor <- cor(datos_numericos)
"Matriz de Correlaciones (R)"
(round(matriz_cor, 3))


pairs(datos_numericos, 
      main = "Matriz de Dispersión - Puntajes Casanare", 
      pch = 20, 
      col = "darkblue")

datos_largos <- datos_casanare %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "Materia", values_to = "Puntaje")

grafico_hist <- ggplot(datos_largos, aes(x = Puntaje)) +
  geom_histogram(binwidth = 5, fill = "#4E84C4", color = "white") +
  facet_wrap(~Materia, scales = "free") + 
  labs(title = "Distribución de Puntajes por Materia", 
       y = "Frecuencia (Cantidad de Colegios)",
       x = "Puntaje Promedio") +
  theme_minimal()
print(grafico_hist)


grafico_cajas_total <- ggplot(datos_largos, aes(x = reorder(Materia, Puntaje), y = Puntaje, fill = Materia)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() + 
  labs(title = "Comparación de Rendimiento entre Asignaturas",
       subtitle = "¿En qué materia les va mejor a los colegios de Casanare?",
       x = "",
       y = "Puntaje Promedio") +
  theme_minimal() +
  theme(legend.position = "none")
print(grafico_cajas_total)


#PUNTO DE TABLAS DE FRECUENCIA Y GRÁFICOS PARA VARIABLES CATEGÓRICAS

tabla_nat_limpia <- datos_casanare %>%
  count(NATURALEZA, name = "Cantidad") %>%
  mutate(Porcentaje = round((Cantidad / sum(Cantidad)) * 100, 2))
tabla_nat_limpia

tabla_jor_limpia <- datos_casanare %>%
  count(JORNADA, name = "Cantidad", sort = TRUE)
tabla_jor_limpia

tabla_mun_limpia <- datos_casanare %>%
  count(NOMBREMUNICIPIO, name = "Cantidad", sort = TRUE)
tabla_mun_limpia

ggplot(datos_casanare, aes(x = NATURALEZA)) +
  geom_bar(fill = "darkblue", color = "black", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Distribución de Instituciones Educativas",
       subtitle = "Departamento de Casanare (2016-2)",
       y = "Cantidad de Colegios",
       x = "Naturaleza (Oficial / No Oficial)") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5)

ggplot(tabla_mun_limpia, aes(x = reorder(NOMBREMUNICIPIO, Cantidad), y = Cantidad)) +
  geom_col(fill = "darkgreen", alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Colegios por Municipio", x = "", y = "Cantidad")

pie(tabla_jor_limpia$Cantidad, 
    labels = paste(tabla_jor_limpia$JORNADA, "-", tabla_jor_limpia$Cantidad), 
    col = c("#4E84C4", "#52854C", "#D16103", "#C4961A", "#FFDB6D"),
    main = "Proporción de Colegios por Jornada")


#PUNTO CRUCE DE VARIABLES NUMÉRICAS VS. VARIABLES CATEGÓRICAS

resumen_mun_ingles <- datos_casanare %>%
  group_by(NOMBREMUNICIPIO) %>%
  summarise(
    Media = mean(PROMINGLES),
    Desv = sd(PROMINGLES),
    Min = min(PROMINGLES),
    Max = max(PROMINGLES),
    n = n()
  ) %>%
  arrange(desc(Media)) 
resumen_mun_ingles

ggplot(datos_casanare, aes(x = reorder(NOMBREMUNICIPIO, PROMINGLES), y = PROMINGLES, fill = NOMBREMUNICIPIO)) +
  geom_boxplot() +
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none")


resumen_nat <- datos_casanare %>%
  group_by(NATURALEZA) %>%
  summarise(
    Media = mean(PROMMATEMATICA),
    Desv = sd(PROMMATEMATICA),
    Min = min(PROMMATEMATICA),
    Max = max(PROMMATEMATICA),
    n = n()
  )
resumen_nat

ggplot(datos_casanare, aes(x = NATURALEZA, y = PROMMATEMATICA, fill = NATURALEZA)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none")


resumen_jor <- datos_casanare %>%
  group_by(JORNADA) %>%
  summarise(
    Media = mean(PROMMATEMATICA),
    Desv = sd(PROMMATEMATICA),
    Min = min(PROMMATEMATICA),
    Max = max(PROMMATEMATICA),
    n = n()
  )
resumen_jor

ggplot(datos_casanare, aes(x = JORNADA, y = PROMMATEMATICA, fill = JORNADA)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none")



