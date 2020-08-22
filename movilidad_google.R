library(tidyverse)
library(ggmap)
library(lubridate)
#vamos a cargar la libreria gganimate para animar gráficos que vamos a hacer con ggplot
library(gganimate)
#estas librerías nos sirven para acomodar nuestro gráfico
library(transformr)
library(viridis)
library(hrbrthemes)
#llamamos a las librerías que vamos a usar para trabajar con datos geoespaciales
library(sf)
library(tidyverse)


options(scipen = 999)

#comunas_geo
comunas <- st_read('https://bitsandbricks.github.io/data/CABA_comunas.geojson')
comunas <- rename(comunas, comuna = comunas)

#movilidad google
movilidad_google <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=56adbfa96dfa23c3")
movilidad_google <- movilidad_google %>% filter(country_region_code == "AR")

#vemos qué hay dentro de la variable sub_region_1
levels(movilidad_google$sub_region_1)

#filtramos solo la data correspondiente a Buenos Aires
movilidad_google_buenos_aires <- movilidad_google %>% filter(sub_region_1 == "Buenos Aires")
summary(movilidad_google_buenos_aires)

#dejamos fuera las siguientes columnas
movilidad_google_buenos_aires <- select(movilidad_google_buenos_aires, -metro_area, -iso_3166_2_code, -census_fips_code)

#como las comunas en nuestro dataset "comunas" aparecen con números y en el dataset de Google con comuna + número, vamos a renombrar.
comunas$comuna <- (comunas$comuna = case_when(comunas$comuna == 1 ~ "Comuna 1",
                                              comunas$comuna == 2 ~ "Comuna 2",
                                              comunas$comuna == 3 ~ "Comuna 3",
                                              comunas$comuna == 4 ~ "Comuna 4",
                                              comunas$comuna == 5 ~ "Comuna 5",
                                              comunas$comuna == 6 ~ "Comuna 6",
                                              comunas$comuna == 7 ~ "Comuna 7",
                                              comunas$comuna == 8 ~ "Comuna 8",
                                              comunas$comuna == 9 ~ "Comuna 9",
                                              comunas$comuna == 10 ~ "Comuna 10",
                                              comunas$comuna == 11 ~ "Comuna 11",
                                              comunas$comuna == 12 ~ "Comuna 12",
                                              comunas$comuna == 13 ~ "Comuna 13",
                                              comunas$comuna == 14 ~ "Comuna 14",
                                              comunas$comuna == 15 ~ "Comuna 15"))

#dejamos fuera columnas que no nos interesan
comunas <- select(comunas, -barrios, -perimetro, -area)

#renombramos la columna que contiene las comunas para poder crear un nuevo dataset
movilidad_google_buenos_aires <- movilidad_google_buenos_aires %>% rename(comuna = sub_region_2)

#unimos el dataset con la información de movilidad de google con el de comunas para darle atributos geoespaciales
movilidad_comunas <- inner_join(movilidad_google_buenos_aires, comunas, by = "comuna")

#vamos a borrar el _percent_change_from_baseline de las columnas
movilidad_comunas <- movilidad_comunas %>% rename(
  retail_and_recreation = retail_and_recreation_percent_change_from_baseline,
  grocery_and_pharmacy = grocery_and_pharmacy_percent_change_from_baseline,
  parks = parks_percent_change_from_baseline,
  transit_stations = transit_stations_percent_change_from_baseline,
  workplaces = workplaces_percent_change_from_baseline,
  residential = residential_percent_change_from_baseline)

movilidad_comunas$date <- as_date( movilidad_comunas$date)

#movilidad en tiendas y lugares de ocio
  tiendas_y_ocio <- ggplot(movilidad_comunas) +
  geom_sf(data = movilidad_comunas$geometry, aes(fill = movilidad_comunas$retail_and_recreation)) +
  transition_time(movilidad_comunas$date) +
    scale_fill_distiller(palette =  "Spectral") +
  labs(title = "Tendencias en la movilidad en tiendas y lugares de ocio - CABA",
       subtitle = "Cero es el estandar de normalidad.
       Fecha: {(frame_time)}",
       x = "",
       y = "",
       fill = "% variación",
       caption = "fuente: Google - COVID-19 Community Mobility Report") +
  theme_minimal()

animate(tiendas_y_ocio, fps=1)

anim_save("movilidad_tiendas_y_ocio.gif")


#movilidad en supermercados y farmacias
supermercados_y_farmacias <- ggplot(movilidad_comunas) +
  geom_sf(data = movilidad_comunas$geometry, aes(fill = movilidad_comunas$grocery_and_pharmacy)) +
  transition_time(movilidad_comunas$date) +
  scale_fill_distiller(palette =  "Spectral") +
  labs(title = "Tendencias en la movilidad en supermercados
       y farmacias - CABA",
       subtitle = "Cero es el estandar de normalidad.
       Fecha: {(frame_time)}",
       x = "",
       y = "",
       fill = "% variación",
       caption = "fuente: Google - COVID-19 Community Mobility Report") +
  theme_minimal()

animate(supermercados_y_farmacias, fps=1)

anim_save("movilidad_supermercados_y_farmacias.gif")




#movilidad en parques
parques <- ggplot(movilidad_comunas) +
  geom_sf(data = movilidad_comunas$geometry, aes(fill = movilidad_comunas$parks)) +
  transition_time(movilidad_comunas$date) +
  scale_fill_distiller(palette =  "Spectral") +
  labs(title = "Tendencias en la movilidad en parques - CABA",
       subtitle = "Cero es el estandar de normalidad.
       Fecha: {(frame_time)}",
       x = "",
       y = "",
       fill = "% variación",
       caption = "fuente: Google - COVID-19 Community Mobility Report") +
  theme_minimal()

animate(parques, fps=1)


anim_save("movilidad_parques.gif")




#movilidad en estaciones de transporte
estaciones_transporte <- ggplot(movilidad_comunas) +
  geom_sf(data = movilidad_comunas$geometry, aes(fill = movilidad_comunas$transit_stations)) +
  transition_time(movilidad_comunas$date) +
  scale_fill_distiller(palette =  "Spectral") +
  labs(title = "Tendencias en la movilidad en estaciones de transporte - CABA",
       subtitle = "Cero es el estandar de normalidad.
       Fecha: {(frame_time)}",
       x = "",
       y = "",
       fill = "% variación",
       caption = "fuente: Google - COVID-19 Community Mobility Report") +
  theme_minimal()

animate(estaciones_transporte, fps=1)


anim_save("movilidad_estaciones_transport.gif")





#movilidad en lugares de trabajo
lugares_trabajo <- ggplot(movilidad_comunas) +
  geom_sf(data = movilidad_comunas$geometry, aes(fill = movilidad_comunas$workplaces)) +
  transition_time(movilidad_comunas$date) +
  scale_fill_distiller(palette =  "Spectral") +
  labs(title = "Tendencias en la movilidad en lugares de trabajo - CABA",
       subtitle = "Cero es el estandar de normalidad.
       Fecha: {(frame_time)}",
       x = "",
       y = "",
       fill = "% variación",
       caption = "fuente: Google - COVID-19 Community Mobility Report") +
  theme_minimal()

animate(lugares_trabajo, fps=1)


anim_save("movilidad_lugares_trabajo.gif")




#movilidad en zonas residenciales
zonas_residenciales <- ggplot(movilidad_comunas) +
  geom_sf(data = movilidad_comunas$geometry, aes(fill = movilidad_comunas$residential)) +
  transition_time(movilidad_comunas$date) +
  scale_fill_distiller(palette =  "Spectral") +
  labs(title = "Tendencias en la movilidad en zonas residenciales - CABA",
       subtitle = "Cero es el estandar de normalidad.
       Fecha: {(frame_time)}",
       x = "",
       y = "",
       fill = "% variación",
       caption = "fuente: Google - COVID-19 Community Mobility Report") +
  theme_minimal()

animate(zonas_residenciales, fps=1)


anim_save("movilidad_zonas_residenciales.gif")
