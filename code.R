library(tidyverse)
library(sf)
library(ggplot2)
library(cholera)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(osrm)


data(package = "cholera")

head(fatalities)              # Muestra las primeras filas de las muertes por cólera
plot(pumps)                # Grafica la ubicación de las bombas
summary(road.segments)        # Resumen de los segmentos de calle




# Cargar los datos de casos y bombas
cases <- fatalities        # Datos de muertes por cólera
pumps <- pumps                # Ubicación de las bombas de agua



# Convertir casos a objeto sf
cases_sf <- st_as_sf(fatalities, coords = c("lon", "lat"), crs = 4326)

# Convertir bombas a objeto sf
pumps_sf <- st_as_sf(pumps, coords = c("lon", "lat"), crs = 4326)




# Convertir a objetos espaciales usando lon/lat
cases_sf <- st_as_sf(fatalities, coords = c("lon", "lat"), crs = 4326)
pumps_sf <- st_as_sf(pumps, coords = c("lon", "lat"), crs = 4326)

# Crear el gráfico
ggplot() +
  geom_sf(data = cases_sf, color = "red", size = 1, alpha = 0.6) +
  geom_sf(data = pumps_sf, color = "blue", shape = 17, size = 2) +
  labs(
    title = "Brote de cólera en Londres (1854)",
    subtitle = "Visualización de muertes y ubicación de bombas de agua según el mapa de John Snow",
    caption = "Fuente: Paquete {cholera} en R, basado en datos históricos de Dodson y Tobler"
  ) +
  theme_minimal()

# Transforma los datos a una proyección métrica (EPSG:3857) para poder calcular distancias en metros.



# Convertir a objetos espaciales con WGS84
cases_sf <- st_as_sf(fatalities, coords = c("lon", "lat"), crs = 27700)
pumps_sf <- st_as_sf(pumps, coords = c("lon", "lat"), crs = 27700)

# Transformar a proyección métrica (EPSG:3857)
cases_m <- st_transform(cases_sf, crs =27700)
pumps_m <- st_transform(pumps_sf, crs =27700)



# Generar buffers de 100 metros alrededor de cada bomba
pumps_buffer <- st_buffer(pumps_m, dist = 100)

# 8_Calcula cuántos casos hay dentro del buffer de cada bomba.
#¿Cuál bomba tiene más casos cercanos?


# 1. Convertir a objetos espaciales
cases_sf <- st_as_sf(fatalities, coords = c("lon", "lat"), crs = 4326)
pumps_sf <- st_as_sf(pumps, coords = c("lon", "lat"), crs = 4326)

# 2. Transformar a proyección métrica
cases_m <- st_transform(cases_sf, crs = 3857)
pumps_m <- st_transform(pumps_sf, crs = 3857)

# 3. Crear buffers de 100 metros
pumps_buffer <- st_buffer(pumps_m, dist = 100)

# 4. Identificar qué casos caen dentro de cada buffer
cases_in_buffer <- st_join(cases_m, pumps_buffer, join = st_within)

# 5. Contar casos por bomba
cases_count <- cases_in_buffer %>%
  group_by(id) %>%  # 'id' es el identificador de cada bomba
  summarise(casos = n()) %>%
  arrange(desc(casos))

# 6. Mostrar resultados
print(cases_count)


#



# Convertir a objetos espaciales
cases_sf <- st_as_sf(fatalities, coords = c("lon", "lat"), crs = 4326)
pumps_sf <- st_as_sf(pumps, coords = c("lon", "lat"), crs = 4326)

# Transformar a proyección métrica para medir distancias en metros
cases_m <- st_transform(cases_sf, crs = 3857)
pumps_m <- st_transform(pumps_sf, crs = 3857)

# Crear buffers de 100 metros alrededor de cada bomba
pumps_buffer <- st_buffer(pumps_m, dist = 100)

# Identificar qué casos caen dentro de cada buffer
cases_in_buffer <- st_join(cases_m, pumps_buffer, join = st_within)

# Contar casos por bomba
tabla_final <- cases_in_buffer %>%
  group_by(id) %>%
  summarise(`Casos dentro del buffer` = n()) %>%
  rename(`ID de bomba` = id) %>%
  arrange(desc(`Casos dentro del buffer`))

# Mostrar la tabla
final <- print(tabla_final)


# 




# Convertir a objetos espaciales
cases_sf <- st_as_sf(fatalities, coords = c("lon", "lat"), crs = 4326)
pumps_sf <- st_as_sf(pumps, coords = c("lon", "lat"), crs = 4326)

# Transformar a proyección métrica para crear buffers
cases_m <- st_transform(cases_sf, crs = 4326)
pumps_m <- st_transform(pumps_sf, crs = 4326)

# Crear buffers de 100 metros
pumps_buffer <- st_buffer(pumps_m, dist = 100)

# Volver a CRS geográfico para Leaflet
pumps_buffer_geo <- st_transform(pumps_buffer, crs = 4326)

# Crear mapa
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = pumps_sf, color = "blue", radius = 5, label = ~paste("Bomba", id)) %>%
  addCircleMarkers(data = cases_sf, color = "yellow", radius = 3, label = ~paste("Caso")) %>%
  addPolygons(data = pumps_buffer_geo, color = "green", weight = 2, fillOpacity = 0.2, label = ~paste("Buffer bomba", id))
