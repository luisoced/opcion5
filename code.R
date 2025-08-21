# Clase 1 - Analisis exploratorio de datos espaciales
# Actividad práctica
# Mapa de John Snow (1854) - Datos reales (paquete cholera)

# Cargar librerías

library(tidyverse)       # Visualización

library(sf)              # Manejo de datos espaciales
library(leaflet)         # Visualización interactiva
library(leaflet.extras)  # Funcionalidades extras de leaflet

library(cholera)         # Datos y funciones de John Snow

library(osrm)            # Consulta de rutas y isocronas

# 1. Cargar datos reales --------------------

street <- cholera::roads
cases  <- cholera::fatalities   # casos de muertes
pumps  <- cholera::pumps        # bombas de agua

# Convertir a sf con CRS WGS84 (EPSG:4326)
cases_sf  <- st_as_sf(cases, coords = c("lon", "lat"), crs = 4326)
pumps_sf  <- st_as_sf(pumps, coords = c("lon", "lat"), crs = 4326)
street_sf <- street %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  group_by(street) %>%
  dplyr::summarize(do_union = FALSE) %>%  # do_union=FALSE doesn't work as well
  st_cast("LINESTRING")

# 2. Visualización básica -----------------------------

ggplot() +
  geom_sf(data = street_sf, color = "grey40", size = 1, alpha = 0.6) +
  geom_sf(data = cases_sf, color = "tomato2", size = 1, alpha = 0.6) +
  geom_sf(data = pumps_sf, color = "darkblue", size = 3, shape = 19) +
  geom_sf_text(data = pumps_sf, aes(label = id), color = "darkblue",
               size = 3, nudge_y = 0.0005) +
  labs(
    title = "Mapa de casos de cólera - Londres 1854",
    subtitle = "Datos reales de John Snow (paquete cholera)",
    caption = "Fuente: paquete cholera en R"
  ) +
  theme_void()

# 3. Conteo de casos en un radio de 100 metros de cada bomba ------------

# Transformar a proyección métrica
cases_m <- st_transform(cases_sf, 27700)
pumps_m <- st_transform(pumps_sf, 27700)

# Buffers de 100 m
pumps_buffer <- st_buffer(pumps_m, dist = 100)

# Conteo por buffer
conteo <- st_intersects(pumps_buffer, cases_m) %>%
  lengths() %>%
  data.frame(Bomba = pumps$id, Casos = .)

conteo

# 4. Visualización interactiva --------------------------------

leaflet() %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  addTiles() %>%
  addCircleMarkers(data = cases_sf, color = "tomato",
                   radius = 3, label = ~paste("Caso"),
                   group = "Casos") %>%
  addCircleMarkers(data = pumps_sf, color = "darkblue", radius = 6,
                   label = ~street,  # Mostrar la calle
                   popup = ~paste("Bomba en:", street),
                   group = "pumps") %>%
  addPolygons(data = st_transform(pumps_buffer, 4326),
              color = "darkblue", weight = 1, fillOpacity = 0.2,
              label = ~paste("Buffer 100m"),
              group = "Buffers 100m") %>%
  addLegend(position = "bottomright",
            colors = c("tomato", "darkblue"),
            labels = c("Casos", "Bombas de agua"),
            title = "Leyenda") |>
  addLayersControl(
    overlayGroups = c("Casos", "pumps", "Buffers 100m"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addSearchFeatures(
    targetGroups = "pumps",       # Grupo de bombas
    options = searchFeaturesOptions(
      propertyName = "street",    # Campo por el que se busca
      zoom = 18,
      openPopup = TRUE,
      firstTipSubmit = TRUE,
      autoCollapse = TRUE,
      hideMarkerOnCollapse = TRUE
    )
  )


# 5. Cálculo de Isocronas ----------------------

# Asegurarse de que los datos estén en WGS84
origen <- pumps_sf |> filter(id == 7)     # bomba 7 (Broad Street)
destino <- cases_sf[1, ]    # caso 1

# Cálculo de ruta con OSRM
ruta <- osrmRoute(src = origen, dst = destino)

# Cálculo de isocrona
iso <- osrmIsochrone(loc = origen,
                     breaks = c(1:5),
                     osrm.profile = "foot")

# Mapeamos con Isocronas

pal <- colorFactor("viridis", domain = iso$isomax, ordered = TRUE)

leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolygons(data = iso, color = ~pal(isomax), fillOpacity = 0.2,
              label = ~paste("De ", isomin, "a", isomax, "minutos"),
              group = "Isócrona") %>%
  addCircleMarkers(data = cases_sf, color = "tomato",
                   radius = 1, opacity = 0.3,
                   label = ~paste("Caso"),
                   group = "Casos") %>%
  addCircleMarkers(data = pumps_sf, color = "darkblue", radius = 6,
                   label = ~street,
                   popup = ~paste("Bomba en:", street),
                   group = "pumps") %>%
  addLegend(position = "bottomright",
            colors = c("tomato", "darkblue"),
            labels = c("Casos", "Bombas"),
            title = "Leyenda") %>%
  addLayersControl(
    overlayGroups = c("Casos", "pumps"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addSearchFeatures(
    targetGroups = "pumps",
    options = searchFeaturesOptions(
      propertyName = "street",
      zoom = 18,
      openPopup = TRUE,
      firstTipSubmit = TRUE,
      autoCollapse = TRUE,
      hideMarkerOnCollapse = TRUE
    )
  )


