# =============================================================================
# ARCHIVO 3: MAPA CON PUNTOS Y VALORES - Tutorial Paso a Paso
# =============================================================================

# PASO 1: Instalar paquetes (solo la primera vez)
# install.packages(c("sf", "ggplot2", "dplyr", "viridis", "rnaturalearth"))

# PASO 2: Cargar librerías
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)         # Para colores bonitos
library(rnaturalearth)

# PASO 3: Obtener el mapa base (usaremos Perú como ejemplo)
pais <- "Peru"  # Cambia por tu país
mapa_pais <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name == pais)

# PASO 4: Crear datos de ejemplo (simularemos estaciones meteorológicas)
set.seed(123)  # Para que siempre salgan los mismos datos

# Datos de ejemplo: estaciones con temperatura
estaciones <- data.frame(
  nombre = c("Estación Norte", "Estación Sur", "Estación Este", 
             "Estación Oeste", "Estación Centro", "Estación Costa",
             "Estación Sierra", "Estación Selva"),
  latitud = c(-10.5, -18.2, -12.8, -14.1, -15.2, -8.1, -11.9, -6.8),
  longitud = c(-76.8, -70.1, -69.2, -75.5, -72.3, -79.0, -77.2, -73.5),
  temperatura = c(28, 12, 25, 22, 18, 30, 15, 32)  # Temperaturas en °C
)

print("Datos creados:")
print(estaciones)

# PASO 5: Convertir a objeto espacial
puntos_sf <- st_as_sf(estaciones, 
                      coords = c("longitud", "latitud"), 
                      crs = 4326)  # Sistema de coordenadas WGS84

# PASO 6: Crear mapa básico con puntos
mapa_basico <- ggplot() +
  geom_sf(data = mapa_pais, fill = "lightgray", color = "black") +
  geom_sf(data = puntos_sf, color = "red", size = 3) +
  theme_void() +
  labs(title = "Ubicación de Estaciones Meteorológicas")

print(mapa_basico)

# PASO 7: Mapa con colores según valores (¡Aquí está la magia!)
mapa_colores <- ggplot() +
  geom_sf(data = mapa_pais, fill = "lightgray", color = "black") +
  geom_sf(data = puntos_sf, aes(color = temperatura), size = 4) +
  scale_color_viridis_c(name = "Temperatura\n(°C)") +  # Escala de colores
  theme_void() +
  labs(title = "Temperatura en Estaciones Meteorológicas")

print(mapa_colores)

# PASO 8: Cambiar la paleta de colores
mapa_colores_calor <- ggplot() +
  geom_sf(data = mapa_pais, fill = "lightgray", color = "black") +
  geom_sf(data = puntos_sf, aes(color = temperatura), size = 5) +
  scale_color_gradient(low = "blue", high = "red", 
                       name = "Temperatura\n(°C)") +
  theme_void() +
  labs(title = "Mapa de Calor - Temperaturas")

print(mapa_colores_calor)

# PASO 9: Diferentes tamaños según valores
mapa_tamaños <- ggplot() +
  geom_sf(data = mapa_pais, fill = "lightgray", color = "black") +
  geom_sf(data = puntos_sf, aes(size = temperatura), color = "darkred") +
  scale_size_continuous(name = "Temperatura\n(°C)", range = c(2, 8)) +
  theme_void() +
  labs(title = "Tamaño de Puntos según Temperatura")

print(mapa_tamaños)

# PASO 10: Combinar color Y tamaño
mapa_combinado <- ggplot() +
  geom_sf(data = mapa_pais, fill = "lightgray", color = "black") +
  geom_sf(data = puntos_sf, aes(color = temperatura, size = temperatura)) +
  scale_color_gradient(low = "blue", high = "red", 
                       name = "Temperatura\n(°C)") +
  scale_size_continuous(name = "Temperatura\n(°C)", range = c(3, 7)) +
  theme_void() +
  labs(title = "Color y Tamaño según Temperatura")

print(mapa_combinado)

# PASO 11: Añadir etiquetas con nombres
mapa_etiquetas <- ggplot() +
  geom_sf(data = mapa_pais, fill = "lightgray", color = "black") +
  geom_sf(data = puntos_sf, aes(color = temperatura), size = 4) +
  geom_sf_text(data = puntos_sf, aes(label = paste(nombre, "\n", temperatura, "°C")),
               size = 3, nudge_y = 0.5) +  # Mover texto hacia arriba
  scale_color_viridis_c(name = "Temperatura\n(°C)") +
  theme_void() +
  labs(title = "Estaciones con Temperaturas Etiquetadas")

print(mapa_etiquetas)

# PASO 12: Categorizar valores en grupos
# Crear categorías de temperatura
puntos_sf$categoria <- cut(puntos_sf$temperatura, 
                           breaks = c(0, 15, 25, 35), 
                           labels = c("Frío", "Templado", "Caliente"))

mapa_categorias <- ggplot() +
  geom_sf(data = mapa_pais, fill = "lightgray", color = "black") +
  geom_sf(data = puntos_sf, aes(color = categoria), size = 5) +
  scale_color_manual(values = c("Frío" = "blue", 
                                "Templado" = "green", 
                                "Caliente" = "red"),
                     name = "Categoría") +
  theme_void() +
  labs(title = "Categorías de Temperatura")

print(mapa_categorias)

# PASO 13: Función para crear tus propios mapas
crear_mapa_valores <- function(datos, pais_nombre, columna_valor, titulo) {
  # Obtener mapa del país
  mapa <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(name == pais_nombre)
  
  # Convertir datos a espacial
  datos_sf <- st_as_sf(datos, coords = c("longitud", "latitud"), crs = 4326)
  
  # Crear mapa
  ggplot() +
    geom_sf(data = mapa, fill = "lightgray", color = "black") +
    geom_sf(data = datos_sf, aes_string(color = columna_valor), size = 4) +
    scale_color_viridis_c(name = columna_valor) +
    theme_void() +
    labs(title = titulo)
}

# Ejemplo de uso:
# mi_mapa <- crear_mapa_valores(estaciones, "Peru", "temperatura", 
#                              "Mi Mapa de Temperaturas")
# print(mi_mapa)

# PASO 14: Guardar tu mapa
ggsave("mapa_temperaturas.png", plot = mapa_combinado, 
       width = 12, height = 8, dpi = 300)


