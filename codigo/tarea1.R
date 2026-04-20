# =============================================================================
# ARCHIVO 1: MAPA BÁSICO - Tutorial Paso a Paso
# =============================================================================

# PASO 1: Instalar paquetes (solo la primera vez)
# install.packages(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthdata"))

# PASO 2: Cargar las librerías necesarias
library(sf)              # Para manejar datos espaciales
library(ggplot2)         # Para hacer gráficos bonitos
library(rnaturalearth)   # Para obtener mapas del mundo

# PASO 3: Obtener datos del mapa mundial
mundo <- ne_countries(scale = "medium", returnclass = "sf")


# PASO 4: Crear tu primer mapa básico del mundo
mapa_mundo <- ggplot(mundo) +
  geom_sf(fill = "lightblue", color = "white") +  # Color de relleno y bordes
  theme_void() +                                   # Quitar ejes y fondo
  labs(title = "Mi Primer Mapa del Mundo")        # Título

# Mostrar el mapa
print(mapa_mundo)

# PASO 5: Crear un mapa de un país específico
pais_elegido <- "Peru"  # Puedes cambiar por: "Mexico", "Colombia", "Brazil", etc.
mi_pais <- mundo[mundo$name == pais_elegido, ]

# Crear el mapa del país
mapa_pais <- ggplot(mi_pais) +
  geom_sf(fill = "darkgreen", color = "black") +
  theme_void() +
  labs(title = paste("Mapa de", pais_elegido))

# Mostrar el mapa del país
print(mapa_pais)

# PASO 6: Personalizar colores y estilo
mapa_personalizado <- ggplot(mi_pais) +
  geom_sf(fill = "coral", color = "navy", size = 1.2) +  # Cambiar colores
  theme_void() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),    # Título centrado
    plot.background = element_rect(fill = "lightgray")     # Fondo gris
  ) +
  labs(title = paste("Mapa Personalizado de", pais_elegido))

print(mapa_personalizado)

# PASO 7: Guardar tu mapa

ggsave("mi_primer_mapa.png", plot = mapa_personalizado, 
       width = 10, height = 8, dpi = 300)



