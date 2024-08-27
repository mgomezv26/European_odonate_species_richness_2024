
# Load library

library(ggplot2)
library(sf)
library(viridis)
library(patchwork)

# Load data
grid <- st_read("Data/shp/Grid_Richness.shp")
colnames(grid)

# ETRS89-LAEA coordinate system (EPSG:3035)
grid <- st_transform(grid, crs = 3035)

# Create a map for Odonata
map_od <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Odonata), color = NA) +
  scale_fill_viridis(name = "Odonata richness", option = "viridis") +
  labs(title = "Odonata", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "right")

# Create a map for Lentic
map_lentic <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Lentic), color = NA) +
  scale_fill_viridis(name = "Lentic richness", option = "viridis") +
  labs(title = "Lentic", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "right", axis.title.x = element_blank(), axis.title.y = element_blank()) #hide axes with element_blank()

# Create a map for Lotic
map_lotic <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Lotic), color = NA) +
  scale_fill_viridis(name = "Lotic richness", option = "viridis") +
  labs(title = "Lotic", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "right", axis.title.x = element_blank(), axis.title.y = element_blank()) #hide axes with element_blank()

# Combine the three maps into a single figure
combined_map <- (map_od + map_lentic + map_lotic) + plot_layout(ncol = 3, guides = "collect")

# Show the combined figure
print(combined_map)
