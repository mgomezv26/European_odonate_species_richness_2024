
# Load library

library(ggplot2) # version: 3.5.0 
library(sf) # version: 1.0.16 
library(viridis) # version: 0.6.5 
library(patchwork) # version: 0.6.5  

cat("ggplot2:", as.character(packageVersion("ggplot2")), "\n")
cat("sf:", as.character(packageVersion("sf")), "\n")
cat("viridis:", as.character(packageVersion("viridis")), "\n")
cat("patchwork:", as.character(packageVersion("patchwork")), "\n")

# Load data
grid <- st_read("Data/shp/Grid_Richness.shp")
colnames(grid)

# ETRS89-LAEA coordinate system (EPSG:3035)
grid <- st_transform(grid, crs = 3035)

# Create a map for Odonata
map_od <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Odonata), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Odonata", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "right")

# Create a map for Lentic
map_lentic <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Lentic), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Lentic", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "right") #hide axes with element_blank()
  #theme(legend.position = "right", axis.title.x = element_blank(), axis.title.y = element_blank()) #hide axes with element_blank()


# Create a map for Lotic
map_lotic <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Lotic), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Lotic", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "right") #hide axes with element_blank()
  #theme(legend.position = "right", axis.title.x = element_blank(), axis.title.y = element_blank()) #hide axes with element_blank()

# Combine the three maps into a single figure
combined_map <- (map_od | map_lentic | map_lotic)

# Show the combined figure
print(combined_map)




# Create a map for Anisoptera
map_ani <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Ani), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Anisoptera", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "right")

# Create a map for Zygoptera
map_zyg <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Zyg), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Zygoptera", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "right", axis.title.x = element_blank(), axis.title.y = element_blank())


# Create a map for Anisoptera - Lentic
map_ani_len <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Ani_Len), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Anisoptera Lentic", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "right")

# Create a map for Anisoptera - Lotic
map_ani_lot <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Ani_Lot), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Anisoptera Lotic", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "right", axis.title.x = element_blank(), axis.title.y = element_blank()) #hide axes with element_blank()


# Create a map for Zygoptera - Lentic
map_zyg_len <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Zyg_Len), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Zygoptera Lentic", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "right")

# Create a map for Zygoptera - Lotic
map_zyg_lot <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Zyg_Lot), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Zygoptera Lotic", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "right", axis.title.x = element_blank(), axis.title.y = element_blank()) #hide axes with element_blank()

# Combine the three maps into a single figure

combined_map <- (map_ani | map_zyg) /
  (map_ani_len | map_ani_lot) /
  (map_zyg_len | map_zyg_lot)

# Show the combined figure
print(combined_map)

# Create a map for Odonata
labels <- c("Southern ", "Northern ")

grid$Zone = as.factor(grid$Zone)

map_zone <- ggplot(data = grid) +
  geom_sf(aes(fill = Zone)) +
  scale_fill_manual(values = c("#E0EEEE", "#49A4B9"), labels = labels) +
  labs(title = "Study area", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "right")

map_zone
