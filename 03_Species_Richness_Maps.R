# The following script describe how to create the maps of the paper

###########################################
##           LOAD LIBRARIES              ##
###########################################

library(ggplot2) # version: 3.5.0 
library(sf) # version: 1.0.16 
library(viridis) # version: 0.6.5 
library(patchwork) # version: 0.6.5  

###########################################
##             LOAD DATA                 ##
###########################################

grid <- st_read("Data/shp/Grid_Richness.shp")
#colnames(grid)
# ETRS89-LAEA coordinate system (EPSG:3035) 
grid <- st_transform(grid, crs = 3035)

###################################################
##           MAP OF THE STUDY AREA               ##
###################################################

# Figure 1. Map of the study area. 
labels <- c("Southern ", "Northern ")
grid$Zone = as.factor(grid$Zone)

map_zone <- ggplot(data = grid) +
  geom_sf(aes(fill = Zone)) +
  scale_fill_manual(values = c("#E0EEEE", "#49A4B9"), labels = labels) +
  labs(title = "Study area", x = "Longitud", y = "Latitud") +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 20, face = "bold"),  
    axis.title = element_text(size = 18),                
    axis.text = element_text(size = 16),                 
    legend.title = element_text(size = 14),              
    legend.text = element_text(size = 12)               
  )

# Show
map_zone
# Save the figure as a PNG file with a resolution of 600 ppi.
ggsave("Figure_1.png", plot = map_zone, width = 10, height = 15, units = "in", dpi = 600)


###################################################
##             SPECIES RICHNESS MAP              ##
###################################################

# Figure 2. Species richness map.
## Create a map for Odonata
map_od <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Odonata), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Odonata", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 30),  
    axis.title = element_text(size = 24),                
    axis.text = element_text(size = 18),                 
    legend.title = element_text(size = 20),              
    legend.text = element_text(size = 18)               
  )

## Create a map for Lentic
map_lentic <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Lentic), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Lentic", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 30),  
    axis.title = element_text(size = 24),                
    axis.text = element_text(size = 18),                 
    legend.title = element_text(size = 20),              
    legend.text = element_text(size = 18)               
  )

## Create a map for Lotic
map_lotic <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Lotic), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Lotic", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 30),  
    axis.title = element_text(size = 24),                
    axis.text = element_text(size = 18),                 
    legend.title = element_text(size = 20),              
    legend.text = element_text(size = 18)               
  )

# Combine the three maps into a single figure
combined_map <- (map_od | map_lentic | map_lotic)
# Show the combined figure
print(combined_map)

# Save the figure as a PNG file with a resolution of 600 ppi.
ggsave("Figure_2.png", plot = combined_map, width = 20, height = 25, units = "in", dpi = 600)


###################################################
##           SUPPLEMENTARY FIGURE S2.2           ##
###################################################

# Create a map for Anisoptera
map_ani <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Ani), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Anisoptera", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 20, face = "bold"),  
    axis.title = element_text(size = 18),                
    axis.text = element_text(size = 16),                 
    legend.title = element_text(size = 14),              
    legend.text = element_text(size = 12)               
  )

# Create a map for Zygoptera
map_zyg <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Zyg), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Zygoptera", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 20, face = "bold"),  
    axis.title = element_text(size = 18),                
    axis.text = element_text(size = 16),                 
    legend.title = element_text(size = 14),              
    legend.text = element_text(size = 12)               
  )

# Create a map for Anisoptera - Lentic
map_ani_len <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Ani_Len), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Anisoptera Lentic", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 20, face = "bold"),  
    axis.title = element_text(size = 18),                
    axis.text = element_text(size = 16),                 
    legend.title = element_text(size = 14),              
    legend.text = element_text(size = 12)               
  )

# Create a map for Anisoptera - Lotic
map_ani_lot <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Ani_Lot), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Anisoptera Lotic", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 20, face = "bold"),  
    axis.title = element_text(size = 18),                
    axis.text = element_text(size = 16),                 
    legend.title = element_text(size = 14),              
    legend.text = element_text(size = 12)               
  )

# Create a map for Zygoptera - Lentic
map_zyg_len <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Zyg_Len), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Zygoptera Lentic", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 20, face = "bold"),  
    axis.title = element_text(size = 18),                
    axis.text = element_text(size = 16),                 
    legend.title = element_text(size = 14),              
    legend.text = element_text(size = 12)               
  )

# Create a map for Zygoptera - Lotic
map_zyg_lot <- ggplot(data = grid) +
  geom_sf(aes(fill = R_Zyg_Lot), color = NA) +
  scale_fill_viridis(name = "Richness", option = "viridis") +
  labs(title = "Zygoptera Lotic", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 20, face = "bold"),  
    axis.title = element_text(size = 18),                
    axis.text = element_text(size = 16),                 
    legend.title = element_text(size = 14),              
    legend.text = element_text(size = 12)               
  )

# Combine the three maps into a single figure
combined_map <- (map_ani | map_zyg) /
  (map_ani_len | map_ani_lot) /
  (map_zyg_len | map_zyg_lot)

# Show the combined figure
print(combined_map)

# Save the figure as a PNG file with a resolution of 600 ppi.
ggsave("Figure_S2_2.png", plot = combined_map, width = 10, height = 15, units = "in", dpi = 600)

