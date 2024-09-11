

# This file contains the code used to perform the Supplementary Appendix S2 correlations.

###########################################
##           LOAD LIBRARIES              ##
###########################################

library(sf)
library(scales)
library(ggplot2)
library(patchwork)

###########################################
##             LOAD DATA                 ##
###########################################

grid <- st_read("Data/shp/Grid_Richness.shp")
colnames(grid)

###########################################
##             CORRELATIONS              ##
###########################################

# Anisoptera lentic – Zygoptera lentic 
Lentic.cortest <- cor.test(
  as.numeric(as.character(grid$R_Ani_Len)),
  as.numeric(as.character(grid$R_Zyg_Len)),
  method = "spearman"
)
print(Lentic.cortest) #0.8102116 

# Anisoptera lotic – Zygoptera lotic 
Lotic.cortest <- cor.test(
  as.numeric(as.character(grid$R_Ani_Lot)),
  as.numeric(as.character(grid$R_Zyg_Lot)),
  method = "spearman"
)
print(Lotic.cortest) #0.7487992

# Zygoptera lentic – Zygoptera lotic 
Zygop.Lentic.Lotic.cortest <- cor.test(
  as.numeric(as.character(grid$R_Zyg_Len)),
  as.numeric(as.character(grid$R_Zyg_Lot)),
  method = "spearman"
)
print(Zygop.Lentic.Lotic.cortest) #0.6118159 

# Anisoptera lentic – Anisoptera lotic 
Ani.Lentic.Lotic.cortest <- cor.test(
  as.numeric(as.character(grid$R_Ani_Len)),
  as.numeric(as.character(grid$R_Ani_Lot)),
  method = "spearman"
)
print(Ani.Lentic.Lotic.cortest) #0.5000127  


###########################################
##             GRAPHS              ##
###########################################

# Generate the graphs
plot1 <-ggplot(grid, aes(x = R_Ani_Len, y = R_Zyg_Len)) +
  geom_point() +
  theme_bw() +
  labs(title = "Anisoptera lentic – Zygoptera lentic",
       x = "Richness of Anisoptera lentic species", 
       y = "Richness of Zygoptera lentic species")

plot2 <-ggplot(grid, aes(x = R_Ani_Lot, y = R_Zyg_Lot)) +
  geom_point() +
  theme_bw() +
  labs(title = "Anisoptera lotic – Zygoptera lotic",
       x = "Richness of Anisoptera lotic species", 
       y = "Richness of Zygoptera lotic species")

plot3 <-ggplot(grid, aes(x = R_Ani_Len, y = R_Ani_Lot)) +
  geom_point() +
  theme_bw() +
  labs(title = "Anisoptera lentic – lotic",
       x = "Richness of Anisoptera lentic species", 
       y = "Richness of Anisoptera lotic species")

plot4 <-ggplot(grid, aes(x = R_Zyg_Len, y = R_Zyg_Lot)) +
  geom_point() +
  theme_bw() +
  labs(title = "Zygoptera lentic – lotic",
       x = "Richness of Zygoptera lentic species", 
       y = "Richness of Zygoptera lotic species")


# Combine the three maps into a single figure
combined_plot <- (plot1 | plot2) /
  (plot3 | plot4)

# Save the figure as a PNG file with a resolution of 600 ppi.
ggsave("Figure_S2_1_600ppi.png", plot = combined_plot, width = 8, height = 8, units = "in", dpi = 600)

