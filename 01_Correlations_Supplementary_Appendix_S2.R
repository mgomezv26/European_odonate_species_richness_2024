
"
This file contains the code used to perform the Supplementary Appendix S2 correlations.
"


# Load library
library(sf)

# Load data
grid <- st_read("Data/Grid_Richness.shp")
colnames(grid)


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



# Configure the figure with 2 columns and 2 rows.
par(mfrow = c(2, 2))

# Generate the graphs
plot(as.numeric(as.character(grid$R_Ani_Len)), 
     as.numeric(as.character(grid$R_Zyg_Len)), 
     xlab ="Richness of Anisoptera species", 
     ylab="Richness of Zygoptera species", 
     main="Anisoptera lentic – Zygoptera lentic", 
     pch=19, col=alpha("grey",0.5))

plot(as.numeric(as.character(grid$R_Ani_Lot)), 
     as.numeric(as.character(grid$R_Zyg_Lot)), 
     xlab ="Richness of Anisoptera species", 
     ylab="Richness of Zygoptera species", 
     main="Anisoptera lotic – Zygoptera lotic", 
     pch=19, col=alpha("grey",0.5))

plot(as.numeric(as.character(grid$R_Ani_Len)), 
     as.numeric(as.character(grid$R_Ani_Lot)), 
     xlab ="Richness of lentic species", 
     ylab="Richness of lotic species", 
     main="Anisoptera lotic – lentic", 
     pch=19, col=alpha("grey",0.5))

plot(as.numeric(as.character(grid$R_Zyg_Len)), 
     as.numeric(as.character(grid$R_Zyg_Lot)), 
     xlab ="Richness of lentic species", 
     ylab="Richness of lotic species", 
     main="Zygoptera lentic – lotic", 
     pch=19, col=alpha("grey",0.5))

# Reset the layout
par(mfrow = c(1, 1))
