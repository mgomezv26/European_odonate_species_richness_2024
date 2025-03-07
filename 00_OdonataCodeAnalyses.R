
"
###########################################
##           LOAD LIBRARIES              ##
###########################################
R version 4.3.3 (2024-02-29 ucrt)
"

library(dplyr)
library (ggplot2)
library(gridExtra)
library(tidyverse)
library(paletteer)
library(eulerr)
library(cowplot)
library(grid)
library(patchwork) 
library(vegan)
library(fossil)
library(letsR)
library(ape)
library(adespatial)
library(sf)
library(sp)


cat("Versions of the libraries used:\n")
cat("dplyr:", as.character(packageVersion("dplyr")), "\n") # dplyr: 1.1.4 
cat("ggplot2:", as.character(packageVersion("ggplot2")), "\n") # ggplot2: 3.5.1 
cat("gridExtra:", as.character(packageVersion("gridExtra")), "\n") # gridExtra: 2.3 
cat("tidyverse:", as.character(packageVersion("tidyverse")), "\n") # tidyverse: 2.0.0 
cat("paletteer:", as.character(packageVersion("paletteer")), "\n") # paletteer: 1.6.0 
cat("eulerr:", as.character(packageVersion("eulerr")), "\n") # eulerr: 7.0.2 
cat("cowplot:", as.character(packageVersion("cowplot")), "\n") # cowplot: 1.1.3
cat("vegan:", as.character(packageVersion("vegan")), "\n") # vegan: 2.6.10 
cat("fossil:", as.character(packageVersion("fossil")), "\n") # fossil: 0.4.0 
cat("letsR:", as.character(packageVersion("letsR")), "\n") # letsR: 5.0 
cat("ape:", as.character(packageVersion("ape")), "\n") # ape: 5.8.1 
cat("adespatial:", as.character(packageVersion("sf")), "\n") # adespatial: 1.0.19 
cat("sf:", as.character(packageVersion("fossil")), "\n") # sf: 0.4.0 
cat("sp:", as.character(packageVersion("sp")), "\n") # sp: 2.1.4


"
###########################################
##             LOAD DATA                 ##
###########################################
The data used for the statistical analysis are stored in the ‘Data’ folder. This folder contains 2 files in CSV format, 
where the delimiter is ';' and the decimal separator is '.'. 

The available files are:

1) df_odonata.csv: The dataset has been named ‘da’ in the R code. Each record in the dataset refers to a geographical grid, of the 50x50 km grid. 
The dataset contains a total of 16 variables.

The dependent variables are:

- R_Odonata: richness of the Odonata group
- R_Lotic : richness of lotic species group
- R_Lentic: richness of lentic species group

The independent variables are:

- Temp_21: represents the past temperature
- Prec_21: represents the past precipitation
- Temp_0: represents the contemporary temperature
- Prec_0: represents the contemporary precipitation
- vart: represents temperature variation
- varp: represents precipitation variation
- H_Lent: availability of lentic habitat
- H_Lot: availability of lotic habitat

2) df_odonata.csv: The dataset has been named ‘devianza’ in the R code. These dataset contains the results obtained with SAM for Partitioning of explained deviances. 
In R we use these results to make Venn diagrams to display the results graphically.

In this dataset, each variable corresponds to:

- id.group: study group
- id.reg: region of Europe
- pure.pas.r2: Past_Climate
- pure.pre.r2: Current_Climate
- pure.hab.r2: Habitat_Availabilit
- over.pas.pre1: Past_Climate&Current_Climate
- over.pas.hab: Past_Climate&Habitat_Availabilit
- over.pre.hab1: Current_Climate&Habitat_Availabilit
- over.tot: Current_Climate&Habitat_Availabilit&Past_Climate
"



###########################################
##             LOAD DATA                 ##
###########################################

da = read.csv('Data/df_odonata.csv', sep = ';')
devianza = read.csv('Data/table_devianza.csv', sep = ';')

da <- read.csv('Data/df_odonata.csv', sep = ';')
da$R_Odonata   <- as.numeric(gsub(",", ".", da$R_Odonata))
da$R_Lentic   <- as.numeric(gsub(",", ".", da$R_Lentic))
da$R_Lotic   <- as.numeric(gsub(",", ".", da$R_Lotic))

da$Temp_21   <- as.numeric(gsub(",", ".", da$Temp_21))
da$Prec_21   <- as.numeric(gsub(",", ".", da$Prec_21))
da$BIO12_Prec <- as.numeric(gsub(",", ".", da$BIO12_Prec))
da$Temp_0    <- as.numeric(gsub(",", ".", da$Temp_0))
da$Prec_0    <- as.numeric(gsub(",", ".", da$Prec_0))
da$vart      <- as.numeric(gsub(",", ".", da$vart))
da$varp      <- as.numeric(gsub(",", ".", da$varp))
da$H_Lent    <- as.numeric(gsub(",", ".", da$H_Lent))
da$H_Lot     <- as.numeric(gsub(",", ".", da$H_Lot))

"
###################################################################
##     Dividing study area in Northern and Southern Europe       ##
###################################################################

To assess the effect of glaciations on richness, we divided the study area in Northern and Southern Europe, based on the 0 ºC isotherm at LGM.
"

nor <- da[da$Temp_21<0,]
sur <- da[da$Temp_21>0,]
## Summary for each region
### Northern Europe
summary_nor<-summary(nor)
summary_nor
### Southern Europe
summary_sur<-summary(sur)
summary_sur

"
###################################################################
##       Are local assemblages also richer in the south?         ##
###################################################################

Before creating the boxplots, a new column 'Zone' will be created to identify which region of Europe each record belongs to. 
For Northern Europe, the value 1 will be assigned, and for Southern Europe, the value 0 will be assigned. Subsequently, 
the records from both datasets will be concatenated to create a single dataset, which will be used to generate the boxplots, keeping only the following columns:
  
- Id
- ET_ID
- ET_Index
- R_Odonata
- R_Lotic 
- R_Lentic
- Zone

"
##############################
## BOXPLOT
##############################

nor_b <-nor %>% 
  mutate(Zone = 1)

sur_b <- sur %>% 
  mutate(Zone = 0)

stacked_data <- rbind(nor_b, sur_b)

stacked_data <- stacked_data %>%
  select(Id, ET_ID,ET_Index,R_Odonata, R_Lotic , R_Lentic, Zone)
stacked_data$Zone <- as.factor(stacked_data$Zone)
str(stacked_data)

## Create graphics
labels <- c("Southern ", "Northern ")

boxplot_odonata <- ggplot(stacked_data, aes(x = Zone, y = R_Odonata, fill = Zone)) +
  geom_boxplot(alpha = 0.5, outlier.colour = "#A52A2A") +
  scale_fill_manual(values = c("#F0FFFF", "#49A4B9"), labels = labels) +
  scale_x_discrete(labels = labels) +
  xlab("Zone") +
  ylab("Odonata species richness ") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none"
  )

boxplot_lotic <- ggplot(stacked_data, aes(x = Zone, y = R_Lotic , fill = Zone)) +
  geom_boxplot(alpha = 0.5, outlier.colour = "#A52A2A") +
  scale_fill_manual(values = c("#F0FFFF", "#49A4B9"), labels = labels) +
  scale_x_discrete(labels = labels) +
  xlab("Zone") +
  ylab("Lotic species richness") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none"
  )

boxplot_lentic <- ggplot(stacked_data, aes(x = Zone, y = R_Lentic, fill = Zone)) +
  geom_boxplot(alpha = 0.5, outlier.colour = "#A52A2A") +
  scale_fill_manual(values = c("#F0FFFF", "#49A4B9"), labels = labels) +
  scale_x_discrete(labels = labels) +
  xlab("Zone") +
  ylab("Lentic species richness") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none"
  )


# Combine the three maps into a single figure
combined_plot <- (boxplot_odonata | boxplot_lotic | boxplot_lentic)

# Show the combined figure
print(combined_plot)

# Save the figure as a PNG file with a resolution of 600 ppi.
ggsave("Figure_3_600ppi_v2.png", plot = combined_plot, width = 3, height = 2, units = "in", dpi = 600)



## function to see the results
summary_boxplot <- function(data, variable) {
  summary_data <- data %>%
    group_by(Zone) %>%
    summarise(
      min = min({{ variable }}),
      q1 = quantile({{ variable }}, 0.25),
      median = median({{ variable }}),
      mean = mean({{ variable }}),
      q3 = quantile({{ variable }}, 0.75),
      max = max({{ variable }})
    )
  return(summary_data)
}

#use the above function
summary_boxplot_odonata <- summary_boxplot(stacked_data, R_Odonata)
summary_boxplot_odonata

summary_boxplot_lotic <- summary_boxplot(stacked_data, R_Lotic )
summary_boxplot_lotic

summary_boxplot_lentic <- summary_boxplot(stacked_data, R_Lentic)
summary_boxplot_lentic

# Mann-Whitney U-test

##############################
## Mann-Whitney U-test
##############################

wilcox.test (stacked_data$R_Odonata ~ stacked_data$Zone)
wilcox.test (stacked_data$R_Lotic  ~ stacked_data$Zone)
wilcox.test (stacked_data$R_Lentic ~ stacked_data$Zone)

"
##############################################################################################
##   Relationships between richness, climate, climate stability, and habitat availability   ##
##############################################################################################

We used Generalised Linear Models (GLMs) to explore the relationship between species richness, past and contemporary climate, and habitat availability. 
As species richness values represent count data (or similar), we assumed a Poisson distribution and a logarithmic link function.
To assess whether the species richness determinants change from one region to another, we performed statistical models for (1) Northern Europe and (2) Southern Europe.

Subsequently, the independent variables were standardized (scaled to 0 mean and 1 standard deviation) to allow comparison of their effects on species richness pattern. 
To account for spatial autocorrelation, we included spatial filters derived from Principal Coordinates of Neighbourhood Matrices (PCNM)
"

#------------------------- # Northern Europe # ----------------------------------------------#

##--------------------------------------------------------------------------------
# Evaluate spatial correlation - Principal Coordinates of Neighbor Matrices (PCNM)
##--------------------------------------------------------------------------------

## 1. Convert geographic to Cartesian coordinates
xy <- as.matrix(nor[, c("longitude", "latitude")])

## 2. Calculate the spatial distance matrix
d<-earth.dist(xy, dist=TRUE)
d <- as.matrix(d)
d[d==0] <- 0.0000001 # Avoid divisions by zero
w <- 1/d # Matrix of spatial weights (inverse of distance)
diag(w) <- 0 # Diagonal at zero (no autocorrelation with itself)

## 3. Calculate PCNM (Principal Coordinates of Neighbourhood Matrices)
pcnm_result <- pcnm(d)
eigen<-pcnm_result$vectors
ncol(eigen)

## 4. Evaluate the autocorrelation in residuals of linear models.
selection <- function(z, d, eigen)
{ n <-ncol(eigen) # number of eigenvectors
  imor <- numeric(n)
  for(i in 1:n){
    print(n-i)
    lm1 <- lm(z~eigen[, i]) # A linear model (lm1) is fitted between the response variable and each eigenvector of PCNM.
    imor[i] <- lets.correl(lm1$residuals, d, 10, plot=F)[1,1] # Calculation of the Moran Index (lets.correl()), which measures the spatial autocorrelation in the residuals.
  } 
  posran <- numeric()
  i=1
  correlX <- lets.correl(lm1$residuals, d, 10)
  p <- 0.04
  rank1 <- rank(imor)
  mud=0.2
  while((correlX[1,4]<0.05 | p<0.05) & mud>=0.1){
    print(i)
    posran <- c(posran, which(rank1==i))
    dat <- cbind(data.frame(Y=z),as.data.frame(eigen[, posran]))
    lm2 <- lm(Y~., data=dat)
    ant <- correlX[1,1]
    correlX <- lets.correl(lm2$residuals, d, 10)
    dep <- correlX[1,1]
    mud <- (ant-dep)/ant
    p <- Moran.I(lm2$residuals, w)
    p <- p$p.value
    i=i+1
  }
  return(posran)
}

## 5. Select an optimal subset of spatial filters, minimising autocorrelation and perform the GLM

##----- Odonata species -----##

z <- nor$R_Odonata # Variable response
filters.1<- selection(z, d, eigen)
filters <-eigen[ ,filters.1]
nor$PCNM1 <- filters[,1]

# GLM 
m.nor.all <- glm(R_Odonata ~ scale(vart) + scale(varp) + scale(Temp_0) + scale(Prec_0) + scale(H_Lot) + scale(H_Lent) +
                           PCNM1, family = poisson, data = nor)
summary(m.nor.all)
1 - summary(m.nor.all)$deviance/summary(m.nor.all)$null.deviance

##----- Lentic species -----##
z <- nor$R_Lentic # Variable response
filters.1<- selection(z, d, eigen)
filters <-eigen[ ,filters.1]
nor$PCNM1 <- filters[,1]

# GLM 
m.nor.len <- glm(R_Lentic ~ scale(vart) + scale(varp) + scale(Temp_0) + scale(Prec_0) + scale(H_Lent) +
                           PCNM1, family = poisson, data = nor)
summary(m.nor.len)
1 - summary(m.nor.len)$deviance/summary(m.nor.len)$null.deviance


##----- Lotic species -----##

z <- nor$R_Lotic # Variable response
filters.1<- selection(z, d, eigen)
filters <-eigen[ ,filters.1]
nor$PCNM1 <- filters[,1]

# GLM 
m.nor.lot <- glm(R_Lotic ~ scale(vart) + scale(varp) + scale(Temp_0) + scale(Prec_0) + scale(H_Lot) +
                           PCNM1, family = poisson, data = nor)
summary(m.nor.lot)
1 - summary(m.nor.lot)$deviance/summary(m.nor.lot)$null.deviance



#------------------------- # Southern Europe - LOTIC SPECIES  # ----------------------------------------------#

##--------------------------------------------------------------------------------
# Evaluate spatial correlation - Principal Coordinates of Neighbor Matrices (PCNM)
##--------------------------------------------------------------------------------

## 1. Convert geographic to Cartesian coordinates
xy <- as.matrix(sur[, c("longitude", "latitude")])

## 2. Calculate the spatial distance matrix
d<-earth.dist(xy, dist=TRUE)
d <- as.matrix(d)
d[d==0] <- 0.0000001 # Avoid divisions by zero
w <- 1/d # Matrix of spatial weights (inverse of distance)
diag(w) <- 0 # Diagonal at zero (no autocorrelation with itself)

## 3. Calculate PCNM (Principal Coordinates of Neighbourhood Matrices)
pcnm_result <- pcnm(d)
eigen<-pcnm_result$vectors
ncol(eigen)

## 4. Evaluate the autocorrelation in residuals of linear models.
selection <- function(z, d, eigen)
{ n <-ncol(eigen) # number of eigenvectors
  imor <- numeric(n)
  for(i in 1:n){
    print(n-i)
    lm1 <- lm(z~eigen[, i]) # A linear model (lm1) is fitted between the response variable and each eigenvector of PCNM.
    imor[i] <- lets.correl(lm1$residuals, d, 10, plot=F)[1,1] # Calculation of the Moran Index (lets.correl()), which measures the spatial autocorrelation in the residuals.
  } 
  posran <- numeric()
  i=1
  correlX <- lets.correl(lm1$residuals, d, 10)
  p <- 0.04
  rank1 <- rank(imor)
  mud=0.2
  while((correlX[1,4]<0.05 | p<0.05) & mud>=0.1){
    print(i)
    posran <- c(posran, which(rank1==i))
    dat <- cbind(data.frame(Y=z),as.data.frame(eigen[, posran]))
    lm2 <- lm(Y~., data=dat)
    ant <- correlX[1,1]
    correlX <- lets.correl(lm2$residuals, d, 10)
    dep <- correlX[1,1]
    mud <- (ant-dep)/ant
    p <- Moran.I(lm2$residuals, w)
    p <- p$p.value
    i=i+1
  }
  return(posran)
}

## 5. Select an optimal subset of spatial filters, minimising autocorrelation and perform the GLM

##----- Odonata species -----##
z <- sur$R_Odonata # Variable response
filters.1<- selection(z, d, eigen)
filters <- eigen[, filters.1, drop = FALSE]
class(filters)
str(filters)
sur$PCNM1 <- filters[,1]

# GLM 
m.sur.all <- glm(R_Odonata ~ scale(vart) + scale(varp) + scale(Temp_0) + scale(Prec_0) + scale(H_Lot) + scale(H_Lent) +
                           PCNM1, family = poisson, data = sur)
summary(m.sur.all)
1 - summary(m.sur.all)$deviance/summary(m.sur.all)$null.deviance

##----- Lentic species -----##

z <- sur$R_Lentic # Variable response
filters.1<- selection(z, d, eigen)
filters <- eigen[, filters.1, drop = FALSE]
sur$PCNM1 <- filters[,1]

# GLM 
m.sur.len <- glm(R_Lentic ~ scale(vart) + scale(varp) + scale(Temp_0) + scale(Prec_0) + scale(H_Lent) +
                           PCNM1, family = poisson, data = sur)
summary(m.sur.len)
1 - summary(m.sur.len)$deviance/summary(m.sur.len)$null.deviance

##----- Lotic species -----##
z <- sur$R_Lotic # Variable response
filters.1<- selection(z, d, eigen)
filters <- eigen[, filters.1, drop = FALSE]
sur$PCNM1 <- filters[,1]

# GLM 
m.sur.lot <- glm(R_Lotic ~ scale(vart) + scale(varp) + scale(Temp_0) + scale(Prec_0) + scale(H_Lot) +
                           PCNM1, family = poisson, data = sur)
summary(m.sur.lot)
1 - summary(m.sur.lot)$deviance/summary(m.sur.lot)$null.deviance




########################################################################
###           Scatter plots of supplementary materials               ###
########################################################################

#### Odonata northern Europe
Odonata_plot_nor = nor %>%
  select(ET_ID, ET_Index, R_Odonata, vart, varp, Temp_0, Prec_0, H_Lent, H_Lot)
str(Odonata_plot_nor)

Odonata_plot_nor_long <- Odonata_plot_nor %>% 
  gather(key = study_variable , value = value, vart:H_Lot, factor_key = TRUE)

str(Odonata_plot_nor_long)

my_colors <- paletteer_c("grDevices::Temps", 6) #color palette

SCplot_N <- ggplot(Odonata_plot_nor_long) +
  geom_point(mapping = aes(x = value, y = R_Odonata, colour = study_variable), alpha = 0.5) +
  geom_smooth(mapping = aes(x = value, y = R_Odonata), method = "lm", se = TRUE, color = 'darkgrey') +
  scale_color_manual(values = my_colors) +
  facet_wrap(~ study_variable, scales = "free") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Scatter plots for Odonates in northern Europe")

#### Odonata Southern Europe
Odonata_plot_sur = sur %>%
  select(ET_ID, ET_Index, R_Odonata, vart, varp, Temp_0, Prec_0, H_Lent, H_Lot)

Odonata_plot_sur_long <- Odonata_plot_sur %>% 
  gather(key = study_variable , value = value, vart:H_Lot, factor_key = TRUE)

str(Odonata_plot_sur_long)

my_colors <- paletteer_c("grDevices::Temps", 6) #color palette

SCplot_S <- ggplot(Odonata_plot_sur_long) +
  geom_point(mapping = aes(x = value, y = R_Odonata, colour = study_variable), alpha = 0.5) +
  geom_smooth(mapping = aes(x = value, y = R_Odonata), method = "lm", se = TRUE, color = 'darkgrey') +
  scale_color_manual(values = my_colors) +
  facet_wrap(~ study_variable, scales = "free") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Scatter plots for Odonates in southern Europe")

# Combine the three maps into a single figure
combined_plot <- (SCplot_N | SCplot_S)
combined_plot
# Save the figure as a PNG file with a resolution of 600 ppi.
ggsave("Figure_S3_1_600ppi.png", plot = combined_plot, width = 15, height = 5, units = "in", dpi = 600)



########################################
#### Lotic species in northern Europe
Lotic_plot_nor = nor %>%
  select(ET_ID, ET_Index, R_Lotic , vart, varp, Temp_0, Prec_0, H_Lot)

Lotic_plot_nor_long <- Lotic_plot_nor %>% 
  gather(key = study_variable , value = value, vart:H_Lot, factor_key = TRUE)
#str(Lotic_plot_nor_long)


my_colors <- paletteer_c("grDevices::Temps", 5) #color palette

SCplot_N <-ggplot(Lotic_plot_nor_long) +
  geom_point(mapping = aes(x = value, y = R_Lotic , colour = study_variable), alpha = 0.5) +
  geom_smooth(mapping = aes(x = value, y = R_Lotic ), method = "lm", se = TRUE, color = 'darkgrey') +
  scale_color_manual(values = my_colors) +
  facet_wrap(~ study_variable, scales = "free") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Scatter plots for Lotic species in northern Europe")

#### Lotic species in southern Europe
Lotic_plot_sur = sur %>%
  select(ET_ID, ET_Index, R_Lotic , vart, varp, Temp_0, Prec_0, H_Lot)

Lotic_plot_sur_long <- Lotic_plot_sur %>% 
  gather(key = study_variable , value = value, vart:H_Lot, factor_key = TRUE)
#str(Lotic_plot_sur_long)

SCplot_S <-ggplot(Lotic_plot_sur_long) +
  geom_point(mapping = aes(x = value, y = R_Lotic , colour = study_variable), alpha = 0.5) +
  geom_smooth(mapping = aes(x = value, y = R_Lotic ), method = "lm", se = TRUE, color = 'darkgrey') +
  scale_color_manual(values = my_colors) +
  facet_wrap(~ study_variable, scales = "free") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Scatter plots for Lotic species in southern Europe")

# Combine the three maps into a single figure
combined_plot <- (SCplot_N | SCplot_S)
combined_plot
# Save the figure as a PNG file with a resolution of 600 ppi.
ggsave("Figure_S3_2_600ppi.png", plot = combined_plot, width = 15, height = 5, units = "in", dpi = 600)



########################################
#### Lentic species in northern Europe

Lentic_plot_nor = nor %>%
  select(ET_ID, ET_Index, R_Lentic, vart, varp, Temp_0, Prec_0, H_Lent)

Lentic_plot_nor_long <- Lentic_plot_nor %>% 
  gather(key = study_variable , value = value, vart:H_Lent, factor_key = TRUE)
#str(Lentic_plot_nor_long)

SCplot_N <-ggplot(Lentic_plot_nor_long) +
  geom_point(mapping = aes(x = value, y = R_Lentic, colour = study_variable), alpha = 0.5) +
  geom_smooth(mapping = aes(x = value, y = R_Lentic), method = "lm", se = TRUE, color = 'darkgrey') +
  scale_color_manual(values = my_colors) +
  facet_wrap(~ study_variable, scales = "free") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Scatter plots for Lentic species in northern Europe")

#### Lentic species in southern Europe
Lentic_plot_sur = sur %>%
  select(ET_ID, ET_Index, R_Lentic, vart, varp, Temp_0, Prec_0, H_Lent)

Lentic_plot_sur_long <- Lentic_plot_sur %>% 
  gather(key = study_variable , value = value, vart:H_Lent, factor_key = TRUE)

#tr(Lentic_plot_sur_long)

SCplot_S <-ggplot(Lentic_plot_sur_long) +
  geom_point(mapping = aes(x = value, y = R_Lentic, colour = study_variable), alpha = 0.5) +
  geom_smooth(mapping = aes(x = value, y = R_Lentic), method = "lm", se = TRUE, color = 'darkgrey') +
  scale_color_manual(values = my_colors) +
  facet_wrap(~ study_variable, scales = "free") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Scatter plots for Lentic species in southern Europe")

# Combine the three maps into a single figure
combined_plot <- (SCplot_N | SCplot_S)
combined_plot
# Save the figure as a PNG file with a resolution of 600 ppi.
ggsave("Figure_S3_3_600ppi.png", plot = combined_plot, width = 15, height = 5, units = "in", dpi = 600)



############################################
## Partitioned the explained deviances
############################################

# The results obtained with SAM for Partitioning of explained deviances
devianza

## Venn diagrams

###  Odonata.South
Odonata.South <- euler(c(Past_Climate= 9.75,
                      Current_Climate= 15.34,
                      Habitat_Availabilit= 0.62,
                     "Past_Climate&Current_Climate"=6.21,
                      "Past_Climate&Habitat_Availabilit"= 2.26,
                      "Current_Climate&Habitat_Availabilit"= 1.13,
                      "Current_Climate&Habitat_Availabilit&Past_Climate" = 0.02)) #negativo

plot(Odonata.South,
     quantities = list(type = "counts", fontsize = 18),
     labels = FALSE,
     legend = list(labels = c("Past Climate", "Current Climate", "Habitat availability"),
       side = "right"))

# Add a title to the graph
grid.text("Odonata South", 
          x = unit(1, "npc") - unit(1, "lines"), 
          y = unit(1, "npc") - unit(1, "lines"), 
          just = c("right", "top"),               
          gp = gpar(fontsize = 16, fontface = "bold"))


###  Odonata.North
Odonata.North <- euler(c(Past_Climate= 11.24,
                      Current_Climate= 7.73,
                      Habitat_Availabilit= 2.37,
                     "Past_Climate&Current_Climate"=32.28,
                      "Past_Climate&Habitat_Availabilit"= 2.44,
                      "Current_Climate&Habitat_Availabilit"= 0.77, #negativo
                      "Current_Climate&Habitat_Availabilit&Past_Climate" = 8.33))

plot(Odonata.North,
     quantities = list(type = "counts", fontsize = 18),
     labels = FALSE,
     legend = list(labels = c("Past Climate", "Current Climate", "Habitat availability"),
       side = "right"))


# Add a title to the graph
grid.text("Odonata North", 
          x = unit(1, "npc") - unit(1, "lines"), # Posicionar cerca del borde derecho
          y = unit(1, "npc") - unit(1, "lines"), # Posicionar cerca del borde superior
          just = c("right", "top"),               # Alinear a la derecha y en la parte superior
          gp = gpar(fontsize = 16, fontface = "bold"))

### Lotic.South
Lotic.South <- euler(c(Past_Climate= 6.97,
                      Current_Climate= 14.24,
                      Habitat_Availabilit= 2.39,
                     "Past_Climate&Current_Climate"=3.05,#negative
                      "Past_Climate&Habitat_Availabilit"= 1.5,
                      "Current_Climate&Habitat_Availabilit"= 3.48, 
                      "Current_Climate&Habitat_Availabilit&Past_Climate" = 0.03))#negative

plot(Lotic.South,
     quantities = list(type = "counts", fontsize = 18),
     labels = FALSE,
     legend = list(labels = c("Past Climate", "Current Climate", "Habitat availability"),
       side = "right"))

# Add a title to the graph
grid.text("Lotic South", 
          x = unit(1, "npc") - unit(1, "lines"), 
          y = unit(1, "npc") - unit(1, "lines"), 
          just = c("right", "top"),               
          gp = gpar(fontsize = 16, fontface = "bold"))



###  Lotic.North
Lotic.North <- euler(c(Past_Climate= 9.04,
                      Current_Climate= 3.9,
                      Habitat_Availabilit= 5.74,
                     "Past_Climate&Current_Climate"=22.72,
                      "Past_Climate&Habitat_Availabilit"= 3.18,
                      "Current_Climate&Habitat_Availabilit"= 0.65, 
                      "Current_Climate&Habitat_Availabilit&Past_Climate" = 4.45))
plot(Lotic.North,
     quantities = list(type = "counts", fontsize = 18),
     labels = FALSE,
     legend = list(labels = c("Past Climate", "Current Climate", "Habitat availability"),
       side = "right"))

# Add a title to the graph
grid.text("Lotic North", 
          x = unit(1, "npc") - unit(1, "lines"), 
          y = unit(1, "npc") - unit(1, "lines"), 
          just = c("right", "top"),               
          gp = gpar(fontsize = 16, fontface = "bold"))




### Lentic.South
Lentic.South <- euler(c(Past_Climate= 14.18,
                      Current_Climate= 18.75,
                      Habitat_Availabilit= 0.07,
                     "Past_Climate&Current_Climate"=8.51,
                      "Past_Climate&Habitat_Availabilit"= 1.69,
                      "Current_Climate&Habitat_Availabilit"= 1.32,
                      "Current_Climate&Habitat_Availabilit&Past_Climate" = 1.06))
plot(Lentic.South,
     quantities = list(type = "counts", fontsize = 18),
     labels = FALSE,
     legend = list(labels = c("Past Climate", "Current Climate", "Habitat availability"),
       side = "right"))

# Add a title to the graph
grid.text("Lentic South", 
          x = unit(1, "npc") - unit(1, "lines"), 
          y = unit(1, "npc") - unit(1, "lines"), 
          just = c("right", "top"),               
          gp = gpar(fontsize = 16, fontface = "bold"))



### Lentic.North
Lentic.North <- euler(c(Past_Climate= 11.17,
                      Current_Climate= 8.24,
                      Habitat_Availabilit= 1.82,
                     "Past_Climate&Current_Climate"=32.78,
                      "Past_Climate&Habitat_Availabilit"= 2.17,
                      "Current_Climate&Habitat_Availabilit"= 0.75,  #negative
                      "Current_Climate&Habitat_Availabilit&Past_Climate" = 8.72))
plot(Lentic.North,
     quantities = list(type = "counts", fontsize = 18),
     labels = FALSE,
     legend = list(labels = c("Past Climate", "Current Climate", "Habitat availability"),
       side = "right"))

# Add a title to the graph
grid.text("Lentic North", 
          x = unit(1, "npc") - unit(1, "lines"), 
          y = unit(1, "npc") - unit(1, "lines"), 
          just = c("right", "top"),               
          gp = gpar(fontsize = 16, fontface = "bold"))



