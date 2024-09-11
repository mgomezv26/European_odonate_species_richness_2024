
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

cat("Versions of the libraries used:\n")
cat("dplyr:", as.character(packageVersion("dplyr")), "\n")
cat("ggplot2:", as.character(packageVersion("ggplot2")), "\n")
cat("gridExtra:", as.character(packageVersion("gridExtra")), "\n")
cat("tidyverse:", as.character(packageVersion("tidyverse")), "\n")
cat("paletteer:", as.character(packageVersion("paletteer")), "\n")
cat("eulerr:", as.character(packageVersion("eulerr")), "\n")
cat("cowplot:", as.character(packageVersion("cowplot")), "\n")



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



"
###################################################################
##     Dividing study area in Northern and Southern Europe       ##
###################################################################
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
As species richness is a count data, we assumed a Poisson distribution and a logarithmic joint function
"

##############################
## GLM
##############################

## Lotic species
m.eu.lot <- glm(R_Lotic  ~ scale(vart) + scale(varp) + scale(Temp_0)  + scale(Prec_0) + scale(H_Lot) ,family=poisson, data=da)
m.nor.lot <- glm(R_Lotic  ~ scale(vart) + scale(varp) + scale(Temp_0)  + scale(Prec_0) + scale(H_Lot) ,family=poisson, data=nor)
m.sur.lot <- glm(R_Lotic  ~ scale(vart) + scale(varp) + scale(Temp_0)  + scale(Prec_0) + scale(H_Lot)  ,family=poisson, data=sur)

summary(m.eu.lot)
summary(m.nor.lot)
summary(m.sur.lot)

### R2
1 - summary(m.eu.lot)$deviance/summary(m.eu.lot)$null.deviance
1 - summary(m.nor.lot)$deviance/summary(m.nor.lot)$null.deviance
1 - summary(m.sur.lot)$deviance/summary(m.sur.lot)$null.deviance

## Lentic species
m.eu.len <- glm(R_Lentic ~ scale(vart) + scale(varp) + scale(Temp_0)  + scale(Prec_0) + scale(H_Lent) ,family=poisson, data=da)
m.nor.len <- glm(R_Lentic ~ scale(vart) + scale(varp) + scale(Temp_0)  + scale(Prec_0) + scale(H_Lent) ,family=poisson, data=nor)
m.sur.len <- glm(R_Lentic ~ scale(vart) + scale(varp) + scale(Temp_0)  + scale(Prec_0) + scale(H_Lent) ,family=poisson, data=sur)

summary(m.eu.len)
summary(m.nor.len)
summary(m.sur.len)

### R2
1 - summary(m.eu.len)$deviance/summary(m.eu.len)$null.deviance
1 - summary(m.nor.len)$deviance/summary(m.nor.len)$null.deviance
1 - summary(m.sur.len)$deviance/summary(m.sur.len)$null.deviance

## Odonata
m.eu.all <- glm(R_Odonata ~ scale(vart) + scale(varp) + scale(Temp_0)  + scale(Prec_0) + scale(H_Lot) + scale(H_Lent),family=poisson, data=da)
m.nor.all <- glm(R_Odonata ~ scale(vart) + scale(varp) + scale(Temp_0)  + scale(Prec_0) + scale(H_Lot) + scale(H_Lent),family=poisson, data=nor)
m.sur.all<- glm(R_Odonata ~ scale(vart) + scale(varp) + scale(Temp_0)  + scale(Prec_0) + scale(H_Lot) + scale(H_Lent),family=poisson, data=sur)

summary(m.eu.all)
summary(m.nor.all)
summary(m.sur.all)

### R2
1 - summary(m.eu.all)$deviance/summary(m.eu.all)$null.deviance
1 - summary(m.nor.all)$deviance/summary(m.nor.all)$null.deviance
1 - summary(m.sur.all)$deviance/summary(m.sur.all)$null.deviance

### Plots
plot(m.eu.lot)
plot(m.nor.lot)
plot(m.sur.lot)
plot(m.eu.len)
plot(m.nor.len)
plot(m.nor.len)
plot(m.eu.all)
plot(m.nor.all)
plot(m.nor.all)

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



