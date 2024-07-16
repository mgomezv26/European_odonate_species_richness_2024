
"
###########################################
##           LOAD LIBRARIES              ##
###########################################
"
# R version 4.3.3 (2024-02-29 ucrt)
library(dplyr) # version: 1.1.4 
library (ggplot2) # version: 3.5.0  
library(gridExtra) # version: 2.3
library(tidyverse) # version: 2.0.0 
library(paletteer) # version: 1.6.0 
library(eulerr) # version: 7.0.1 


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
- pure.pas.r2: Climate_Anomaly
- pure.pre.r2:  Climate_Anomaly
- pure.hab.r2: Habitat_Amount
- over.pas.pre1: Climate_Anomaly&Current_Climate
- over.pas.hab: Current_Climate&Habitat_Amount
- over.pre.hab1: Current_Climate&Habitat_Amount
- over.tot: Current_Climate&Habitat_Amount&Climate_Anomaly

"

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
boxplot_odonata <- ggplot(stacked_data, aes(x = Zone, y = R_Odonata, fill = Zone)) +
  geom_boxplot(alpha = 0.5, outlier.colour = "#A52A2A") +
  scale_fill_manual(values = c("#FF7F50", "#49A4B9")) +
  theme_bw()

boxplot_lotic <- ggplot(stacked_data, aes(x = Zone, y = R_Lotic , fill = Zone)) +
  geom_boxplot(alpha = 0.5, outlier.colour = "#A52A2A") +
  scale_fill_manual(values = c("#FF7F50", "#49A4B9")) +
  theme_bw()

boxplot_lentic <- ggplot(stacked_data, aes(x = Zone, y = R_Lentic, fill = Zone)) +
  geom_boxplot(alpha = 0.5, outlier.colour = "#A52A2A") +
  scale_fill_manual(values = c("#FF7F50", "#49A4B9")) +
  theme_bw()

## Grouping graphics into a single figure, adjusting the aspect ratio
grid.arrange(boxplot_odonata, boxplot_lotic, boxplot_lentic, ncol = 3)

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

m.nor.lot <- glm(R_Lotic  ~ scale(vart) + scale(varp) + scale(Temp_0)  + scale(Prec_0) + scale(H_Lot) ,family=poisson, data=nor)
m.sur.lot <- glm(R_Lotic  ~ scale(vart) + scale(varp) + scale(Temp_0)  + scale(Prec_0) + scale(H_Lot)  ,family=poisson, data=sur)

summary(m.nor.lot)
summary(m.sur.lot)

### R2
1 - summary(m.nor.lot)$deviance/summary(m.nor.lot)$null.deviance
1 - summary(m.sur.lot)$deviance/summary(m.sur.lot)$null.deviance

## Lentic species
m.nor.len <- glm(R_Lentic ~ scale(vart) + scale(varp) + scale(Temp_0)  + scale(Prec_0) + scale(H_Lent) ,family=poisson, data=nor)
m.sur.len <- glm(R_Lentic ~ scale(vart) + scale(varp) + scale(Temp_0)  + scale(Prec_0) + scale(H_Lent) ,family=poisson, data=sur)

summary(m.nor.len)
summary(m.sur.len)

### R2
1 - summary(m.nor.len)$deviance/summary(m.nor.len)$null.deviance
1 - summary(m.sur.len)$deviance/summary(m.sur.len)$null.deviance


## Odonata
m.nor.all <- glm(R_Odonata ~ scale(vart) + scale(varp) + scale(Temp_0)  + scale(Prec_0) + scale(H_Lot) + scale(H_Lent),family=poisson, data=nor)
m.sur.all<- glm(R_Odonata ~ scale(vart) + scale(varp) + scale(Temp_0)  + scale(Prec_0) + scale(H_Lot) + scale(H_Lent),family=poisson, data=sur)

summary(m.nor.all)
summary(m.sur.all)

### R2
1 - summary(m.nor.all)$deviance/summary(m.nor.all)$null.deviance
1 - summary(m.sur.all)$deviance/summary(m.sur.all)$null.deviance


### Plots
plot(m.nor.lot)
plot(m.sur.lot)
plot(m.nor.len)
plot(m.nor.len)
plot(m.nor.all)
plot(m.nor.all)


### Scatter plots of supplementary materials

#### Odonata northern Europe

Odonata_plot_nor = nor %>%
  select(ET_ID, ET_Index, R_Odonata, vart, varp, Temp_0, Prec_0, H_Lent, H_Lot)
str(Odonata_plot_nor)

Odonata_plot_nor_long <- Odonata_plot_nor %>% 
  gather(key = study_variable , value = value, vart:H_Lot, factor_key = TRUE)

str(Odonata_plot_nor_long)

my_colors <- paletteer_c("grDevices::Temps", 6) #color palette

ggplot(Odonata_plot_nor_long) +
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

ggplot(Odonata_plot_sur_long) +
  geom_point(mapping = aes(x = value, y = R_Odonata, colour = study_variable), alpha = 0.5) +
  geom_smooth(mapping = aes(x = value, y = R_Odonata), method = "lm", se = TRUE, color = 'darkgrey') +
  scale_color_manual(values = my_colors) +
  facet_wrap(~ study_variable, scales = "free") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Scatter plots for Odonates in southern Europe")


#### Lotic species in northern Europe
Lotic_plot_nor = nor %>%
  select(ET_ID, ET_Index, R_Lotic , vart, varp, Temp_0, Prec_0, H_Lot)

Lotic_plot_nor_long <- Lotic_plot_nor %>% 
  gather(key = study_variable , value = value, vart:H_Lot, factor_key = TRUE)

str(Lotic_plot_nor_long)


my_colors <- paletteer_c("grDevices::Temps", 5) #color palette

ggplot(Lotic_plot_nor_long) +
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

str(Lotic_plot_sur_long)

ggplot(Lotic_plot_sur_long) +
  geom_point(mapping = aes(x = value, y = R_Lotic , colour = study_variable), alpha = 0.5) +
  geom_smooth(mapping = aes(x = value, y = R_Lotic ), method = "lm", se = TRUE, color = 'darkgrey') +
  scale_color_manual(values = my_colors) +
  facet_wrap(~ study_variable, scales = "free") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Scatter plots for Lotic species in southern Europe")


#### Lentic species in northern Europe

Lentic_plot_nor = nor %>%
  select(ET_ID, ET_Index, R_Lentic, vart, varp, Temp_0, Prec_0, H_Lent)

Lentic_plot_nor_long <- Lentic_plot_nor %>% 
  gather(key = study_variable , value = value, vart:H_Lent, factor_key = TRUE)

str(Lentic_plot_nor_long)

ggplot(Lentic_plot_nor_long) +
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

str(Lentic_plot_sur_long)

ggplot(Lentic_plot_sur_long) +
  geom_point(mapping = aes(x = value, y = R_Lentic, colour = study_variable), alpha = 0.5) +
  geom_smooth(mapping = aes(x = value, y = R_Lentic), method = "lm", se = TRUE, color = 'darkgrey') +
  scale_color_manual(values = my_colors) +
  facet_wrap(~ study_variable, scales = "free") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Scatter plots for Lentic species in southern Europe")


############################################
## Partitioned the explained deviances
############################################

# The results obtained with SAM for Partitioning of explained deviances
devianza

## Venn diagrams

###  Odonata.all
plot.venn <- euler(c(Climate_Anomaly= 16.59,
                     Current_Climate= 3.01,
                     Habitat_Amount= 4.20,
                     "Climate_Anomaly&Current_Climate"=7.95,
                     "Climate_Anomaly&Habitat_Amount"= 3.37,
                     "Current_Climate&Habitat_Amount"= 0.33, #negativo
                     "Current_Climate&Habitat_Amount&Climate_Anomaly" = 3.92))

plot(plot.venn,
     quantities = list(type = "counts"))

plot(plot.venn,
     quantities = list(type = "counts"),
     legend = list(side = "right"))



###  Odonata.South
plot.venn <- euler(c(Climate_Anomaly= 9.75,
                     Current_Climate= 15.34,
                     Habitat_Amount= 0.62,
                     "Climate_Anomaly&Current_Climate"=6.21,
                     "Climate_Anomaly&Habitat_Amount"= 2.26,
                     "Current_Climate&Habitat_Amount"= 1.13,
                     "Current_Climate&Habitat_Amount&Climate_Anomaly" = 0.02)) #negativo
plot(plot.venn,
     quantities = list(type = "counts"),
     legend = list(side = "right"))

###  Odonata.North
plot.venn <- euler(c(Climate_Anomaly= 11.24,
                     Current_Climate= 7.73,
                     Habitat_Amount= 2.37,
                     "Climate_Anomaly&Current_Climate"=32.28,
                     "Climate_Anomaly&Habitat_Amount"= 2.44,
                     "Current_Climate&Habitat_Amount"= 0.77, #negativo
                     "Current_Climate&Habitat_Amount&Climate_Anomaly" = 8.33))
plot(plot.venn,
     quantities = list(type = "counts"),
     legend = list(side = "right"))


###  Odonata.North
plot.venn <- euler(c(Climate_Anomaly= 11.24,
                     Current_Climate= 7.73,
                     Habitat_Amount= 2.37,
                     "Climate_Anomaly&Current_Climate"=32.28,
                     "Climate_Anomaly&Habitat_Amount"= 2.44,
                     "Current_Climate&Habitat_Amount"= 0.77, #negativo
                     "Current_Climate&Habitat_Amount&Climate_Anomaly" = 8.33))
plot(plot.venn,
     quantities = list(type = "counts"),
     legend = list(side = "right"))


###  Lotic.all
plot.venn <- euler(c(Climate_Anomaly= 5.82,
                     Current_Climate= 2.44,
                     Habitat_Amount= 4.99,
                     "Climate_Anomaly&Current_Climate"=25.21,
                     "Climate_Anomaly&Habitat_Amount"= 1.41,
                     "Current_Climate&Habitat_Amount"= 0.55, #negativo
                     "Current_Climate&Habitat_Amount&Climate_Anomaly" = 6.14))
plot(plot.venn,
     quantities = list(type = "counts"),
     legend = list(side = "right"))


### Lotic.South
plot.venn <- euler(c(Climate_Anomaly= 6.97,
                     Current_Climate= 14.24,
                     Habitat_Amount= 2.39,
                     "Climate_Anomaly&Current_Climate"=3.05,#negativo
                     "Climate_Anomaly&Habitat_Amount"= 1.5,
                     "Current_Climate&Habitat_Amount"= 3.48, 
                     "Current_Climate&Habitat_Amount&Climate_Anomaly" = 0.03))#negativo

plot(plot.venn,
     quantities = list(type = "counts"),
     legend = list(side = "right"))

###  Lotic.North
plot.venn <- euler(c(Climate_Anomaly= 9.04,
                     Current_Climate= 3.9,
                     Habitat_Amount= 5.74,
                     "Climate_Anomaly&Current_Climate"=22.72,
                     "Climate_Anomaly&Habitat_Amount"= 3.18,
                     "Current_Climate&Habitat_Amount"= 0.65, 
                     "Current_Climate&Habitat_Amount&Climate_Anomaly" = 4.45))
plot(plot.venn,
     quantities = list(type = "counts"),
     legend = list(side = "right"))

###  Lentic.all
plot.venn <- euler(c(Climate_Anomaly= 20.43,
                     Current_Climate= 3.97,
                     Habitat_Amount= 3.21,
                     "Climate_Anomaly&Current_Climate"=1.94,
                     "Climate_Anomaly&Habitat_Amount"= 3.29,
                     "Current_Climate&Habitat_Amount"= 0.22,  #negativo
                     "Current_Climate&Habitat_Amount&Climate_Anomaly" = 2.7))
plot(plot.venn,
     quantities = list(type = "counts"),
     legend = list(side = "right"))

### Lentic.South
plot.venn <- euler(c(Climate_Anomaly= 14.18,
                     Current_Climate= 18.75,
                     Habitat_Amount= 0.07,
                     "Climate_Anomaly&Current_Climate"=8.51,
                     "Climate_Anomaly&Habitat_Amount"= 1.69,
                     "Current_Climate&Habitat_Amount"= 1.32,
                     "Current_Climate&Habitat_Amount&Climate_Anomaly" = 1.06))
plot(plot.venn,
     quantities = list(type = "counts"),
     legend = list(side = "right"))

### Lentic.North
plot.venn <- euler(c(Climate_Anomaly= 11.17,
                     Current_Climate= 8.24,
                     Habitat_Amount= 1.82,
                     "Climate_Anomaly&Current_Climate"=32.78,
                     "Climate_Anomaly&Habitat_Amount"= 2.17,
                     "Current_Climate&Habitat_Amount"= 0.75,  #negativo
                     "Current_Climate&Habitat_Amount&Climate_Anomaly" = 8.72))
plot(plot.venn,
     quantities = list(type = "counts"),
     legend = list(side = "right"))
