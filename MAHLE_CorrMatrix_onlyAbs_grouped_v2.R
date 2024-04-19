##################################################################################################
# Business Analytics Master Project
# MCT 4: Maximilian Drüschler, Ivana Grbus, Kim Hoffmann, Mika Sang, Wolfram Stahl
##################################################################################################


getwd()

# Installing and loading packages
pkgs <- list("glmnet", "reticulate", "stringr", "rstudioapi", "data.table", "parallel", "minpack.lm", "doParallel",
             "foreach", "pROC", "gplots", "pwr", "dplyr", "caret", "sm", "ggplot2", "scales", "reshape2", "Hmisc",
             "bayesAB", "gridExtra", "plotly", "flux", "RColorBrewer", "plm", "xts", "pdp", "vip", "ranger", "vioplot", "corrplot")
# install packages in list
lapply(pkgs, install.packages, character.only = T)

# load packages in list
lapply(pkgs, require, character.only = T)

# loading data
data <- BAMP_Master_Excel_v6_WIP_corrected_

## Hier Spalten relevanten auswählen
data_selected <- data %>%
  select(c(1:8, 9:10, 15:16, 21:22, 27:28, 33:34, 39:40, 45:46, 51:52, 57:58, 63:64, 69:70, 75:76, 81:82, 87:88, 93:94, 99:107))
# checking last columns 
summary(data_selected)[, 39:47]
# column 99 -> Platzhalter, keine Werte
# Sales keine Kennzahl der OSC-> brauchen wir für Korrelationsanalyse nicht

data_selected <- data_selected %>%
  select(c(1:38, 40 , 42))

summary(data_selected)

## creating one data frame for YTD features and one for MTD features
data_YTD <- data_selected %>%
  select(c(1:8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 39))

data_MTD <- data_selected %>%
  select(c(1:8, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 40))



### turning 'NULL' into 'NA' and changing column types to numerical

numerical_columns <- c(9:24) # replace with your column numbers

# for YTD dataframe
data_YTD[, numerical_columns] <- lapply(data_YTD[, numerical_columns], function(x) {
  x[x == 'NULL'] <- NA
  return(x)
})


data_YTD[, numerical_columns] <- lapply(data_YTD[, numerical_columns], function(x) {
  x <- as.numeric(x)
  return(x)
})

summary(data_YTD)

# for MTD dataframe
data_MTD[, numerical_columns] <- lapply(data_MTD[, numerical_columns], function(x) {
  x[x == 'NULL'] <- NA
  return(x)
})


data_MTD[, numerical_columns] <- lapply(data_MTD[, numerical_columns], function(x) {
  x <- as.numeric(x)
  return(x)
})

summary(data_MTD)

########################## Creating subsets for each region

###### Europe
data_MTD_Europe <- data_MTD %>%
  filter(Region=="Europe")

data_YTD_Europe <- data_YTD %>%
  filter(Region=="Europe")


###### Africa
data_MTD_Africa <- data_MTD %>%
  filter(Region=="Africa")

data_YTD_Africa <- data_YTD %>%
  filter(Region=="Africa")


###### South America
data_MTD_SouthAmerica <- data_MTD %>%
  filter(Region=="South America")

data_YTD_SouthAmerica <- data_YTD %>%
  filter(Region=="South America")


###### North America
data_MTD_NorthAmerica <- data_MTD %>%
  filter(Region=="North America")

data_YTD_NorthAmerica <- data_YTD %>%
  filter(Region=="North America")


###### Asia/Pacific
data_MTD_AsiaPacific <- data_MTD %>%
  filter(Region=="Asia/Pacific")

data_YTD_AsiaPacific <- data_YTD %>%
  filter(Region=="Asia/Pacific")

########################## Correlation analysis
################## Europe
################# MTD
############ With missing values

# Standardize the variables
data_MTD_Europe_scaled <- scale(data_MTD_Europe[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_Europe <- cor(data_MTD_Europe_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_MTD_Europe)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD <- function(cor_matrix_MTD){
  cor_matrix_MTD[lower.tri(cor_matrix_MTD)]<- NA
  return(cor_matrix_MTD)
}

upper_tri_MTD_Europe <- get_upper_tri_MTD(cor_matrix_MTD_Europe)

melted_cormat_MTD_Europe <- melt(upper_tri_MTD_Europe, na.rm = TRUE)

cor_heat_MTD_Europe <- ggplot(data = melted_cormat_MTD_Europe, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("MTD Europe with NA Absolutes only")

cor_heat_MTD_Europe

# Save to working directory
ggsave("cormat_MTD_Europe_onlyAbs.png", plot = cor_heat_MTD_Europe, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_Europe_onlyAbs.jpg", plot = cor_heat_MTD_Europe, width = 10, height = 8, units = "in", dpi = 600)

################# MTD
############ Without missing values

# Remove rows with missing values
MTD_Europe_noNA <- na.omit(data_MTD_Europe)

# Standardize the variables
MTD_Europe_noNA_scaled <- scale(MTD_Europe_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_Europe_noNA <- cor(MTD_Europe_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_MTD_Europe_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD_noNA <- function(cor_matrix_MTD_noNA){
  cor_matrix_MTD_noNA[lower.tri(cor_matrix_MTD_noNA)]<- NA
  return(cor_matrix_MTD_noNA)
}

upper_tri_MTD_Europe_noNA <- get_upper_tri_MTD_noNA(cor_matrix_MTD_Europe_noNA)

melted_cormat_MTD_Europe_noNA <- melt(upper_tri_MTD_Europe_noNA, na.rm = TRUE)

cor_heat_MTD_Europe_noNA <- ggplot(data = melted_cormat_MTD_Europe_noNA, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("MTD Europe without NA Absolutes only")

cor_heat_MTD_Europe_noNA

# Save to working directory
ggsave("cormat_MTD_Europe_noNA_onlyAbs.png", plot = cor_heat_MTD_Europe_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_Europe_noNA_onlyAbs.jpg", plot = cor_heat_MTD_Europe_noNA, width = 10, height = 8, units = "in", dpi = 600)

################# YTD
############ With missing values

# Standardize the variables
data_YTD_Europe_scaled <- scale(data_YTD_Europe[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_Europe <- cor(data_YTD_Europe_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_YTD_Europe)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD <- function(cor_matrix_YTD){
  cor_matrix_YTD[lower.tri(cor_matrix_YTD)]<- NA
  return(cor_matrix_YTD)
}

upper_tri_YTD_Europe <- get_upper_tri_YTD(cor_matrix_YTD_Europe)

melted_cormat_YTD_Europe <- melt(upper_tri_YTD_Europe, na.rm = TRUE)

cor_heat_YTD_Europe <- ggplot(data = melted_cormat_YTD_Europe, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("YTD Europe with NA Absolutes only")

cor_heat_YTD_Europe

# Save to working directory
ggsave("cormat_YTD_Europe_onlyAbs.png", plot = cor_heat_YTD_Europe, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_Europe_onlyAbs.jpg", plot = cor_heat_YTD_Europe, width = 10, height = 8, units = "in", dpi = 600)

########################## Correlation analysis
################# YTD
############ Without missing values

# Remove rows with missing values
YTD_Europe_noNA <- na.omit(data_YTD_Europe)

# Standardize the variables
YTD_Europe_noNA_scaled <- scale(YTD_Europe_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_Europe_noNA <- cor(YTD_Europe_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_YTD_Europe_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD_noNA <- function(cor_matrix_YTD_noNA){
  cor_matrix_YTD_noNA[lower.tri(cor_matrix_YTD_noNA)]<- NA
  return(cor_matrix_YTD_noNA)
}

upper_tri_YTD_Europe_noNA <- get_upper_tri_YTD_noNA(cor_matrix_YTD_Europe_noNA)

melted_cormat_YTD_Europe_noNA <- melt(upper_tri_YTD_Europe_noNA, na.rm = TRUE)

cor_heat_YTD_Europe_noNA <- ggplot(data = melted_cormat_YTD_Europe_noNA, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("YTD Europe without NA Absolutes only")

cor_heat_YTD_Europe_noNA

# Save to working directory
ggsave("cormat_YTD_Europe_noNA_onlyAbs.png", plot = cor_heat_YTD_Europe_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_Europe_noNA_onlyAbs.jpg", plot = cor_heat_YTD_Europe_noNA, width = 10, height = 8, units = "in", dpi = 600)



########################################################################

################## Africa
################# MTD
############ With missing values

# Standardize the variables
data_MTD_Africa_scaled <- scale(data_MTD_Africa[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_Africa <- cor(data_MTD_Africa_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_MTD_Africa)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD <- function(cor_matrix_MTD){
  cor_matrix_MTD[lower.tri(cor_matrix_MTD)]<- NA
  return(cor_matrix_MTD)
}

upper_tri_MTD_Africa <- get_upper_tri_MTD(cor_matrix_MTD_Africa)

melted_cormat_MTD_Africa <- melt(upper_tri_MTD_Africa, na.rm = TRUE)

cor_heat_MTD_Africa <- ggplot(data = melted_cormat_MTD_Africa, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("MTD Africa with NA Absolutes only")

cor_heat_MTD_Africa

# Save to working directory
ggsave("cormat_MTD_Africa_onlyAbs.png", plot = cor_heat_MTD_Africa, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_Africa_onlyAbs.jpg", plot = cor_heat_MTD_Africa, width = 10, height = 8, units = "in", dpi = 600)

################# MTD
############ Without missing values

# Remove rows with missing values
MTD_Africa_noNA <- na.omit(data_MTD_Africa)

# Standardize the variables
MTD_Africa_noNA_scaled <- scale(MTD_Africa_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_Africa_noNA <- cor(MTD_Africa_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_MTD_Africa_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD_noNA <- function(cor_matrix_MTD_noNA){
  cor_matrix_MTD_noNA[lower.tri(cor_matrix_MTD_noNA)]<- NA
  return(cor_matrix_MTD_noNA)
}

upper_tri_MTD_Africa_noNA <- get_upper_tri_MTD_noNA(cor_matrix_MTD_Africa_noNA)

melted_cormat_MTD_Africa_noNA <- melt(upper_tri_MTD_Africa_noNA, na.rm = TRUE)

cor_heat_MTD_Africa_noNA <- ggplot(data = melted_cormat_MTD_Africa_noNA, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("MTD Africa without NA Absolutes only")

cor_heat_MTD_Africa_noNA

# Save to working directory
ggsave("cormat_MTD_Africa_noNA_onlyAbs.png", plot = cor_heat_MTD_Africa_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_Africa_noNA_onlyAbs.jpg", plot = cor_heat_MTD_Africa_noNA, width = 10, height = 8, units = "in", dpi = 600)

################# YTD
############ With missing values

# Standardize the variables
data_YTD_Africa_scaled <- scale(data_YTD_Africa[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_Africa <- cor(data_YTD_Africa_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_YTD_Africa)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD <- function(cor_matrix_YTD){
  cor_matrix_YTD[lower.tri(cor_matrix_YTD)]<- NA
  return(cor_matrix_YTD)
}

upper_tri_YTD_Africa <- get_upper_tri_YTD(cor_matrix_YTD_Africa)

melted_cormat_YTD_Africa <- melt(upper_tri_YTD_Africa, na.rm = TRUE)

cor_heat_YTD_Africa <- ggplot(data = melted_cormat_YTD_Africa, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("YTD Africa with NA Absolutes only")

cor_heat_YTD_Africa

# Save to working directory
ggsave("cormat_YTD_Africa_onlyAbs.png", plot = cor_heat_YTD_Africa, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_Africa_onlyAbs.jpg", plot = cor_heat_YTD_Africa, width = 10, height = 8, units = "in", dpi = 600)

########################## Correlation analysis
################# YTD
############ Without missing values

# Remove rows with missing values
YTD_Africa_noNA <- na.omit(data_YTD_Africa)

# Standardize the variables
YTD_Africa_noNA_scaled <- scale(YTD_Africa_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_Africa_noNA <- cor(YTD_Africa_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_YTD_Africa_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD_noNA <- function(cor_matrix_YTD_noNA){
  cor_matrix_YTD_noNA[lower.tri(cor_matrix_YTD_noNA)]<- NA
  return(cor_matrix_YTD_noNA)
}

upper_tri_YTD_Africa_noNA <- get_upper_tri_YTD_noNA(cor_matrix_YTD_Africa_noNA)

melted_cormat_YTD_Africa_noNA <- melt(upper_tri_YTD_Africa_noNA, na.rm = TRUE)

cor_heat_YTD_Africa_noNA <- ggplot(data = melted_cormat_YTD_Africa_noNA, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("YTD Africa without NA Absolutes only")

cor_heat_YTD_Africa_noNA

# Save to working directory
ggsave("cormat_YTD_Africa_noNA_onlyAbs.png", plot = cor_heat_YTD_Africa_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_Africa_noNA_onlyAbs.jpg", plot = cor_heat_YTD_Africa_noNA, width = 10, height = 8, units = "in", dpi = 600)


########################################################################

################## SouthAmerica
################# MTD
############ With missing values

# Standardize the variables
data_MTD_SouthAmerica_scaled <- scale(data_MTD_SouthAmerica[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_SouthAmerica <- cor(data_MTD_SouthAmerica_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_MTD_SouthAmerica)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD <- function(cor_matrix_MTD){
  cor_matrix_MTD[lower.tri(cor_matrix_MTD)]<- NA
  return(cor_matrix_MTD)
}

upper_tri_MTD_SouthAmerica <- get_upper_tri_MTD(cor_matrix_MTD_SouthAmerica)

melted_cormat_MTD_SouthAmerica <- melt(upper_tri_MTD_SouthAmerica, na.rm = TRUE)

cor_heat_MTD_SouthAmerica <- ggplot(data = melted_cormat_MTD_SouthAmerica, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("MTD SouthAmerica with NA Absolutes only")

cor_heat_MTD_SouthAmerica

# Save to working directory
ggsave("cormat_MTD_SouthAmerica_onlyAbs.png", plot = cor_heat_MTD_SouthAmerica, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_SouthAmerica_onlyAbs.jpg", plot = cor_heat_MTD_SouthAmerica, width = 10, height = 8, units = "in", dpi = 600)

################# MTD
############ Without missing values

# Remove rows with missing values
MTD_SouthAmerica_noNA <- na.omit(data_MTD_SouthAmerica)

# Standardize the variables
MTD_SouthAmerica_noNA_scaled <- scale(MTD_SouthAmerica_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_SouthAmerica_noNA <- cor(MTD_SouthAmerica_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_MTD_SouthAmerica_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD_noNA <- function(cor_matrix_MTD_noNA){
  cor_matrix_MTD_noNA[lower.tri(cor_matrix_MTD_noNA)]<- NA
  return(cor_matrix_MTD_noNA)
}

upper_tri_MTD_SouthAmerica_noNA <- get_upper_tri_MTD_noNA(cor_matrix_MTD_SouthAmerica_noNA)

melted_cormat_MTD_SouthAmerica_noNA <- melt(upper_tri_MTD_SouthAmerica_noNA, na.rm = TRUE)

cor_heat_MTD_SouthAmerica_noNA <- ggplot(data = melted_cormat_MTD_SouthAmerica_noNA, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("MTD SouthAmerica without NA Absolutes only")

cor_heat_MTD_SouthAmerica_noNA

# Save to working directory
ggsave("cormat_MTD_SouthAmerica_noNA_onlyAbs.png", plot = cor_heat_MTD_SouthAmerica_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_SouthAmerica_noNA_onlyAbs.jpg", plot = cor_heat_MTD_SouthAmerica_noNA, width = 10, height = 8, units = "in", dpi = 600)

################# YTD
############ With missing values

# Standardize the variables
data_YTD_SouthAmerica_scaled <- scale(data_YTD_SouthAmerica[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_SouthAmerica <- cor(data_YTD_SouthAmerica_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_YTD_SouthAmerica)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD <- function(cor_matrix_YTD){
  cor_matrix_YTD[lower.tri(cor_matrix_YTD)]<- NA
  return(cor_matrix_YTD)
}

upper_tri_YTD_SouthAmerica <- get_upper_tri_YTD(cor_matrix_YTD_SouthAmerica)

melted_cormat_YTD_SouthAmerica <- melt(upper_tri_YTD_SouthAmerica, na.rm = TRUE)

cor_heat_YTD_SouthAmerica <- ggplot(data = melted_cormat_YTD_SouthAmerica, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("YTD SouthAmerica with NA Absolutes only")

cor_heat_YTD_SouthAmerica

# Save to working directory
ggsave("cormat_YTD_SouthAmerica_onlyAbs.png", plot = cor_heat_YTD_SouthAmerica, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_SouthAmerica_onlyAbs.jpg", plot = cor_heat_YTD_SouthAmerica, width = 10, height = 8, units = "in", dpi = 600)

########################## Correlation analysis
################# YTD
############ Without missing values

# Remove rows with missing values
YTD_SouthAmerica_noNA <- na.omit(data_YTD_SouthAmerica)

# Standardize the variables
YTD_SouthAmerica_noNA_scaled <- scale(YTD_SouthAmerica_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_SouthAmerica_noNA <- cor(YTD_SouthAmerica_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_YTD_SouthAmerica_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD_noNA <- function(cor_matrix_YTD_noNA){
  cor_matrix_YTD_noNA[lower.tri(cor_matrix_YTD_noNA)]<- NA
  return(cor_matrix_YTD_noNA)
}

upper_tri_YTD_SouthAmerica_noNA <- get_upper_tri_YTD_noNA(cor_matrix_YTD_SouthAmerica_noNA)

melted_cormat_YTD_SouthAmerica_noNA <- melt(upper_tri_YTD_SouthAmerica_noNA, na.rm = TRUE)

cor_heat_YTD_SouthAmerica_noNA <- ggplot(data = melted_cormat_YTD_SouthAmerica_noNA, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("YTD SouthAmerica without NA Absolutes only")

cor_heat_YTD_SouthAmerica_noNA

# Save to working directory
ggsave("cormat_YTD_SouthAmerica_noNA_onlyAbs.png", plot = cor_heat_YTD_SouthAmerica_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_SouthAmerica_noNA_onlyAbs.jpg", plot = cor_heat_YTD_SouthAmerica_noNA, width = 10, height = 8, units = "in", dpi = 600)


########################################################################

################## NorthAmerica
################# MTD
############ With missing values

# Standardize the variables
data_MTD_NorthAmerica_scaled <- scale(data_MTD_NorthAmerica[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_NorthAmerica <- cor(data_MTD_NorthAmerica_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_MTD_NorthAmerica)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD <- function(cor_matrix_MTD){
  cor_matrix_MTD[lower.tri(cor_matrix_MTD)]<- NA
  return(cor_matrix_MTD)
}

upper_tri_MTD_NorthAmerica <- get_upper_tri_MTD(cor_matrix_MTD_NorthAmerica)

melted_cormat_MTD_NorthAmerica <- melt(upper_tri_MTD_NorthAmerica, na.rm = TRUE)

cor_heat_MTD_NorthAmerica <- ggplot(data = melted_cormat_MTD_NorthAmerica, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("MTD NorthAmerica with NA Absolutes only")

cor_heat_MTD_NorthAmerica

# Save to working directory
ggsave("cormat_MTD_NorthAmerica_onlyAbs.png", plot = cor_heat_MTD_NorthAmerica, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_NorthAmerica_onlyAbs.jpg", plot = cor_heat_MTD_NorthAmerica, width = 10, height = 8, units = "in", dpi = 600)

################# MTD
############ Without missing values

# Remove rows with missing values
MTD_NorthAmerica_noNA <- na.omit(data_MTD_NorthAmerica)

# Standardize the variables
MTD_NorthAmerica_noNA_scaled <- scale(MTD_NorthAmerica_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_NorthAmerica_noNA <- cor(MTD_NorthAmerica_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_MTD_NorthAmerica_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD_noNA <- function(cor_matrix_MTD_noNA){
  cor_matrix_MTD_noNA[lower.tri(cor_matrix_MTD_noNA)]<- NA
  return(cor_matrix_MTD_noNA)
}

upper_tri_MTD_NorthAmerica_noNA <- get_upper_tri_MTD_noNA(cor_matrix_MTD_NorthAmerica_noNA)

melted_cormat_MTD_NorthAmerica_noNA <- melt(upper_tri_MTD_NorthAmerica_noNA, na.rm = TRUE)

cor_heat_MTD_NorthAmerica_noNA <- ggplot(data = melted_cormat_MTD_NorthAmerica_noNA, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("MTD NorthAmerica without NA Absolutes only")

cor_heat_MTD_NorthAmerica_noNA

# Save to working directory
ggsave("cormat_MTD_NorthAmerica_noNA_onlyAbs.png", plot = cor_heat_MTD_NorthAmerica_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_NorthAmerica_noNA_onlyAbs.jpg", plot = cor_heat_MTD_NorthAmerica_noNA, width = 10, height = 8, units = "in", dpi = 600)

################# YTD
############ With missing values

# Standardize the variables
data_YTD_NorthAmerica_scaled <- scale(data_YTD_NorthAmerica[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_NorthAmerica <- cor(data_YTD_NorthAmerica_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_YTD_NorthAmerica)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD <- function(cor_matrix_YTD){
  cor_matrix_YTD[lower.tri(cor_matrix_YTD)]<- NA
  return(cor_matrix_YTD)
}

upper_tri_YTD_NorthAmerica <- get_upper_tri_YTD(cor_matrix_YTD_NorthAmerica)

melted_cormat_YTD_NorthAmerica <- melt(upper_tri_YTD_NorthAmerica, na.rm = TRUE)

cor_heat_YTD_NorthAmerica <- ggplot(data = melted_cormat_YTD_NorthAmerica, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("YTD NorthAmerica with NA Absolutes only")

cor_heat_YTD_NorthAmerica

# Save to working directory
ggsave("cormat_YTD_NorthAmerica_onlyAbs.png", plot = cor_heat_YTD_NorthAmerica, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_NorthAmerica_onlyAbs.jpg", plot = cor_heat_YTD_NorthAmerica, width = 10, height = 8, units = "in", dpi = 600)

########################## Correlation analysis
################# YTD
############ Without missing values

# Remove rows with missing values
YTD_NorthAmerica_noNA <- na.omit(data_YTD_NorthAmerica)

# Standardize the variables
YTD_NorthAmerica_noNA_scaled <- scale(YTD_NorthAmerica_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_NorthAmerica_noNA <- cor(YTD_NorthAmerica_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_YTD_NorthAmerica_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD_noNA <- function(cor_matrix_YTD_noNA){
  cor_matrix_YTD_noNA[lower.tri(cor_matrix_YTD_noNA)]<- NA
  return(cor_matrix_YTD_noNA)
}

upper_tri_YTD_NorthAmerica_noNA <- get_upper_tri_YTD_noNA(cor_matrix_YTD_NorthAmerica_noNA)

melted_cormat_YTD_NorthAmerica_noNA <- melt(upper_tri_YTD_NorthAmerica_noNA, na.rm = TRUE)

cor_heat_YTD_NorthAmerica_noNA <- ggplot(data = melted_cormat_YTD_NorthAmerica_noNA, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("YTD NorthAmerica without NA Absolutes only")

cor_heat_YTD_NorthAmerica_noNA

# Save to working directory
ggsave("cormat_YTD_NorthAmerica_noNA_onlyAbs.png", plot = cor_heat_YTD_NorthAmerica_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_NorthAmerica_noNA_onlyAbs.jpg", plot = cor_heat_YTD_NorthAmerica_noNA, width = 10, height = 8, units = "in", dpi = 600)


########################################################################

################## AsiaPacific
################# MTD
############ With missing values

# Standardize the variables
data_MTD_AsiaPacific_scaled <- scale(data_MTD_AsiaPacific[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_AsiaPacific <- cor(data_MTD_AsiaPacific_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_MTD_AsiaPacific)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD <- function(cor_matrix_MTD){
  cor_matrix_MTD[lower.tri(cor_matrix_MTD)]<- NA
  return(cor_matrix_MTD)
}

upper_tri_MTD_AsiaPacific <- get_upper_tri_MTD(cor_matrix_MTD_AsiaPacific)

melted_cormat_MTD_AsiaPacific <- melt(upper_tri_MTD_AsiaPacific, na.rm = TRUE)

cor_heat_MTD_AsiaPacific <- ggplot(data = melted_cormat_MTD_AsiaPacific, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("MTD AsiaPacific with NA Absolutes only")

cor_heat_MTD_AsiaPacific

# Save to working directory
ggsave("cormat_MTD_AsiaPacific_onlyAbs.png", plot = cor_heat_MTD_AsiaPacific, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_AsiaPacific_onlyAbs.jpg", plot = cor_heat_MTD_AsiaPacific, width = 10, height = 8, units = "in", dpi = 600)

################# MTD
############ Without missing values

# Remove rows with missing values
MTD_AsiaPacific_noNA <- na.omit(data_MTD_AsiaPacific)

# Standardize the variables
MTD_AsiaPacific_noNA_scaled <- scale(MTD_AsiaPacific_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_AsiaPacific_noNA <- cor(MTD_AsiaPacific_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_MTD_AsiaPacific_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD_noNA <- function(cor_matrix_MTD_noNA){
  cor_matrix_MTD_noNA[lower.tri(cor_matrix_MTD_noNA)]<- NA
  return(cor_matrix_MTD_noNA)
}

upper_tri_MTD_AsiaPacific_noNA <- get_upper_tri_MTD_noNA(cor_matrix_MTD_AsiaPacific_noNA)

melted_cormat_MTD_AsiaPacific_noNA <- melt(upper_tri_MTD_AsiaPacific_noNA, na.rm = TRUE)

cor_heat_MTD_AsiaPacific_noNA <- ggplot(data = melted_cormat_MTD_AsiaPacific_noNA, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("MTD AsiaPacific without NA Absolutes only")

cor_heat_MTD_AsiaPacific_noNA

# Save to working directory
ggsave("cormat_MTD_AsiaPacific_noNA_onlyAbs.png", plot = cor_heat_MTD_AsiaPacific_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_AsiaPacific_noNA_onlyAbs.jpg", plot = cor_heat_MTD_AsiaPacific_noNA, width = 10, height = 8, units = "in", dpi = 600)

################# YTD
############ With missing values

# Standardize the variables
data_YTD_AsiaPacific_scaled <- scale(data_YTD_AsiaPacific[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_AsiaPacific <- cor(data_YTD_AsiaPacific_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_YTD_AsiaPacific)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD <- function(cor_matrix_YTD){
  cor_matrix_YTD[lower.tri(cor_matrix_YTD)]<- NA
  return(cor_matrix_YTD)
}

upper_tri_YTD_AsiaPacific <- get_upper_tri_YTD(cor_matrix_YTD_AsiaPacific)

melted_cormat_YTD_AsiaPacific <- melt(upper_tri_YTD_AsiaPacific, na.rm = TRUE)

cor_heat_YTD_AsiaPacific <- ggplot(data = melted_cormat_YTD_AsiaPacific, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("YTD AsiaPacific with NA Absolutes only")

cor_heat_YTD_AsiaPacific

# Save to working directory
ggsave("cormat_YTD_AsiaPacific_onlyAbs.png", plot = cor_heat_YTD_AsiaPacific, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_AsiaPacific_onlyAbs.jpg", plot = cor_heat_YTD_AsiaPacific, width = 10, height = 8, units = "in", dpi = 600)

########################## Correlation analysis
################# YTD
############ Without missing values

# Remove rows with missing values
YTD_AsiaPacific_noNA <- na.omit(data_YTD_AsiaPacific)

# Standardize the variables
YTD_AsiaPacific_noNA_scaled <- scale(YTD_AsiaPacific_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_AsiaPacific_noNA <- cor(YTD_AsiaPacific_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_YTD_AsiaPacific_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD_noNA <- function(cor_matrix_YTD_noNA){
  cor_matrix_YTD_noNA[lower.tri(cor_matrix_YTD_noNA)]<- NA
  return(cor_matrix_YTD_noNA)
}

upper_tri_YTD_AsiaPacific_noNA <- get_upper_tri_YTD_noNA(cor_matrix_YTD_AsiaPacific_noNA)

melted_cormat_YTD_AsiaPacific_noNA <- melt(upper_tri_YTD_AsiaPacific_noNA, na.rm = TRUE)

cor_heat_YTD_AsiaPacific_noNA <- ggplot(data = melted_cormat_YTD_AsiaPacific_noNA, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20, unit = "pt")))+
  ggtitle("YTD AsiaPacific without NA Absolutes only")

cor_heat_YTD_AsiaPacific_noNA

# Save to working directory
ggsave("cormat_YTD_AsiaPacific_noNA_onlyAbs.png", plot = cor_heat_YTD_AsiaPacific_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_AsiaPacific_noNA_onlyAbs.jpg", plot = cor_heat_YTD_AsiaPacific_noNA, width = 10, height = 8, units = "in", dpi = 600)

