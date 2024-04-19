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

########################## Creating subsets for each BU

###### BU1
data_MTD_BU1 <- data_MTD %>%
  filter(BU=="BU1")

data_YTD_BU1 <- data_YTD %>%
  filter(BU=="BU1")


###### BU2
data_MTD_BU2 <- data_MTD %>%
  filter(BU=="BU2")

data_YTD_BU2 <- data_YTD %>%
  filter(BU=="BU2")


###### BU3
data_MTD_BU3 <- data_MTD %>%
  filter(BU=="BU3")

data_YTD_BU3 <- data_YTD %>%
  filter(BU=="BU3")


###### BU4
data_MTD_BU4 <- data_MTD %>%
  filter(BU=="BU4")

data_YTD_BU4 <- data_YTD %>%
  filter(BU=="BU4")


###### BU6
data_MTD_BU6 <- data_MTD %>%
  filter(BU=="BU6")

data_YTD_BU6 <- data_YTD %>%
  filter(BU=="BU6")


###### PC04
data_MTD_PC04 <- data_MTD %>%
  filter(BU=="PC04")

data_YTD_PC04 <- data_YTD %>%
  filter(BU=="PC04")


###### PC11
data_MTD_PC11 <- data_MTD %>%
  filter(BU=="PC11")

data_YTD_PC11 <- data_YTD %>%
  filter(BU=="PC11")


###### PC30
data_MTD_PC30 <- data_MTD %>%
  filter(BU=="PC30")

data_YTD_PC30 <- data_YTD %>%
  filter(BU=="PC30")

########################## Correlation analysis
################## BU 1 
################# MTD
############ With missing values

# Standardize the variables
data_MTD_BU1_scaled <- scale(data_MTD_BU1[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_BU1 <- cor(data_MTD_BU1_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_MTD_BU1)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD <- function(cor_matrix_MTD){
  cor_matrix_MTD[lower.tri(cor_matrix_MTD)]<- NA
  return(cor_matrix_MTD)
}

upper_tri_MTD_BU1 <- get_upper_tri_MTD(cor_matrix_MTD_BU1)

melted_cormat_MTD_BU1 <- melt(upper_tri_MTD_BU1, na.rm = TRUE)

cor_heat_MTD_BU1 <- ggplot(data = melted_cormat_MTD_BU1, aes(Var2, Var1, fill = value))+
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
  ggtitle("MTD BU1 with NA Absolutes only")

cor_heat_MTD_BU1

# Save to working directory
ggsave("cormat_MTD_BU1_onlyAbs.png", plot = cor_heat_MTD_BU1, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_BU1_onlyAbs.jpg", plot = cor_heat_MTD_BU1, width = 10, height = 8, units = "in", dpi = 600)

################# MTD
############ Without missing values

# Remove rows with missing values
MTD_BU1_noNA <- na.omit(data_MTD_BU1)

# Standardize the variables
MTD_BU1_noNA_scaled <- scale(MTD_BU1_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_BU1_noNA <- cor(MTD_BU1_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_MTD_BU1_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD_noNA <- function(cor_matrix_MTD_noNA){
  cor_matrix_MTD_noNA[lower.tri(cor_matrix_MTD_noNA)]<- NA
  return(cor_matrix_MTD_noNA)
}

upper_tri_MTD_BU1_noNA <- get_upper_tri_MTD_noNA(cor_matrix_MTD_BU1_noNA)

melted_cormat_MTD_BU1_noNA <- melt(upper_tri_MTD_BU1_noNA, na.rm = TRUE)

cor_heat_MTD_BU1_noNA <- ggplot(data = melted_cormat_MTD_BU1_noNA, aes(Var2, Var1, fill = value))+
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
  ggtitle("MTD BU1 without NA Absolutes only")

cor_heat_MTD_BU1_noNA

# Save to working directory
ggsave("cormat_MTD_BU1_noNA_onlyAbs.png", plot = cor_heat_MTD_BU1_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_BU1_noNA_onlyAbs.jpg", plot = cor_heat_MTD_BU1_noNA, width = 10, height = 8, units = "in", dpi = 600)

################# YTD
############ With missing values

# Standardize the variables
data_YTD_BU1_scaled <- scale(data_YTD_BU1[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_BU1 <- cor(data_YTD_BU1_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_YTD_BU1)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD <- function(cor_matrix_YTD){
  cor_matrix_YTD[lower.tri(cor_matrix_YTD)]<- NA
  return(cor_matrix_YTD)
}

upper_tri_YTD_BU1 <- get_upper_tri_YTD(cor_matrix_YTD_BU1)

melted_cormat_YTD_BU1 <- melt(upper_tri_YTD_BU1, na.rm = TRUE)

cor_heat_YTD_BU1 <- ggplot(data = melted_cormat_YTD_BU1, aes(Var2, Var1, fill = value))+
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
  ggtitle("YTD BU1 with NA Absolutes only")

cor_heat_YTD_BU1

# Save to working directory
ggsave("cormat_YTD_BU1_onlyAbs.png", plot = cor_heat_YTD_BU1, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_BU1_onlyAbs.jpg", plot = cor_heat_YTD_BU1, width = 10, height = 8, units = "in", dpi = 600)


################# YTD
############ Without missing values

# Remove rows with missing values
YTD_BU1_noNA <- na.omit(data_YTD_BU1)

# Standardize the variables
YTD_BU1_noNA_scaled <- scale(YTD_BU1_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_BU1_noNA <- cor(YTD_BU1_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_YTD_BU1_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD_noNA <- function(cor_matrix_YTD_noNA){
  cor_matrix_YTD_noNA[lower.tri(cor_matrix_YTD_noNA)]<- NA
  return(cor_matrix_YTD_noNA)
}

upper_tri_YTD_BU1_noNA <- get_upper_tri_YTD_noNA(cor_matrix_YTD_BU1_noNA)

melted_cormat_YTD_BU1_noNA <- melt(upper_tri_YTD_BU1_noNA, na.rm = TRUE)

cor_heat_YTD_BU1_noNA <- ggplot(data = melted_cormat_YTD_BU1_noNA, aes(Var2, Var1, fill = value))+
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
  ggtitle("YTD BU1 without NA Absolutes only")

cor_heat_YTD_BU1_noNA

# Save to working directory
ggsave("cormat_YTD_BU1_noNA_onlyAbs.png", plot = cor_heat_YTD_BU1_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_BU1_noNA_onlyAbs.jpg", plot = cor_heat_YTD_BU1_noNA, width = 10, height = 8, units = "in", dpi = 600)


################################################################################
################## BU 2
################# MTD
############ With missing values

# Standardize the variables
data_MTD_BU2_scaled <- scale(data_MTD_BU2[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_BU2 <- cor(data_MTD_BU2_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_MTD_BU2)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD <- function(cor_matrix_MTD){
  cor_matrix_MTD[lower.tri(cor_matrix_MTD)]<- NA
  return(cor_matrix_MTD)
}

upper_tri_MTD_BU2 <- get_upper_tri_MTD(cor_matrix_MTD_BU2)

melted_cormat_MTD_BU2 <- melt(upper_tri_MTD_BU2, na.rm = TRUE)

cor_heat_MTD_BU2 <- ggplot(data = melted_cormat_MTD_BU2, aes(Var2, Var1, fill = value))+
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
  ggtitle("MTD BU2 with NA Absolutes only")

cor_heat_MTD_BU2

# Save to working directory
ggsave("cormat_MTD_BU2_onlyAbs.png", plot = cor_heat_MTD_BU2, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_BU2_onlyAbs.jpg", plot = cor_heat_MTD_BU2, width = 10, height = 8, units = "in", dpi = 600)

################# MTD
############ Without missing values

# Remove rows with missing values
MTD_BU2_noNA <- na.omit(data_MTD_BU2)

# Standardize the variables
MTD_BU2_noNA_scaled <- scale(MTD_BU2_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_BU2_noNA <- cor(MTD_BU2_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_MTD_BU2_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD_noNA <- function(cor_matrix_MTD_noNA){
  cor_matrix_MTD_noNA[lower.tri(cor_matrix_MTD_noNA)]<- NA
  return(cor_matrix_MTD_noNA)
}

upper_tri_MTD_BU2_noNA <- get_upper_tri_MTD_noNA(cor_matrix_MTD_BU2_noNA)

melted_cormat_MTD_BU2_noNA <- melt(upper_tri_MTD_BU2_noNA, na.rm = TRUE)

cor_heat_MTD_BU2_noNA <- ggplot(data = melted_cormat_MTD_BU2_noNA, aes(Var2, Var1, fill = value))+
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
  ggtitle("MTD BU2 without NA Absolutes only")

cor_heat_MTD_BU2_noNA

# Save to working directory
ggsave("cormat_MTD_BU2_noNA_onlyAbs.png", plot = cor_heat_MTD_BU2_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_BU2_noNA_onlyAbs.jpg", plot = cor_heat_MTD_BU2_noNA, width = 10, height = 8, units = "in", dpi = 600)

################# YTD
############ With missing values

# Standardize the variables
data_YTD_BU2_scaled <- scale(data_YTD_BU2[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_BU2 <- cor(data_YTD_BU2_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_YTD_BU2)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD <- function(cor_matrix_YTD){
  cor_matrix_YTD[lower.tri(cor_matrix_YTD)]<- NA
  return(cor_matrix_YTD)
}

upper_tri_YTD_BU2 <- get_upper_tri_YTD(cor_matrix_YTD_BU2)

melted_cormat_YTD_BU2 <- melt(upper_tri_YTD_BU2, na.rm = TRUE)

cor_heat_YTD_BU2 <- ggplot(data = melted_cormat_YTD_BU2, aes(Var2, Var1, fill = value))+
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
  ggtitle("YTD BU2 with NA Absolutes only")

cor_heat_YTD_BU2

# Save to working directory
ggsave("cormat_YTD_BU2_onlyAbs.png", plot = cor_heat_YTD_BU2, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_BU2_onlyAbs.jpg", plot = cor_heat_YTD_BU2, width = 10, height = 8, units = "in", dpi = 600)


################# YTD
############ Without missing values

# Remove rows with missing values
YTD_BU2_noNA <- na.omit(data_YTD_BU2)

# Standardize the variables
YTD_BU2_noNA_scaled <- scale(YTD_BU2_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_BU2_noNA <- cor(YTD_BU2_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_YTD_BU2_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD_noNA <- function(cor_matrix_YTD_noNA){
  cor_matrix_YTD_noNA[lower.tri(cor_matrix_YTD_noNA)]<- NA
  return(cor_matrix_YTD_noNA)
}

upper_tri_YTD_BU2_noNA <- get_upper_tri_YTD_noNA(cor_matrix_YTD_BU2_noNA)

melted_cormat_YTD_BU2_noNA <- melt(upper_tri_YTD_BU2_noNA, na.rm = TRUE)

cor_heat_YTD_BU2_noNA <- ggplot(data = melted_cormat_YTD_BU2_noNA, aes(Var2, Var1, fill = value))+
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
  ggtitle("YTD BU2 without NA Absolutes only")

cor_heat_YTD_BU2_noNA

# Save to working directory
ggsave("cormat_YTD_BU2_noNA_onlyAbs.png", plot = cor_heat_YTD_BU2_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_BU2_noNA_onlyAbs.jpg", plot = cor_heat_YTD_BU2_noNA, width = 10, height = 8, units = "in", dpi = 600)


################################################################################
################## BU 3
################# MTD
############ With missing values

# Standardize the variables
data_MTD_BU3_scaled <- scale(data_MTD_BU3[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_BU3 <- cor(data_MTD_BU3_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_MTD_BU3)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD <- function(cor_matrix_MTD){
  cor_matrix_MTD[lower.tri(cor_matrix_MTD)]<- NA
  return(cor_matrix_MTD)
}

upper_tri_MTD_BU3 <- get_upper_tri_MTD(cor_matrix_MTD_BU3)

melted_cormat_MTD_BU3 <- melt(upper_tri_MTD_BU3, na.rm = TRUE)

cor_heat_MTD_BU3 <- ggplot(data = melted_cormat_MTD_BU3, aes(Var2, Var1, fill = value))+
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
  ggtitle("MTD BU3 with NA Absolutes only")

cor_heat_MTD_BU3

# Save to working directory
ggsave("cormat_MTD_BU3_onlyAbs.png", plot = cor_heat_MTD_BU3, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_BU3_onlyAbs.jpg", plot = cor_heat_MTD_BU3, width = 10, height = 8, units = "in", dpi = 600)

################# MTD
############ Without missing values

# Remove rows with missing values
MTD_BU3_noNA <- na.omit(data_MTD_BU3)

# Standardize the variables
MTD_BU3_noNA_scaled <- scale(MTD_BU3_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_BU3_noNA <- cor(MTD_BU3_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_MTD_BU3_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD_noNA <- function(cor_matrix_MTD_noNA){
  cor_matrix_MTD_noNA[lower.tri(cor_matrix_MTD_noNA)]<- NA
  return(cor_matrix_MTD_noNA)
}

upper_tri_MTD_BU3_noNA <- get_upper_tri_MTD_noNA(cor_matrix_MTD_BU3_noNA)

melted_cormat_MTD_BU3_noNA <- melt(upper_tri_MTD_BU3_noNA, na.rm = TRUE)

cor_heat_MTD_BU3_noNA <- ggplot(data = melted_cormat_MTD_BU3_noNA, aes(Var2, Var1, fill = value))+
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
  ggtitle("MTD BU3 without NA Absolutes only")

cor_heat_MTD_BU3_noNA

# Save to working directory
ggsave("cormat_MTD_BU3_noNA_onlyAbs.png", plot = cor_heat_MTD_BU3_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_BU3_noNA_onlyAbs.jpg", plot = cor_heat_MTD_BU3_noNA, width = 10, height = 8, units = "in", dpi = 600)

################# YTD
############ With missing values

# Standardize the variables
data_YTD_BU3_scaled <- scale(data_YTD_BU3[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_BU3 <- cor(data_YTD_BU3_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_YTD_BU3)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD <- function(cor_matrix_YTD){
  cor_matrix_YTD[lower.tri(cor_matrix_YTD)]<- NA
  return(cor_matrix_YTD)
}

upper_tri_YTD_BU3 <- get_upper_tri_YTD(cor_matrix_YTD_BU3)

melted_cormat_YTD_BU3 <- melt(upper_tri_YTD_BU3, na.rm = TRUE)

cor_heat_YTD_BU3 <- ggplot(data = melted_cormat_YTD_BU3, aes(Var2, Var1, fill = value))+
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
  ggtitle("YTD BU3 with NA Absolutes only")

cor_heat_YTD_BU3

# Save to working directory
ggsave("cormat_YTD_BU3_onlyAbs.png", plot = cor_heat_YTD_BU3, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_BU3_onlyAbs.jpg", plot = cor_heat_YTD_BU3, width = 10, height = 8, units = "in", dpi = 600)


################# YTD
############ Without missing values

# Remove rows with missing values
YTD_BU3_noNA <- na.omit(data_YTD_BU3)

# Standardize the variables
YTD_BU3_noNA_scaled <- scale(YTD_BU3_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_BU3_noNA <- cor(YTD_BU3_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_YTD_BU3_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD_noNA <- function(cor_matrix_YTD_noNA){
  cor_matrix_YTD_noNA[lower.tri(cor_matrix_YTD_noNA)]<- NA
  return(cor_matrix_YTD_noNA)
}

upper_tri_YTD_BU3_noNA <- get_upper_tri_YTD_noNA(cor_matrix_YTD_BU3_noNA)

melted_cormat_YTD_BU3_noNA <- melt(upper_tri_YTD_BU3_noNA, na.rm = TRUE)

cor_heat_YTD_BU3_noNA <- ggplot(data = melted_cormat_YTD_BU3_noNA, aes(Var2, Var1, fill = value))+
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
  ggtitle("YTD BU3 without NA Absolutes only")

cor_heat_YTD_BU3_noNA

# Save to working directory
ggsave("cormat_YTD_BU3_noNA_onlyAbs.png", plot = cor_heat_YTD_BU3_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_BU3_noNA_onlyAbs.jpg", plot = cor_heat_YTD_BU3_noNA, width = 10, height = 8, units = "in", dpi = 600)


################################################################################
################## BU 4
################# MTD
############ With missing values

# Standardize the variables
data_MTD_BU4_scaled <- scale(data_MTD_BU4[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_BU4 <- cor(data_MTD_BU4_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_MTD_BU4)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD <- function(cor_matrix_MTD){
  cor_matrix_MTD[lower.tri(cor_matrix_MTD)]<- NA
  return(cor_matrix_MTD)
}

upper_tri_MTD_BU4 <- get_upper_tri_MTD(cor_matrix_MTD_BU4)

melted_cormat_MTD_BU4 <- melt(upper_tri_MTD_BU4, na.rm = TRUE)

cor_heat_MTD_BU4 <- ggplot(data = melted_cormat_MTD_BU4, aes(Var2, Var1, fill = value))+
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
  ggtitle("MTD BU4 with NA Absolutes only")

cor_heat_MTD_BU4

# Save to working directory
ggsave("cormat_MTD_BU4_onlyAbs.png", plot = cor_heat_MTD_BU4, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_BU4_onlyAbs.jpg", plot = cor_heat_MTD_BU4, width = 10, height = 8, units = "in", dpi = 600)

################# MTD
############ Without missing values

# Remove rows with missing values
MTD_BU4_noNA <- na.omit(data_MTD_BU4)

# Standardize the variables
MTD_BU4_noNA_scaled <- scale(MTD_BU4_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_BU4_noNA <- cor(MTD_BU4_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_MTD_BU4_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD_noNA <- function(cor_matrix_MTD_noNA){
  cor_matrix_MTD_noNA[lower.tri(cor_matrix_MTD_noNA)]<- NA
  return(cor_matrix_MTD_noNA)
}

upper_tri_MTD_BU4_noNA <- get_upper_tri_MTD_noNA(cor_matrix_MTD_BU4_noNA)

melted_cormat_MTD_BU4_noNA <- melt(upper_tri_MTD_BU4_noNA, na.rm = TRUE)

cor_heat_MTD_BU4_noNA <- ggplot(data = melted_cormat_MTD_BU4_noNA, aes(Var2, Var1, fill = value))+
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
  ggtitle("MTD BU4 without NA Absolutes only")

cor_heat_MTD_BU4_noNA

# Save to working directory
ggsave("cormat_MTD_BU4_noNA_onlyAbs.png", plot = cor_heat_MTD_BU4_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_BU4_noNA_onlyAbs.jpg", plot = cor_heat_MTD_BU4_noNA, width = 10, height = 8, units = "in", dpi = 600)

################# YTD
############ With missing values

# Standardize the variables
data_YTD_BU4_scaled <- scale(data_YTD_BU4[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_BU4 <- cor(data_YTD_BU4_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_YTD_BU4)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD <- function(cor_matrix_YTD){
  cor_matrix_YTD[lower.tri(cor_matrix_YTD)]<- NA
  return(cor_matrix_YTD)
}

upper_tri_YTD_BU4 <- get_upper_tri_YTD(cor_matrix_YTD_BU4)

melted_cormat_YTD_BU4 <- melt(upper_tri_YTD_BU4, na.rm = TRUE)

cor_heat_YTD_BU4 <- ggplot(data = melted_cormat_YTD_BU4, aes(Var2, Var1, fill = value))+
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
  ggtitle("YTD BU4 with NA Absolutes only")

cor_heat_YTD_BU4

# Save to working directory
ggsave("cormat_YTD_BU4_onlyAbs.png", plot = cor_heat_YTD_BU4, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_BU4_onlyAbs.jpg", plot = cor_heat_YTD_BU4, width = 10, height = 8, units = "in", dpi = 600)


################# YTD
############ Without missing values

# Remove rows with missing values
YTD_BU4_noNA <- na.omit(data_YTD_BU4)

# Standardize the variables
YTD_BU4_noNA_scaled <- scale(YTD_BU4_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_BU4_noNA <- cor(YTD_BU4_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_YTD_BU4_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD_noNA <- function(cor_matrix_YTD_noNA){
  cor_matrix_YTD_noNA[lower.tri(cor_matrix_YTD_noNA)]<- NA
  return(cor_matrix_YTD_noNA)
}

upper_tri_YTD_BU4_noNA <- get_upper_tri_YTD_noNA(cor_matrix_YTD_BU4_noNA)

melted_cormat_YTD_BU4_noNA <- melt(upper_tri_YTD_BU4_noNA, na.rm = TRUE)

cor_heat_YTD_BU4_noNA <- ggplot(data = melted_cormat_YTD_BU4_noNA, aes(Var2, Var1, fill = value))+
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
  ggtitle("YTD BU4 without NA Absolutes only")

cor_heat_YTD_BU4_noNA

# Save to working directory
ggsave("cormat_YTD_BU4_noNA_onlyAbs.png", plot = cor_heat_YTD_BU4_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_BU4_noNA_onlyAbs.jpg", plot = cor_heat_YTD_BU4_noNA, width = 10, height = 8, units = "in", dpi = 600)



################################################################################
################## BU 6
################# MTD
############ With missing values

# Standardize the variables
data_MTD_BU6_scaled <- scale(data_MTD_BU6[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_BU6 <- cor(data_MTD_BU6_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_MTD_BU6)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD <- function(cor_matrix_MTD){
  cor_matrix_MTD[lower.tri(cor_matrix_MTD)]<- NA
  return(cor_matrix_MTD)
}

upper_tri_MTD_BU6 <- get_upper_tri_MTD(cor_matrix_MTD_BU6)

melted_cormat_MTD_BU6 <- melt(upper_tri_MTD_BU6, na.rm = TRUE)

cor_heat_MTD_BU6 <- ggplot(data = melted_cormat_MTD_BU6, aes(Var2, Var1, fill = value))+
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
  ggtitle("MTD BU6 with NA Absolutes only")

cor_heat_MTD_BU6

# Save to working directory
ggsave("cormat_MTD_BU6_onlyAbs.png", plot = cor_heat_MTD_BU6, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_BU6_onlyAbs.jpg", plot = cor_heat_MTD_BU6, width = 10, height = 8, units = "in", dpi = 600)

################# MTD
############ Without missing values

# Remove rows with missing values
MTD_BU6_noNA <- na.omit(data_MTD_BU6)

# Standardize the variables
MTD_BU6_noNA_scaled <- scale(MTD_BU6_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_BU6_noNA <- cor(MTD_BU6_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_MTD_BU6_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD_noNA <- function(cor_matrix_MTD_noNA){
  cor_matrix_MTD_noNA[lower.tri(cor_matrix_MTD_noNA)]<- NA
  return(cor_matrix_MTD_noNA)
}

upper_tri_MTD_BU6_noNA <- get_upper_tri_MTD_noNA(cor_matrix_MTD_BU6_noNA)

melted_cormat_MTD_BU6_noNA <- melt(upper_tri_MTD_BU6_noNA, na.rm = TRUE)

cor_heat_MTD_BU6_noNA <- ggplot(data = melted_cormat_MTD_BU6_noNA, aes(Var2, Var1, fill = value))+
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
  ggtitle("MTD BU6 without NA Absolutes only")

cor_heat_MTD_BU6_noNA

# Save to working directory
ggsave("cormat_MTD_BU6_noNA_onlyAbs.png", plot = cor_heat_MTD_BU6_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_BU6_noNA_onlyAbs.jpg", plot = cor_heat_MTD_BU6_noNA, width = 10, height = 8, units = "in", dpi = 600)

################# YTD
############ With missing values

# Standardize the variables
data_YTD_BU6_scaled <- scale(data_YTD_BU6[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_BU6 <- cor(data_YTD_BU6_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_YTD_BU6)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD <- function(cor_matrix_YTD){
  cor_matrix_YTD[lower.tri(cor_matrix_YTD)]<- NA
  return(cor_matrix_YTD)
}

upper_tri_YTD_BU6 <- get_upper_tri_YTD(cor_matrix_YTD_BU6)

melted_cormat_YTD_BU6 <- melt(upper_tri_YTD_BU6, na.rm = TRUE)

cor_heat_YTD_BU6 <- ggplot(data = melted_cormat_YTD_BU6, aes(Var2, Var1, fill = value))+
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
  ggtitle("YTD BU6 with NA Absolutes only")

cor_heat_YTD_BU6

# Save to working directory
ggsave("cormat_YTD_BU6_onlyAbs.png", plot = cor_heat_YTD_BU6, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_BU6_onlyAbs.jpg", plot = cor_heat_YTD_BU6, width = 10, height = 8, units = "in", dpi = 600)


################# YTD
############ Without missing values

# Remove rows with missing values
YTD_BU6_noNA <- na.omit(data_YTD_BU6)

# Standardize the variables
YTD_BU6_noNA_scaled <- scale(YTD_BU6_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_BU6_noNA <- cor(YTD_BU6_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_YTD_BU6_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD_noNA <- function(cor_matrix_YTD_noNA){
  cor_matrix_YTD_noNA[lower.tri(cor_matrix_YTD_noNA)]<- NA
  return(cor_matrix_YTD_noNA)
}

upper_tri_YTD_BU6_noNA <- get_upper_tri_YTD_noNA(cor_matrix_YTD_BU6_noNA)

melted_cormat_YTD_BU6_noNA <- melt(upper_tri_YTD_BU6_noNA, na.rm = TRUE)

cor_heat_YTD_BU6_noNA <- ggplot(data = melted_cormat_YTD_BU6_noNA, aes(Var2, Var1, fill = value))+
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
  ggtitle("YTD BU6 without NA Absolutes only")

cor_heat_YTD_BU6_noNA

# Save to working directory
ggsave("cormat_YTD_BU6_noNA_onlyAbs.png", plot = cor_heat_YTD_BU6_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_BU6_noNA_onlyAbs.jpg", plot = cor_heat_YTD_BU6_noNA, width = 10, height = 8, units = "in", dpi = 600)



################################################################################
################## PC 04
################# MTD
############ With missing values

# Standardize the variables
data_MTD_PC04_scaled <- scale(data_MTD_PC04[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_PC04 <- cor(data_MTD_PC04_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_MTD_PC04)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD <- function(cor_matrix_MTD){
  cor_matrix_MTD[lower.tri(cor_matrix_MTD)]<- NA
  return(cor_matrix_MTD)
}

upper_tri_MTD_PC04 <- get_upper_tri_MTD(cor_matrix_MTD_PC04)

melted_cormat_MTD_PC04 <- melt(upper_tri_MTD_PC04, na.rm = TRUE)

cor_heat_MTD_PC04 <- ggplot(data = melted_cormat_MTD_PC04, aes(Var2, Var1, fill = value))+
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
  ggtitle("MTD PC04 with NA Absolutes only")

cor_heat_MTD_PC04

# Save to working directory
ggsave("cormat_MTD_PC04_onlyAbs.png", plot = cor_heat_MTD_PC04, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_PC04_onlyAbs.jpg", plot = cor_heat_MTD_PC04, width = 10, height = 8, units = "in", dpi = 600)

################# MTD
############ Without missing values

# Remove rows with missing values
MTD_PC04_noNA <- na.omit(data_MTD_PC04)

# Standardize the variables
MTD_PC04_noNA_scaled <- scale(MTD_PC04_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_PC04_noNA <- cor(MTD_PC04_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_MTD_PC04_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD_noNA <- function(cor_matrix_MTD_noNA){
  cor_matrix_MTD_noNA[lower.tri(cor_matrix_MTD_noNA)]<- NA
  return(cor_matrix_MTD_noNA)
}

upper_tri_MTD_PC04_noNA <- get_upper_tri_MTD_noNA(cor_matrix_MTD_PC04_noNA)

melted_cormat_MTD_PC04_noNA <- melt(upper_tri_MTD_PC04_noNA, na.rm = TRUE)

cor_heat_MTD_PC04_noNA <- ggplot(data = melted_cormat_MTD_PC04_noNA, aes(Var2, Var1, fill = value))+
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
  ggtitle("MTD PC04 without NA Absolutes only")

cor_heat_MTD_PC04_noNA

# Save to working directory
ggsave("cormat_MTD_PC04_noNA_onlyAbs.png", plot = cor_heat_MTD_PC04_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_PC04_noNA_onlyAbs.jpg", plot = cor_heat_MTD_PC04_noNA, width = 10, height = 8, units = "in", dpi = 600)

################# YTD
############ With missing values

# Standardize the variables
data_YTD_PC04_scaled <- scale(data_YTD_PC04[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_PC04 <- cor(data_YTD_PC04_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_YTD_PC04)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD <- function(cor_matrix_YTD){
  cor_matrix_YTD[lower.tri(cor_matrix_YTD)]<- NA
  return(cor_matrix_YTD)
}

upper_tri_YTD_PC04 <- get_upper_tri_YTD(cor_matrix_YTD_PC04)

melted_cormat_YTD_PC04 <- melt(upper_tri_YTD_PC04, na.rm = TRUE)

cor_heat_YTD_PC04 <- ggplot(data = melted_cormat_YTD_PC04, aes(Var2, Var1, fill = value))+
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
  ggtitle("YTD PC04 with NA Absolutes only")

cor_heat_YTD_PC04

# Save to working directory
ggsave("cormat_YTD_PC04_onlyAbs.png", plot = cor_heat_YTD_PC04, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_PC04_onlyAbs.jpg", plot = cor_heat_YTD_PC04, width = 10, height = 8, units = "in", dpi = 600)


################# YTD
############ Without missing values

# Remove rows with missing values
YTD_PC04_noNA <- na.omit(data_YTD_PC04)

# Standardize the variables
YTD_PC04_noNA_scaled <- scale(YTD_PC04_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_PC04_noNA <- cor(YTD_PC04_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_YTD_PC04_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD_noNA <- function(cor_matrix_YTD_noNA){
  cor_matrix_YTD_noNA[lower.tri(cor_matrix_YTD_noNA)]<- NA
  return(cor_matrix_YTD_noNA)
}

upper_tri_YTD_PC04_noNA <- get_upper_tri_YTD_noNA(cor_matrix_YTD_PC04_noNA)

melted_cormat_YTD_PC04_noNA <- melt(upper_tri_YTD_PC04_noNA, na.rm = TRUE)

cor_heat_YTD_PC04_noNA <- ggplot(data = melted_cormat_YTD_PC04_noNA, aes(Var2, Var1, fill = value))+
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
  ggtitle("YTD PC04 without NA Absolutes only")

cor_heat_YTD_PC04_noNA

# Save to working directory
ggsave("cormat_YTD_PC04_noNA_onlyAbs.png", plot = cor_heat_YTD_PC04_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_PC04_noNA_onlyAbs.jpg", plot = cor_heat_YTD_PC04_noNA, width = 10, height = 8, units = "in", dpi = 600)



################################################################################
################## PC 11
################# MTD
############ With missing values

# Standardize the variables
data_MTD_PC11_scaled <- scale(data_MTD_PC11[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_PC11 <- cor(data_MTD_PC11_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_MTD_PC11)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD <- function(cor_matrix_MTD){
  cor_matrix_MTD[lower.tri(cor_matrix_MTD)]<- NA
  return(cor_matrix_MTD)
}

upper_tri_MTD_PC11 <- get_upper_tri_MTD(cor_matrix_MTD_PC11)

melted_cormat_MTD_PC11 <- melt(upper_tri_MTD_PC11, na.rm = TRUE)

cor_heat_MTD_PC11 <- ggplot(data = melted_cormat_MTD_PC11, aes(Var2, Var1, fill = value))+
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
  ggtitle("MTD PC11 with NA Absolutes only")

cor_heat_MTD_PC11

# Save to working directory
ggsave("cormat_MTD_PC11_onlyAbs.png", plot = cor_heat_MTD_PC11, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_PC11_onlyAbs.jpg", plot = cor_heat_MTD_PC11, width = 10, height = 8, units = "in", dpi = 600)

################# MTD
############ Without missing values

# Remove rows with missing values
MTD_PC11_noNA <- na.omit(data_MTD_PC11)

# Standardize the variables
MTD_PC11_noNA_scaled <- scale(MTD_PC11_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_PC11_noNA <- cor(MTD_PC11_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_MTD_PC11_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD_noNA <- function(cor_matrix_MTD_noNA){
  cor_matrix_MTD_noNA[lower.tri(cor_matrix_MTD_noNA)]<- NA
  return(cor_matrix_MTD_noNA)
}

upper_tri_MTD_PC11_noNA <- get_upper_tri_MTD_noNA(cor_matrix_MTD_PC11_noNA)

melted_cormat_MTD_PC11_noNA <- melt(upper_tri_MTD_PC11_noNA, na.rm = TRUE)

cor_heat_MTD_PC11_noNA <- ggplot(data = melted_cormat_MTD_PC11_noNA, aes(Var2, Var1, fill = value))+
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
  ggtitle("MTD PC11 without NA Absolutes only")

cor_heat_MTD_PC11_noNA

# Save to working directory
ggsave("cormat_MTD_PC11_noNA_onlyAbs.png", plot = cor_heat_MTD_PC11_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_PC11_noNA_onlyAbs.jpg", plot = cor_heat_MTD_PC11_noNA, width = 10, height = 8, units = "in", dpi = 600)

################# YTD
############ With missing values

# Standardize the variables
data_YTD_PC11_scaled <- scale(data_YTD_PC11[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_PC11 <- cor(data_YTD_PC11_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_YTD_PC11)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD <- function(cor_matrix_YTD){
  cor_matrix_YTD[lower.tri(cor_matrix_YTD)]<- NA
  return(cor_matrix_YTD)
}

upper_tri_YTD_PC11 <- get_upper_tri_YTD(cor_matrix_YTD_PC11)

melted_cormat_YTD_PC11 <- melt(upper_tri_YTD_PC11, na.rm = TRUE)

cor_heat_YTD_PC11 <- ggplot(data = melted_cormat_YTD_PC11, aes(Var2, Var1, fill = value))+
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
  ggtitle("YTD PC11 with NA Absolutes only")

cor_heat_YTD_PC11

# Save to working directory
ggsave("cormat_YTD_PC11_onlyAbs.png", plot = cor_heat_YTD_PC11, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_PC11_onlyAbs.jpg", plot = cor_heat_YTD_PC11, width = 10, height = 8, units = "in", dpi = 600)


################# YTD
############ Without missing values

# Remove rows with missing values
YTD_PC11_noNA <- na.omit(data_YTD_PC11)

# Standardize the variables
YTD_PC11_noNA_scaled <- scale(YTD_PC11_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_PC11_noNA <- cor(YTD_PC11_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_YTD_PC11_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD_noNA <- function(cor_matrix_YTD_noNA){
  cor_matrix_YTD_noNA[lower.tri(cor_matrix_YTD_noNA)]<- NA
  return(cor_matrix_YTD_noNA)
}

upper_tri_YTD_PC11_noNA <- get_upper_tri_YTD_noNA(cor_matrix_YTD_PC11_noNA)

melted_cormat_YTD_PC11_noNA <- melt(upper_tri_YTD_PC11_noNA, na.rm = TRUE)

cor_heat_YTD_PC11_noNA <- ggplot(data = melted_cormat_YTD_PC11_noNA, aes(Var2, Var1, fill = value))+
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
  ggtitle("YTD PC11 without NA Absolutes only")

cor_heat_YTD_PC11_noNA

# Save to working directory
ggsave("cormat_YTD_PC11_noNA_onlyAbs.png", plot = cor_heat_YTD_PC11_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_PC11_noNA_onlyAbs.jpg", plot = cor_heat_YTD_PC11_noNA, width = 10, height = 8, units = "in", dpi = 600)



################################################################################
################## PC 30
################# MTD
############ With missing values

# Standardize the variables
data_MTD_PC30_scaled <- scale(data_MTD_PC30[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_PC30 <- cor(data_MTD_PC30_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_MTD_PC30)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD <- function(cor_matrix_MTD){
  cor_matrix_MTD[lower.tri(cor_matrix_MTD)]<- NA
  return(cor_matrix_MTD)
}

upper_tri_MTD_PC30 <- get_upper_tri_MTD(cor_matrix_MTD_PC30)

melted_cormat_MTD_PC30 <- melt(upper_tri_MTD_PC30, na.rm = TRUE)

cor_heat_MTD_PC30 <- ggplot(data = melted_cormat_MTD_PC30, aes(Var2, Var1, fill = value))+
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
  ggtitle("MTD PC30 with NA Absolutes only")

cor_heat_MTD_PC30

# Save to working directory
ggsave("cormat_MTD_PC30_onlyAbs.png", plot = cor_heat_MTD_PC30, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_PC30_onlyAbs.jpg", plot = cor_heat_MTD_PC30, width = 10, height = 8, units = "in", dpi = 600)

################# MTD
############ Without missing values

# Remove rows with missing values
MTD_PC30_noNA <- na.omit(data_MTD_PC30)

# Standardize the variables
MTD_PC30_noNA_scaled <- scale(MTD_PC30_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_MTD_PC30_noNA <- cor(MTD_PC30_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_MTD_PC30_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_MTD_noNA <- function(cor_matrix_MTD_noNA){
  cor_matrix_MTD_noNA[lower.tri(cor_matrix_MTD_noNA)]<- NA
  return(cor_matrix_MTD_noNA)
}

upper_tri_MTD_PC30_noNA <- get_upper_tri_MTD_noNA(cor_matrix_MTD_PC30_noNA)

melted_cormat_MTD_PC30_noNA <- melt(upper_tri_MTD_PC30_noNA, na.rm = TRUE)

cor_heat_MTD_PC30_noNA <- ggplot(data = melted_cormat_MTD_PC30_noNA, aes(Var2, Var1, fill = value))+
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
  ggtitle("MTD PC30 without NA Absolutes only")

cor_heat_MTD_PC30_noNA

# Save to working directory
ggsave("cormat_MTD_PC30_noNA_onlyAbs.png", plot = cor_heat_MTD_PC30_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_MTD_PC30_noNA_onlyAbs.jpg", plot = cor_heat_MTD_PC30_noNA, width = 10, height = 8, units = "in", dpi = 600)

################# YTD
############ With missing values

# Standardize the variables
data_YTD_PC30_scaled <- scale(data_YTD_PC30[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_PC30 <- cor(data_YTD_PC30_scaled, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix_YTD_PC30)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD <- function(cor_matrix_YTD){
  cor_matrix_YTD[lower.tri(cor_matrix_YTD)]<- NA
  return(cor_matrix_YTD)
}

upper_tri_YTD_PC30 <- get_upper_tri_YTD(cor_matrix_YTD_PC30)

melted_cormat_YTD_PC30 <- melt(upper_tri_YTD_PC30, na.rm = TRUE)

cor_heat_YTD_PC30 <- ggplot(data = melted_cormat_YTD_PC30, aes(Var2, Var1, fill = value))+
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
  ggtitle("YTD PC30 with NA Absolutes only")

cor_heat_YTD_PC30

# Save to working directory
ggsave("cormat_YTD_PC30_onlyAbs.png", plot = cor_heat_YTD_PC30, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_PC30_onlyAbs.jpg", plot = cor_heat_YTD_PC30, width = 10, height = 8, units = "in", dpi = 600)


################# YTD
############ Without missing values

# Remove rows with missing values
YTD_PC30_noNA <- na.omit(data_YTD_PC30)

# Standardize the variables
YTD_PC30_noNA_scaled <- scale(YTD_PC30_noNA[, numerical_columns])

# Calculate the correlation matrix
cor_matrix_YTD_PC30_noNA <- cor(YTD_PC30_noNA_scaled)

# Print the correlation matrix
print(cor_matrix_YTD_PC30_noNA)

# helper function to get upper triangle of the correlation matrix
get_upper_tri_YTD_noNA <- function(cor_matrix_YTD_noNA){
  cor_matrix_YTD_noNA[lower.tri(cor_matrix_YTD_noNA)]<- NA
  return(cor_matrix_YTD_noNA)
}

upper_tri_YTD_PC30_noNA <- get_upper_tri_YTD_noNA(cor_matrix_YTD_PC30_noNA)

melted_cormat_YTD_PC30_noNA <- melt(upper_tri_YTD_PC30_noNA, na.rm = TRUE)

cor_heat_YTD_PC30_noNA <- ggplot(data = melted_cormat_YTD_PC30_noNA, aes(Var2, Var1, fill = value))+
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
  ggtitle("YTD PC30 without NA Absolutes only")

cor_heat_YTD_PC30_noNA

# Save to working directory
ggsave("cormat_YTD_PC30_noNA_onlyAbs.png", plot = cor_heat_YTD_PC30_noNA, width = 10, height = 8, units = "in", dpi = 600)
ggsave("cormat_YTD_PC30_noNA_onlyAbs.jpg", plot = cor_heat_YTD_PC30_noNA, width = 10, height = 8, units = "in", dpi = 600)
