#Loading relevant packages
library(reshape2)
library(TH.data)
library(survival)
library(survminer)
library(cluster)
library(purrr)
library(survminer)

#Isolate data set with just the 5 numerical covariates
GBSG2.features <- GBSG2

#NULL out any categorical features
GBSG2.features$menostat <- NULL
GBSG2.features$horTh <- NULL
GBSG2.features$time <- NULL
GBSG2.features$cens <- NULL
GBSG2.features$age1 <- NULL
GBSG2.features$tgrade <- NULL
GBSG2.features$Colour <- NULL
GBSG2.features$observations <- NULL
GBSG2.features$age_group <- NULL
GBSG2.features$estrec <- NULL
GBSG2.features$age <- NULL

#Scale data
GBSG2.features_scaled <- scale(GBSG2.features)

#Using Elbow plot to find optimal K
tot_withinss <- map_dbl(1:20, function(k){
  model_E <- kmeans(x = GBSG2.features_scaled, centers = k)
  model_E$tot.withinss
})

elbow_df <- data.frame(
  k = 1:20,
  tot_withinss = tot_withinss
)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:20)

#Using highest average Silhoutte width to find optimal k
sil_width <- map_dbl(2:20, function(k){
  model_S <- pam(x = GBSG2.features_scaled, k = k)
  model_S$silinfo$avg.width
})

sil_df <- data.frame(
  k = 2:20,
  sil_width = sil_width
)

ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:20)