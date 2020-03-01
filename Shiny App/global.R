library(ggplot2)
library(shiny)
library(plotly)
library(stargazer)
library(compare)
library(survival)
library(purrr)
library(cluster)
library(plyr)

##################################################################
#### You may need to edit the following lines
#### if data or the model are not defined correctly
##################################################################

data <- readRDS('dataset.rds')
model <- coxph(Surv(time, cens) ~ horTh + age + menostat + tsize + pnodes + progrec + estrec, data = data)
covariate <- 'slider'
GBSG2 <- data

mlm.f <- function(newd){
  new.GBSG2 <- newd
  GBSG2 <- rbind.fill(GBSG2, new.GBSG2)
  
  #Isolate data set with just the 5 numerical covariates
  GBSG2.features <- GBSG2
  GBSG2.covariates <- GBSG2
  #NULL out any categorical features
  GBSG2.features$menostat <- NULL
  GBSG2.features$horTh <- NULL
  GBSG2.features$time <- NULL
  GBSG2.features$cens <- NULL
  GBSG2.features$age1 <- NULL
  GBSG2.features$Colour <- NULL
  GBSG2.features$observations <- NULL
  GBSG2.features$age_group <- NULL
  GBSG2.features$tgrade <- NULL
  
  
  GBSG2.covariates$time <- NULL
  GBSG2.covariates$cens <- NULL
  GBSG2.covariates$age1 <- NULL
  GBSG2.covariates$tgrade <- NULL
  GBSG2.features$Colour <- NULL
  GBSG2.features$observations <- NULL
  GBSG2.features$age_group <- NULL
  
  #Scale data
  GBSG2.features_scaled <- scale(GBSG2.features)
  
  #k-means clustering using silhoutte prediction
  mdl <- kmeans(GBSG2.features_scaled, centers = 5)
  GBSG2_clustered <- mutate(GBSG2.covariates, cluster = mdl$cluster)
  
  #Grouping using the categorical variables as well
  GBSG2_clustered$group[GBSG2_clustered$cluster == 1 & GBSG2_clustered$horTh == "yes" 
                        & GBSG2_clustered$menostat == "Pre"] <- 1
  GBSG2_clustered$group[GBSG2_clustered$cluster == 1 & GBSG2_clustered$horTh == "yes" 
                        & GBSG2_clustered$menostat == "Post"] <- 2
  GBSG2_clustered$group[GBSG2_clustered$cluster == 1 & GBSG2_clustered$horTh == "no" 
                        & GBSG2_clustered$menostat == "Pre"] <- 3
  GBSG2_clustered$group[GBSG2_clustered$cluster == 1 & GBSG2_clustered$horTh == "no" 
                        & GBSG2_clustered$menostat == "Post"] <- 4
  
  GBSG2_clustered$group[GBSG2_clustered$cluster == 2 & GBSG2_clustered$horTh == "yes" 
                        & GBSG2_clustered$menostat == "Pre"] <- 5
  GBSG2_clustered$group[GBSG2_clustered$cluster == 2 & GBSG2_clustered$horTh == "yes" 
                        & GBSG2_clustered$menostat == "Post"] <- 6
  GBSG2_clustered$group[GBSG2_clustered$cluster == 2 & GBSG2_clustered$horTh == "no" 
                        & GBSG2_clustered$menostat == "Pre"] <- 7
  GBSG2_clustered$group[GBSG2_clustered$cluster == 2 & GBSG2_clustered$horTh == "no" 
                        & GBSG2_clustered$menostat == "Post"] <- 8
  
  GBSG2_clustered$group[GBSG2_clustered$cluster == 3 & GBSG2_clustered$horTh == "yes" 
                        & GBSG2_clustered$menostat == "Pre"] <- 9
  GBSG2_clustered$group[GBSG2_clustered$cluster == 3 & GBSG2_clustered$horTh == "yes" 
                        & GBSG2_clustered$menostat == "Post"] <- 10
  GBSG2_clustered$group[GBSG2_clustered$cluster == 3 & GBSG2_clustered$horTh == "no" 
                        & GBSG2_clustered$menostat == "Pre"] <- 11
  GBSG2_clustered$group[GBSG2_clustered$cluster == 3 & GBSG2_clustered$horTh == "no" 
                        & GBSG2_clustered$menostat == "Post"] <- 12
  
  GBSG2_clustered$group[GBSG2_clustered$cluster == 4 & GBSG2_clustered$horTh == "yes" 
                        & GBSG2_clustered$menostat == "Pre"] <- 13
  GBSG2_clustered$group[GBSG2_clustered$cluster == 4 & GBSG2_clustered$horTh == "yes" 
                        & GBSG2_clustered$menostat == "Post"] <- 14
  GBSG2_clustered$group[GBSG2_clustered$cluster == 4 & GBSG2_clustered$horTh == "no" 
                        & GBSG2_clustered$menostat == "Pre"] <- 15
  GBSG2_clustered$group[GBSG2_clustered$cluster == 4 & GBSG2_clustered$horTh == "no" 
                        & GBSG2_clustered$menostat == "Post"] <- 16
  
  GBSG2_clustered$group[GBSG2_clustered$cluster == 5 & GBSG2_clustered$horTh == "yes" 
                        & GBSG2_clustered$menostat == "Pre"] <- 17
  GBSG2_clustered$group[GBSG2_clustered$cluster == 5 & GBSG2_clustered$horTh == "yes" 
                        & GBSG2_clustered$menostat == "Post"] <- 18
  GBSG2_clustered$group[GBSG2_clustered$cluster == 5 & GBSG2_clustered$horTh == "no" 
                        & GBSG2_clustered$menostat == "Pre"] <- 19
  GBSG2_clustered$group[GBSG2_clustered$cluster == 5 & GBSG2_clustered$horTh == "no" 
                        & GBSG2_clustered$menostat == "Post"] <- 20
  
  f.tab <- count(GBSG2_clustered$group == GBSG2_clustered$group[nrow(GBSG2_clustered)])
  f.tab <- na.omit(f.tab)
  perc <- (f.tab$freq[f.tab$x=="TRUE"]-1)/686
  return(round(100*perc,2))
}

#newd <- data.frame(horTh="no", menostat="Post", age=53,
 #                  tsize=65, pnodes=5, progrec=1133, estrec=94, time=1124)
#mlm.f(new.d)

