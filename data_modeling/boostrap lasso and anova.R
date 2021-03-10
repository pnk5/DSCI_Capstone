#############################################################################
# Bootstrapping Lasso and Linear Models
# Inputs: trainTractsDroppedStandard.csv, testTractsDroppedStandard.csv
# Outputs: BARPLOT_inc.csv, BARPLOT_medinc.csv, BARPLOT_fireinc.csv
# BARPLOT csvs were then exported into tableau for graphs. 
# Code also outputs model fittings and values that were formatted
# and inserted into the final report. 
#############################################################################
library(dplyr)
library(tidyr)
library(glmnet)
library(selectiveInference)
library(rpart)
library(rpart.plot)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

# functions ---------------------------------------------------------------
# Does BoxCox
doBC <- function(in_val, lam){
  return(((in_val^lam) - 1) / lam)
}

# Checks for repeated columns
isin <- function(x, listx){
  return(ifelse(sum(grepl(x, listx)) > 0, TRUE, FALSE))
}

# Get optimal lambda for lasso
getOptimalLambda <- function(xdata, ydata, grid = 10^seq(0,-10, length = 500)){
  go <- glmnet(xdata, ydata, alpha = 1, lambda = grid)
  minlam <- cv.glmnet(xdata, ydata, alpha = 1)$lambda.min
  print(paste0("Number of Predictors Selected: ", 
               sum(glmnet(xdata, ydata, alpha = 1, lambda = minlam)$beta != 0)))
  
  return(minlam)
}

# loop function
bootLasso <- function(lam, xdata, ydata){
  size <- length(ydata)
  sampleVec <- sample(length(ydata), size = size, replace = TRUE)
  lglm <- glmnet(xmat[sampleVec, ], ydata[sampleVec], alpha = 1, lambda = lam)
  return(colnames(xmat)[which(lglm$beta != 0)])
}

# Export bootstrap results to csv
exportBoot <- function(bootobj, filename, n = 20, total = 1000){
  boott <- bootobj %>% unlist() %>% table() %>% 
    sort(decreasing = TRUE) %>% head(n = n)
  write.csv(data.frame("names" = names(boott), 
                       "percent" = unname(boott) / total * 100), filename)
}

# reading and cleaning data -----------------------------------------------
ftrain <- read.csv("data_source/trainTractsDroppedStandard.csv")

# Which columns are repeated
a <- sapply(colnames(ftrain)[101:163], isin, listx = colnames(ftrain)[4:100])
sum(!a)
# Everything 101:163 is repeated
# Leave out Washington as basline for state indicators

set.seed(1)

# Per capita
strain <- ftrain %>% 
  dplyr::select(c(incidents_per_capita, 4:36, 38:100))
yvec <- log(strain$incidents_per_capita)
xmat <- data.matrix(strain[, -1])

bestlam <- getOptimalLambda(xmat, yvec)
lambdas <- rep(bestlam, 1000)
bootList <- lapply(lambdas, bootLasso, xdata = xmat, ydata = yvec)

exportBoot(bootList, "BARPLOT_inc.csv")


# Medical Incidents barplot
strain <- ftrain %>% 
  filter(medical_per_capita != 0) %>% 
  dplyr::select(c(medical_per_capita, 4:36, 38:100)) %>% 
  drop_na()
yvec <- log(strain$medical_per_capita)
xmat <- data.matrix(strain[, -1])

bestlam <- getOptimalLambda(xmat, yvec)
lambdas <- rep(bestlam, 1000)
bootList <- lapply(lambdas, bootLasso, xdata = xmat, ydata = yvec)

exportBoot(bootList, "BARPLOT_medinc.csv")


# Fire Incidents barplot
strain <- ftrain %>% 
  filter(fire_per_capita != 0) %>% 
  dplyr::select(c(fire_per_capita, 4:36, 38:100)) %>% 
  drop_na() 
yvec <- log(strain$fire_per_capita)
xmat <- data.matrix(strain[, -1])

bestlam <- getOptimalLambda(xmat, yvec)
lambdas <- rep(bestlam, 1000)
bootList <- lapply(lambdas, bootLasso, xdata = xmat, ydata = yvec)

exportBoot(bootList, "BARPLOT_fireinc.csv")


# ANOVA columns -----------------------------------------------------------
# Running ANOVA models to determine how many variables to keep
strain <- read.csv("data_source/trainTractsDroppedStandard.csv") %>% 
  dplyr::select(c(incidents_per_capita, fire_per_capita, medical_per_capita, 
                  4:36, 38:100)) %>% 
  mutate_at(vars(incidents_per_capita), log) %>% 
  mutate_at(vars(fire_per_capita), function(x){
    ifelse(x == 0, NA, log(x))
  }) %>% 
  mutate_at(vars(medical_per_capita), function(x){
    ifelse(x == 0, NA, log(x))
  })

# Modelling logged total incidents per capita
a1 <- lm(incidents_per_capita ~., 
         data = strain[, c("incidents_per_capita", read.csv("BARPLOT_inc.csv", stringsAsFactors = F)$names)])
anova(a1)
# keep 7

# Modelling logged medical incidents per capita
a2 <- lm(medical_per_capita ~., 
         data = strain[, c("medical_per_capita", read.csv("BARPLOT_medinc.csv", stringsAsFactors = F)$names)])
anova(a2)
# keep 7

# Modelling logged fire incidents per capita
a3 <- lm(fire_per_capita ~., 
         data = strain[, c("fire_per_capita", read.csv("BARPLOT_fireinc.csv", stringsAsFactors = F)$names)])
anova(a3)
# keep 8


# ANOVA Linear Models -----------------------------------------------------
# Using number of columns determined by previous chunk to run a 
# linear model on the test set to interpret coefficients
strain <- read.csv("data_source/testTractsDroppedStandard.csv") %>% 
  dplyr::select(c(incidents_per_capita, fire_per_capita, medical_per_capita, 
                  4:36, 38:100)) %>% 
  mutate_at(vars(incidents_per_capita), log) %>% 
  mutate_at(vars(fire_per_capita), function(x){
    ifelse(x == 0, NA, log(x))
  }) %>% 
  mutate_at(vars(medical_per_capita), function(x){
    ifelse(x == 0, NA, log(x))
  })

# Graph function for model assumptions
mgraphs <- function(x){
  {par(mfrow = c(2,2), mar = c(2,2,2,1))
    plot(x, 1)
    plot(x, 2)
    plot(x, 3)
    plot(x, 4)
  }
}

# Modelling logged total incidents per capita
m1 <- lm(incidents_per_capita ~., 
         data = strain[, colnames(strain) %in% c("incidents_per_capita", 
              read.csv("BARPLOT_inc.csv", stringsAsFactors = F)$names[1:7])])
tab_model(m1)
summary(m1)
mgraphs(m1)

# Modelling logged medical incidents per capita
data2 <- strain[, colnames(strain) %in% c("medical_per_capita", 
      read.csv("BARPLOT_medinc.csv", stringsAsFactors = F)$names[1:7])] %>%
  drop_na()
m2 <- lm(medical_per_capita ~., data = data2)
tab_model(m2)
summary(m2)
mgraphs(m2)

# Modelling logged fire incidents per capita
data3 <- strain[, colnames(strain) %in% c("fire_per_capita", 
      read.csv("BARPLOT_fireinc.csv", stringsAsFactors = F)$names[1:8])] %>%
  drop_na()
m3 <- lm(fire_per_capita ~., data = data3)
summary(m3)
tab_model(m3)
mgraphs(m3)



# unused ------------------------------------------------------------------
# The following code is irrelevant to the report and unused. 
# It also doesn't work

# og <- bootResults
# t2 <- bootResults
# t3 <- bootResults
# t4 <- bootResults
# write.csv(data.frame("og" = names(og), "ogval" = unname(og),
#                      "t2" = names(t2), "t2val" = unname(t2),
#                      "t3" = names(t3), "t3val" = unname(t3),
#                      "t4" = names(t4), "t4val" = unname(t4)
#                      ), "bootcompare.csv")

# forward stepwise inference ----------------------------------------------
incfs <- fs(x = xmat, y = yvec, maxsteps = 15)

incinf <- fsInf(incfs)
colnames(xmat)[incinf$vars]

# selective inference -----------------------------------------------------
# grid <- 10^seq(0,-10, length = 500) og grid
grid <- 10^seq(0,-5, length = 10000000)
glasso <- glmnet(xmat, yvec, alpha = 1, lambda = grid, exact = TRUE, 
                 thresh = 1e-20)
lmin <- 0.014
#lout <- predict(glasso, type = "coefficients", s = lmin)
#length(lout@i)

tb <- coef(glasso, lmin / nrow(xmat), exact = TRUE, x = xmat, y = yvec)[-1]
a <- fixedLassoInf(x = xmat, y = yvec, beta = tb, lambda = lmin)
a


# Checking stuff
a <- read.csv("data_source/trainTractsDroppedStandard.csv")
b <- read.csv("data_source/testTractsDroppedStandard.csv")
c <- read.csv("data_source/trainTractsCorrected.csv")
d <- read.csv("data_source/testTractsCorrected.csv")
sum(c$secondTroubleGroup)
sum(d$secondTroubleGroup)


ftrain <- read.csv("data_source/trainTractsDroppedStandard.csv")
tnames <- colnames(ftrain)[38:100]


get_name <- function(x){
  a <- unlist(strsplit(x, split = "_"))
  paste0(a[1:length(a)-1], collapse = "_")
}
get_func <- function(x){
  a <- unlist(strsplit(x, split = "_"))
  b <- a[length(a)]
  if (b == "log") {return("Logarithm")}
  if (b == "sqrt") {return("Square Root")}
  if (b == "boxcox") {return("BoxCox")}
  if (substr(b, 1,3) == "exp") {return(paste0("Exponent Power to ", substring(b, 4)))}
}

write.csv(data.frame("var" = unname(sapply(tnames, get_name)),
                     "tr" = unname(sapply(tnames, get_func))), "ttt.csv")





# lm model ----------------------------------------------------------------
# strain <- read.csv("data_source/trainTractsCorrected.csv") %>% 
#   dplyr::select(c(incidents_per_capita, 4:36, 38:100))
# yvec <- log(strain$incidents_per_capita)
# xmat <- data.matrix(strain[, -1])
# test <- lm(yvec ~ firstTroubleGroup + secondTroubleGroup + thirdTroubleGroup, 
#    data = strain)
# 
# summary(test)
# 
# 
# test <- lm(yvec ~ firstTroubleGroup + secondTroubleGroup, 
#            data = strain)
# summary(test)

# checkthis <- read.csv("BARPLOT_inc.csv", stringsAsFactors = F)
# checkthis$names

strain <- ftrain %>% 
  dplyr::select(c(incidents_per_capita, 7:31, 33:102))

checkdata <- strain[, colnames(strain) %in% 
                      c("incidents_per_capita", checkthis$names)]
checklm <- lm(log(incidents_per_capita) ~., data = checkdata)
summary(checklm)
{par(mfrow = c(2,2), mar = c(1,2,2,0))
  plot(checklm, 1)
  plot(checklm, 2)
  plot(checklm, 3)
  plot(checklm, 4)
}

tab_model(checklm)


testset <- read.csv("data_source/testTractsDroppedStandard.csv") %>% 
  dplyr::select(c(incidents_per_capita, 7:31, 33:102))

testset$incidents_per_capita <- log(testset$incidents_per_capita)

stde <- predict(checklm, testset) - testset$incidents_per_capita
stde %>% abs() %>% hist()
c(testset$incidents_per_capita - mean(testset$incidents_per_capita)) %>% 
  abs() %>% hist()


# regression tree ---------------------------------------------------------
# strain <- ftrain %>% 
#   dplyr::filter(firstTroubleGroup != 1, 
#                 secondTroubleGroup != 1, 
#                 thirdTroubleGroup != 1) %>%  
#   dplyr::select(c(incidents_per_capita, 7:31, 33:102))
# yvec <- log(strain$incidents_per_capita)
# xmat <- data.matrix(strain[, -1])
# m1 <- rpart(yvec ~ ., data = data.frame(xmat), method  = "anova")
# rpart.plot(m1, type = 4)
# plotcp(m1)
# 
# 
# fit <- rpart(yvec ~., data = data.frame(xmat), method="anova")
# rpart.plot(fit)
# pfit<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
# rpart.plot(pfit)





# per capita squared
# strain <- ftrain %>% dplyr::select(c(incidents_per_capita, 4:31, 33:102))
# yvec <- log(strain$incidents_per_capita / ftrain$total_pop)
# hist(yvec)
# xmat <- data.matrix(strain[, -1])
# 
# bestlam <- getOptimalLambda(xmat, yvec)
# lambdas <- rep(bestlam, 1000)
# bootList <- lapply(lambdas, bootLasso, xdata = xmat, ydata = yvec)
# 
# exportBoot(bootList, "BARPLOT_cap2.csv")

# Drop 0s
