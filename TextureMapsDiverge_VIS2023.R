library(lme4)
library(tidyr)
library(reshape2)
library(tidyverse)
library(stringr)
library(lmerTest)
library(MuMIn)
library(corrplot)

current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(current_dir)
opcl_df <- read.csv("regression_df_opcl.csv")
hilo_df<- read.csv("regression_df_hilo.csv")

# 
# opcl_model<- glmer(response~sign_fill*sign_rough + sign_fill*strength_fill + sign_fill*strength_rough
#                    + sign_rough*strength_fill+ sign_rough*strength_rough + strength_rough*strength_fill
#                    + (1|id),family=binomial, data = opcl_df)

X<-model.matrix(response~sign_fill*sign_rough + sign_fill*strength_fill + 
               sign_fill*strength_rough + 
               sign_rough*strength_fill+ sign_rough*strength_rough + 
               strength_rough*strength_fill, data=opcl_df)

corr_matrix<-cor(X[,2:ncol(X)])
# Plot the correlation matrix with labels on x and y axis
corrplot(corr_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)




opcl_model<- glmer(response~sign_fill + sign_rough + strength_fill + strength_rough
                   + (1|id),family=binomial, data = opcl_df, control = glmerControl(optCtrl = list(maxfun = 10000),optimizer = "bobyqa"))
summary(opcl_model)



hilo_model<- glmer(response~sign_fill + sign_rough + strength_fill + strength_rough
                   + (1|id),family=binomial, data = hilo_df, control = glmerControl(optCtrl = list(maxfun = 10000),optimizer = "bobyqa"))
summary(hilo_model)



