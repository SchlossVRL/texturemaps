library(lme4)
library(tidyr)
library(reshape2)
library(tidyverse)
library(stringr)

current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(current_dir)

df <- read.csv("TextureMaps1_long1.csv")

#df_wide <- read.csv("TextureMaps1.csv")
#df_wide$Subj <- factor(df_wide$Subj)
#df <- melt(df_wide, id.vars="Subj")
#write.csv(df, "TextureMaps1_long.csv", row.names=FALSE)
#View(df)



# Subset data according to kind, and test for differences against chance (significan intercept)
# ROUGHNESS
m.roughness <- glmer(response ~ (1|subj), family = binomial, subset(df, texture == 'Roughness'), control=glmerControl(optimizer="bobyqa"))
summary(m.roughness)

# DIAMETER
m.diameter <- glmer(response ~ (1|subj), family = binomial, subset(df, texture == 'Diameter'), control=glmerControl(optimizer="bobyqa"))
summary(m.diameter)

# HEIGHT
m.height <- glmer(response ~ (1|subj), family = binomial, subset(df, texture == 'Height'), control=glmerControl(optimizer="bobyqa"))
summary(m.height)


#### Plots #####

## barplot
df%>%group_by(subj,texture)%>%summarize(subj_mean = mean(response))%>%
  group_by(texture)%>%summarize(agg_mean= mean(subj_mean),agg_se = sd(subj_mean)/sqrt(n()))%>%
  ggplot(aes(x=texture,y=agg_mean))+geom_bar(stat = 'identity')+
  geom_errorbar(aes(ymin = agg_mean - agg_se*1.96, ymax = agg_mean + agg_se*1.96), width = 0.2) +
  labs(title = "", x = "Texture Type", y = "Proportion") + ### change labels here
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 20),
        panel.grid = element_blank(),
        axis.line = element_line(size = 1))
ggsave("exp1_proportions.pdf", width = 10, height = 8, units = "in")


## pointplot
df%>%group_by(subj,texture)%>%summarize(subj_mean = mean(response))%>%
  group_by(texture)%>%summarize(agg_mean= mean(subj_mean),agg_se = sd(subj_mean)/sqrt(n()))%>%
  ggplot(aes(x=texture,y=agg_mean))+geom_point(size = 5)+
  geom_errorbar(aes(ymin = agg_mean - agg_se*1.96, ymax = agg_mean + agg_se*1.96), width = 0.2) +
  labs(title = "", x = "Texture Type", y = "Proportion") + ### change labels here
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  theme_minimal()+
  theme(axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 20),
        panel.grid = element_blank(),
        axis.line = element_line(size = 1))
ggsave("exp1_proportions_point.pdf", width = 10, height = 8, units = "in")


