getwd()
setwd("C:/Users/alexa/Documents/R projects")
getwd()

surveys_complete <- read.csv("C:/Users/alexa/Documents/R projects/portal_data_joined.csv")

install.packages("tidyverse")
library(tidyverse)
as_tibble(surveys_complete)
colnames(surveys_complete)

ggplot(surveys_complete, aes(x=weight, y=hindfoot_length))+
  geom_point(position = position_jitter())

surveys_complete %>% 
  filter(!is.na(weight)&!is.na(hindfoot_length)) %>% 
   ggplot(aes(x=weight, y=hindfoot_length))+
           geom_point()

any(is.na(surveys_complete["weight"])) 

ggplot(surveys_complete, aes(x=weight, y=hindfoot_length, colour=species_id, 
                             shape=as.factor(plot_id)))+
  geom_point(alpha=0.3, position = position_jitter())

ggplot(surveys_complete, aes(x = weight, y = hindfoot_length, colour = species_id)) +
  geom_point(alpha = 0.3,  position = position_jitter()) + stat_smooth(method = "glm") +
  theme(legend.position = "none")

surveys_complete %>%
  filter(species_id == "DS") %>%
  ggplot(aes(x = weight, y = hindfoot_length, colour = species_id)) +
  geom_point(alpha = 0.3,  position = position_jitter()) + stat_smooth(method = "lm")

## Same output using subset method
## ggplot(subset(surveys_complete,  species_id == "DS"), aes(x = weight, y = hindfoot_length, colour = species_id)) +
##  geom_point(alpha = 0.3,  position = position_jitter()) + stat_smooth(method = "lm")


ggplot(subset(surveys_complete, species_id == "DS"),
       aes(x = weight, y = hindfoot_length, colour = species_id)) +
  geom_point(alpha = 0.3,  position = position_jitter()) + stat_smooth(method = "lm") +
  ylim(c(0,60))

##Using ylim subsets the data to be represented:
ggplot(surveys_complete,
       aes(x = weight, y = hindfoot_length, colour = species_id)) +
  geom_point(alpha = 0.3,  position = position_jitter()) + stat_smooth(method = "lm") +
  ylim(c(40, 60))

##while setting limits with coord_cartesian acts a magnifying glass:
ggplot(surveys_complete,
       aes(x = weight, y = hindfoot_length, colour = species_id)) +
  geom_point(alpha = 0.3,  position = position_jitter()) + stat_smooth(method = "lm") +
  coord_cartesian(ylim = c(40, 60))

ggplot(subset(surveys_complete, !is.na(weight)), aes(x=species_id, y=weight))+
  geom_boxplot()

##By adding points to boxplot, we can see particular measurements and the abundance of measurements.
ggplot(subset(surveys_complete, !is.na(weight)), aes(x = species_id, y = weight)) +
  geom_point(alpha=0.3, color="tomato", position = "jitter") +
  geom_boxplot(alpha=0)+ coord_flip()

subset(surveys_complete, species_id=="SS") %>% 
  select(species_id, weight) %>% 
  drop_na()

surveys_complete %>% 
  ggplot(aes(x=species_id, y=hindfoot_length)) +
           geom_violin(alpha=0.3, color="green", position = "jitter") +
           geom_boxplot(alpha=0) + coord_flip()

##FACETING

ggplot(subset(surveys_complete, species_id %in% c("DO", "DM", "DS") & sex %in% c("F", "M")),
       aes(x = sex, y = weight,  colour = interaction(sex, species_id))) + facet_wrap( ~ species_id) +
  geom_point(alpha = 0.3, position = "jitter") +
  geom_boxplot(alpha = 0, colour = "black")

ggplot(subset(filter(surveys_complete, sex %in% c("M","F"), species_id %in% c("NL", "DM", "DS")), !is.na(weight)), 
       aes(x = species_id, y = weight, color=sex)) +
  geom_point(alpha=0.3, position = "jitter") +
  geom_boxplot(alpha=0, color="black")+ coord_flip() + facet_wrap(~sex)

unique(surveys_complete$sex)
library(dplyr)
n_distinct(surveys_complete$sex)

surveys_complete %>% 
  count(sex)

surveys_complete["sex"]

## BARPLOT

ggplot(surveys_complete, aes(species_id))+
  geom_bar()

ggplot(surveys_complete, aes(species_id)) + geom_bar()

surveys_complete %>%
  filter(!is.na(weight)) %>%
  group_by(species_id) %>%
  summarize(mean_weight = mean(weight)) %>%
  ggplot(aes(x = species_id, y = mean_weight)) + geom_bar(stat = "identity")

###Plotting time series data
##Let’s calculate number of counts per year for each species. To do that we need to group data first and count records within each group.
yearly_counts <- surveys_complete %>%
  group_by(year, species_id) %>%
  summarise(count=n())

yearly_counts

ggplot(yearly_counts, aes(x=year, y=count))+
  geom_bar(stat = "identity")

ggplot(yearly_counts, aes(x=year, y=count, group=species_id, color=species_id))+
  geom_line()
  

