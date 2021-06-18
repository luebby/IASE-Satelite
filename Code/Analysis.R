library(tidyverse)
library(mosaic)

Conclusion <- read.csv2("Data/Conclusion.csv")
  

# Sample Size
nrow(Conclusion)
tally( ~ type, data = Conclusion)

# Demographics
tally( ~ gender, data = Conclusion, format = "proportion")

# For data privacy reasons not provided in public data.
# favstats( ~ age, data = Conclusion)

tally( ~ management, data = Conclusion, format = "proportion")
tally( ~ lifestyle, data = Conclusion, format = "proportion")


# Visual diyplay of overall results
gf_percents( ~ adjusted | exposure + dag.first, data = Conclusion)
gf_percents( ~ graph | exposure + dag.first, data = Conclusion)
gf_percents( ~ consistent | exposure + dag.first, data = Conclusion)

# Analysis: Adjusted
Conclusion %>%
  group_by(exposure, dag.first) %>%
  summarise(prop = sum(adjusted)/n())

Conclusion %>%
  group_by(exposure, dag.first) %>%
  summarise(prop = sum(adjusted)/n()) %>%
  ungroup() %>%
  group_by(exposure) %>%
  summarise(prop = mean(prop))


# Analysis: Graph
Conclusion %>%
  group_by(exposure, dag.first) %>%
  summarise(prop_Fork = sum(graph == "Fork")/n())

Conclusion %>%
  group_by(exposure, dag.first) %>%
  summarise(prop_Fork = sum(sum(graph == "Fork"))/n()) %>%
  ungroup() %>%
  group_by(exposure) %>%
  summarise(prop_Fork = mean(prop_Fork))

# Analysis: Consistent
Conclusion %>%
  group_by(exposure, dag.first) %>%
  summarise(prop = sum(consistent)/n())

Conclusion %>%
  group_by(exposure, dag.first) %>%
  summarise(prop = sum(consistent)/n()) %>%   
  ungroup() %>%
  group_by(dag.first) %>%
  summarise(prop = mean(prop))

Conclusion %>%
  group_by(exposure, dag.first) %>%
  summarise(prop = sum(consistent)/n()) %>%   
  ungroup() %>%
  group_by(dag.first) %>%
  summarise(prop = mean(prop)) %>%
  ungroup() %>%
  summarise(prop = mean(prop))

# Inference
glm(adjusted ~ exposure + dag.first, data = Conclusion, family = binomial) %>% 
  summary()
glm(I(graph == "Fork") ~ exposure + dag.first, data = Conclusion, family = binomial) %>% 
  summary()
glm(consistent ~ exposure + dag.first, data = Conclusion, family = binomial) %>% 
  summary()
