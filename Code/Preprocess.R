######################################################
# Code for data preprocssing
# The raw files are not provided due to data 
# privacy reasons. 
# This code is only provided for transparency reasons
######################################################

library(dplyr)

## Collect individual answer tables (csv)

A1 <- read.csv("Data/results-survey678416.csv") %>%
  mutate(type = "A1") %>%
  mutate(dag.first = FALSE) %>%
  mutate(exposure = "gender") %>%
  mutate(stereotype = TRUE)

A2 <- read.csv("Data/results-survey795517.csv") %>%
  mutate(type = "A2") %>%
  mutate(dag.first = TRUE) %>%
  mutate(exposure = "gender") %>%
  mutate(stereotype = TRUE)

###

B1 <- read.csv("Data/results-survey685455.csv") %>%
  mutate(type = "B1") %>%
  mutate(dag.first = FALSE) %>%
  mutate(exposure = "gender") %>%
  mutate(stereotype = FALSE)

B2 <- read.csv("Data/results-survey371367.csv") %>%
  mutate(type = "B2") %>%
  mutate(dag.first = TRUE) %>%
  mutate(exposure = "gender") %>%
  mutate(stereotype = FALSE)

######

C1 <- read.csv("Data/results-survey218711.csv") %>%
  mutate(type = "C1") %>%
  mutate(dag.first = FALSE) %>%
  mutate(exposure = "lifestyle") %>%
  mutate(stereotype = FALSE)

C2 <- read.csv("Data/results-survey564219.csv") %>%
  mutate(type = "C2") %>%
  mutate(dag.first = TRUE) %>%
  mutate(exposure = "lifestyle") %>%
  mutate(stereotype = FALSE)

###

D1 <- read.csv("Data/results-survey859373.csv") %>%
  mutate(type = "D1") %>%
  mutate(dag.first = FALSE) %>%
  mutate(exposure = "lifestyle") %>%
  mutate(stereotype = TRUE)

D2 <- read.csv("Data/results-survey787476.csv") %>%
  mutate(type = "D2") %>%
  mutate(dag.first = TRUE) %>%
  mutate(exposure = "lifestyle") %>%
  mutate(stereotype = TRUE)

####################################################


Conclusion <- bind_rows(A1,A2,B1,B2,C1,C2,D1,D2) %>%
  na.omit() %>%
  mutate(adjusted = ifelse(T1 == 175, TRUE, FALSE)) %>%
  mutate(graph = ifelse(G1=="Modell A", "Chain", "Fork")) %>%
  mutate(gender = ifelse(Geschlecht == "weiblich", "female", "male")) %>%
  mutate(management = ifelse(Management == "Ja", TRUE, FALSE)) %>%
  mutate(lifestyle = ifelse(Lebenswandel == "Ja", TRUE, FALSE)) %>%
  mutate(consistent = case_when( adjusted == FALSE & graph == "Chain" ~ TRUE,
                                 adjusted == TRUE & graph == "Fork" ~ TRUE,
                                 TRUE ~ FALSE)) %>%
  rename(age = Alter) %>%
  select(type, adjusted, graph, consistent, gender, management, lifestyle, dag.first, exposure, age)

# Restrict to types in abstract, correct age if appropriate
Conclusion <- Conclusion %>%
  filter(type %in% c("A1", "A2", "D1", "D2")) %>%
  mutate(age = ifelse(age < 1950, age, 2021-age))

write.csv2(Conclusion, "Paper/Data/Conclusion.csv", row.names = FALSE)