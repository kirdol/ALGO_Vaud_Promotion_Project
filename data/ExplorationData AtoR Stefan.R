data <- read.csv(here("data", "TMS_dataset_Vaud_20240202_15.03Extract.csv"), sep = ";")
data <- data %>% select(c(1:18))

library("dplyr")
library("ggplot2")

### F05_01 ###

#nb of occurences
counts0501 <- table(data$F05_01.ENG) %>%
  print()

#plot
plot(counts0501)

### F05_02 ###

#list all countries
ListCountries <- list(unique(data$F05_02.ENG))
ListCountries2 <- unique(data$F05_02.ENG)

OccurencesCity <- data %>% 
  group_by(data$F05_02.ENG) %>% 
  count(sort = TRUE) %>% 
  print() 

#nb of occurences 
counts0502 <- table(data$F05_02.ENG) %>% 
  print()

LeftJoin <- ListCountries2 %>% left_join()

#plot
plot(counts0502)


