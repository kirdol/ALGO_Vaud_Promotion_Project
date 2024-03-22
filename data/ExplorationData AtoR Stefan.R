data <- read.csv(here("data", "TMS_dataset_Vaud_20240202_15.03Extract.csv"), sep = ";")
data <- data %>% select(c(1:18))

library("dplyr")
library("ggplot2")

### F05_01 ###
#nb of occurrences by country and continents 
OccurencesCountry <- data %>% 
  group_by(data$F05_02.ENG) %>% 
  count(sort = TRUE) %>% 
  print() 

OccurencesContinent <- data %>% 
  group_by(data$F05_01.ENG) %>% 
  count(sort = TRUE) %>% 
  print() 

#list all countries and continents 
print(unique(data$F05_02.ENG))
print(unique(data$F05_01.ENG))

#in percentages and plot continents 
F05_01_PCT <- data %>% 
  group_by(F05_01.ENG) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>% 
  arrange(desc(Percentage))

ggplot(F05_01_PCT, aes(x=reorder(F05_01.ENG, Percentage), y=Percentage, fill=F05_01.ENG)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(x="Continents", y="Percentage", title="Percentage of Each Unique Continent") +
  theme(legend.title = element_blank(), # Remove the legend title
        legend.position = "none", # Hide the legend as the colors are not necessary with labels
        plot.title = element_text(hjust = 0.5), # Center the plot title
        axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for readability
  geom_text(aes(label=sprintf("%.2f%%", Percentage)), position=position_dodge(width=0.9), vjust=-0.25) # Add percentage labels

### F05_02 ###
#plot with continents 
F05_02_PCT <- data %>%
  group_by(F05_02.ENG, F05_01.ENG) %>%  # Group by both country and continent
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  arrange(desc(Percentage))

ggplot(F05_02_PCT, aes(x = reorder(`F05_02.ENG`, -Percentage), y = Percentage, fill = `F05_01.ENG`)) + 
  geom_bar(stat = "identity") +
  coord_flip() +  # Make the plot horizontal
  scale_fill_brewer(palette = "Set3", name = "Continent") +  # Use a color palette for continents
  theme_minimal() +
  labs(x = "Country", y = "Percentage", title = "Percentage of Responses by Country and Continent") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")  # Adjust legend position





