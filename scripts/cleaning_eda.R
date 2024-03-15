data <- read.csv(here("data", "TMS_dataset_Vaud_20240202_15.03Extract.csv"), sep = ";")

#Distribution of visits by country of origin
visits_by_country <- table(data$`F05_02.ENG`)

# Convert the table to a dataframe for use with ggplot2
visits_df <- as.data.frame(visits_by_country)

#renaming the columns
names(visits_df) <- c("Country", "Visits")

# Sort the dataframe by number of visits in descending order
visits_df <- visits_df[order(-visits_df$Visits),]

# Plot the distribution of visits by country of origin using ggplot2
ggplot(visits_df, aes(x = reorder(Country, -Visits), y = Visits)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Distribution of Visits by Country of Origin", x = "Country", y = "Number of Visits")
