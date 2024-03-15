data <- read.csv(here("data", "TMS_dataset_Vaud_20240202_15.03Extract.csv"), sep = ";")

#select columns fulfilled
data_selected <- data |>
  select(Serial2, F05_01.ENG, F05_02, F05_03, F05_04, F12, F13, F15.ENG,
         F16.ENG, F17, F20, F21, F30.ENG, F31_01_ENG, F31_02_ENG, F31_03_ENG,
         F31_04_ENG, F31_05_ENG, F31_06_ENG, F31_07_ENG, F31_08_ENG, F31_09,
         F32, F33, hFerienDauer, hGruppePersonen, F80, F80_a05, F81_01, F81_02,
         F81_03, F81_04, F81_05, F81_06, F81_Total, F82_01, F82_02, F82_03,
         F82_04, F82_05, F82_06, F82_07, F82_Total, F83, F85, F105, F105A,
         F105_Code, F105_Code_nr, F105_OrgCode, F125_ENG, F125_a11.ENG, F30,
         F130_a07, F135_01, F12_F13_Mittelwert_2Cat_final)

# plot(data_selected$F16.ENG, data_selected$F135_01)

# Aggregate data by country of origin and create a table
visits_by_country <- table(data$`F05_02 ENG`)

# Convert the table to a data frame for plotting
visits_df <- as.data.frame(visits_by_country)
# Correctly set the names for the dataframe
names(visits_df) <- c("Country", "Visits")

# Sort the data frame by Visits in descending order for better visualization
visits_df <- visits_df[order(-visits_df$Visits),]

# Create the bar chart
ggplot(visits_df, aes(x=reorder(Country, -Visits), y=Visits)) +
  geom_bar(stat="identity", fill="skyblue") +
  theme_minimal() +
  coord_flip() + # Flip coordinates to make it horizontal
  labs(x="Country", y="Number of Visits", title="Distribution of Visits by Country of Origin")