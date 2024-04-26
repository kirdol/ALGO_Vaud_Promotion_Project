# Loading data
data <- read.csv(here("data", "TMS_dataset_Vaud_20240314_FINAL.csv"),
                 sep = ";")
### BB - BS ###
data_non_na <- na.omit(data$F82_06)
### General
# Calculating the number of non-NA values for each column, normalized by the length of F82_06$Serial2
non_NA = sapply(data, function(x) sum(!is.na(x))/length(data$Serial2))
a = non_NA[non_NA < 1]
b = sort(a, decreasing = TRUE)

# Converting to a dataframe for ggplot2
non_NA_df <- data.frame(Column = names(b), Non_NA_Values = b)

# Creating a bar plot with ggplot2
ggplot(non_NA_df, aes(x = reorder(Column, Non_NA_Values), y = Non_NA_Values)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Proportion of Non-NA Values by Column",
       x = "Columns",
       y = "Number of Non-NA Values") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme_minimal()

### BB F82_06       Q82: How much did you spend in total during your stay in Switzerland? - Other shopping:
data_82_06 <- data %>% filter(!is.na(F82_06))
ggplot(data_82_06, aes(x = F82_06)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogramme de F86_02", x = "F82_06", y = "Fréquence")

ggplot(data_82_06, aes(x = F82_06)) +
  geom_density(fill = "blue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Densité de F82_06", x = "F82_06", y = "Densité")

### BC F82_07       Q82: How much did you spend in total during your stay in Switzerland? - Other expenses:
data_82_07 <- data %>% filter(!is.na(F82_07))
ggplot(data_82_07, aes(x = F82_07)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogramme de F82_06", x = "F82_07", y = "Fréquence")

ggplot(data_82_07, aes(x = F82_07)) +
  geom_density(fill = "blue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Densité de F82_07", x = "F82_07", y = "Densité")

### BD F82_Total    Q82: How much did you spend in total during your stay in Switzerland? - Total expenditure excluding expenditure on package holidays
data_82_Total <- data %>% filter(!is.na(F82_Total))
ggplot(data_82_Total, aes(x = F82_Total)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogramme de F86_Total", x = "F82_Total", y = "Fréquence")

ggplot(data_82_Total, aes(x = F82_Total)) +
  geom_density(fill = "blue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Densité de F82_Total", x = "F82_Total", y = "Densité")

### BE F83          Q83: Does the package include travel to and from the resort?
data_83 <- data %>% filter(!is.na(F83))
ggplot(data_83, aes(x = F83)) +
  geom_bar(stat = "count", fill = "blue") +
  theme_minimal() +
  labs(title = "Bar Plot of F83", x = "F83", y = "Count")

### BF F85          Q85: You have therefore spent a total of [amount] (excluding package costs) during your stay in Switzerland. Is that right?
data_85 <- data %>% filter(!is.na(F85))
ggplot(data_85, aes(x = F85)) +
  geom_bar(stat = "count", fill = "blue") +
  theme_minimal() +
  labs(title = "Bar Plot of F85", x = "F85", y = "Count")

### BG F105         Q105: Your resort where you were staying when your email address was collected for this survey:
data_105 <- data %>% filter(!is.na(F105))
ggplot(data_105, aes(x = F105)) +
  geom_bar(stat = "count", fill = "blue") +
  theme_minimal() +
  labs(title = "Bar Plot of F105", x = "F105", y = "Count") +
  coord_flip()

# Unique values for the resorts
unique_values <- sort(unique(data_105$F105))
datatable(data.frame(UniqueValues = unique_values), options = list(pageLength = 100))

### BH F105A        Q105: Your resort where you were staying when your email address was collected for this survey. - Other resort:
data_105A <- data %>% filter(!is.na(F105A))

# Unique values for the other resorts
unique_values_105A <- sort(unique(data_105A$F105A))
datatable(data.frame(UniqueValues = unique_values_105A), options = list(pageLength = 100))

### BI F105_Code    Q105: Your resort where you were staying when your email address was collected for this survey. -Code:
data_105_Code <- data %>% filter(!is.na(F105_Code))
ggplot(data_105_Code, aes(x = F105_Code)) +
  geom_bar(stat = "count", fill = "blue") +
  theme_minimal() +
  labs(title = "Bar Plot of F105_code", x = "F105_code", y = "Count") +
  coord_flip()

# Unique values for the resort codes
unique_values_105_Code <- sort(unique(data_105_Code$F105_Code))
datatable(data.frame(UniqueValues = unique_values_105_Code), options = list(pageLength = 100))

# BJ F105_Code_nr Q105: Your resort where you were staying when your email address was collected for this survey. -Code: Number:
data_105_Code_nr <- data %>% filter(!is.na(F105_Code_nr))
ggplot(data_105_Code_nr, aes(x = F105_Code_nr)) +
  geom_bar(stat = "count", fill = "blue") +
  theme_minimal() +
  labs(title = "Bar Plot of F105_Code_nr", x = "F105_Code_nr", y = "Count") +
  coord_flip()

# Unique values for the resort codes numbers
unique_values_105_Code_nr <- sort(unique(data_105_Code_nr$F105_Code_nr))
datatable(data.frame(UniqueValues = unique_values_105_Code_nr), options = list(pageLength = 100))

# BK F105_OrgCode Q105: Your resort where you were staying when your email address was collected for this survey. - Organization Code:
data_105_OrgCode <- data %>% filter(!is.na(F105_OrgCode))
ggplot(data_105_OrgCode, aes(x = F105_OrgCode)) +
  geom_bar(stat = "count", fill = "blue") +
  theme_minimal() +
  labs(title = "Bar Plot of F105_OrgCode", x = "F105_OrgCode", y = "Count") +
  coord_flip()

# Unique values for the resort organization codes
unique_values_105_OrgCode <- sort(unique(data_105_OrgCode$F105_OrgCode))
datatable(data.frame(UniqueValues = unique_values_105_OrgCode), options = list(pageLength = 100))


# BM F125_ENG     Q125: Your accommodation:
data_125_ENG <- data %>% filter(!is.na(F125_ENG))
ggplot(data_125_ENG, aes(x = F125_ENG)) +
  geom_bar(stat = "count", fill = "blue") +
  theme_minimal() +
  labs(title = "Bar Plot of F125_ENG", x = "F125_ENG", y = "Count") +
  coord_flip()

# Unique values for the accommodations
unique_values_125_ENG <- sort(unique(data_125_ENG$F125_ENG))
datatable(data.frame(UniqueValues = unique_values_125_ENG), options = list(pageLength = 100))

# BO F125_a11 ENG Q125: Your accommodation: Other, namely:
data_125_a11_ENG <- data %>% filter(!is.na(F125_a11.ENG))
ggplot(data_125_a11_ENG, aes(x = F125_a11.ENG)) +
  geom_bar(stat = "count", fill = "blue") +
  theme_minimal() +
  labs(title = "Bar Plot of F125_a11.ENG", x = "F125_a11.ENG", y = "Count") +
  coord_flip()

# Unique values for the other accommodations
unique_values_125_a11_ENG <- sort(unique(data_125_a11_ENG$F125_a11.ENG))
datatable(data.frame(UniqueValues = unique_values_125_a11_ENG), options = list(pageLength = 100))

# BP F130         F130: Star category of the hotel / spa house visited:
data_130 <- data %>% filter(!is.na(F130))
ggplot(data_130, aes(x = F130)) +
  geom_bar(stat = "count", fill = "blue") +
  theme_minimal() +
  labs(title = "Bar Plot of F130", x = "F130", y = "Count") +
  coord_flip()

# Unique values for the star categories
unique_values_130 <- sort(unique(data_130$F130))
datatable(data.frame(UniqueValues = unique_values_130), options = list(pageLength = 100))

# BQ F130_a07     F130: Star category of the hotel / spa house visited: Other classification, namely:
data_130_a07 <- data %>% filter(!is.na(F130_a07))
ggplot(data_130_a07, aes(x = F130_a07)) +
  geom_bar(stat = "count", fill = "blue") +
  theme_minimal() +
  labs(title = "Bar Plot of F130_a07", x = "F130_a07", y = "Count") +
  coord_flip()

# Unique values for the other star categories
unique_values_130_a07 <- sort(unique(data_130_a07$F130_a07))
datatable(data.frame(UniqueValues = unique_values_130_a07), options = list(pageLength = 100))

# BR F135_01      Q135: Number of nights in [Answer from F105]:
data_135_01 <- data %>% filter(!is.na(F135_01))
ggplot(data_135_01, aes(x = F135_01)) +
  geom_bar(stat = "count", fill = "blue") +
  theme_minimal() +
  labs(title = "Bar Plot of F135_01", x = "F135_01", y = "Count") +
  coord_flip()

# BS F12_F13_Mittelwert_2Cat_final Winter - Summer (average of arrival and departure date with limit end of April / beginning of May)
data_12_F13_Mittelwert_2Cat_final <- data %>% filter(!is.na(F12_F13_Mittelwert_2Cat_final))
ggplot(data_12_F13_Mittelwert_2Cat_final, aes(x = F12_F13_Mittelwert_2Cat_final)) +
  geom_bar(stat = "count", fill = "blue") +
  theme_minimal() +
  labs(title = "Bar Plot of F12_F13_Mittelwert_2Cat_final", x = "F12_F13_Mittelwert_2Cat_final", y = "Count") +
  coord_flip()

# Unique possible values for the average of arrival and departure date with limit end of April / beginning of May
unique_values_12_F13_Mittelwert_2Cat_final <- sort(unique(data_12_F13_Mittelwert_2Cat_final$F12_F13_Mittelwert_2Cat_final))
datatable(data.frame(UniqueValues = unique_values_12_F13_Mittelwert_2Cat_final), options = list(pageLength = 100))




