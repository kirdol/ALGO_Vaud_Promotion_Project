data <- read.csv(here("data", "TMS_dataset_Vaud_20240202_15.03Extract.csv"),
                 sep = ";")


### BB - BS ###

# BB F82_06       Q82: How much did you spend in total during your stay in Switzerland? - Other shopping:
F82_06 <- data |>
  select(Serial2, F82_06)

# Calculating the number of non-NA values for each column, normalized by the length of F82_06$Serial2
non_NA = sapply(data, function(x) sum(!is.na(x))/length(F82_06$Serial2))
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
# BC F82_07       Q82: How much did you spend in total during your stay in Switzerland? - Other expenses:

# BD F82_Total    Q82: How much did you spend in total during your stay in Switzerland? - Total expenditure excluding expenditure on package holidays

# BE F83          Q83: Does the package include travel to and from the resort?

# BF F85          Q85: You have therefore spent a total of [amount] (excluding package costs) during your stay in Switzerland. Is that right?

# BG F105         Q105: Your resort where you were staying when your email address was collected for this survey:

# BH F105A        Q105: Your resort where you were staying when your email address was collected for this survey. - Other resort:

# BI F105_Code    Q105: Your resort where you were staying when your email address was collected for this survey. -Code:

# BJ

# BK F105_OrgCode Q105: Your resort where you were staying when your email address was collected for this survey. - Organization Code:

# BM F125_ENG     Q125: Your accommodation:

# BO F125_a11 ENG Q125: Your accommodation: Other, namely:

# BP F130         F130: Star category of the hotel / spa house visited:

# BQ F130_a07     F130: Star category of the hotel / spa house visited: Other classification, namely:

# BR F135_01      Q135: Number of nights in [Answer from F105]:

# BS F12_F13_Mittelwert_2Cat_final Winter - Summer (average of arrival and departure date with limit end of April / beginning of May)




