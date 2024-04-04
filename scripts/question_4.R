data <- read.csv(here("data", "TMS_dataset_Vaud_20240314_FINAL.csv"),
                 sep = ";")

### Most Popular Destinations ###
resort_counts <- table(data$F105_Code)
accommodation_counts <- table(data$F125_ENG)

# Proportion
resort_counts_df <- as.data.frame(resort_counts)
resort_counts_df$Proportion <- resort_counts_df$Freq / sum(resort_counts_df$Freq)

# Plot
plot_ly(resort_counts_df, labels = ~Var1, values = ~Proportion, type = 'pie',
        textinfo = 'label+percent',
        insidetextorientation = 'radial') %>%
  layout(title = 'Most Popular Destinations',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



