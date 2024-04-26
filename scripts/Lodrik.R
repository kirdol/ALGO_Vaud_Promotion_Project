#### Distribution of Stars by District

```{r Distribution of Stars by City, echo = FALSE, warning = FALSE}
# Filtering to exclude cities with no valid data in F130_num
data_filtered <- data %>%
  filter(!is.na(F130_num)) %>%
  group_by(leaflet_districts) %>%
  # Calculate the average to ensure we only include cities with data
  summarise(Average_F130_num = mean(F130_num, na.rm = TRUE)) %>%
  filter(!is.na(Average_F130_num)) %>%
  arrange(desc(Average_F130_num))

# Reorder the F105_Code factor in the filtered dataframe according to the calculated averages
data$F105_Code <- factor(data$leaflet_districts, levels = data_filtered$leaflet_districts)

# Ensure that the original dataframe is filtered to match the selected cities
data <- data %>%
  filter(leaflet_districts %in% data_filtered$leaflet_districts)

# Create the chart with geom_density_ridges_gradient for cities with valid data in F130_num
ggplot(data, aes(x = F130_num, y = leaflet_districts, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Distribution of F130_num by City with Gradient",
       x = "F130_num",
       y = "District") +
  theme_ridges() + theme(legend.position = "none")
```