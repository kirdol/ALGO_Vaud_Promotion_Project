# setwd("~/GitHub/ALGO_Vaud_Promotion_Project")

data <- read.csv("data/TMS_dataset_Vaud_20240314_FINAL.csv", sep = ",")

explanations <- read.csv("data/explanations.csv")

which(colnames(data) == "F31_01")
which(colnames(data) == "F32")

df <- data[, 18:33]

# Keep english columns only
seq(2, 17, 2)
df <- df[, seq(2, 17, 2)]

# Set responses to binary values
for (i in 1:ncol(df)) {
  df[, i] <- ifelse(df[, i] == "Applies", 1, ifelse(df[, i] == "Not applicable", 0, NA))
}

# We add a column to separate couples from families. Here people only traveled with their partner.

df$couples <- ifelse(df$F31_02_ENG == 1 & df$F31_03_ENG == 0 & df$F31_04_ENG == 0 & df$F31_05_ENG == 0, 1, 0)

# Sums of responses for frequency plot
df_sums <- list()

for (col in names(df)) {
  df_sums[[col]] <- sum(df[[col]])
}

# ggplot of frequency of responses
df_sums_df <- data.frame(Column = names(df_sums), Sum = unlist(df_sums))

#Change column names in df_sums_df
df_sums_df$Column <- c("Alone", "Partner", "Friends", "Children", "Other family", "Unknown people (group)", "Dog", "Other pet(s)", "Only with partner")
colnames(df_sums_df)[colnames(df_sums_df) == "Traveled with"] <- "Column"

# Sort by descending sum
df_sums_df$Column <- factor(df_sums_df$Column, levels = df_sums_df$Column[order(-df_sums_df$Sum)])

#Plot frequency
ggplot(df_sums_df, aes(x = Column, y = Sum)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of responses", x = "Traveled with", y = "Sum")

# make the plot in percentages

df_sums_df$Percentage <- (df_sums_df$Sum)/nrow(df)*100

ggplot(df_sums_df, aes(x = Column, y = Percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of responses", x = "Traveled with", y = "Percentage")


# Compare traveled with and how much average CHF was spent per person

# Create total spent column and add group size columns
df$Total_spent <- ifelse(data$F81_Total != "", data$F81_Total, data$F82_Total)
df$group_size <- ifelse(data$F32 != "", data$F32, data$F33)
df$group_size <- as.numeric(df$group_size)

#Reset column names in df_sums_df
df_sums_df$Column <- c("Alone", "Partner", "Friends", "Children", "Other family", "Unknown people (group)", "Dog", "Other pet(s)", "Only with partner")

# Average spending vector
average_spending <- numeric(0)

# Average spending for visitor Alone
average_spending <- c(average_spending, sum(df$Total_spent[df$F31_01_ENG == 1] / df$group_size[df$F31_01_ENG == 1], na.rm = TRUE) / sum(df$F31_01_ENG))
# With partner
average_spending <- c(average_spending, sum(df$Total_spent[df$F31_02_ENG == 1] / df$group_size[df$F31_02_ENG == 1], na.rm = TRUE) / sum(df$F31_02_ENG))
# With friends
average_spending <- c(average_spending, sum(df$Total_spent[df$F31_03_ENG == 1] / df$group_size[df$F31_03_ENG == 1], na.rm = TRUE) / sum(df$F31_03_ENG))
# With children
average_spending <- c(average_spending, sum(df$Total_spent[df$F31_04_ENG == 1] / df$group_size[df$F31_04_ENG == 1], na.rm = TRUE) / sum(df$F31_04_ENG))
# With other family
average_spending <- c(average_spending, sum(df$Total_spent[df$F31_05_ENG == 1] / df$group_size[df$F31_05_ENG == 1], na.rm = TRUE) / sum(df$F31_05_ENG))
# With unknown people
average_spending <- c(average_spending, sum(df$Total_spent[df$F31_06_ENG == 1] / df$group_size[df$F31_06_ENG == 1], na.rm = TRUE) / sum(df$F31_06_ENG))
# With dog
average_spending <- c(average_spending, sum(df$Total_spent[df$F31_07_ENG == 1] / df$group_size[df$F31_07_ENG == 1], na.rm = TRUE) / sum(df$F31_07_ENG))
# With other pet
average_spending <- c(average_spending, sum(df$Total_spent[df$F31_08_ENG == 1] / df$group_size[df$F31_08_ENG == 1], na.rm = TRUE) / sum(df$F31_08_ENG))
# Only partner
average_spending <- c(average_spending, sum(df$Total_spent[df$couples == 1] / df$group_size[df$couples == 1], na.rm = TRUE) / sum(df$couples))


# Add column to df_sums_df
df_sums_df$Spent = average_spending

# Plot average spending
# Sort by descending sum
df_sums_df$Column <- factor(df_sums_df$Column, levels = df_sums_df$Column[order(-df_sums_df$Spent)])

#Plot frequency
ggplot(df_sums_df, aes(x = Column, y = Spent)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average CHF spent by group type", x = "Traveled with", y = "Amount spent / person")
#######################################################################################################################################
# Swiss german column

unique(data[data$F05_02 == "Schweiz", ]$F05_03)

swiss_german_cantons = c("Luzern", "Schwyz", "Bern", "Neuenburg", "Aargau",
                         "Zürich", "Basel-Stadt", "Basel-Landschaft", "Obwalden",
                         "Appenzell Innerrhoden", "Schaffhausen", "Thurgau",
                         "St. Gallen", "Solothurn", "Uri", "Zug", "Nidwalden",
                         "Appenzell Ausserrhoden")


# Binary column for swiss germans
data$swiss_german <- ifelse(data$F05_02 == "Schweiz" & data$F05_03 %in% swiss_german_cantons, 1, 0)


# Percent of total tourists that are Swiss-German
nrow(data[data$swiss_german == 1,])/nrow(data)*100

# Percent of total tourists that are Swiss
nrow(data[data$F05_02 == "Schweiz",])/nrow(data)*100


#######################################################################################################################################
# Leaflet map

# Data from https://labs.karavia.ch/swiss-boundaries-geojson/
districts <- read_sf("data/swiss_districts.geojson")
districts

# Keep only Vaud
vaud <- districts |>
  filter(KANTONSNUM == 22)

# We will transform data to include corresponding leaflet district. 
unique(data$F105_Code)

# Create a new column 'leaflet_region' with NA values
data$leaflet_districts <- NA

# Assign cities to districts based on conditions
data$leaflet_districts[data$F105_Code == "Lausanne"] <- "Lausanne"
data$leaflet_districts[data$F105_Code %in% c("Montreux", "Vevey", "La Tour-de-Peilz", "Rougemont", "Château-d'Oex", "Veytaux", "Rossinière", "Corsier-sur-Vevey", "Saint-Légier-La-Chiésaz", "Blonay", "Chardonne", "Chernex")] <- "Riviera-Pays-d'Enhaut"
data$leaflet_districts[data$F105_Code %in% c("Morges", "Yens", "Aubonne")] <- "Morges"
data$leaflet_districts[data$F105_Code %in% c("Yverdon-les-Bains", "L'Abbaye", "Vallorbe", "Orbe", "Yvonand", "La Praz", "Le Chenit", "Bullet", "Montagny-près-Yverdon", "Saint Croix", "Le Rocheray", "Le Pont", "Le Sentier", "Le Brassus", "Les Rasses", "Les Charbonnieres")] <- "Jura-Nord vaudois"
data$leaflet_districts[data$F105_Code %in% c("Aigle", "Leysin", "Villeneuve (VD)", "Ormont-Dessus", "Bex", "Lavey-Morcles", "Gryon", "Ormont-Dessous", "Les Diablerets", "Villars-sur-Ollon")] <- "Aigle"
data$leaflet_districts[data$F105_Code %in% c("Nyon", "Prangins", "Chavannes-de-Bogis", "Coppet", "Rolle", "Commugny", "Duillier", "Gilly", "Founex")] <- "Nyon"
data$leaflet_districts[data$F105_Code %in% c("Valbroye", "Lucens", "Avenches", "Payerne", "Cudrefin", "St. Aubin", "Estavayer-le-Lac")] <- "Broye-Vully"
data$leaflet_districts[data$F105_Code %in% c("Crissier", "Bussigny", "Ecublens (VD)", "Renens", "Saint-Sulpice (VD)")] <- "Ouest lausannois"
data$leaflet_districts[data$F105_Code %in% c("Chexbres", "Bourg-en-Lavaux", "Cully")] <- "Lavaux-Oron"
data$leaflet_districts[data$F105_Code %in% c("Echallens")] <- "Gros-de-Vaud"

# Count districts in data and add to vaud
vaud$sum <- NA

vaud$sum[vaud$NAME == "Jura-Nord vaudois"] <- sum(data$leaflet_districts == "Jura-Nord vaudois")
vaud$sum[vaud$NAME == "Aigle"] <- sum(data$leaflet_districts == "Aigle")
vaud$sum[vaud$NAME == "Morges"] <- sum(data$leaflet_districts == "Morges")
vaud$sum[vaud$NAME == "Nyon"] <- sum(data$leaflet_districts == "Nyon")
vaud$sum[vaud$NAME == "Riviera-Pays-d'Enhaut"] <- sum(data$leaflet_districts == "Riviera-Pays-d'Enhaut")
vaud$sum[vaud$NAME == "Gros-de-Vaud"] <- sum(data$leaflet_districts == "Gros-de-Vaud")
vaud$sum[vaud$NAME == "Broye-Vully"] <- sum(data$leaflet_districts == "Broye-Vully")
vaud$sum[vaud$NAME == "Lavaux-Oron"] <- sum(data$leaflet_districts == "Lavaux-Oron")
vaud$sum[vaud$NAME == "Lausanne"] <- sum(data$leaflet_districts == "Lausanne")
vaud$sum[vaud$NAME == "Broye-Vully"] <- sum(data$leaflet_districts == "Broye-Vully")
vaud$sum[vaud$NAME == "Ouest lausannois"] <- sum(data$leaflet_districts == "Ouest lausannois")

# Transform sum to percentage
vaud$percentage <- round((vaud$sum)/nrow(data)*100, 2)

# Transform to leaflet projection 
vaud <- st_transform(vaud, crs = '+proj=longlat +datum=WGS84')

palette <- colorNumeric(palette = "Greens", domain = vaud$percentage)

leaflet(vaud) %>%
  addTiles() %>%
  setView(lng = 6.63, lat = 46.51, zoom = 9) %>%
  addPolygons(fillOpacity = 0.75, color = ~palette(vaud$percentage), weight = 0) %>%
  addPolygons(color = "black", weight = 2, fillOpacity = 0, label = paste(vaud$NAME, vaud$percentage, "%"))



# Add couples column to data
data$couples <- df$couples

# Function to calculate sums for couples
calculate_couples_sum <- function(data, vaud) {
  vaud$couples_sum <- NA
  unique_values <- unique(data$leaflet_districts)
  for (value in unique_values) {
    vaud$couples_sum[vaud$NAME == value] <- sum(data$leaflet_districts == value & data$couples == 1)
  }
  return(vaud)
}
vaud <- calculate_couples_sum(data, vaud)

# Create couples percentage from couples_sum
vaud$couples_percentage <- round((vaud$couples_sum)/sum(data$couples)*100, 2)

# Couples plot
couples_palette <- colorNumeric(palette = "Reds", domain = vaud$couples_percentage)

leaflet(vaud) %>%
  addTiles() %>%
  setView(lng = 6.63, lat = 46.61, zoom = 9) %>%
  addPolygons(fillOpacity = 0.75, color = ~couples_palette(vaud$couples_percentage), weight = 0) %>%
  addPolygons(color = "black", weight = 2, fillOpacity = 0, label = paste(vaud$NAME, vaud$couples_percentage, "%"))


# Add children column to data
data$children <- df$F31_04_ENG

# Function to calculate sums for children
calculate_children_sum <- function(data, vaud) {
  vaud$children_sum <- NA
  unique_values <- unique(data$leaflet_districts)
  for (value in unique_values) {
    vaud$children_sum[vaud$NAME == value] <- sum(data$leaflet_districts == value & data$children == 1)
  }
  return(vaud)
}
vaud <- calculate_children_sum(data, vaud)

# Create children percentage from couples_sum
vaud$children_percentage <- round((vaud$children_sum)/sum(data$children)*100, 2)

# Children plot
children_palette <- colorNumeric(palette = "Purples", domain = vaud$children_percentage)

leaflet(vaud) %>%
  addTiles() %>%
  setView(lng = 6.63, lat = 46.51, zoom = 9) %>%
  addPolygons(fillOpacity = 0.75, color = ~children_palette(vaud$children_percentage), weight = 0) %>%
  addPolygons(color = "black", weight = 2, fillOpacity = 0, label = paste(vaud$NAME, vaud$children_percentage, "%"))


# Now we will plot the proportion of couples for each district in %
vaud$couples_proportion <- round((vaud$couples_sum / vaud$sum) * 100, 2)

# Couples proportion plot
couples_palette <- colorNumeric(palette = "Oranges", domain = vaud$couples_proportion)

leaflet(vaud) %>%
  addTiles() %>%
  setView(lng = 6.63, lat = 46.51, zoom = 9) %>%
  addPolygons(fillOpacity = 0.75, color = ~couples_palette(vaud$couples_proportion), weight = 0) %>%
  addPolygons(color = "black", weight = 2, fillOpacity = 0, label = paste(vaud$NAME, vaud$couples_proportion, "%"))
# High proportion of couples without children in Broye-Vully and Lavaux-Oron


# Now we will plot the proportion of children for each district in %
vaud$children_proportion <- round((vaud$children_sum / vaud$sum) * 100, 2)

# Children proportion plot
children_palette <- colorNumeric(palette = "Purples", domain = vaud$children_proportion)

leaflet(vaud) %>%
  addTiles() %>%
  setView(lng = 6.63, lat = 46.51, zoom = 9) %>%
  addPolygons(fillOpacity = 0.75, color = ~children_palette(vaud$children_proportion), weight = 0) %>%
  addPolygons(color = "black", weight = 2, fillOpacity = 0, label = paste(vaud$NAME, vaud$children_proportion, "%"))
# High proportion of children in Aigle discrict, maybe for ski holidays.

## Add friends 
# Add friends column to data
data$friends <- df$F31_03_ENG

# Function to calculate sums for friends
calculate_friends_sum <- function(data, vaud) {
  vaud$friends_sum <- NA
  unique_values <- unique(data$leaflet_districts)
  for (value in unique_values) {
    vaud$friends_sum[vaud$NAME == value] <- sum(data$leaflet_districts == value & data$friends == 1)
  }
  return(vaud)
}
vaud <- calculate_friends_sum(data, vaud)

# Now we will plot the proportion of friends for each district in %
vaud$friends_proportion <- round((vaud$friends_sum / vaud$sum) * 100, 2)

# Friends proportion plot
friends_palette <- colorNumeric(palette = "Purples", domain = vaud$friends_proportion)

leaflet(vaud) %>%
  addTiles() %>%
  setView(lng = 6.63, lat = 46.51, zoom = 9) %>%
  addPolygons(fillOpacity = 0.75, color = ~friends_palette(vaud$friends_proportion), weight = 0) %>%
  addPolygons(color = "black", weight = 2, fillOpacity = 0, label = paste(vaud$NAME, vaud$friends_proportion, "%"))



#############################################################################################################################
# French tourists


unique(data[data$F05_02 == "Frankreich", ]$F05_03)

table(data$F05_02_ENG)

# Filter the data for rows where F05_02 is equal to "Frankreich"
french_regions <- data[data$F05_02 == "Frankreich", ]$F05_03

# Count the occurrences of each unique value of F05_03
table(french_regions)




#############################################################################################################################





# Save data to new dataset
#write.csv(data, file = "data/leaflet_dataset.csv", row.names = FALSE)

