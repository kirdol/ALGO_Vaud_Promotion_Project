setwd("~/GitHub/ALGO_Vaud_Promotion_Project")

data <- read.csv("data/TMS_dataset_Vaud_20240202_15.03Extract.csv", sep = ";")

explanations <- read.csv("data/explanations.csv")

which(colnames(data) == "F31_01")
which(colnames(data) == "F32")

df <- data[, 19:36]

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

ggplot(df_sums_df, aes(x = Column, y = Sum)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_bar(data = subset(df_sums_df, Column == "couples"), fill = "orange", stat = "identity") +
  labs(title = "Frequency of responses", x = "Column", y = "Sum")

# make the plot in percentages

df_sums_df$Percentage <- (df_sums_df$Sum)/nrow(df)*100

ggplot(df_sums_df, aes(x = Column, y = Percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_bar(data = subset(df_sums_df, Column == "couples"), fill = "orange", stat = "identity") +
  labs(title = "Frequency of responses", x = "Column", y = "Percentage")


#######################################################################################################################################
# Leaflet map

library(leaflet)
library(sf)

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
data$leaflet_districts[data$F105_Code %in% c("Montreux", "Vevey", "La Tour-de-Peilz", "Rougemont", "Château-d'Oex", "Veytaux", "Rossinière", "Corsier-sur-Vevey")] <- "Riviera-Pays-d'Enhaut"
data$leaflet_districts[data$F105_Code %in% c("Morges", "Yens", "Aubonne")] <- "Morges"
data$leaflet_districts[data$F105_Code %in% c("Yverdon-les-Bains", "L'Abbaye", "Vallorbe", "Orbe", "Yvonand", "La Praz", "Le Chenit", "Bullet", "Montagny-près-Yverdon")] <- "Jura-Nord vaudois"
data$leaflet_districts[data$F105_Code %in% c("Aigle", "Leysin", "Villeneuve (VD)", "Ormont-Dessus", "Bex", "Lavey-Morcles", "Gryon", "Ormont-Dessous")] <- "Aigle"
data$leaflet_districts[data$F105_Code %in% c("Nyon", "Prangins", "Chavannes-de-Bogis", "Coppet", "Rolle", "Commugny", "Duillier", "Gilly", "Founex")] <- "Nyon"
data$leaflet_districts[data$F105_Code %in% c("Valbroye", "Lucens", "Avenches", "Payerne", "Cudrefin")] <- "Broye-Vully"
data$leaflet_districts[data$F105_Code %in% c("Crissier", "Bussigny", "Ecublens (VD)")] <- "Ouest lausannois"
data$leaflet_districts[data$F105_Code %in% c("Chexbres", "Bourg-en-Lavaux")] <- "Lavaux-Oron"
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


# Transform to leaflet projection 
vaud <- st_transform(vaud, crs = '+proj=longlat +datum=WGS84')

palette <- colorNumeric(palette = "Purples", domain = vaud$sum)

leaflet(vaud) %>%
  addTiles() %>%
  setView(lng = 6.63, lat = 46.51, zoom = 9) %>%
  addPolygons(fillOpacity = 0.75, color = ~palette(vaud$sum), weight = 0) %>%
  addPolygons(color = "black", weight = 2, fillOpacity = 0, label = paste(vaud$NAME, vaud$sum))

# Save data to new dataset
write.csv(data, file = "data/leaflet_dataset.csv", row.names = FALSE)
