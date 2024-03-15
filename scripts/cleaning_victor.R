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


# Sums of responses for frequency plot
df_sums <- list()

for (col in names(df)) {
  df_sums[[col]] <- sum(df[[col]])
}

# ggplot of frequency of responses
df_sums_df <- data.frame(Column = names(df_sums), Sum = unlist(df_sums))

ggplot(df_sums_df, aes(x = Column, y = Sum)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of responses", x = "Column", y = "Sum")

# Make a plot that shows if people chose multiple responses. Ex. How many of the people who
# travelled with partner also travelled with friends, children, etc
