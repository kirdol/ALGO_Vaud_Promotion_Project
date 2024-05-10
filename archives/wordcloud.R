library(tm)
library(ggwordcloud)


#### Other accomodation word cloud (column F125_a11_ENG)

# Read data and create Corpus
data <- read.csv("data/TMS_dataset_Vaud_20240314_FINAL.csv", sep = ",")
other_accomodation <- data$F125_a11_ENG[data$F125_a11_ENG != ""]
other_accomodation
docs <- Corpus(VectorSource(other_accomodation))

# Clean data, remove numbers, whitespace, punctuation, stopwords.
docs <- docs |>
  tm_map(removeNumbers) |>
  tm_map(removePunctuation) |>
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

# Create dataframe with frequency of remaining words
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
other_accomodation_df <- data.frame(word = names(words),freq=words)

# Create wordcloud
accomodation_wordcloud <- ggplot(other_accomodation_df, aes(label = word, size = freq, color = word)) +
  geom_text_wordcloud(shape = "square") +
  scale_size_area(max_size = 20)

accomodation_wordcloud

### Other currency wordcloud

# Read data and create Corpus
other_currency <- data$F80_a05[data$F80_a05 != ""]
other_currency
docs <- Corpus(VectorSource(other_currency))

# Clean data, remove numbers, whitespace, punctuation, stopwords.
docs <- docs |>
  tm_map(removeNumbers) |>
  tm_map(removePunctuation) |>
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

# Create dataframe with frequency of remaining words
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
other_currency_df <- data.frame(word = names(words),freq=words)

# Create wordcloud
currency_wordcloud <- ggplot(other_currency_df, aes(label = word, size = freq, color = word)) +
  geom_text_wordcloud(shape = "square") +
  scale_size_area(max_size = 20)

currency_wordcloud
