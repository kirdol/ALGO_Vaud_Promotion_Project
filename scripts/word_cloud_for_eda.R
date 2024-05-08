# Loading the packages
source(here::here("scripts", "setup.R"))

# Read data
data_wc <- read.csv(here("data", "TMS_dataset_Vaud_20240314_FINAL.csv"), sep = ",")

# Filter non-empty responses
other_accomodation <- data_wc$F125_a11_ENG[data_wc$F125_a11_ENG != ""]

# Create a text corpus
docs <- Corpus(VectorSource(other_accomodation))

# Define a custom cleaning function
cleanText <- function(doc) {
  orig_doc <- doc  # Preserve original for comparison
  doc <- tolower(doc)
  doc <- removeNumbers(doc)
  doc <- removePunctuation(doc)
  doc <- stripWhitespace(doc)
  doc <- removeWords(doc, stopwords("english"))
  # Ensure that doc is a single character string
  doc <- paste(doc, collapse = " ")
  # If the document is empty after cleaning, keep a placeholder text
  if (nchar(trimws(doc)) == 0)
    {return("emptyplaceholder")}  # Placeholder for empty documents
  return(doc)}

# Apply the custom cleaning function
suppressWarnings(docs <- tm_map(docs, content_transformer(cleanText)))

# Remove any placeholder for empty documents
docs <- docs[sapply(docs, function(x) x != "emptyplaceholder")]

# Create a term-document matrix
dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix), decreasing = TRUE)
other_accomodation_df <- data.frame(word = names(words), freq = words)

# Create a word cloud
accomodation_wordcloud <- ggplot(other_accomodation_df,
                                 aes(label = word,
                                     size = freq,
                                     color = word)) +
  geom_text_wordcloud(shape = 'square',
                      rm_outside = TRUE) +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  theme(text = element_text(size = 12))

# Print the word cloud
print(accomodation_wordcloud)