#Question 3.1)
#Webscraping website chosen is Quotes.toscrape.com

library(rvest)

url <- "https://quotes.toscrape.com/"
webpage <- read_html(url)

#Firstly, extracting the quotations
quotes <- html_elements(webpage, ".quote .text")
quotes <- html_text2(quotes)

#Next, extracting the authors
authors <- html_elements(webpage, ".author")
authors <- html_text2(authors)

#Now to brifely clean the extracted text
quotes <- trimws(quotes)
authors <- trimws(authors)

#Now to store the extracted info into a data frame
quote_dataframe <- data.frame(
  Quote = quotes,
  Author = authors
)

print(quote_dataframe)
View(quote_dataframe)
str(quote_dataframe)
dim(quote_dataframe)

#Question 3.2)
library(tm)

cleaned_quotes <- quote_dataframe$Quote

#Now for each preprocessing technique. Starting with lowercase
cleaned_quotes <- tolower(cleaned_quotes)
#Removing URLS (if there are any)
cleaned_quotes <- gsub("https?://[^[:space:]]+", " ", cleaned_quotes)
#Numbers removal
cleaned_quotes <- gsub("[[:digit:]]+", " ", cleaned_quotes)
#Punctuation removal
cleaned_quotes <- gsub("[[:punct:]]", " ", cleaned_quotes)
#Isolated letter removal
cleaned_quotes <- gsub("\\b[a-z]\\b", " ", cleaned_quotes)
#Removing white spaces
cleaned_quotes <- gsub("[[:space:]]+", " ", cleaned_quotes)
cleaned_quotes <- trimws(cleaned_quotes)

#Stop words and keeping negative words
stop_words <- stopwords("english")
stop_words <- setdiff(stop_words, c("no", "never", "not"))

#Now to remove stop words from each extracted quotation
for (i in seq_along(cleaned_quotes)) {
  words <- strsplit(cleaned_quotes[i], " ")[[1]]
  #Removing words that appear in stop word list
  words <- words[!(words %in% stop_words)]
  #Combining the remaining words into a sentence
  cleaned_quotes[i] <- paste(words, collapse = " ")
}

#Finally, adding the cleaned text to the original data frame
quote_dataframe$Cleaned_Quote <- cleaned_quotes

print(quote_dataframe[, c("Quote", "Cleaned_Quote")])
View(quote_dataframe)
str(quote_dataframe)
dim(quote_dataframe)

#Question 3.3)
#Preparing readable text for summarisation
quote_dataframe$Summary_Text <- trimws(quote_dataframe$Quote)

quote_dataframe$Summary_Text <- gsub("[[:space:]]+", " ", quote_dataframe$Summary_Text)

#Building a word frequency table
word_list <- strsplit(quote_dataframe$Cleaned_Quote, " ")
#Creating a list of unique words
vocabulary <- sort(unique(unlist(word_list)))
term_counts <- matrix(
  0,
  nrow = nrow(quote_dataframe),
  ncol = length(vocabulary)
)

#Now to assign the word names to the matrix columns
colnames(term_counts) <- vocabulary
for (i in 1:nrow(quote_dataframe)) {
  term_counts[i, ] <- as.numeric(
    table(factor(word_list[[i]], levels = vocabulary))
  )
}

word_freq <- colSums(term_counts)
#Displaying top words
top_words <- head(
  sort(word_freq, decreasing = TRUE), 10
)
print(top_words)

#First Summarisation: Frequency-Based Extractive Summarisation

freq_scores <- numeric(nrow(quote_dataframe))

for (i in 1:nrow(quote_dataframe)) { #Calculating frequency for each quotation
  total_words <- sum(term_counts[i, ])
  
  if (total_words > 0) {
    freq_scores[i] <- sum(
      term_counts[i, ] * word_freq
    ) / total_words
  }
}

#Determining the three highest scoring quotations
top_freq <- order(
  freq_scores, decreasing = TRUE
)[1:3]

#Creating the first summary
freq_summary <- data.frame(
  Author = quote_dataframe$Author[top_freq],
  Quote = quote_dataframe$Summary_Text[top_freq],
  Score = round(freq_scores[top_freq], 2)
)
print(freq_summary)

#Second Summarisation: TF-IDF Centroid Based Summarisation

document_freq <- colSums(term_counts > 0)

#Calculating IDF
idf <- log(
  (nrow(quote_dataframe) + 1) / (document_freq + 1)
) + 1

#Creating TF-IDF matrix and for every quote
tfidf <- term_counts
for (i in 1:nrow(quote_dataframe)) {
  total_words <- sum(term_counts[i, ])
  if (total_words > 0) {
    tfidf[i, ] <- (
      term_counts[i, ] / total_words
    ) * idf
  }
}

#Calculating the centroid (average TF-IDF vector)
centroid <- colMeans(tfidf)
sim_scores <- numeric(nrow(quote_dataframe))
for (i in 1:nrow(quote_dataframe)) {
  
#Calculate cosine similarity
  numerator <- sum(tfidf[i, ] * centroid)
  
  denominator <- sqrt(sum(tfidf[i, ]^2)) *
    sqrt(sum(centroid^2))
  
  if (denominator > 0) {
    sim_scores[i] <- numerator / denominator
  }
}
#Identifying the three most similar quotations
top_tfidf <- order(
  sim_scores, decreasing = TRUE
)[1:3]

#Creating the second summary
tfidf_summary <- data.frame(
  Author = quote_dataframe$Author[top_tfidf],
  Quote = quote_dataframe$Summary_Text[top_tfidf],
  Score = round(sim_scores[top_tfidf], 3)
)
print(tfidf_summary)

#Term-Based Extractive Summarisation Visualisation: Horizontal Bar plot

par(mar = c(5, 9, 4,2))
#Horizontal bar plot
barplot(
  rev(top_words),
  horiz = TRUE,
  las = 1,
  main = "Top 10 Most Frequent Words",
  xlab = "Word Frequency",
  col = "yellow",
  cex.names = 0.8
)

#Comparing the two Summarisation techniques: Grouped bar chart
#First, normalising term frequency scores
freq_normalisation <- freq_scores / max(freq_scores)

#Next, normalising TF-IDF similarity scores
tfidf_normalisation <- sim_scores / max(sim_scores)

#Combining the scores into a matrix
comp_scores <- rbind(
  freq_normalisation,
  tfidf_normalisation
)
par(mar = c(5, 5, 5, 2))
barplot(
  comp_scores,
  beside = TRUE,
  names.arg = paste0("Q", 1:nrow(quote_dataframe)),
  col = c("red", "blue"),
  main = "Comparison of Summarisation Scores",
  xlab = "Quotation Number",
  ylab = "Normal Score",
  ylim = c(0, 1.2)
  )
legend(
  "topright",
  legend = c("Frequency", "TF-IDF"),
  fill = c("red", "blue"),
  bty = "n",
  cex = 0.8
)