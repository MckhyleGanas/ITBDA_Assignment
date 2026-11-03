#Question 2.1)
docs_dir <- "C:/Users/mckhy/Documents/ITBDA/ITBDA-Assignment2/sa_news"

files <- file.path(
  docs_dir,
  paste0("news_", 1:10, ".txt")
)

#Making sure that all the 10 files actually exist
if (!all(file.exists(files))) {
  stop("One or more news text files were not able to be found")
}

cat("Documents found:", length(files), "\n")

#Now for creating the dataframe
text_data <- data.frame(
  File_Name = basename(files),
  Content = rep("", length(files))
)

#Now for reading and denosing each content of the news files
for (i in 1:length(files)) {
  lines <- readLines(files[i], warn = FALSE) #Reading the documents contents
  
  content <- paste(lines, collapse = " ") #A text string combining all lines into one
  
  content <- tolower(content) #Text to lowercase
  
  content <- gsub("[[:punct:]]", " ", content) #Removal of punctuation
  
  content <- gsub("[[:space:]]+", " ", content) #White space removal
  
  content <- trimws(content) #Removal of any leading or trailing white spaces
  
  #Storing the cleaned content into the data frame
  text_data$Content[i] <- content
}

print(text_data)
View(text_data)

#Finally, a verification of the dataframe dimensions
cat("Total Documents:", nrow(text_data), "\n")
cat("Total Columns:", nrow(text_data), "\n")

#Question 2.2)
library(tm)
library(topicmodels)

n_topics <- 3 
top_k <- 5 #I have chosen to set the number of topic and words as 3 and 5

corpus <- Corpus(VectorSource(text_data$Content))

corpus <- tm_map(
  corpus,
  removeWords,
  stopwords("english")
)

dtm <-DocumentTermMatrix(corpus)
#Checking that the document has usable words
word_counts <- rowSums(as.matrix(dtm))

if (any(word_counts == 0)) {
  stop("One or more news text files have no words after cleaning")
}

#Now to create the LDA model
set.seed(123)

lda <- LDA(dtm, k = n_topics, control = list(seed = 123)
           )
#Now to get the word and topic probabilities
lda_results <- posterior(lda)

word_probability <- lda_results$terms
topic_probability <- lda_results$topics
top_words <- data.frame(
  Topic = integer(),
  Word = character(),
  Probability = numeric()
)

for (i in 1:n_topics) {
  
  #Sorting the words from highest to lowest probability
  positions <- order(word_probability[i, ], decreasing = TRUE)
  
  positions <- head(positions, top_k)
  
  #Adding the words and probabilities to the results
  for (j in positions) {
    top_words <- rbind(
      top_words,
      data.frame(
        Topic = i,
        Word = colnames(word_probability)[j],
        Probability = round(word_probability[i, j], 4) #Rounding off 4 decimal places
      )
    )
  }
}

#Now for displaying all the results
print(top_words)
View(top_words)

document_topics <- data.frame(
  File_Name = text_data$File_Name,
  round(topic_probability, 4)
)

colnames(document_topics) <- c(
  "File_Name",
  paste0("Topic_", 1:n_topics)
)

print(document_topics)
View(document_topics)

#Question 2.3)
#Bar Graph Plot

for (i in 1:n_topics){
  topic_data <- top_words[top_words$Topic == i, ]
  
  barplot(
    topic_data$Probability,
    names.arg = topic_data$Word,
    main = paste("Top Words in Topic", i),
    xlab = "Words",
    ylab = "Probability",
    las = 2,
    cex.names = 0.7
  )
}

par(mfrow = c(1, 1))

#Heat Map
words <- unique(top_words$Word)

heatmap_data <- word_probability[, words]

par(mar =c(8, 6, 4, 2))

heatmap(
  heatmap_data,
  Rowv = NA,
  Colv = NA,
  scale = "none",
  margins = c(10, 6),
  cexCol = 0.9,
  cexRow = 0.9,
  xlab = "Keywords",
  ylab = "Topics",
  main = "Keyword Probabilities Across Topics",
  labRow = paste("Topic", 1:n_topics),
)
par(mar = c(5, 4, 4, 2) + 0.1)