#Question 4.1
#Reading the chosen web page "books.toscrape"
library(rvest)

url <- "https://books.toscrape.com/catalogue/page-1.html"
webpage <- read_html(url)

#Now to identify the book listings
books <- html_elements(webpage, ".product_pod")
length(books)

#Creating empty vectors to store information
display_title <- character(length(books))
full_title <- character(length(books))
raw_price <- character(length(books))
availability <- character(length(books))

for (i in 1:length(books)) {
  title_element <- html_element(books[[i]], "h3 a") #Extracting book title
  display_title[i] <- html_text2(title_element)
  
  full_title[i] <- html_attr(title_element, "title") #Extracting complete title
  
  price_element <- html_element(books[[i]], ".price_color") 
  raw_price[i] <- html_text2(price_element) #Extracting price and its currency symbol
  
  stock_element <- html_element(books[[i]], ".availability")
  availability[i] <- html_text(stock_element) #Extracting the available text
}

#Now to create the raw data frame
book_data <- data.frame(
  Display_Title = display_title,
  Full_Title = full_title,
  Raw_Price = raw_price,
  Availability = availability
)
print(book_data)

#Using Regex to identify incomplete titles
book_data$Title_Incomplete <- grepl(
  "\\.\\.\\.$",
  trimws(book_data$Display_Title)
)

#Using Regex to extract numerical text
book_data$Price <- as.numeric(
  gsub("[^0-9.]", "", book_data$Raw_Price)
)

#Cleaning the availability text
book_data$Availability <- trimws(
  book_data$Availability
)
print(book_data)
View (book_data)
str(book_data)
dim(book_data)


#Question 4.2

#Creating a copy of the data
cleaned_data <- book_data

#Cleaning the book titles using the complete titles
cleaned_data$Title <- trimws(cleaned_data$Full_Title)

#Removing any whitespaces from availability
cleaned_data$Availability <- trimws(gsub("[[:space:]]+", " ", cleaned_data$Availability))

#Checking for any missing prices
sum(is.na(cleaned_data$Price))

#If necessary, replacing any missing prices with the median
if (any(is.na(cleaned_data$Price))) {
  cleaned_data$Price[is.na(clean_data$Price)] <- median(cleaned_data$Price, na.rm = TRUE)
}

#Keep only the relevant columns 
cleaned_data <- cleaned_data[, c("Title", "Price", "Availability")]

#Exporting the cleaned data
write.csv(cleaned_data, "cleaned_books.csv", row.names = FALSE)

#Verifying the cleaned data
head(cleaned_data)
View(cleaned_data)
dim(cleaned_data)

#Question 4.3


#Question 4.4
