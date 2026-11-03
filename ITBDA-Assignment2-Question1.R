# MapReduce: Keyword search
# Mapper implementation
# Question 1.2: Mapper Implementation
docs_dir <- "C:/Users/mckhy/Documents/ITBDA/ITBDA-Assignment2/map_reduce_test_docs"

files <- list.files(
  path = docs_dir,
  pattern = "\\.txt$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

# Checking if the files were correctly located
if(length(files) ==0) {
  stop("No text files found. Re-check the folder path.")
}
# And confirming number of files within the folder
cat("Documents found:", length(files), "\n")

#Now that the documents have been found, next is to define the keyword
#Request the keyword dynamically from the user as per the assignment instructions

# Request the keyword dynamically using an RStudio pop-up
keyword <- rstudioapi::showPrompt(
  title = "MapReduce Keyword Search",
  message = "Enter the keyword to search:",
  default = ""
)

# Check whether a valid keyword was entered
if (is.null(keyword) || nchar(trimws(keyword)) == 0) {
  stop("Please enter a valid keyword.")
}

# Remove unnecessary spaces from the keyword
keyword <- trimws(keyword)

# Display the selected keyword
cat("Searching for keyword:", keyword, "\n")

#Now to define the Mapper function
mapper <- function(file_path, keyword) {
  lines <- readLines(file_path, warn = FALSE)
  
  #Creating an empty table for matching positions
  results <- data.frame(
    Document = character(),
    Row = integer(),
    Column = integer()
  )
  
  #Looping through every line in the document
  for (row_num in seq_along(lines)) {
    current_line <- lines[row_num]
    
    #Finding every occurences of the keyword
    positions <- gregexpr(
      pattern = paste0("\\b", keyword, "\\b"),
      text = current_line,
      ignore.case = TRUE,
      perl = TRUE
    ) [[1]]
    
    #Only processing the lines that contain the keyword
    if (positions[1] != -1) {
      for (col_num in positions) {
        #Creating a key value pair
        match_record <- data.frame(
          Document = basename(file_path),
          Row = row_num,
          Column = col_num
        )
        
        #Adding the match to the Mapper output
        results <- rbind(results, match_record)
      }
    }
  }
  
  #Return all the matching positions
  return(results)
}

#Now to apply the Mapper to all documents

mapped_list <- lapply(
  files,
  mapper,
  keyword = keyword
)

mapper_output <- do.call(rbind, mapped_list)

print(mapper_output)

#To display the keyword occuerence
cat(
  "Total keyword occurences:",
  nrow(mapper_output),
  "\n"
)

# Question 1.3:Reducer Implementation

grouped_data <- split(
  mapper_output,
  mapper_output$Document
)

reducer <- function(document_data) {
  document_data <- document_data[
    order(document_data$Row, document_data$Column),
  ]
  locations <- paste0(
    "(",
    document_data$Row,
    ", ",
    document_data$Column,
    ")",
    collapse = ", "
  )
  #Returning the recorded results
  return(data.frame(
    Document = document_data$Document[1],
    Locations = locations,
    Total = nrow(document_data)
  ))
}
#Applying the Reducer to each group
reduced_list <- lapply(grouped_data, reducer)

#Combining the reduced results
reducer_output <- do.call(rbind, reduced_list)

print(reducer_output)
View(reducer_output)
