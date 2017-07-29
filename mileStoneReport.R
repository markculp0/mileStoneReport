# ================
# Milestone Report
# ================

library(tm)

# Set data directory
cname <- file.path("C:/Users/Mark/Documents/code/r/coursera",
    "20170724_capstone/swiftkeyProject/final", "en_US")

# List files in data directory
dir(cname)

# Read in corpus of text documents
docs <- VCorpus(DirSource(cname))   

# Summarize corpus of text documents
summary(docs) 

# List # of characters per document
inspect(docs[1:3])

# View metadata of document #1
meta(docs[[1]])

# Read in one text file directly
blogsTxt <- readLines("../final/en_US/en_US.blogs.txt")

# Extract character vector of same file
# from the corpus
blogs <- lapply(docs[1],as.character)
blogs <- unlist(blogs)

# Compare the character vectors; they  
# only differ by rownames
identical(blogsTxt, blogs)
all.equal(blogsTxt, blogs)

# The tm package assigns row names
# to the character vectors 
blogsTxt[2]
blogs[2]

# Ok, comparison done.  We'll work with
# the documents pulled from the corpus
rm(blogsTxt)

# Get the other two documents:
# The unedited news 
news <- lapply(docs[2],as.character)
news <- unlist(news)

# The unedited tweets
tweets <- lapply(docs[3],as.character)
tweets <- unlist(tweets)

# Create lists of stats
BlogStat <- list("Words", "AlphaChars", "AllChars", "Lines")
NewsStat <- list("Words", "AlphaChars", "AllChars", "Lines")
TweetStat <- list("Words", "AlphaChars", "AllChars", "Lines")

# Count words in each document 
library(stringr)

# Count words in blogs 
cnt <- lapply(blogs, function(x) str_count(x,"\\W+"))
BlogStat$Words <- do.call(sum, cnt)

# Count alphabetic characters
cnt <- lapply(blogs, function(x) str_count(x,"[a-zA-Z]"))
BlogStat$AlphaChars <-  do.call(sum, cnt)

# Count total characters in each document
cnt <- lapply(blogs,nchar)
BlogStat$AllChars <- do.call(sum, cnt)

# Count number of lines per document
BlogStat$Lines <- length(blogs)
length(news)
length(tweets)

# nchar(blogs, type = "chars")




# ====================
# Preprocess documents
# ====================

# Remove punctuation
docs <- tm_map(docs,removePunctuation)

# Remove numbers
docs <- tm_map(docs, removeNumbers)  

# Convert characters to lower case
docs <- tm_map(docs, toLower)  

# Remove stop words
docs <- tm_map(docs, removeWords, stopwords("english"))

# Stem the document
docs <- tm_map(docs, stemDocument)

# Strip white space 
docs <- tm_map(docs, stripWhitespace)

# Treat as plain text document
docs <- tm_map(docs, PlainTextDocument)


# ====================
# Re-examine documents
# ====================


# List # of characters per document
inspect(docs[1:3])


lapply(docs[1], as.character)


# Create document term matrix
dtm <- DocumentTermMatrix(docs)






