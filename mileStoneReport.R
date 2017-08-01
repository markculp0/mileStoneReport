# ================
# Milestone Report
# ================

library(tm)

# Read in one blogs text file directly
blogsTxt <- readLines("../final/en_US/en_US.blogs.txt")
blogsTxt <- iconv(blogsTxt, to = "latin1")
blogsTxt <- (blogsTxt[!is.na(blogsTxt)])
blogsTxt <-  paste(blogsTxt, collapse = "\n")

# Read in one news text file directly
newsTxt <- readLines("../final/en_US/en_US.news.txt")
newsTxt <- iconv(newsTxt, to = "latin1")
newsTxt <- (newsTxt[!is.na(newsTxt)])
newsTxt <- paste(newsTxt, collapse = "\n")

# Read in one twitter text file directly
tweetsTxt <- readLines("../final/en_US/en_US.twitter.txt")
tweetsTxt <- iconv(tweetsTxt, to = "latin1")
tweetsTxt <- (tweetsTxt[!is.na(tweetsTxt)])
tweetsTxt <- paste(tweetsTxt, collapse = "\n")

# Add text documents to corpus
docs <- VCorpus(VectorSource(c(blogsTxt, newsTxt, tweetsTxt)))



# Sample 40% of the blogs 
# n <- sample(899288,359716,replace=F)

# Create blog subset
# blogss <- blogsTxt[n]

# Add blog subset to corpus
# docBlog <- VCorpus(VectorSource(blogss))  

# =================
# Original approach
# =================

# Set data directory
cname <- file.path("C:/Users/Mark/Documents/code/r/coursera",
    "20170724_capstone/swiftkeyProject/final", "en_US")

# List files in data directory
dir(cname)

# Read in corpus of text documents
docs <- VCorpus(DirSource(cname, encoding = "latin1"))   
rm(cname)

# Summarize corpus of text documents
summary(docs) 

# List # of characters per document
inspect(docs[1:3])

# View metadata of document #1
meta(docs[[1]])

meta(docs[[1]])$author <- "SwiftKey"
meta(docs[[2]])$author <- "SwiftKey"
meta(docs[[3]])$author <- "SwiftKey"
meta(docs[[1]])$description <- "Blog text"
meta(docs[[2]])$description <- "News text"
meta(docs[[3]])$description <- "Twitter text"
meta(docs[[1]])$language <- "English Latin 1"
meta(docs[[2]])$language <- "English Latin 1"
meta(docs[[3]])$language <- "English Latin 1"

# ===============================
# Compare Corpus Document to Text 
# ===============================

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

# ==================
# Compile Stat Table
# ==================

# Get the other two documents:
## The unedited news 
news <- lapply(docs[2],as.character)
news <- unlist(news)

## The unedited tweets
tweets <- lapply(docs[3],as.character)
tweets <- unlist(tweets)

# Create lists of stats:  "Words", 
# "AlphaChars", "AllChars", "Lines"
BlogStat <- list()
NewsStat <- list()
TweetStat <- list()

# Count words in each document 
library(stringr)

# Count words in blogs, news, and tweets 
# blogs
cnt <- lapply(blogs, function(x) str_count(x,"\\W+"))
BlogStat$Words <- do.call(sum, cnt)
# news
cnt <- lapply(news, function(x) str_count(x,"\\W+"))
NewsStat$Words <- do.call(sum, cnt)
# tweets
cnt <- lapply(tweets, function(x) str_count(x,"\\W+"))
TweetStat$Words <- do.call(sum, cnt)

# Count alphabetic characters in blogs, news, and tweets 
## blogs
cnt <- lapply(blogs, function(x) str_count(x,"[a-zA-Z]"))
BlogStat$AlphaChars <-  do.call(sum, cnt)
## news
cnt <- lapply(news, function(x) str_count(x,"[a-zA-Z]"))
NewsStat$AlphaChars <-  do.call(sum, cnt)
## tweets
cnt <- lapply(tweets, function(x) str_count(x,"[a-zA-Z]"))
TweetStat$AlphaChars <-  do.call(sum, cnt)


# Count total characters in blogs, news, and tweets 
## blogs
cnt <- lapply(blogs,nchar)
BlogStat$AllChars <- do.call(sum, cnt)
## news
cnt <- lapply(news,nchar)
NewsStat$AllChars <- do.call(sum, cnt)
## tweets
cnt <- lapply(tweets,nchar)
TweetStat$AllChars <- do.call(sum, cnt)

# Count number of lines in each document
BlogStat$Lines <- length(blogs)
NewsStat$Lines <- length(news)
TweetStat$Lines <- length(tweets)

# Combine stat vectors
stat <- rbind(BlogStat, NewsStat, TweetStat)

# Display with kable 
# library(knitr); fig.align="center"
stat

# nchar(blogs, type = "chars")

# ====================
# Process documents
# ====================

# Remove punctuation
docs <- tm_map(docs,removePunctuation)

# Remove numbers
docs <- tm_map(docs, removeNumbers)  

# Convert characters to lower case
docs <- tm_map(docs, content_transformer(tolower))  

# Remove stop words
docs <- tm_map(docs, removeWords, stopwords("english"))

# Strip white space 
docs <- tm_map(docs, stripWhitespace)

# Stem the document
docs <- tm_map(docs, stemDocument)

# Treat as plain text document
docs <- tm_map(docs, PlainTextDocument)

# Save for fast retrieval
saveRDS(docs, file = "docs.rds")


# ====================
# Re-examine documents
# ====================

# List # of characters per document
inspect(docs[1:3])

# Get the documents again:
## The processed blogs
blogs <- lapply(docs[1],as.character)
blogs <- unlist(blogs)

## The processed news 
news <- lapply(docs[2],as.character)
news <- unlist(news)

## The processed tweets
tweets <- lapply(docs[3],as.character)
tweets <- unlist(tweets)

# Create lists of stats:  "Words", 
# "AlphaChars", "AllChars", "Lines"
BlogStat <- list()
NewsStat <- list()
TweetStat <- list()

# Count words in each document 
library(stringr)

# Count words in blogs, news, and tweets 
# blogs
cnt <- lapply(blogs, function(x) str_count(x,"\\W+"))
BlogStat$Words <- do.call(sum, cnt)
# news
cnt <- lapply(news, function(x) str_count(x,"\\W+"))
NewsStat$Words <- do.call(sum, cnt)
# tweets
cnt <- lapply(tweets, function(x) str_count(x,"\\W+"))
TweetStat$Words <- do.call(sum, cnt)

# Count alphabetic characters in blogs, news, and tweets 
## blogs
cnt <- lapply(blogs, function(x) str_count(x,"[a-zA-Z]"))
BlogStat$AlphaChars <-  do.call(sum, cnt)
## news
cnt <- lapply(news, function(x) str_count(x,"[a-zA-Z]"))
NewsStat$AlphaChars <-  do.call(sum, cnt)
## tweets
cnt <- lapply(tweets, function(x) str_count(x,"[a-zA-Z]"))
TweetStat$AlphaChars <-  do.call(sum, cnt)

# Count total characters in blogs, news, and tweets 
## blogs
cnt <- lapply(blogs,nchar)
BlogStat$AllChars <- do.call(sum, cnt)
## news
cnt <- lapply(news,nchar)
NewsStat$AllChars <- do.call(sum, cnt)
## tweets
cnt <- lapply(tweets,nchar)
TweetStat$AllChars <- do.call(sum, cnt)

# Count number of lines in each document
BlogStat$Lines <- length(blogs)
NewsStat$Lines <- length(news)
TweetStat$Lines <- length(tweets)

# Combine stat vectors
stat <- rbind(BlogStat, NewsStat, TweetStat)

# Display with kable 
# library(knitr); fig.align="center"
stat

# ===========================================
# Create and Examine the Term-Document Matrix
# ===========================================

library(tm)
library(SnowballC)

# Create document term matrix
dtm <- DocumentTermMatrix(docs)

# dtm <- readRDS("dtm.rds")

# View first 10 words in each document
inspect(dtms[1:3, 1:10])

# Find some frequently occurring terms
findFreqTerms(dtms, 75000)

# Find some associates to a frequently
# occurring word
findAssocs(dtm, "love", 0.8)

# Inspect, then remove sparse terms
inspect(removeSparseTerms(dtm, 0.4))
dtms <- removeSparseTerms(dtm, 0.4)

# Sort matrix in descending order 
wordFreq <- sort(colSums(as.matrix(dtms)), decreasing=TRUE)   
head(wordFreq, 10) 

# Create dataframe for ggplot
wordFreqDF <- data.frame(Word=names(wordFreq), Freq=wordFreq)   
head(wordFreqDF)

library(ggplot2)

# Subset to the most frequent words 
wordFreqDF75k <- subset(wordFreqDF, Freq>75000)

# Create word plot
wordPlot <- ggplot(wordFreqDF75k, aes(x = reorder(Word, -Freq), y = Freq)) +
  geom_bar(stat = "identity") + 
  labs(x = "Word", y = "Frequency") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# Display plot
wordPlot  



