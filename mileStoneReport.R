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


