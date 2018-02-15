library(ggplot2)
library(dplyr)
library(qdap)
library(RWeka)
library(tm)
library(wordcloud)


# Set seed for reproducability
set.seed(222)

getData <- function(file_list) {
    dat <- NULL
    for(f in file_list) {
        txt <- readLines(con=f, encoding="UTF-8", skipNul=TRUE)
        
        # Concatenate data
        dat <- c(dat, txt)
    }
    
    # Shuffle data
    dat <- dat[sample(seq(length(dat)))]
    
    # Split data into train and train sets
    smp_size <- floor(0.8 * length(dat))
    
    inTrain <- sample(seq_len(length(dat)), size=smp_size)
    
    train <- dat[inTrain]
    test <- dat[-inTrain]
    
    return(list(train=train, test=test))
}

qdap_clean <- function(x) {
    x <- replace_abbreviation(x)
    x <- replace_ordinal(x)
    x <- replace_symbol(x)
    x <- tolower(x)
    
    return(x)
}

tm_clean <- function(corpus){
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    
    return(corpus)
}

wordFreq <- function(tdm) {
    freq <- colSums(as.matrix(tdm))
    word_freq <- data.frame(word=names(freq), freq=freq)
    
    word_freq$word <- as.character(word_freq$word)
    rownames(word_freq) <- c()
    
    return(word_freq)
}

getIdx <- function(max_idx, n) {
    idx <- seq(0, max_idx, by=floor(10000 / n))
    idx[length(idx)] <- max_idx
    
    return(idx)
}

getNGram <- function(charVec, n) {
    # Define bigram tokenizer
    tokenizer <- function(x)
        NGramTokenizer(x, Weka_control(min=n, max=n))
    
    word_freq <- NULL
    idx <- getIdx(length(charVec), n)
    for(i in 1:(length(idx)-1)) {
        # Chunk data
        dat <- charVec[(idx[i]+1):idx[i+1]]
        
        # Clean chunk and transform into corpus
        dat <- qdap_clean(dat)
        corpus <- VCorpus(VectorSource(dat))
        corpus <- tm_clean(corpus)
        
        # Make an ngram TDM
        dtm <- 
            DocumentTermMatrix(
                corpus,
                control=list(tokenize=tokenizer)
            )
        
        #dtm <- removeSparseTerms(dtm, sparse=0.9999)
        
        # Create word-frequency dataframe
        dtm_m <- as.matrix(dtm)
        
        word_freq <- rbind(word_freq, wordFreq(dtm_m))
        word_freq <- word_freq[order(word_freq$freq, decreasing=TRUE), ]
        word_freq <- aggregate(freq ~ word, word_freq, sum)
    
        # Remove unneeded object to reclaim memory
        rm(list=c("dat", "corpus", "dtm", "dtm_m"))
    }
    
    return(word_freq[order(word_freq$freq, decreasing=TRUE), ])
}

makePlot <- function(data, title) {
    data[1:30, ] %>%
        ggplot(aes(reorder(word, -freq), freq)) +
        geom_bar(stat="identity", fill=I("darkred")) +
        theme(axis.text.x=element_text(angle=45, size=10, hjust=1)) +
        labs(x="Ngrams", y="Count") +
        ggtitle(title)
}

# Main code ====================================================================
if(file.exists("../data/dat.rda")) {
    load("../data/dat.rda")
} else {
    file_list <- c(
        "../final/en_US/en_US.blogs.txt",
        "../final/en_US/en_US.news.txt",
        "../final/en_US/en_US.twitter.txt"
    )
    
    dat <- getData(file_list)
    save(dat, file="../data/dat.rda")
}

if(file.exists("../data/unigrams.rda")) {
    load("../data/unigrams.rda")
} else {
    unigrams <- getNGram(dat$train, 1)
    save(unigrams, file="../data/unigrams.rda")
}

if(file.exists("../data/bigrams.rda")) {
    load("../data/bigrams.rda")
} else {
    bigrams <- getNGram(dat$train, 2)
    save(bigrams, file="../data/bigrams.rda")
}

if(file.exists("../data/trigrams.rda")) {
    load("../data/trigrams.rda")
} else {
    trigrams <- getNGram(dat$train, 3)
    save(trigrams, file="../data/trigrams.rda")
}

if(file.exists("../data/quadgrams.rda")) {
    load("../data/quadgrams.rda")
} else {
    quadgrams <- getNGram(dat$train, 4)
    save(quadgrams, file="../data/quadgrams.rda")
}

makePlot(unigrams, "30 Most Common Unigrams")
makePlot(bigrams, "30 Most Common Bigrams")
makePlot(trigrams, "30 Most Common Trigrams")
makePlot(quadgrams, "30 Most Common Quadgrams")

# Plot a wordcloud from bigrams
wordcloud(
    bigrams$word, 
    bigrams$freq, 
    max.words=50, 
    colors=c("grey80", "darkgoldenrod1", "tomato")
)

# Where is "cat"?
match("cat", unigrams$word)