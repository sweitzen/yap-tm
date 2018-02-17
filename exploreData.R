# exploreData.R
#
# Resources used in creating this code:
# http://rpubs.com/erodriguez/milestone1
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/capstone-ngramComputerCapacity.md
# https://www.coursera.org/learn/data-science-project/discussions/forums/bXKqKZfYEeaRew5BAmrkbw/threads/dW1Z5sKMEeeTyArhA0ZGig

library(data.table)
library(ggplot2)
library(wordcloud)


# Given input data and a title, this function plots a barchart of the 30 most
# frequent entries in the data.
# Input:
#        data - a data.frame or data.table with a character column 'ngram' and
#               a numeric/integer column 'count'
#        title - the title for the barchart
# Output:
#        none (a ggplot2 chart printed to the screen)
makePlot <- function(data, title) {
    
    # Select top 30 data points
    data <- data[1:30]
    
    # Add a column for theoretical Zipf frequency
    maxCount <- data[1, count]
    for (i in 1:30) data[i, zipf:=(maxCount / i)]
    
    ggplot(data=data, aes(reorder(ngram, -count), count, group=1)) +
        geom_bar(stat="identity", fill=I("darkred")) +
        geom_line(data=data, aes(reorder(ngram, -count), zipf), size=2, 
                  color="darkblue") +
        theme(axis.text.x=element_text(angle=45, size=10, hjust=1)) +
        labs(x="Ngrams", y="Count") +
        ggtitle(title)
}


# ==============================================================================

# Load Ngrams
load(paste0("../data/train/dts.rda"))

# Maximum size of Ngrams
Nmax <- 5

# Order Ngrams by count (frequency)
for (j in 1:Nmax) {
    dts[[j]] <- dts[[j]][order(-count)]
}

# Plot barcharts of most common N-grams with theoretical Zipf frequency overlaid
makePlot(dts[[1]], "30 Most Common Unigrams")
makePlot(dts[[2]], "30 Most Common Bigrams")
makePlot(dts[[3]], "30 Most Common Trigrams")
makePlot(dts[[4]], "30 Most Common Quadgrams")
makePlot(dts[[5]], "30 Most Common Pentagrams")

# Try a word cloud
set.seed(222)
wordcloud(
    dts[[1]]$ngram, 
    dts[[1]]$count, 
    scale=c(10,1),
    max.words=100,
    random.order=FALSE,
    rot.per=0.35,
    use.r.layout=FALSE,
    colors=brewer.pal(6, 'Spectral')
)