################################################################################
# exploreData.R
#
# Resources used in creating this code:
# http://rpubs.com/erodriguez/milestone1
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/capstone-ngramComputerCapacity.md
# https://www.coursera.org/learn/data-science-project/discussions/forums/bXKqKZfYEeaRew5BAmrkbw/threads/dW1Z5sKMEeeTyArhA0ZGig

library(data.table)
library(ggplot2)
library(wordcloud)


################################################################################
# Given input data and a title, this function plots a barchart of the 30 most
# frequent entries in the data, and a line of the theoretical Zipf distribution.
# Input:
#    data
#        a data.frame or data.table with a numeric/integer column 'count' and
#        character columns 'X' and 'y' which combine to produce the n-gram
#    title
#        the title for the barchart
# Output:
#        none (a ggplot2 chart printed to the screen)
makePlot <- function(data, title) {
    
    # total_count will be used to rescale count as frequency
    total_count <- sum(data$count)
    # max_freq will be used to generate theoretical Zipf frequency
    max_freq <- data[1, count] / total_count
    
    # Select top 30 data points
    data <- data[1:30]
    
    # Add columns for ngram, frequency and theoretical Zipf frequency
    for (i in 1:30) data[i, ':=' (
        ngram = gsub("_", " ", trimws(paste0(X, " ", y))),
        frequency = (count / total_count),
        zipf = max_freq / i
    )]
    
    ggplot(data=data) +
        geom_bar(
            mapping=aes(reorder(ngram, frequency), frequency, group=1),
            stat="identity",
            fill=I("darkred")
        ) +
        geom_line(
            mapping=aes(reorder(ngram, frequency), zipf, group=2),
            size=2,
            color="darkblue"
        ) +
        labs(
            x="Ngrams",
            y="Frequency"
        ) +
        ggtitle(title) +
        coord_flip()
}

# ==============================================================================

# Load Ngrams
load("../data/train/dts_pruned_8.rda")

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

n <- 1

wordcloud(
    trimws(paste0(dts[[n]]$X, " ", dts[[n]]$y)),
    dts[[n]]$count, 
    scale=c(10,1),
    max.words=100,
    random.order=FALSE,
    rot.per=0.35,
    use.r.layout=FALSE,
    colors=brewer.pal(6, 'Spectral')
)