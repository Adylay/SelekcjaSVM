library(e1071)

SVMRFE <- function(x, y, CRITERIA) {
    RANK <- data.frame(c(1:(length(CRITERIA)-1)))
    p <- length(CRITERIA)
    while (p >= 1) {
        model <- svm(x, y, kernel = "linear", scale=FALSE)
        rank_criteria <- model[["decision.values"]]^2
        min_index <- which.min(rank_criteria)

        RANK[p, 1] <- min_index
        RANK[p, 2] <- CRITERIA[min_index]
        
        x <- x[,-c(min_index)]
        p <- p - 1
    }
    return (RANK)
}

FScoreSelection <- function(x, y, CRITERIA) {
    fscores <- c()
    bestFscores <- c()
    bestIndices <- c()
    for(col in 1:ncol(x)) {
        x1 = x[which(y == 1), col]
        x0 = x[which(y == -1), col]
        fscore = abs(mean(x0) - mean(x1))/sqrt(var(x0) + var(x1))
        fscores <- c(fscores, fscore)
        if (fscore > 2.0) {         # potem to bedzie lepiej rozwiazane
            bestFscores <- c(bestFscores, fscore)
            bestIndices <- c(bestIndices, col)
        }
    }
    # print(length(CRITERIA))
    # print(length(fscores))
    # df <- data.frame(CRITERIA, fscores)
    # print(order(fscores))
    hist(fscores)
    hist(bestFscores)
    return (CRITERIA[bestIndices])
}
