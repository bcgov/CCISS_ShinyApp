################################################################################
# Katsavounidis, I., C.-C. Jay Kuo, Z. Zhang, 1994. A new initialization
# technique for generalized Lloyd iteration. Signal Processing Letters,
# 1(10):144-146. doi:10.1109/97.329844
#
# Cannon, A.J., 2015. Selecting GCM Scenarios that Span the Range of Changes
# in a Multimodel Ensemble: Application to CMIP5 Climate Extremes Indices.
# Journal of Climate, 28(3), 1260-1267.
#
# Alex Cannon (alex.cannon@canada.ca)
################################################################################

subset.kkz <-
# Use KKZ algorithm to find n.cases representative rows from x
# Modified so that the first case is closest to the baseline
function(x, n.cases, baseline=rep(0, ncol(x)), newdata=NULL, silent=TRUE)
{
    kdist <- function(x, y, ...) apply((sweep(y, 2, x, '-'))^2, 1, sum)
    xdata <- x
    n.cases <- ifelse(n.cases < 2, 2, n.cases)
    n.cases <- ifelse(n.cases > nrow(x), nrow(x), n.cases)
    xindex <- 1:nrow(x)
    cases.index <- rep(NA, n.cases)
    cases <- matrix(0, ncol=ncol(x), nrow=n.cases)
    if(!silent) cat(1,'')
    norms <- kdist(baseline, x)
    indexN <- which.min(norms)
    norm.index <- 1:nrow(x)
    norm.index <- norm.index[-c(indexN)]
    cases[1,] <- x[indexN,]
    cases.index[1] <- xindex[indexN]
    x <- x[norm.index,,drop=FALSE]
    xindex <- xindex[norm.index]
    if(!silent) cat(2,'')
    norms <- kdist(cases[1,,drop=FALSE], x)
    indexN <- which.max(norms)
    norm.index <- 1:nrow(x)
    norm.index <- norm.index[-c(indexN)]
    cases[2,] <- x[indexN,]
    cases.index[2] <- xindex[indexN]
    x <- x[norm.index,,drop=FALSE]
    xindex <- xindex[norm.index]
    norms <- cbind(norms[-c(indexN)], kdist(cases[2,,drop=FALSE], x))
    if(n.cases > 2){
        for(i in 3:n.cases){
            if(!silent) cat(i,'')
            indexN <- which.max(apply(norms, 1, min))
            norm.index <- 1:nrow(x)
            norm.index <- norm.index[-c(indexN)]
            cases[i,] <- x[indexN,,drop=FALSE]
            cases.index[i] <- xindex[indexN]
            x <- x[norm.index,,drop=FALSE]
            xindex <- xindex[norm.index]
            norms <- norms[-c(indexN),,drop=FALSE]
            norms <- cbind(norms, kdist(cases[i,,drop=FALSE], x))
        }
    }
    if(!silent) cat('\n')
    clusters <- assign.kkz(xdata, cases)
    centroids <- centroid.kkz(xdata, clusters)
    predictions <- centroids[clusters,,drop=FALSE]
    rownames(cases) <- rownames(centroids) <- cases.index
    colnames(cases) <- colnames(centroids) <- colnames(predictions) <-
        colnames(xdata)
    cases.newdata <- centroids.newdata <- predictions.newdata <- NULL
    if(!is.null(newdata)){
        cases.newdata <- newdata[cases.index,,drop=FALSE]
        centroids.newdata <- centroid.kkz(newdata, clusters)
        predictions.newdata <- centroids.newdata[clusters,,drop=FALSE]
        rownames(cases.newdata) <- rownames(centroids.newdata) <- cases.index
        colnames(cases.newdata) <- colnames(centroids.newdata) <-
            colnames(predictions.newdata) <- colnames(newdata)
    }
    list(cases=cases, clusters=clusters, centroids=centroids,
         predictions=predictions, cases.newdata=cases.newdata,
         centroids.newdata=centroids.newdata,
         predictions.newdata=predictions.newdata)
}

assign.kkz <-
# Assign rows of x to the cluster given by the nearest KKZ-selected
# case from cases
function(x, cases)
{
    kdist <- function(x, y, ...) apply((sweep(y, 2, x, '-'))^2, 1, sum)
    clusters <- matrix(rep(NA, nrow(x)))
    for(i in 1:nrow(x))
        clusters[i] <- which.min(kdist(x[i,], cases))
    rownames(clusters) <- rownames(cases)[clusters]
    clusters
}

centroid.kkz <-
# Calculate cluster centroids based on vector of clusters
function(x, clusters)
{
    centroids <- matrix(Inf, ncol=ncol(x), nrow=max(clusters))
    for(i in 1:max(clusters))
        centroids[i,] <- colMeans(x[clusters==i,,drop=FALSE])
    centroids
}

################################################################################
