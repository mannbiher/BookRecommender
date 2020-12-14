library(recommenderlab)
library(Matrix)
# set.seed(7672)

load_rating <- function() {
    myurl = "https://liangfgithub.github.io/MovieData/"
    ratings = read.csv(
        paste0(myurl, 'ratings.dat?raw=true'),
        sep = ':',
        colClasses = c('integer', 'NULL'),
        header = FALSE
    )
    colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
    return(ratings)
    
}


train_ubcf <- function(Rmat) {
    rec_UBCF = Recommender(
        Rmat,
        method = 'UBCF',
        parameter = list(
            normalize = 'Z-score',
            method = 'Cosine',
            nn = 25
        )
    )
}

create_rating_matrix <- function(data) {
    i = paste0('u', data$UserID)
    j = paste0('m', data$MovieID)
    x = data$Rating
    tmp = data.frame(i, j, x, stringsAsFactors = T)
    Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
    rownames(Rmat) = levels(tmp$i)
    colnames(Rmat) = levels(tmp$j)
    Rmat = new('realRatingMatrix', data = Rmat)
    return(Rmat)
}

create_user_rating_matrix <- function(data, base_matrix) {
    i = paste0('u', data$UserID)
    j = paste0('m', data$MovieID)
    x = data$Rating
    tmp = data.frame(i, j, x, stringsAsFactors = T)
    Rmat = sparseMatrix(
        as.integer(tmp$i),
        as.integer(tmp$j),
        x = tmp$x,
        dims = c(1, ncol(base_matrix))
    )
    rownames(Rmat) = levels(tmp$i)
    colnames(Rmat) = colnames(base_matrix)
    Rmat = new('realRatingMatrix', data = Rmat)
    return(Rmat)
}
