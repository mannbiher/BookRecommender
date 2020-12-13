library(recommenderlab)
library(Matrix)
set.seed(7672)

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

create_rating_matrix <- function(data, base_matrix=NULL) {
    i = paste0('u', data$UserID)
    j = paste0('m', data$MovieID)
    x = data$Rating
    tmp = data.frame(i, j, x, stringsAsFactors = T)
    if (is.null(base_matrix)) {
        mat_colnames <- levels(tmp$j)
        mat_dims <- c(max(as.integer(tmp$i)), max(as.integer(tmp$j)))
        # dim_matrix = 
    } else {
        mat_colnames <- colnames(base_matrix)
        mat_dims <- c(1, ncol(base_matrix))
        print(mat_dims)
    }
    
    Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x, dims=mat_dims)
    rownames(Rmat) = levels(tmp$i)
    colnames(Rmat) = mat_colnames
    Rmat = new('realRatingMatrix', data = Rmat)
    return(Rmat)
}
