## server.R

# load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures
source('functions/train_ubcf.R')

# define functions
get_user_ratings = function(value_list) {
  dat = data.table(
    MovieID = sapply(strsplit(names(value_list), "_"),
                     function(x)
                       ifelse(length(x) > 1, x[[2]], NA)),
    Rating = unlist(as.character(value_list))
  )
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies,
                  split = "::",
                  fixed = TRUE,
                  useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID,
                          function(x)
                            paste0(small_image_url, x, '.jpg?raw=true'))


rating_data <- load_rating()
max_user = max(rating_data$UserID)
train_rating_matrix = create_rating_matrix(rating_data)
print(dim(train_rating_matrix))

rec_ubcf <- train_ubcf(train_rating_matrix)



# read genre csv
movies_by_genres <- read.csv('data/Top10Genres.csv', header=TRUE)
# print(head(movies_by_genres))
shinyServer(function(input, output, session) {
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(
          width = 2,
          div(style = "text-align:center", img(
            src = movies$image_url[(i - 1) * num_movies + j], height = 150
          )),
          #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
          div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
          div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(
            paste0("select_", movies$MovieID[(i - 1) * num_movies + j]),
            label = "",
            dataStop = 5
          ))
        )) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", {
      # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <-
        "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      userid = max_user + 1
      user_ratings$UserID = userid
      # print(colnames(train_rating_matrix))
      user_sparse <- create_rating_matrix(user_ratings, train_rating_matrix)
      # print(as(user_sparse, 'matrix'))
      predicted <- predict(rec_ubcf, 
        user_sparse, n = 10, type="topNList")

      user_predicted_ids = as.integer(gsub("\\m", "", as(predicted, "list")[[1]]))

      print(user_predicted_ids)
      print(movies$Title[movies$MovieID %in% user_predicted_ids])
      # print((as(predicted, "list"))[[1]])
      user_results = (1:10) / 10
      # user_predicted_ids = 1:10
      recom_results <- data.table(
        Rank = 1:10,
        MovieID = movies$MovieID[user_predicted_ids],
        Title = movies$Title[user_predicted_ids],
        Predicted_rating =  user_results
      )
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(
          width = 2,
          status = "success",
          solidHeader = TRUE,
          title = paste0("Rank ", (i - 1) * num_movies + j),
          
          div(style = "text-align:center",
              a(
                img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150)
              )),
          div(style = "text-align:center; font-size: 100%",
              strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]]))
          
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  
  # TODO: Create eventReactive for System-1, When user select to get Rating Run it
  by_genre <- eventReactive(input$btngenre, {
    withBusyIndicatorServer("btngenre", {
      # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <-
        "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # print(input$genre)
      # print(movies_by_genres$Genres)
      selected_genre = movies_by_genres[movies_by_genres$Genres == input$genre,]
      print(head(selected_genre))
      print(movies$Title[movies$MovieID %in% selected_genre$MovieID])
      print(selected_genre$MovieID)
      user_results = selected_genre$WeightedRating
      user_predicted_ids = selected_genre$MovieID
      recom_results <- data.table(
        Rank = 1:10,
        MovieID = user_predicted_ids,
        Title = movies$Title[movies$MovieID %in% user_predicted_ids],
        Predicted_rating = user_results
      )
      
    }) # still busy
  })
  
  output$resultsgenre <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- by_genre()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(
          width = 2,
          status = "success",
          solidHeader = TRUE,
          title = paste0("Rank ", (i - 1) * num_movies + j),
          
          div(style = "text-align:center",
              a(
                img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150)
              )),
          div(style = "text-align:center; font-size: 100%",
              strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]]))
          
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  
}) # server function
