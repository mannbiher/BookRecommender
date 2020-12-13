## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

genres = c(
  "Action",
  "Adventure",
  "Animation",
  "Children's",
  "Comedy",
  "Crime",
  "Documentary",
  "Drama",
  "Fantasy",
  "Film-Noir",
  "Horror",
  "Musical",
  "Mystery",
  "Romance",
  "Sci-Fi",
  "Thriller",
  "War",
  "Western"
)

shinyUI(dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Movie Recommender"),
  
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Recommend By Genre",
      tabName = "genre",
      icon = icon("dashboard")
    ),
    menuItem(
      "Recommend By Collaboration",
      tabName = "collab",
      icon = icon("th")
    )
  )),
  
  dashboardBody(includeCSS("css/movies.css"),
                tabItems(
                  # First tab content
                  # First tab content
                  tabItem(tabName = "genre",
                          
                          fluidRow(
                            box(
                              width = 12,
                              title = "Step 1: Chose a Genre",
                              status = "info",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              div(selectInput("genre", "Genres:",
                                              genres),
                                  tableOutput("data"))
                            )
                          ),
                          fluidRow(
                            useShinyjs(),
                            box(
                              width = 12,
                              status = "info",
                              solidHeader = TRUE,
                              title = "Step 2: Discover movies you might like",
                              br(),
                              withBusyIndicatorUI(
                                actionButton("btngenre", "Click here to get your recommendations", class = "btn-warning")
                              ),
                              br(),
                              tableOutput("resultsgenre")
                            )
                          )),
                  
                  tabItem(tabName = "collab",
                          fluidRow(
                            box(
                              width = 12,
                              title = "Step 1: Rate as many movies as possible",
                              status = "info",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              div(class = "rateitems",
                                  uiOutput('ratings'))
                            )
                          ),
                          fluidRow(
                            useShinyjs(),
                            box(
                              width = 12,
                              status = "info",
                              solidHeader = TRUE,
                              title = "Step 2: Discover movies you might like",
                              br(),
                              withBusyIndicatorUI(
                                actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                              ),
                              br(),
                              tableOutput("results")
                            )
                          ))
                ))
))