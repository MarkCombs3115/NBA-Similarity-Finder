library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(FNN)

NBA_careers <- read.csv("https://raw.githubusercontent.com/MarkCombs3115/NBA-Similarity-Finder/refs/heads/main/NBA_careers.csv")

ui <- fluidPage(
  titlePanel("NBA Player Similarity Finder"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("player", "Enter NBA Player Name:", choices = NBA_careers$Player, options = list(create = FALSE)),
      selectInput("stats", "Select Stats to Compare:", 
                  choices = c("PTS", "AST", "TRB", "STL", "BLK", "TOV", "PF", "FG.", "X3P."),
                  selected = c("PTS", "AST", "TRB"),
                  multiple = TRUE),
      actionButton("find", "Find Similar Players")
    ),
    mainPanel(
      DTOutput("similar_players"),
      plotOutput("similarity_plot")
    )
  )
)


server <- function(input, output) {
  find_similar_players <- eventReactive(input$find, {
    req(input$player, input$stats)
    
    # Filter for selected player
    player_row <- NBA_careers %>% filter(Player == input$player)
    if (nrow(player_row) == 0) return(NULL)
    
    # Select only numeric columns for comparison
    stats_df <- NBA_careers %>% select(Player, all_of(input$stats)) %>% na.omit()
    stats_matrix <- stats_df %>% select(-Player) %>% scale()
    
    # Find the nearest neighbors
    player_index <- which(stats_df$Player == input$player)
    if (length(player_index) == 0) return(NULL)
    
    distances <- get.knnx(stats_matrix, stats_matrix[player_index, , drop = FALSE], k = 6)$nn.index
    similar_players <- stats_df[distances, ]
    
    return(similar_players)
  })
  
  output$similar_players <- renderDT({
    req(find_similar_players())
    datatable(find_similar_players(), options = list(pageLength = 5))
  })
  
  output$similarity_plot <- renderPlot({
    req(find_similar_players())
    
    data <- find_similar_players()
    selected_player <- NBA_careers %>% filter(Player == input$player) %>% select(Player, all_of(input$stats))
    
    ggplot(data, aes_string(x = input$stats[1], y = input$stats[2])) +
      geom_point(aes(color = Player, label = Player), size = 4) +
      geom_text(aes(label = Player), vjust = -1, hjust = 1) +
      geom_point(data = selected_player, aes_string(x = input$stats[1], y = input$stats[2]), color = "red", size = 5, shape = 17) +
      theme_minimal() +
      labs(title = "Similarity Plot", x = input$stats[1], y = input$stats[2])
  })
}


shinyApp(ui = ui, server = server)