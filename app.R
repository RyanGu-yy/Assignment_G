library(shiny)
library(tidyverse)
library(readr)

prizes <- read_csv("prizes.csv")

ui <- navbarPage(
  "Nobel Prize Explorer (TidyTuesday 2025-10-28)",
  tabPanel("Overview",
           sidebarLayout(
             sidebarPanel(
               selectInput("genre_filter", "Prize Genre:",
                           choices = c("All", sort(unique(prizes$prize_genre))), 
                           selected = "All"),
               selectInput("name_filter", "Prize Name:",
                           choices = c("All", sort(unique(prizes$prize_name))), 
                           selected = "All")
             ),
             mainPanel(
               h3("Dataset Summary"),
               tableOutput("summary_tbl"),
               h3("Counts by Prize Genre"),
               plotOutput("genre_plot")
             )
           )
  ),
  tabPanel("Diversity Explorer",
           sidebarLayout(
             sidebarPanel(
               selectInput("div_genre", "Select Prize Genre:",
                           choices = sort(unique(prizes$prize_genre)))
             ),
             mainPanel(
               h3("Ethnicity Distribution by Prize Genre"),
               plotOutput("ethnicity_plot")
             )
           )
  ),
  tabPanel("Academic Pathways",
           mainPanel(
             h3("Top 20 Degree Institutions"),
             plotOutput("inst_bar"),
             h3("Degrees Over Years"),
             plotOutput("inst_year")
           )
  ),
  tabPanel("Residence & Roles",
           mainPanel(
             h3("Roles by UK Residence"),
             plotOutput("role_plot"),
             h3("Cross Tab (UK × Role × Ethnicity)"),
             tableOutput("role_table")
           )
  )
)

server <- function(input, output, session) {
  output$summary_tbl <- renderTable({
    data <- prizes
    
    if (input$genre_filter != "All") {
      data <- data %>% filter(prize_genre == input$genre_filter)
    }
    if (input$name_filter != "All") {
      data <- data %>% filter(prize_name == input$name_filter)
    }
    summary(data)
  })
  
  output$genre_plot <- renderPlot({
    prizes %>%
      count(prize_genre) %>%
      ggplot(aes(x = reorder(prize_genre, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(title = "Counts by Prize Genre",
           x = "Prize Genre",
           y = "Count") +
      theme_minimal()
  })
  
  output$ethnicity_plot <- renderPlot({
    prizes %>%
      filter(prize_genre == input$div_genre) %>%
      count(ethnicity_macro) %>%
      ggplot(aes(x = reorder(ethnicity_macro, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(title = paste("Ethnicity Distribution in", input$div_genre),
           x = "Ethnicity Macro",
           y = "Count") +
      theme_minimal()
  })
  
  output$inst_bar <- renderPlot({
    prizes %>%
      count(degree_institution) %>%
      arrange(desc(n)) %>%
      slice(1:20) %>%
      ggplot(aes(reorder(degree_institution, n), n)) +
      geom_col() +
      coord_flip() +
      labs(title = "Top 20 Degree Institutions",
           x = "Institution",
           y = "Count") +
      theme_minimal()
  })
  
  output$inst_year <- renderPlot({
    prizes %>%
      filter(!is.na(degree_institution)) %>%
      count(year) %>%
      ggplot(aes(year, n)) +
      geom_line() +
      geom_point() +
      labs(title = "Counts of Degrees Over Years",
           x = "Year",
           y = "Count") +
      theme_minimal()
  })
  
  output$role_plot <- renderPlot({
    prizes %>%
      count(uk_residence, person_role) %>%
      ggplot(aes(x = person_role, y = n, fill = uk_residence)) +
      geom_col(position = "dodge") +
      labs(title = "Roles by UK Residence",
           x = "Role",
           y = "Count") +
      theme_minimal()
  })
  
  output$role_table <- renderTable({
    prizes %>% count(uk_residence, person_role, ethnicity_macro)
  })
}

shinyApp(ui, server)