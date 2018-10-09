
library("tidyverse")
library("rlang")
library("shiny")
library("DT")

paths_funcs <-
  list.files(
    # path = "C:/Users/aelhabr/Documents/projects/sports-predict",
    path = "R",
    pattern = "func",
    recursive = FALSE,
    full.names = TRUE
  )
paths_funcs_srcd <-
  invisible(sapply(paths_funcs, source))

conn <- get_db_conn()
data <-
  read_from_db(
    conn = conn,
    table = "odds_tr"
  ) %>% 
  .convert_date_cols_at() %>% 
  .convert_timestamp_cols_at()
conn %>% drop_db_conn()

ui <- fluidPage(
  
  titlePanel("NFL Game Odds"),
  
  sidebarLayout(
    sidebarPanel(
      #   sliderInput("season_input",
      #               "Filter for season:",
      #               # This generates resolution weirdness with slider labels.
      #               # min = yrs[1],
      #               # max = yrs[length(yrs)],
      #               # value = c(yrs[1], yrs[length(yrs)]),
      #               min = 2012,
      #               max = 2018,
      #               value = 2018,
      #               sep = ""),
    ),
    
    mainPanel(
      # plotOutput("plot_output"),
      br(), br(),
      DT::dataTableOutput("table_output")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data_filt <- reactive({
    # if (is.null(input$season_input)) {
    #   return(NULL)
    # }
    
    # output$plot_output <- renderPlot({
    #   if (is.null(data_filt())) {
    #     return()
    #   }
    #   ggplot(data_filt())
    # })
    
    # data %>%
    #   filter(season >= input$season_input[1])
    data
  })
  
  output$table_output <- DT::renderDataTable({
    data_filt()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

