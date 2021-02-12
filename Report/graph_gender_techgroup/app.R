library(shiny)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)
library(plotly)
library(viridis)

# Load data 
fem_share_tech <- read.csv2("female_inventors_graduates_techgroup_USPTO.csv", sep = ",") 
fem_share_tech$female_share_inventors <- round(as.numeric(as.character(fem_share_tech$female_share_inventors)), 4)
fem_share_tech$female_share_graduates <- round(as.numeric(as.character(fem_share_tech$female_share_graduates)), 4)


# Define UI 
ui <- fluidPage(
    # htmltools::htmlDependency("jquery", "3.5.1",
    #                           src = c(href = "https://code.jquery.com/"),
    #                           script = "jquery-3.5.1.min.js"),
    # tags$style(type="text/css",
    #            ".shiny-output-error { visibility: hidden; }",
    #            ".shiny-output-error:before { visibility: hidden; }"),

    
   # Choose organization and model
   fluidRow(
              column = 12,
              id = "fem_share_tech_id", 
              pickerInput(
                   inputId = "fem_share_tech", 
                   # label = "Choose a country", 
                   choices = sort(unique(fem_share_tech$tech_group)), 
                   selected = c("Overall"), 
                   options = list(
                       `max-options` = 1,
                       `actions-box` = TRUE, 
                       `size` = 10,
                       `selected-text-format` = "count > 3",
                       `count-selected-text` = "Tech Field",
                       `deselect-all-text` = "Deselect all",
                       `select-all-text` = "Select all",
                       `none-selected-text` = 'No Tech Field selected'), 
                   multiple = TRUE),
    # create plot
   mainPanel( 
   plotlyOutput("fem_share_tech_plot"))),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
  
)




# Define server 
server <- function(input, output, session) {
  
  dat_set_tech <-  reactive({filter(fem_share_tech, tech_group %in% input$fem_share_tech)})
  
  output$fem_share_tech_plot <- renderPlotly({
  if(nrow(dat_set_tech()) != 0){

  p <-   dat_set_tech() %>% 
    plot_ly(
    x = ~female_share_graduates, 
    y = ~female_share_inventors,
    type = 'scatter',
    mode = 'markers',
    text = ~country,
    hoverinfo = "markers",
    hovertemplate = paste0('Country: %{text}\n',
                         'Female Inventor Share: %{y}\n',
                         'Female Graduate Share: %{x}<extra></extra>'),
    textposition = "top center",
    marker =  list(color= c('#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                                 'red',
                                 '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f',
                                 '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f'))) %>% 
    add_annotations(text = ~inv_ctry,
                    showarrow = F,
                    yanchor = "bottom") %>%
    config(displayModeBar = F) %>% layout(yaxis = list(range = c(min(dat_set_tech()$female_share_inventors) - 0.01, max(dat_set_tech()$female_share_inventors) + 0.015), title = "<b>Female Inventor Share\n(2005-2015 Average)</b>", fixedrange = TRUE, tickformat = ',.1%'), 
                                          xaxis = list(range = c(min(dat_set_tech()$female_share_graduates) - 0.01, max(dat_set_tech()$female_share_graduates) + 0.015), title = "<b>Female Graduate Share in STEM Fields\n(2005-2015 Average)</b>", fixedrange = TRUE, tickformat = ',.1%'), showlegend = F) %>%
    add_segments(x = mean(dat_set_tech()$female_share_graduates), xend = mean(dat_set_tech()$female_share_graduates), y = min(dat_set_tech()$female_share_inventors) - 0.02, yend = max(dat_set_tech()$female_share_inventors) + 0.02, color = I("black"), line = list(dash = "dash", width = 0.1)) %>%
    add_segments(y = mean(dat_set_tech()$female_share_inventors), yend = mean(dat_set_tech()$female_share_inventors), x = min(dat_set_tech()$female_share_graduates) - 0.02, xend = max(dat_set_tech()$female_share_graduates) + 0.02, color = I("black"), line = list(dash = "dash", width = 0.1)) 
    

    p  
} else {} 
})  
}    

# Run the application 
shinyApp(ui = ui, server = server)


