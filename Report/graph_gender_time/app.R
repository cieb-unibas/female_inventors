library(shiny)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)
library(plotly)
library(viridis)

# Load data 
fem_share <- read.csv2("female_inventor_share_USPTO.csv", sep = ",") 
# fem_share$female_share_inventors <- round(as.numeric(as.character(fem_share$female_share_inventors)), 4)
fem_share$info <- ifelse(fem_share$female_share_inventors == 0, "not enough\nobservations", " ")
fem_share <- subset(fem_share, p_year < 2019)

# Define UI 
ui <- fluidPage(
    htmltools::htmlDependency("jquery", "3.5.1",
                              src = c(href = "https://code.jquery.com/"),
                              script = "jquery-3.5.1.min.js"),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    
   # Choose organization and model
   fluidRow(
              column = 12,
              id = "fem_share_id", 
              pickerInput(
                   inputId = "fem_share", 
                   # label = "Choose a country", 
                   choices = sort(unique(fem_share$country)), 
                   selected = c("Germany", "Austria", "Switzerland", "United States", "Japan", "France"), 
                   options = list(
                       `max-options` = 8,
                       `actions-box` = TRUE, 
                       `size` = 10,
                       `selected-text-format` = "count > 3",
                       `count-selected-text` = "Country",
                       `deselect-all-text` = "Deselect all",
                       `select-all-text` = "Select all",
                       `none-selected-text` = 'No country selected'), 
                   multiple = TRUE),
    # create plot
   mainPanel( 
   plotlyOutput("fem_share_plot"))),
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
  
  dat_set <-  reactive({subset(fem_share, country %in% input$fem_share & p_year < 2019)})
  
  output$fem_share_plot <- renderPlotly({
  if(nrow(dat_set()) != 0){

  p <-   dat_set() %>% 
    plot_ly(
    x = ~country, 
    y = ~female_share_inventors,
    frame = ~p_year,
    hoverinfo = 'y',
    type = 'bar',
    text = ~info,
    color = ~female_share_inventors,
    colors = colorRampPalette(c("#e5f5f9", "#2ca25f"))(length(unique(dat_set()$female_share_inventors)))
    ) %>% 
    add_text(textposition = "top") %>%
    config(displayModeBar = F) %>% 
    layout(yaxis = list(title = "<b>Female Inventor Share</b>", fixedrange = TRUE, tickformat = ',.1%'), xaxis = list(title = "", fixedrange = TRUE), showlegend = F) %>%
    animation_opts(frame = 800, redraw = F) %>%
    animation_slider(currentvalue = list(prefix = "Year ", font = list(color = "#7f7f7f")), bgcolor = "white", x =  ifelse(session$clientData$pixelratio > 2, -0.1, 0), y = -0.06,  
                     tickcolor = list(color = "#7f7f7f")) %>%
    hide_colorbar() %>%
    animation_button(label = "<b>Start</b>", x =  ifelse(session$clientData$pixelratio > 2, 0, 0.0), y = 0.05)
    p  
} else {} 
})  
}    


# Run the application 
shinyApp(ui = ui, server = server)


