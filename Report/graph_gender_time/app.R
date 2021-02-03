library(shiny)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)
library(plotly)
library(viridis)

# Load data 
fem_share <- read.csv2("female_inventor_share_USPTO.csv", sep = ",") 
fem_share$female_share_inventors <- as.numeric(as.character(fem_share$female_share_inventors))


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
              id = "fem_share_id", 
              pickerInput(
                   inputId = "fem_share", 
                   label = "Choose a country", 
                   choices = sort(unique(fem_share$inv_ctry)), 
                   selected = c("CH", "US", "AT"), 
                   options = list(
                       # `max-options` = 8,
                       `actions-box` = TRUE, 
                       size = 10,
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
    
  # dat_set <-  reactive({filter(fem_share, inv_ctry %in% input$fem_share)})
  output$fem_share_plot <- renderPlotly({
  p <-  ggplotly(
        ggplot(data = subset(fem_share, inv_ctry %in% input$fem_share), aes(y = inv_ctry, x = female_share_inventors, frame = p_year)) +
          geom_col(position = "dodge2") +
               scale_fill_gradient2(low = "lightgrey", high = "darkred") +
               xlab("Share of female inventors") +
               ylab("") +
               scale_color_viridis(discrete = T, begin = 0, end = 1) +
               theme(plot.background  = element_rect(fill = "#202020"),
                     panel.background = element_rect(fill = "#202020"),
                     panel.grid.minor = element_line(color = "grey50"),
                     panel.grid.major = element_line(color = "grey50"),
                     panel.grid.major.x = element_blank() ,
                     legend.position="none",
                     axis.title = element_text(color = "#fdfafa", size = ifelse(session$clientData$pixelratio > 2, 9, 11)),
                     axis.line = element_line(),
                     axis.text = element_text(size = ifelse(session$clientData$pixelratio > 2, 7, 9), color = "#fdfafa")),
             tooltip = c("label")) %>%
      animation_opts(frame = 1000, redraw = F) %>%
      animation_button(label = "Start", font = list(color = "#fdfafa"), x =  ifelse(session$clientData$pixelratio > 2, -0.25, 0.0), y = 0.1) %>%
      animation_slider(currentvalue = list(visible = FALSE), bordercolor = "#fdfafa", bgcolor = "#fdfafa", x =  ifelse(session$clientData$pixelratio > 2, -0.1, 0), y = -0.06, font = list(color = "#fdfafa"), tickcolor = list(color = "#fdfafa")) %>%
      config(displayModeBar = F) %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  p
  
})
}

# Run the application 
shinyApp(ui = ui, server = server)
