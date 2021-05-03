library(shiny)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)
library(plotly)
library(viridis)
library(ggrepel)
library(countrycode)
# Load data 
fem_share_tech <- read.csv2("female_inventors_graduates_techgroup_USPTO.csv", sep = ",") 
fem_share_tech$female_share_inventors <- as.numeric(as.character(fem_share_tech$female_share_inventors))
fem_share_tech$female_share_graduates <- as.numeric(as.character(fem_share_tech$female_share_graduates))
# fem_share_tech <- mutate(fem_share_tech, east = ifelse(inv_ctry %in% c("RU", "CZ", "EE", "HU", "PL", "SI", "SK", "LT", "LV"), "east", "west"))
fem_share_tech <- filter(fem_share_tech, !(inv_ctry %in% c("PT", "KR", "CR", "LT", "RU")))
# fem_share_tech <- filter(fem_share_tech, !(inv_ctry %in% c("RU")))

## Daten für NZZ-Artikel
fem_share_tech_overall <- filter(fem_share_tech, tech_group == "Overall")
fem_share_tech_overall <- mutate(fem_share_tech_overall, Land = countrycode(inv_ctry, "iso2c", "country.name.de"))
fem_share_tech_overall <- dplyr::select(fem_share_tech_overall, Land, female_share_graduates, female_share_inventors, inv_ctry)
fem_share_tech_overall <- dplyr::rename(fem_share_tech_overall, Frauenanteil_MINT = female_share_graduates, Frauenanteil_Patenterfinder = female_share_inventors, ISO_Code = inv_ctry)
fem_share_tech_overall %>% write.table("C:/Users/christian rutzer/Dropbox/CIEB/Projekte/Innovation/Analysis/Female_Inventors/Zeitungsartikel/Daten_plot.csv", dec = ".", sep = ";", row.names = FALSE)


summary(lm(female_share_inventors ~ female_share_graduates, data = fem_share_tech))

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
  # fit <- lm(female_share_inventors ~ female_share_graduates, data = dat_set_tech())
    
  p <-   ggplotly(
      ggplot(data = filter(dat_set_tech()), aes(x = female_share_graduates, y = female_share_inventors)) +
        geom_point() +
        geom_text(aes(x = female_share_graduates, y = female_share_inventors, label = inv_ctry, colour = 
                        ifelse(inv_ctry == "CH", "red", "green")),  position = position_nudge(y = -0.005)) +
        geom_smooth(method="lm",fullrange = TRUE,formula =y~x, se = FALSE)  +
        xlab("Frauenanteil MINT-Abschlüsse") +
        ylab("Frauenanteil Patenterfindungen") +
        # geom_vline(xintercept = 2015, linetype="dotted") +
        theme(axis.title = element_text(face="bold",size = 10),
              legend.title = element_blank()) +
    scale_y_continuous(limits = c(0, 0.25)) +
    scale_x_continuous(limits = c(0.25, 0.5))  
      ) %>% layout(width = 500, height = 500)
  
  

    p  
} else {} 
})  
}    

# Run the application 
shinyApp(ui = ui, server = server)


