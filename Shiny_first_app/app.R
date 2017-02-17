#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(southafricastats)

mortality <- mortality_zaf %>% filter(indicator != "All causes")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("South Africa Stats"),
   
   # Sidebar with a input
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "Province",
                    label = "Select a Province...:",
                    choices = mortality_zaf %>% distinct(province),
                    selected = "Gauteng",
                    multiple = T
        ),
        checkboxInput(inputId = "chkShowTable",
                      label = "Show Table:",
                      value = F)
                      
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("LinePlot"),
         dataTableOutput("mortalityTable")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$LinePlot <- renderPlot({
     
     mortality %>%
       filter(province %in% input$Province) %>%
       ggplot(aes(year, deaths, color = indicator)) +
       facet_wrap(~province) +
       geom_line(alpha = 0.8, size = 1.5) +
       theme_minimal(base_size = 18) +
       theme(legend.position = "bottom")
     
     
     
   })
   
   output$mortalityTable <- renderDataTable({
     
     if (input$chkShowTable) {
       DT::datatable( mortality %>%
         filter(province %in% input$Province))    
     }
     
   
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

