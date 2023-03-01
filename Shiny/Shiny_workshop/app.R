

pacman::p_load(shiny, sf, tidyverse, tmap, DT, bslib)

sgpools <- read_csv("../Data/aspatial/SGPools_svy21.csv")
sgpools_sf <- st_as_sf(sgpools,coords = c("XCOORD","YCOORD"),
                       crs =3414)


ui <- fluidPage(
  titlePanel("Interactive Proportional Symbol Map"),
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "type",
                  label = "Branch or Outlet?",
                  choices = c("branch" = "Branch",
                              "outlet" = "Outlet"),
                  selected = "Branch",
                  multiple = TRUE),
      sliderInput(inputId = "winning",
                  label = "Number of wins",
                  min = 5,
                  max = 82,
                  value = 20),
      checkboxInput(inputId = "showData",
                    label = "Show data table",
                    value = TRUE)
    ),
    mainPanel(
      tmapOutput("mapPlot"),
      DT::dataTableOutput(outputId = "aTable")
    )
  )
)

#ui <- fluidPage(
 # titlePanel("Choropleth Mappign System "),
 # sidebarLayout(
    
  #  sidebarPanel(
   #   selectInput(inputId = "variable",
   #               label = "Mapping Variable",
     #             choices = c("Young" = "YOUNG",
      #                        "Economy Active" = "ECONOMY ACTIVE"),
      #                        "Aged" = "AGED",
                  
    #              selected = "pretty",
   #               multiple = TRUE),
  #    sliderInput(inputId = "winning",
 #                 label = "Number of wins",
  #                min = 5,
   #               max = 82,
   #               value = 20),
    #  checkboxInput(inputId = "showData",
    #                label = "Show data table",
    #                value = TRUE)
   #   selectInput(inputId = "colour",
   #  #             label = "Colour scheme:",
     #             choices = list("blues" = "Blues",
     #                           "reds" = "Reds"),
      #                          "greens" = "Greens",
     #                           "Yellow-Orange-Red" = "YlOrRd",
      #                          "Yellow-Orange-Brown" = "YlOrBr",
      #                          "Yellow-Green" = "YlGn"
       #                         "Orange-Red" = "OrRD",
     #             selected= "pretty",
     #             "Aged" = "AGED",
 #   ),
 #   mainPanel(
  #    tmapOutput("mapPlot"),
 #     DT::dataTableOutput(outputId = "aTable")
  #  )
 # )
#)





server <- function(input, output){
  dataset <- reactive({
    sgpools_sf %>%
      filter(`OUTLET TYPE` %in% input$type) %>%
      filter(`Gp1Gp2 Winnings` >= input$winning)
  })
  
  output$mapPlot <- renderTmap({
    tm_shape(shp = dataset(),
             bbox = st_bbox(sgpools_sf)) + 
      tm_bubbles(col = "OUTLET TYPE", 
                 size = "Gp1Gp2 Winnings",
                 border.col = "black",
                 border.lwd = 0.5) +
      tm_view(set.zoom.limits = c(11, 16))
  })
  output$aTable <- DT::renderDataTable({
    if(input$showData){
      DT::datatable(data = dataset() %>%
                      select(1:4),
                    options = list(pageLength = 10),
                    rownames = FALSE)
    }
  })
}


#ui <- fluidPage(
#  titlePanel("Static Proportional Symbol Map"),
#  sidebarLayout(
#    sidebarPanel(),
#    mainPanel(
#      plotOutput("mapPlot")
#    )
#  )
#)

#server <- function(input, output){
#  output$mapPlot <- renderPlot({
#    tm_shape(sgpools_sf) + 
#      tm_bubbles(col = "OUTLET TYPE", 
#                 size = "Gp1Gp2 Winnings",
#                 border.col = "black",
#                 border.lwd = 0.5)
#  })
#}

shinyApp(ui = ui, server = server)