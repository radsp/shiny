
# Set Working Directory --------------------------------------

# setwd('~/Software/R/fb-pop-nga/pilot-viz')


# LIBRARIES ---------------------------------------------------

library(raster)
library(leaflet)
library(sf)


# START CODE/ALGORITHM -----------------------------------------


#---------------------------------------------------------------
# Data Prep 
#---------------------------------------------------------------

# Ibarapa N 

f1 <- paste("data/", c("pop-oyo-ibarapaN.tif", "popCU5-oyo-ibarapaN.tif", "popW-oyo-ibarapaN.tif"),
              sep = "")

r1 <- stack(f1)

adm1 <- shapefile("data/admin-oyo-ibarapaN/nga-lga-oyo-ibarapaN.shp")
h1 <- shapefile("data/hamlet-oyo-ibarapaN.shp")


# Itesiwaju

f2 <- paste("data/", c("pop-oyo-itesiwaju.tif", "popCU5-oyo-itesiwaju.tif", "popW-oyo-itesiwaju.tif"),
              sep = "")
r2 <- stack(f2)

adm2 <- shapefile("data/admin-oyo-itesiwaju/nga-lga-oyo-itesiwaju.shp")
h2 <- shapefile("data/hamlet-oyo-itesiwaju.shp")


# Combine data by district

dat1 <- list(a = adm1, h = h1, r = r1)

dat2 <- list(a = adm2, h = h2, r = r2)


# Set population bin or viz

r.bin <- c(2, 10, 15, 20, 25, 50, 100, 500, 1000, 2500)
# r.col <- c("#e7e1ef", "#d4b9da", "#c994c7", "#df65b0",
#            "#e7298a", "#ce1256", "#980043", "#67001f")

# Set map color

r.col <- c("#a1d99b", "#41ab5d", "#74a9cf", "#0570b0",
           "#f768a1", "#dd3497", "#ef3b2c", "#99000d")



#---------------------------------------------------------------
# Shiny 
#---------------------------------------------------------------

# Input selector

in1 <- selectInput(
  inputId = 'pick_prov',
  label = 'State',
  choices = c('Oyo') #, width = '40%'
)


in2 <- selectInput(
  inputId = 'district',
  label = 'LGA',
  choices = c('Ibarapa North', 'Itesiwaju') #, width = '40%'
)



in_poptype <- selectInput(
  inputId = 'poptype',
  label = 'Population',
  choices = c('All', 'Children under 5 years age', 'Women between 15 - 49 years age')
)



# Define UI

ui <- fluidPage(
  titlePanel("PopMap - Nigeria"),
  sidebarLayout(
    sidebarPanel(
      in1, in2, in_poptype
    ),
    
    mainPanel(
      leafletOutput("mymap", width = 600, height = 800)
    )
  )
)



# Define server

server <- function(input, output) {
  
  data_poptype <- reactive({
    switch(input$poptype,
           "All" = 1, 
           "Children under 5 years age" = 2,
           "Women between 15 - 49 years age" = 3)
  })
  
  data_district <- reactive({
    switch(input$district, 
           "Ibarapa North" = dat1, 
           "Itesiwaju" = dat2)
  })
  

  output$mymap <- renderLeaflet({
    
    data <- data_district()
    k <- data_poptype()
    
    r.pal <- colorBin(r.col, bins = r.bin, values(subset(data$r, k)),
                      na.color = "transparent")
    
    leaflet() %>% addTiles() %>%
      addPolygons(data = data$a, fill = FALSE, stroke = TRUE, weight = 1, color = "black", group = "Admin Boundary") %>%
      addRasterImage(subset(data$r, k), colors = r.pal, group = "FB Population") %>%
      addPolygons(data = data$h, fill = FALSE, stroke = TRUE, color = "grey", group = "Hamlet area", weight = 1) %>%
      addLegend(pal = r.pal, values = values(subset(data$r, k)),
                title = "Population") %>%
      addLayersControl(
        overlayGroups = c("FB Population", 
                          "Hamlet area"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
}


# Create the Shiny App
shinyApp(ui = ui, server = server) 