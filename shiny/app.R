library(shiny)

# UI
ui = fluidPage(
  titlePanel("Snake River IPTDS"),
  mainPanel(style = 'width:100% !important; height:90vh;',
    helpText('Please be patient while the map loads.'),
    includeHTML(path = './leaflet/sr_iptds_leaflet_w_recommendations.html')
    )
)

# Server
server = function(input, output) {
}

# App 
shinyApp(ui = ui, server = server)
