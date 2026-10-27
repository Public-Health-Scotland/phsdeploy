# Minimal Shiny app for the phsdeploy checks. It only needs shiny, so it tests
# deployment without any PHS packages. `data.csv` is rewritten by the update
# check, so the page shows which data version is live.
library(shiny)

data <- read.csv("data.csv")

ui <- fluidPage(
  titlePanel("phsdeploy check: basic Shiny app"),
  p("Data version: ", strong(data$version)),
  p("Data written: ", data$written)
)

server <- function(input, output, session) {}

shinyApp(ui, server)
