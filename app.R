## R Shiny Code
## SSI for Open Fractures

library(shiny)
library(survival)
library(dplyr)

open_desc_data <- read.csv("open-desc-data-2024-01-28.csv")

ui <- fluidPage(
  titlePanel("Estimated Infection Risk"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(
          6,
          selectInput("OTA1", "AO-OTA Bone", choices = c("All", "1" = "1", "2R" = "2R", "2U" = "2U", "3" = "3", "4" = "4", "6" = "6", "7" = "7", "8" = "8")),
          selectInput("OTA2", "AO-OTA Segment", choices = c("All", "1" = "1", "2" = "2", "3" = "3", "4" = "4")),
          selectInput("OTAlast", "AO-OTA Region", choices = c("All", "A" = "A", "B" = "B", "C" = "C")),
          selectInput("Gust", "Gustilo-Anderson Classification", choices = c("All", "1" = "1", "2" = "2", "3A" = "3", "3B" = "4", "3C" = "5"))
          # , selectInput("times", "End Point (days post-injury)", choices = c("90" = 90, "180" = 180, "365" = 365))
        )
      ),
      width = 5
    ),
    mainPanel(
      h2("Infection Risk"),
      verbatimTextOutput("InfectionRisk"),
      width = 7
    )
  )
)

server <- function(input, output) {
  # Initialize the filtered data
  data_filtered <- reactive({
    filtered <- open_desc_data

    if (input$OTA1 != "All") {
      filtered <- filtered %>% filter(OTA1 %in% input$OTA1)
    }

    if (input$OTA2 != "All") {
      filtered <- filtered %>% filter(OTA2 %in% input$OTA2)
    }

    if (input$OTAlast != "All") {
      filtered <- filtered %>% filter(OTAlast %in% input$OTAlast)
    }

    if (input$Gust != "All") {
      filtered <- filtered %>% filter(Gust %in% input$Gust)
    }

    return(filtered)
  })

  output$InfectionRisk <- renderText({
    # Access the reactive data
    data <- data_filtered()

    out <- survfit(Surv(SSIDays, SSI) ~ 1, data = data)
    txts <- lapply(c(90, 180, 365), function(times) {
      sum.out <- summary(out, times = times)
      paste0(
        times, " days: ",
        100 - round(sum.out$surv * 100, 1), "%",
        " (95% CI, ", round(100 - sum.out$upper * 100, 1),
        "% to ", 100 - round(sum.out$lower * 100, 1), "%) with ",
        sum.out$n, " observations"
      )
    })
    return(paste0(txts, collapse = "\n"))
    # sum.out <- summary(out, times = 90) # timing

    # if (input$times == "180") {
    #   sum.out <- summary(out, time = 180)
    # }
    # if (input$times == "365") {
    #   sum.out <- summary(out, time = 365)
    # }
    # return(paste0(100 - round(sum.out$surv * 100, 1), "%", " (95% CI, ", round(100 - sum.out$upper * 100, 1), "% to ", 100 - round(sum.out$lower * 100, 1), "%) with ", sum.out$n, " observations"))
  })
}
shinyApp(ui = ui, server = server)
