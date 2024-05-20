## R Shiny Code
## SSI for Open Fractures

library(shiny)
library(survival)
library(dplyr)

open_desc_data <- read.csv("open-desc-data-2024-05-20.csv")

ui <- fluidPage(
  titlePanel("Open Fracture Complication Risk"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(
          6,
          selectInput("OTA1", "OTA-AO Bone", choices = c("All", "1" = "1", "2R" = "2R", "2U" = "2U", "3" = "3", "4" = "4", "6" = "6", "7" = "7", "8" = "8")),
          selectInput("OTA2", "OTA-AO Segment", choices = c("All", "1" = "1", "2" = "2", "3" = "3", "4" = "4")),
          selectInput("OTAlast", "OTA-AO Morphology", choices = c("All", "A" = "A", "B" = "B", "C" = "C")),
          selectInput("Gust", "Gustilo-Anderson Classification", choices = c("All", "1" = "1", "2" = "2", "3A" = "3", "3B" = "4", "3C" = "5"))
        )
      ),
      width = 5
    ),
    mainPanel(
      h2("Overall Infection Rate"),
      verbatimTextOutput("OverallInfectionRisk"),
      h2("Superficial Infection Rate"),
      verbatimTextOutput("SuperficialInfectionRisk"),
      h2("Deep/Organ Space Infection Rate"),
      verbatimTextOutput("DeepOrganInfectionRisk"),
      h2("Delayed Union or Nonunion Rate"),
      verbatimTextOutput("NonunionRisk"),
      width = 6
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
  
  output$OverallInfectionRisk <- renderText({
    # Access the reactive data
    data <- data_filtered()
    out <- survfit(Surv(SSIDays, SSI) ~ 1, data = data)
    txts <- lapply(c(90, 180, 365), function(times) {
      sum.out <- summary(out, times = times)
      paste0(
        times, " days: ",
        round(100 - (sum.out$surv * 100), 1), "%",
        " (95% CI, ", round(100 - sum.out$upper * 100, 1),
        "% to ", round(100 - (sum.out$lower * 100), 1), "%) with ",
        sum.out$n, " observations"
      )
    })
    paste0(txts, collapse = "\n")
  })
  
  output$SuperficialInfectionRisk <- renderText({
    data <- data_filtered()
    out2 <- survfit(Surv(SupDays, SupSSI) ~ 1, data = data)
    txts2 <- lapply(c(90, 180, 365), function(times) {
      sum.out2 <- summary(out2, times = times)
      paste0(
        times, " days: ",
        round(100 - (sum.out2$surv * 100), 1), "%",
        " (95% CI, ", round(100 - sum.out2$upper * 100, 1),
        "% to ", round(100 - (sum.out2$lower * 100), 1), "%) with ",
        sum.out2$n, " observations"
      )
    })
    paste0(txts2, collapse = "\n")
  })
  
  output$DeepOrganInfectionRisk <- renderText({
    data <- data_filtered()
    out3 <- survfit(Surv(DpOrDays, DpOrSSI) ~ 1, data = data)
    txts3 <- lapply(c(90, 180, 365), function(times) {
      sum.out3 <- summary(out3, times = times)
      paste0(
        times, " days: ",
        round(100 - (sum.out3$surv * 100), 1), "%",
        " (95% CI, ", round(100 - sum.out3$upper * 100, 1),
        "% to ", round(100 - (sum.out3$lower * 100), 1), "%) with ",
        sum.out3$n, " observations"
      )
    })
    paste0(txts3, collapse = "\n")
  })
  
  output$NonunionRisk <- renderText({
    data <- data_filtered()
    out4 <- survfit(Surv(ReopDays, ReopUn) ~ 1, data = data)
    txts4 <- lapply(c(90, 180, 365), function(times) {
      sum.out4 <- summary(out4, times = times)
      paste0(
        times, " days: ",
        round(100 - (sum.out4$surv * 100), 1), "%",
        " (95% CI, ", round(100 - sum.out4$upper * 100, 1),
        "% to ", round(100 - (sum.out4$lower * 100), 1), "%) with ",
        sum.out4$n, " observations"
      )
    })
    paste0(txts4, collapse = "\n")
  })
  
}

shinyApp(ui = ui, server = server)
