# app.R
library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)
library(scales)
library(viridis)
library(retroPal)  # for pub_theme()

# optional: helpful while developing
options(shiny.error = recover)

# -----------------------------
# Load data
# -----------------------------
epred_grid <- readRDS("data/epred_grid.rds")
sea_mask   <- readRDS("data/sea_mask.rds")

# Ensure types are correct
epred_grid <- epred_grid %>%
  mutate(
    Date = as.Date(Date),
    Landcover = as.factor(Landcover),
    SiteType = as.factor(SiteType),
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    `Predicted Resistant Count` = as.numeric(`Predicted Resistant Count`)
  )

# Available dates (July–Dec only, and only those that exist)
avail_dates <- sort(unique(epred_grid$Date))
avail_dates <- avail_dates[month(avail_dates) %in% 7:12]
stopifnot(length(avail_dates) > 0)

# -----------------------------
# Plot function
# -----------------------------
make_prediction_map <- function(dat, sea_mask) {
  ggplot(dat, aes(fill = `Predicted Resistant Count`)) +
    geom_tile(aes(Longitude, Latitude), color = "black") +
    geom_sf(data = sea_mask, fill = "white") +
    coord_sf(xlim = c(-8, 2), ylim = c(49, 60)) +
    scale_fill_viridis_c(
      trans  = "log10",
      limits = c(10, 10000),
      oob    = scales::squish,
      breaks = scales::log_breaks()
    ) +
    labs(
      fill = "Predicted Resistant Count",
      x = "Longitude",
      y = "Latitude"
    ) +
    pub_theme(x_axis_labels = TRUE, drop_grid = TRUE)
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("UK predicted resistant Aspergillus fumigatus count"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "Landcover",
        "Landcover",
        choices  = levels(epred_grid$Landcover),
        selected = levels(epred_grid$Landcover)
      ),
      
      checkboxGroupInput(
        "SiteType",
        "Site type",
        choices  = levels(epred_grid$SiteType),
        selected = levels(epred_grid$SiteType)
      ),
      
      tags$hr(),
      
      radioButtons(
        "combine_mode",
        "When multiple categories selected, combine by…",
        choices = c("Sum" = "sum", "Mean" = "mean", "Median" = "median"),
        selected = "sum",
        inline = TRUE
      )
    ),
    
    mainPanel(
      plotOutput("pred_map", height = 750),
      # date slider now under the plot
      div(
        style = "padding-top: 10px;",
        sliderInput(
          "sel_date",
          "Date",
          min = min(avail_dates),
          max = max(avail_dates),
          value = min(avail_dates),
          timeFormat = "%Y-%m-%d",
          width = "100%",
          animate = animationOptions(interval = 1000, loop = TRUE)
        )
      ),
      
      textOutput("sel_date_label")
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
    epred_filtered <- reactive({
    req(input$sel_date, input$Landcover, input$SiteType)
    
    dat <- epred_grid %>%
      filter(
        Date == input$sel_date,
        Landcover %in% input$Landcover,
        SiteType %in% input$SiteType
      )
    
    # collapse to ONE value per grid cell (important when multiple categories selected)
    dat <- dat %>%
      group_by(Date, Longitude, Latitude) %>%
      summarise(
        `Predicted Resistant Count` = dplyr::case_when(
          input$combine_mode == "sum"    ~ sum(`Predicted Resistant Count`, na.rm = TRUE),
          input$combine_mode == "mean"   ~ mean(`Predicted Resistant Count`, na.rm = TRUE),
          input$combine_mode == "median" ~ median(`Predicted Resistant Count`, na.rm = TRUE),
          TRUE ~ sum(`Predicted Resistant Count`, na.rm = TRUE)
        ),
        .groups = "drop"
      )
    
    dat
  })
  
  
  output$pred_map <- renderPlot({
    dat <- epred_filtered()
    
    validate(
      need(nrow(dat) > 0, "No predictions for this combination (date/categories).")
    )
    
    make_prediction_map(dat, sea_mask)
  }, res = 120)
}

shinyApp(ui, server)