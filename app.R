# Load required libraries
library(shiny)             # Shiny framework
library(shinythemes)       # Themes for Shiny
library(leaflet)           # Leaflet maps
library(dplyr)             # Data manipulation
library(ggplot2)           # Plots
library(tidyr)             # Data tidying
library(FactoMineR)        # PCA analysis
library(Factoshiny)        # Shiny wrapper for PCA
library(corrplot)          # Correlation matrix plot
library(tidytext)          # reorder_within for ordered factors
library(RColorBrewer)      # Color palettes
library(sf)                # Simple Features (sf) spatial data
library(rnaturalearth)     # Natural Earth shapefiles
library(rnaturalearthdata) # World country data
library(plotly)
      

# Load world country polygons 
world_spdf <- suppressWarnings(
  ne_countries(scale = "medium", returnclass = "sf") %>%
    select(name, geometry) %>%
    st_cast("POLYGON") %>%
    st_simplify(dTolerance = 0.5)   
)


# Load World Happiness Report data (2015–2023)
df <- read.csv("shiny_data/whr_2015_2023.csv", sep = ",")

# Rename columns from French to English
df <- df %>%
  rename(
    year                         = annee,
    country                      = pays,
    region                       = region,
    gdp_per_capita               = pib_par_habitant,
    social_support               = soutien_social,
    healthy_life_expectancy      = esperance_vie_saine,
    freedom_to_make_life_choices = liberte_choix_vie,
    generosity                   = generosite,
    perception_of_corruption     = perception_corruption,
    happiness_score              = score_bonheur
  )


model <- lm(
  happiness_score ~ 
    gdp_per_capita + social_support +
    healthy_life_expectancy + freedom_to_make_life_choices +
    generosity + perception_of_corruption,
  data = df
)

saveRDS(model, file = "www/model.rds")

model <- readRDS("www/model.rds")

# Define a  palette 

max_cols <- brewer.pal.info["YlOrRd","maxcolors"]
n_cols   <- min(8, max_cols)
pal_bins <- colorBin(
  palette = brewer.pal(n_cols, "YlOrRd"),
  domain  = df$happiness_score,
  bins    = n_cols,
  na.color = "#EEEEEE"
)


# UI definition
ui <- fluidPage(
  tags$head(includeCSS("www/style.css")),   # external CSS
  theme = shinytheme("cerulean"),           # app theme
  
  fluidRow(
    column(
      width = 12,
      tabsetPanel(
        
        # --- About ---
        tabPanel(
          title = tagList(icon("info-circle"), "About"),
          h3("About This Project", align = "center"),
          img(src = "happy.jpg", height = "200px", style = "display:block;margin:0 auto;"),
          p("This dashboard explores country-level happiness data from the ",
            tags$em("World Happiness Report (WHR)"), " (2015–2023). Various factors influencing well-being are analyzed.",
            align = "center", style = "font-size:22px;"),
          h4("Variable Descriptions", align = "left"),
          tags$ul(
            tags$li(strong("Year:"), " Study period (2015–2023)."),
            tags$li(strong("Country:"), " Country name."),
            tags$li(strong("Region:"), " Geographic region."),
            tags$li(strong("GDP per capita:"), " Economic indicator."),
            tags$li(strong("Social support:"), " Reliable social network."),
            tags$li(strong("Healthy life expectancy:"), " Overall health status."),
            tags$li(strong("Freedom to make life choices:"), " Personal satisfaction & rights."),
            tags$li(strong("Generosity:"), " Community engagement."),
            tags$li(strong("Perception of corruption:"), " Institutional integrity.")
          ),
          h4("Data Processing", align = "left"),
          p("Data were imported from CSV files, variables renamed for clarity, and missing values handled (’N/A’ → NA, incomplete rows removed).", align = "left"),
          h4("Data Source", align = "left"),
          p(a(href = "https://www.kaggle.com/datasets/sazidthe1/global-happiness-scores-and-factors",
              target = "_blank", "Kaggle dataset"))
        ),
        
        # --- Descriptive Analysis ---
        tabPanel(
          title = tagList(icon("book"), "Descriptive Analysis"),
          tabsetPanel(
            tabPanel(
              title = tagList(icon("chart-bar"), "Summary Stats"),
              h4("Data Frame Overview", align = "center"),
              verbatimTextOutput("summaryStatsFast"),
              verbatimTextOutput("summaryStats"),
              verbatimTextOutput("yearlyStats")
            ),
            tabPanel(
              title = tagList(icon("line-chart"), "Happiness Trend"),
              plotlyOutput("trendPlot", height = "400px"),
              plotOutput("trendByRegionPlot")
            ),
            tabPanel(
              title = tagList(icon("trophy"), "Top by Year"),
              plotOutput("topHappyByYear"), plotOutput("topUnhappyByYear")
            ),
            tabPanel(
              title = tagList(icon("smile"), "Overall Top"),
              plotOutput("topHappyCountries"), plotOutput("topUnhappyCountries")
            ),
            tabPanel(
              title = tagList(icon("heart"), "Determinants"),
              plotOutput("determinantsHappy"), plotOutput("determinantsUnhappy")
            ),
            tabPanel(
              title = tagList(icon("globe"), "Top by Region"),
              plotOutput("topHappyByRegion"), plotOutput("topUnhappyByRegion")
            ),
            tabPanel(
              title = tagList(icon("th-large"), "Correlation Matrix"),
              plotOutput("correlationMatrixPlot", height = "700px")
            ),
            tabPanel(
              title = tagList(icon("chart-pie"), "PCA on 2023"),
              radioButtons("pca_choice", "Select Plot Type:",
                           choices = c("Scree Plot"="eig","Variables Plot"="var","Individuals Plot"="ind")),
              plotOutput("pcaPlot", height = "650px", width = "100%")
            )
          )
        ),
        
        # --- Map ---
        tabPanel(
          title = tagList(icon("map"), "Map"),
          div(style = "display:flex; justify-content:center; align-items:center; margin-bottom:20px;",
              selectInput("variable", "Choose a metric:",
                          choices = c("happiness_score","gdp_per_capita","social_support",
                                      "healthy_life_expectancy","freedom_to_make_life_choices",
                                      "generosity","perception_of_corruption"),
                          width = "220px"),
              tags$div(style = "width:30px;"),   # spacer
              sliderInput("year", "Year:",
                          min = min(df$year), max = max(df$year),
                          value = min(df$year), step = 1, sep = "", width = "350px")
          ),
          leafletOutput("map", height = 600)
        ),
        
        # --- Country Profile ---
        tabPanel(
          title = tagList(icon("chart-area"), "Country Profile"),
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "country_select", 
                "Choose a country:",
                choices = sort(unique(df$country)),
                selected = "Finland"
              )
            ),
            mainPanel(
              plotlyOutput("radarChart", height = "700px")
            )
          )
        )
        ,

        # --- Metric Explorer ---
        tabPanel(
          title = tagList(icon("chart-scatter"), "Metric Explorer"),
          fluidRow(
            column(4,
                   selectInput("xvar", "X variable:",
                               choices = c("GDP per Capita" = "gdp_per_capita",
                                           "Social" = "social_support",
                                           "Healthy Life Expectancy" = "healthy_life_expectancy",
                                           "Freedom" = "freedom_to_make_life_choices",
                                           "Generosity" = "generosity",
                                           "Corruption" = "perception_of_corruption",
                                           "Happiness Score" = "happiness_score")
                   )
            ),
            column(4,
                   selectInput("yvar", "Y variable:", 
                               choices = c("Happiness Score" = "happiness_score",
                                           "GDP per Capita" = "gdp_per_capita",
                                           "Social" = "social_support",
                                           "Healthy Life Expectancy" = "healthy_life_expectancy",
                                           "Freedom" = "freedom_to_make_life_choices",
                                           "Generosity" = "generosity",
                                           "Corruption" = "perception_of_corruption")
                   )
            ),
            column(4,
                   sliderInput("year_scatter", "Year:",
                               min = min(df$year), max = max(df$year),
                               value = min(df$year), step = 1, sep = "")
            )
          ),
          plotlyOutput("scatterPlot", height = "600px")
        )
      
        ,
        
        # --- Prediction ---
        tabPanel(
          title = tagList(icon("magic"), "Prediction"),
          h3("Predict Happiness Score", align = "center"),
          p(em("Model: multiple linear regression on six determinants.")),  # note élégante
          div(style = "display:flex; flex-direction:column; align-items:center;",
              numericInput("gdp_input",     "GDP per capita:",            value = NA),
              numericInput("social_input",  "Social support:",           value = NA),
              numericInput("health_input",  "Healthy life expectancy:",  value = NA),
              numericInput("freedom_input", "Freedom:",        value = NA),
              numericInput("gen_input",     "Generosity:",                value = NA),
              numericInput("corrupt_input", "Perception of corruption:",  value = NA),
              actionButton("predict_btn", "Predict"),
              verbatimTextOutput("prediction_result")
          )
        )
        
      ) # end tabsetPanel
    )   # end column
  )     # end fluidRow
)       # end ui

# Server logic
server <- function(input, output, session) {
  source("descriptive_analysis_server.R", local = environment())
  
  # Render an interactive radar chart of determinants for the selected country
  output$radarChart <- renderPlotly({
    req(input$country_select)
    
    # 1) Extract mean values of each determinant for the chosen country
    vals <- df %>%
      filter(country == input$country_select) %>%
      summarise(
        gdp        = mean(gdp_per_capita,               na.rm = TRUE),
        social     = mean(social_support,               na.rm = TRUE),
        health     = mean(healthy_life_expectancy,      na.rm = TRUE),
        freedom    = mean(freedom_to_make_life_choices, na.rm = TRUE),
        generosity = mean(generosity,                   na.rm = TRUE),
        corruption = mean(perception_of_corruption,     na.rm = TRUE)
      ) %>%
      unlist()
    
    # 2) Normalize values to 0–1 for consistent radar scaling
    vals_norm <- (vals - min(vals)) / (max(vals) - min(vals))
    
    # 3) Custom labels
    labels <- c(
      "GDP per Capita",
      "Social Support",
      "Healthy Life Expectancy",
      "Freedom",
      "Generosity",
      "Perception of Corruption"
    )
    
    # 4) Build a Plotly polar (radar) chart with larger fonts
    plot_ly(
      type  = "scatterpolar",
      r     = vals_norm,
      theta = labels,
      mode  = "lines+markers",
      fill  = "toself"
    ) %>%
      layout(
        title = list(
          text = paste0("<b>Determinant Profile: ", input$country_select, "</b>"),
          font = list(size = 16),
          xref = "paper",
          x = 0.5
        ),
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range   = c(0, 1),
            tickfont = list(size = 14)        
          ),
          angularaxis = list(
            tickfont = list(size = 14)         
          )
        ),
        showlegend = FALSE
      )
  })
  
  # 1) Render interactive scatter of chosen metrics
  output$scatterPlot <- renderPlotly({
    req(input$xvar, input$yvar, input$year_scatter)
    data_sc <- df %>%
      filter(year == input$year_scatter) %>%
      rename(x = .data[[input$xvar]], y = .data[[input$yvar]])
    
    p <- plot_ly(
      data_sc,
      x = ~x, y = ~y, text = ~country,  # hover shows country
      type = "scatter", mode = "markers",
      color = ~region,                  # color by region
      marker = list(size = 10),
      source = "scatter"
    ) %>%
      layout(
        title = paste0("<b>", input$yvar, " vs ", input$xvar, " (", input$year_scatter, ")</b>"),
        xaxis = list(title = input$xvar),
        yaxis = list(title = input$yvar)
      )
    event_register(p, "plotly_click")
  })
  
  # 2) Drill-down on click: show modal with radar + time-series
  observeEvent(event_data("plotly_click", source = "scatter"), {
    click <- event_data("plotly_click", source = "scatter")
    req(click)
    ct <- click$pointNumber + 1  # index of row in data_sc
    data_sc <- df %>% filter(year == input$year_scatter)
    country_clicked <- data_sc$country[ct]
    
    # Radar as before
    vals <- df %>%
      filter(country == country_clicked) %>%
      summarise(
        gdp        = mean(gdp_per_capita, na.rm = TRUE),
        social     = mean(social_support, na.rm = TRUE),
        health     = mean(healthy_life_expectancy, na.rm = TRUE),
        freedom    = mean(freedom_to_make_life_choices, na.rm = TRUE),
        generosity = mean(generosity, na.rm = TRUE),
        corruption = mean(perception_of_corruption, na.rm = TRUE)
      ) %>% unlist()
    vals_norm <- (vals - min(vals)) / (max(vals) - min(vals))
    labels <- c("GDP per Capita", "Social", "Healthy Life Expectancy",
                "Freedom", "Generosity", "Corruption")
    radar <- plot_ly(
      type  = "scatterpolar", r = vals_norm, theta = labels, fill = "toself",
      mode = "markers+lines"
    ) %>% layout(
      title = list(text = paste0("<b>Determinant: ", country_clicked, "</b>"))
    )
    
    # Time series
    ts <- df %>%
      filter(country == country_clicked) %>%
      group_by(year) %>%
      summarise(score = mean(happiness_score, na.rm = TRUE))
    ts_plot <- plot_ly(
      ts, x = ~year, y = ~score,
      type = "scatter", mode = "lines+markers",
      hovertemplate = "%{x}: %{y:.2f}<extra></extra>"
    ) %>% layout(title = paste0("<b>Happiness Trend: ", country_clicked, "</b>"))
    
    # Show both in modal
    showModal(modalDialog(
      title = paste("Details for", country_clicked),
      fluidRow(
        column(6, renderPlotly(radar)),
        column(6, renderPlotly(ts_plot))
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  
  
  
  
  # Reactive filter for map
  filtered_data <- reactive({ df %>% filter(year == input$year) })
  
  # Render choropleth map with addPolygons()
  # 1) Initial map + default legend for 2015
  output$map <- renderLeaflet({
    # prepare default data (2015)
    default_data <- df %>%
      filter(year == min(year)) %>%
      mutate(value = .data[[input$variable]])
    
    # build palette on that year
    pal0 <- colorNumeric(
      palette  = brewer.pal(9, "YlOrRd"),
      domain   = default_data$value,
      na.color = "#EEEEEE"
    )
    
    leaflet(world_spdf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addPolygons(
        data      = left_join(world_spdf, default_data, by = c("name" = "country")),
        layerId   = ~name,
        fillColor = ~pal0(value),
        weight    = 1,
        color     = "#555555",
        dashArray = "3",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight       = 2,
          color        = "#333333",
          bringToFront = TRUE
        ),
        label = ~paste0(name, ": ", round(value, 2))
      ) %>%
      addLegend(
        pal      = pal0,
        values   = default_data$value,
        position = "bottomright",
        title    = input$variable,
        opacity  = 1
      )
  })
  
  # 2) Then update shapes & legend whenever year or metric changes
  observe({
    data_map <- df %>%
      filter(year == input$year) %>%
      mutate(value = .data[[input$variable]])
    
    world_joined <- left_join(
      world_spdf,
      data_map %>% select(country, value),
      by = c("name" = "country")
    )
    
    pal <- colorNumeric(
      palette  = brewer.pal(9, "YlOrRd"),
      domain   = world_joined$value,
      na.color = "#EEEEEE"
    )
    
    leafletProxy("map", data = world_joined) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        layerId     = ~name,
        fillColor   = ~pal(value),
        weight      = 1,
        color       = "#555555",
        dashArray   = "3",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight       = 2,
          color        = "#333333",
          bringToFront = TRUE
        ),
        label = ~paste0(name, ": ", round(value, 2))
      ) %>%
      addLegend(
        pal      = pal,
        values   = world_joined$value,
        position = "bottomright",
        title    = input$variable,
        opacity  = 1
      )
  })
  
  # Drill-down : display time-series when clicking a country
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    req(click$id)
    ct <- click$id
    
    country_ts <- df %>%
      filter(country == ct) %>%
      group_by(year) %>%
      summarize(score = mean(happiness_score, na.rm = TRUE))
    
    p <- plot_ly(
      country_ts,
      x = ~year, y = ~score,
      type = "scatter", mode = "lines+markers",
      hovertemplate = "%{x}: %{y:.2f}<extra></extra>"
    ) %>%
      layout(
        title = paste("Evolution of Happiness –", ct),
        xaxis = list(title = "Year", tickangle = 45),
        yaxis = list(title = "Happiness Score")
      )
    
    showModal(modalDialog(
      title    = paste("Trend for", ct),
      renderPlotly(p),
      easyClose = TRUE,
      footer    = modalButton("Close")
    ))
  })
  
  
  
  # Placeholder for prediction
  # Prediction using the multiple linear regression model
  observeEvent(input$predict_btn, {
    # 1) Validate inputs
    req(
      !is.na(input$gdp_input),
      !is.na(input$social_input),
      !is.na(input$health_input),
      !is.na(input$freedom_input),
      !is.na(input$gen_input),
      !is.na(input$corrupt_input)
    )
    
    # 2) Build newdata for predict()
    newdata <- data.frame(
      gdp_per_capita               = input$gdp_input,
      social_support               = input$social_input,
      healthy_life_expectancy      = input$health_input,
      freedom_to_make_life_choices = input$freedom_input,
      generosity                   = input$gen_input,
      perception_of_corruption     = input$corrupt_input
    )
    
    # 3) Make prediction
    pred <- predict(model, newdata = newdata)
    
    # 4) Display result
    output$prediction_result <- renderText({
      paste0("Predicted Happiness Score: ", round(pred, 2))
    })
  })

}

# Run the application
shinyApp(ui, server)
