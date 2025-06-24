# World Happiness Dashboard

A Shiny application exploring the **World Happiness Report (2015–2023)** with interactive visualisations, country profiles and predictive modelling.

---

## 🔗 Live Demo

A short demo of the app in action:  
[![Live Demo on ShinyApps.io](https://your-demo-link/demo.gif)](https://your-shinyapps-url.shinyapps.io/world_happiness_app)

---

## Features

- **Descriptive Analysis**  
  – Summary tables, trend lines, top-N rankings, correlation matrices, PCA.  
- **Interactive Map**  
  – Choropleth of any metric by year, dynamic legend, country drill-down.  
- **Country Profile**  
  – Radar chart of six determinants for a selected country.  
- **Metric Explorer**  
  – Scatter plot of any two variables in a given year, with modal drill-down.  
- **Prediction**  
  – Multiple linear regression on six factors; live input and score output.

---

## Getting Started

1. **Clone the repository**  
   ```bash
   git clone https://github.com/your-org/world_happiness_app.git
   cd world_happiness_app
   ```
2. **Install R packages**  
   ```r
   install.packages(c(
     "shiny", "shinythemes", "leaflet", "dplyr", "ggplot2", "tidyr",
     "FactoMineR", "Factoshiny", "corrplot", "tidytext", "RColorBrewer",
     "sf", "rnaturalearth", "rnaturalearthdata", "plotly", "prophet"
   ))
   ```
3. **Run the app**  
   ```r
   library(shiny)
   runApp()
   ```