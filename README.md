# World Happiness Dashboard

Interactive Shiny dashboard exploring **World Happiness Report (2015–2023)** data through visualizations, country profiles, and predictive modeling.


## 🎥 Live Demo

<div align="center">
  <a href="https://youtu.be/1DTRjHESWTc?si=twXWFf3CMHgXKBw-" target="_blank" rel="noopener noreferrer">
    <img src="www/tutorial.png" alt="Live app demo" width="500" height="450" />
  </a>
  <p><strong>Watch the live app demo</strong></p>
</div>




## Features

- **Descriptive Analysis**  
  Summary statistics, trends, rankings, correlation matrices, PCA.

- **Interactive Map**  
  Explore global metrics year-by-year, drill down into countries.

- **Country Profile**  
  Radar chart analysis of happiness determinants per country.

- **Metric Explorer**  
  Interactive scatter plots with detailed country-level insights.

- **Predictive Modeling**  
  Predict happiness score from six key factors via linear regression.


## Getting Started

1. **Clone the repository**  
   ```bash
   git clone https://github.com/Cheikh133/world_hapiness_app.git
   cd world_happiness_app
   ```
2. **Install R packages**  
   ```r
   install.packages(c(
     "shiny", "shinythemes", "leaflet", "dplyr", "ggplot2", "tidyr",
     "FactoMineR", "Factoshiny", "corrplot", "tidytext", "RColorBrewer",
     "sf", "rnaturalearth", "rnaturalearthdata", "plotly"
   ))
   ```
3. **Run the app**  
   ```r
   library(shiny)
   runApp()
   ```

## Author

**Cheikh LO**  
*Data Scientist & Statistical Engineer*

[![LinkedIn](https://img.shields.io/badge/LinkedIn-Cheikh%20LO-blue?logo=linkedin&style=flat-square)](https://www.linkedin.com/in/cheikh-lo-531701193/)  
[![GitHub](https://img.shields.io/badge/GitHub-cheikh133-black?logo=github&style=flat-square)](https://github.com/cheikh133)
