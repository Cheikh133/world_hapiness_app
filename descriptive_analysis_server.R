# R/descriptive_analysis_server.R

# Quick overview of the data frame structure
output$summaryStatsFast <- renderPrint({
  str(df)
})

# Global summary statistics and by year
output$summaryStats <- renderPrint({
  summary_table <- df %>%
    summarize(
      total_records    = n(),
      unique_regions   = n_distinct(region),
      unique_countries = n_distinct(country),
      mean_score       = mean(happiness_score, na.rm = TRUE),
      std_dev          = sd(happiness_score, na.rm = TRUE),
      min_score        = min(happiness_score, na.rm = TRUE),
      median_score     = median(happiness_score, na.rm = TRUE),
      max_score        = max(happiness_score, na.rm = TRUE)
    )
  print(summary_table)
})

output$yearlyStats <- renderPrint({
  yearly_table <- df %>%
    group_by(year) %>%
    summarize(
      total_records    = n(),
      unique_regions   = n_distinct(region),
      unique_countries = n_distinct(country),
      mean_score       = mean(happiness_score, na.rm = TRUE),
      std_dev          = sd(happiness_score, na.rm = TRUE),
      min_score        = min(happiness_score, na.rm = TRUE),
      median_score     = median(happiness_score, na.rm = TRUE),
      max_score        = max(happiness_score, na.rm = TRUE)
    )
  print(yearly_table)
})

# Compute average happiness score per year
output$trendPlot <- renderPlotly({
  avg_score_by_year <- df %>%
    group_by(year) %>%
    summarize(avg_happiness = mean(happiness_score, na.rm = TRUE))
  
  plot_ly(
    data = avg_score_by_year,
    x = ~year,
    y = ~avg_happiness,
    type = "scatter",
    mode = "lines+markers",
    hovertemplate = "Year: %{x}<br>Avg Score: %{y:.2f}<extra></extra>"
  ) %>%
    layout(
      title = "Average Happiness Score Over Time",
      xaxis = list(title = "Year", tickangle = 45),
      yaxis = list(title = "Average Happiness Score")
    )
})


# Compute average happiness by year and region
avg_score_by_year_region <- df %>%
  group_by(year, region) %>%
  summarize(avg_happiness = mean(happiness_score, na.rm = TRUE), .groups = "drop")

# Plot happiness trend by region
output$trendByRegionPlot <- renderPlot({
  ggplot(avg_score_by_year_region,
         aes(x = year, y = avg_happiness, color = region)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = seq(min(avg_score_by_year_region$year),
                                    max(avg_score_by_year_region$year), by = 1)) +
    labs(
      title = "Happiness Trend by Region",
      x     = "Year",
      y     = "Average Happiness Score",
      color = "Region"
    ) +
    theme_minimal(base_size = 20) +
    theme(axis.text.x    = element_text(angle = 45, hjust = 1),
          legend.position = "right")
})

# Top 3 happiest and unhappiest countries each year
top3_happy_by_year <- df %>%
  group_by(year) %>%
  slice_max(order_by = happiness_score, n = 3) %>%
  ungroup()

top3_unhappy_by_year <- df %>%
  group_by(year) %>%
  slice_min(order_by = happiness_score, n = 3) %>%
  ungroup()

output$topHappyByYear <- renderPlot({
  ggplot(top3_happy_by_year,
         aes(x    = reorder_within(country, happiness_score, year),
             y    = happiness_score,
             fill = happiness_score)) +
    geom_col() +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    coord_flip() +
    facet_wrap(~ year, scales = "free_y") +
    scale_x_reordered() +
    labs(
      title = "Top 3 Happiest Countries Each Year",
      x     = "Country",
      y     = "Happiness Score"
    ) +
    theme_minimal(base_size = 16)
})

output$topUnhappyByYear <- renderPlot({
  ggplot(top3_unhappy_by_year,
         aes(x    = reorder_within(country, -happiness_score, year),
             y    = happiness_score,
             fill = happiness_score)) +
    geom_col() +
    scale_fill_gradient(low = "lightcoral", high = "darkred") +
    coord_flip() +
    facet_wrap(~ year, scales = "free_y") +
    scale_x_reordered() +
    labs(
      title = "Top 3 Unhappiest Countries Each Year",
      x     = "Country",
      y     = "Happiness Score"
    ) +
    theme_minimal(base_size = 16)
})

# Compute average happiness per country (2015–2023)
avg_happiness_by_country <- df %>%
  group_by(country) %>%
  summarize(avg_happiness = mean(happiness_score, na.rm = TRUE))

# Top 10 happiest and unhappiest countries overall
top10_happy_countries   <- slice_max(avg_happiness_by_country, avg_happiness, n = 10)
top10_unhappy_countries <- slice_min(avg_happiness_by_country, avg_happiness, n = 10)

output$topHappyCountries <- renderPlot({
  ggplot(top10_happy_countries,
         aes(x    = reorder(country, avg_happiness),
             y    = avg_happiness,
             fill = avg_happiness)) +
    geom_col() +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    coord_flip() +
    labs(
      title = "Top 10 Happiest Countries (2015–2023)",
      x     = "Country",
      y     = "Average Happiness Score"
    ) +
    theme_minimal(base_size = 20)
})

output$topUnhappyCountries <- renderPlot({
  ggplot(top10_unhappy_countries,
         aes(x    = reorder(country, -avg_happiness),
             y    = avg_happiness,
             fill = avg_happiness)) +
    geom_col() +
    scale_fill_gradient(low = "lightcoral", high = "darkred") +
    coord_flip() +
    labs(
      title = "Top 10 Unhappiest Countries (2015–2023)",
      x     = "Country",
      y     = "Average Happiness Score"
    ) +
    theme_minimal(base_size = 20)
})

# Compute and normalize determinants by country
determinants_normalized <- df %>%
  group_by(country) %>%
  summarize(
    gdp_per_capita               = mean(gdp_per_capita, na.rm = TRUE),
    social_support               = mean(social_support, na.rm = TRUE),
    healthy_life_expectancy      = mean(healthy_life_expectancy, na.rm = TRUE),
    freedom_to_make_life_choices = mean(freedom_to_make_life_choices, na.rm = TRUE),
    generosity                   = mean(generosity, na.rm = TRUE),
    perception_of_corruption     = mean(perception_of_corruption, na.rm = TRUE)
  ) %>%
  mutate(across(
    gdp_per_capita:perception_of_corruption,
    ~ (. - min(.)) / (max(.) - min(.))
  ))

# Filter determinants for top/bottom 10 countries
det_top10_happy   <- filter(determinants_normalized, country %in% top10_happy_countries$country)
det_top10_unhappy <- filter(determinants_normalized, country %in% top10_unhappy_countries$country)

# Reshape for plotting
det_happy_long   <- pivot_longer(det_top10_happy,
                                 cols = gdp_per_capita:perception_of_corruption,
                                 names_to = "factor",
                                 values_to = "value")
det_unhappy_long <- pivot_longer(det_top10_unhappy,
                                 cols = gdp_per_capita:perception_of_corruption,
                                 names_to = "factor",
                                 values_to = "value")

# Helper function to plot determinants
plot_determinants <- function(df_long, title_text) {
  ggplot(df_long, aes(x = value, y = factor, color = country)) +
    geom_segment(aes(x = 0, xend = 1, yend = factor), color = "grey") +
    geom_point(size = 3) +
    labs(
      title = title_text,
      x     = "Standardized Value",
      y     = "Determinant"
    ) +
    theme_minimal(base_size = 20)
}

output$determinantsHappy <- renderPlot({
  plot_determinants(det_happy_long, "Determinants of Top 10 Happiest Countries (avg. 2015–2023)")
})

output$determinantsUnhappy <- renderPlot({
  plot_determinants(det_unhappy_long, "Determinants of Top 10 Unhappiest Countries (avg. 2015–2023)")
})

# Compute top 5 happiest & unhappiest countries by region
avg_by_region_country <- df %>%
  group_by(region, country) %>%
  summarize(mean_happiness = mean(happiness_score, na.rm = TRUE), .groups = "drop")

top5_happy_by_region   <- avg_by_region_country %>% group_by(region) %>% slice_max(mean_happiness, n = 5)
top5_unhappy_by_region <- avg_by_region_country %>% group_by(region) %>% slice_min(mean_happiness, n = 5)

output$topHappyByRegion <- renderPlot({
  ggplot(top5_happy_by_region,
         aes(x    = reorder(country, mean_happiness),
             y    = mean_happiness,
             fill = mean_happiness)) +
    geom_col() +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    coord_flip() +
    facet_wrap(~ region, scales = "free_y") +
    labs(
      title = "Top 5 Happiest Countries by Region (2015–2023 avg.)",
      x     = "Country",
      y     = "Mean Happiness Score"
    ) +
    theme_minimal(base_size = 16)
})

output$topUnhappyByRegion <- renderPlot({
  ggplot(top5_unhappy_by_region,
         aes(x    = reorder(country, -mean_happiness),
             y    = mean_happiness,
             fill = mean_happiness)) +
    geom_col() +
    scale_fill_gradient(low = "lightcoral", high = "darkred") +
    coord_flip() +
    facet_wrap(~ region, scales = "free_y") +
    labs(
      title = "Top 5 Unhappiest Countries by Region (2015–2023 avg.)",
      x     = "Country",
      y     = "Mean Happiness Score"
    ) +
    theme_minimal(base_size = 16)
})

# Correlation matrix of numeric variables
output$correlationMatrixPlot <- renderPlot({

  numeric_df <- df %>% select(where(is.numeric))
  
  cor_mat <- cor(numeric_df, use = "pairwise.complete.obs")
  
  corrplot(
    cor_mat,
    method = "square",
    type   = "upper",
    col    = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200),
    tl.col = "black",
    tl.srt = 45,
    mar    = c(0, 0, 1, 0)
  )
})

# Principal Component Analysis for 2023 
output$pcaPlot <- renderPlot({
  # 1) Subset to 2023 and keep country, region, happiness_score, then other numerics
  df_2023 <- df %>%
    filter(year == 2023) %>%
    select(country, region, happiness_score, everything())
  
  # 2) Use country names as row names, then drop the country column
  row.names(df_2023) <- df_2023$country
  df_2023 <- df_2023[, -1]
  
  # 3) Perform PCA
  res_pca <- suppressWarnings(PCA(
    df_2023,
    quali.sup  = 1,    # region
    quanti.sup = 2,    # happiness_score
    scale.unit = TRUE,
    graph      = FALSE
  ))
  
  # 4) Conditional plotting based on user choice
  if (input$pca_choice == "eig") {
    # Scree plot (eigenvalues)
    barplot(
      res_pca$eig[, 1],
      xlab      = "Principal Components",
      ylab      = "Eigenvalue",
      col       = "blue",        # scree bars in blue
      border    = "#4B0000",
      cex.main  = 1.4,           # enlarge main title
      cex.lab   = 1.3,           # enlarge axis labels
      cex.axis  = 1.2,           # enlarge axis tick labels
      main      = "Eigenvalues (Scree Plot)"
    )
    
  } else if (input$pca_choice == "var") {
    # Variables factor map
    plot.PCA(
      res_pca,
      choix         = "var",
      col.var       = "#4B0000",
      col.quanti.sup = "#0000FF",
      label         = "all",
      title         = "",
      cex.lab       = 1.3,    # enlarge axis labels
      cex.axis      = 1.2     # enlarge axis tick labels
    )
    
  } else {
    # Individuals factor map with custom legend at bottom
    factoextra::fviz_pca_ind(
      res_pca,
      label       = "ind",      # label individuals
      habillage   = 1,          # color by region
      geom        = "text",
      repel       = TRUE,
      labelsize   = 5,          # larger country names
      col.ind     = "blue",
      col.ind.sup = "red",
      title       = ""
    ) +
      theme(
        legend.position   = "bottom",
        legend.title      = element_text(size = 12, face = "bold"),
        legend.text       = element_text(size = 10),
        legend.key.size   = unit(0.5, "cm"),
        axis.title        = element_text(size = 14),  # enlarge axis titles
        axis.text         = element_text(size = 12),  # enlarge axis tick labels
        plot.margin       = margin(5, 5, 5, 5)
      )
  }
})
