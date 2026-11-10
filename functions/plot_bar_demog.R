plot_bar_demog <- function(data, x, y, demog) {
  
  # Get underlying data for calculations
  raw_data <- if (inherits(data, "SharedData")) {
    data$origData()
  } else {
    data
  }
  
  colours <- if (demog == "simd") {
    c("#0062AD", "#0078D4", "#3393DD", "#80BCEA", "#B3D7F2")
  } else if (demog == "sex") {
    c("#3F3685", "#9B4393")
  } else if (demog == "age_grp_2") {
    c("#6B5C85", "#0078D4", "#1E7F84")
  }
  
  text_label1 <- if (demog == "simd") {
    "Deprivation Quintile"
  } else if (demog == "sex") {
    "Gender"
  } else if (demog == "age_grp_2") {
    "Age Group"
  }
  
  text_label2 <- if (y == "rate") {
    "Rate per 10,000 Population: %{y:.1f}<extra></extra>"
  } else if (y == "perc_met") {
    "%{y}%<extra></extra>"
  }
  
  bscols(
    widths = 12,
    div(
      style = css(
        width = "98%",
        height = "350px",
        marginBottom = "-20px"
      ),
      plot_ly(
        data,
        x = as.formula(paste0("~", x)),
        y = as.formula(paste0("~", y)),
        meta = ~fy,
        hovertext = as.formula(paste0("~", demog)),
        type = "bar",
        color = as.formula(paste0("~", demog)),
        colors = colours,
        hovertemplate = paste0(
          "%{x} <br>",
          "%{meta} <br>",
          text_label1, ": %{hovertext} <br>",
          text_label2
        )
      ) %>%
        layout(
          clickmode = "none",
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0.5,
            y = 100,
            xanchor = "center"
          ),
          margin = list(l = -5, b = 10, t = 40),
          xaxis = list(
            title = "",
            showline = FALSE,
            linecolor = "#b3b3b3",
            categoryorder = "array",
            categoryarray = c(
              "Scotland",
              setdiff(unique(raw_data[[x]]), "Scotland")
            ),
            tickangle = -45,
            titlefont = list(size = 16)
          ),
          yaxis = list(
            title = if (y == "rate") {
              "Rate per 10,000 Population"
            } else {
              ""
            },
            linecolor = "#b3b3b3",
            titlefont = list(size = 16)
          )
        ) %>%
        config(
          displayModeBar = TRUE,
          doubleClick = FALSE,
          modeBarButtonsToRemove = list(
            "select2d", "lasso2d", "zoomIn2d",
            "zoomOut2d", "autoScale2d", "toggleSpikelines",
            "hoverCompareCartesian", "hoverClosestCartesian",
            "toImage"
          ),
          displaylogo = FALSE,
          editable = FALSE
        )
    )
  )
}
