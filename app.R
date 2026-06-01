## ─────────────────────────────────────────────────────────────────────────────
##  MACRO EXPLORER · Interactive Shiny App
##  40 Years · 30 Countries · 5 Tabs
##
##  Run with:  shiny::runApp("app.R")
##  Requires:  shiny, plotly, dplyr, tidyr, scales, DT
##
##  Place all CSV files in the same folder as this app.R, OR update DATA_PATH.
## ─────────────────────────────────────────────────────────────────────────────

library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(scales)
library(DT)

## ── DATA PATH (edit if CSVs live elsewhere) ───────────────────────────────
DATA_PATH <- "."   # same folder as app.R

## ── LOAD DATA ─────────────────────────────────────────────────────────────
macro   <- read.csv(file.path(DATA_PATH, "macro_indicators.csv"),   stringsAsFactors = FALSE)
monthly <- read.csv(file.path(DATA_PATH, "macro_monthly.csv"),      stringsAsFactors = FALSE)
returns <- read.csv(file.path(DATA_PATH, "asset_returns.csv"),      stringsAsFactors = FALSE)
cors    <- read.csv(file.path(DATA_PATH, "correlations.csv"),       stringsAsFactors = FALSE)
rec     <- read.csv(file.path(DATA_PATH, "recession_episodes.csv"), stringsAsFactors = FALSE)

monthly$date <- as.Date(monthly$date)

## ── DERIVED / LOOKUP OBJECTS ──────────────────────────────────────────────
all_countries  <- sort(unique(macro$country))
all_regions    <- sort(unique(macro$region))
all_macro_vars <- c(
  "GDP Growth (%)"          = "gdp_growth",
  "Inflation (%)"           = "inflation",
  "Policy Rate (%)"         = "policy_rate",
  "Unemployment (%)"        = "unemployment",
  "Current Account (% GDP)" = "current_account",
  "Debt / GDP (%)"          = "debt_to_gdp",
  "FX Change (%)"           = "fx_change",
  "Biz Confidence"          = "biz_confidence",
  "Financial Stress"        = "fin_stress_idx"
)
monthly_vars <- c(
  "CPI YoY (%)"             = "cpi_yoy",
  "Policy Rate (%)"         = "policy_rate",
  "Unemployment (%)"        = "unemployment",
  "PMI"                     = "pmi",
  "Yield Spread 10Y-2Y (pp)"= "yield_spread_10_2",
  "VIX"                     = "vix",
  "Credit Spread (bps)"     = "credit_spread_bps",
  "Oil Change YoY (%)"      = "oil_change_yoy"
)

# Shock event palette
shock_colours <- c(
  "Asian Crisis 1997"                = "#E07B54",
  "Dot-com bust 2001"                = "#D4A843",
  "Global Financial Crisis 2008-09"  = "#C0392B",
  "European Debt Crisis 2010-12"     = "#8E44AD",
  "COVID-19 2020"                    = "#2980B9",
  "Post-COVID inflation 2021-22"     = "#27AE60",
  "Ukraine War / Energy shock 2022"  = "#E74C3C"
)

region_colours <- c(
  NorthAmerica  = "#1A6B8A", Europe        = "#2E86AB",
  EasternEurope = "#5BA4CF", Asia          = "#E84855",
  AsiaPacific   = "#F97068", LatAm         = "#F4A261",
  MiddleEast    = "#E76F51", Africa        = "#C77DFF"
)

## ── SHARED PLOTLY LAYOUT ──────────────────────────────────────────────────
dark_layout <- function(p, title = NULL, xlab = NULL, ylab = NULL) {
  p %>% layout(
    title  = list(text = title, font = list(color = "#E6EDF3", size = 16), x = 0.02),
    xaxis  = list(title = xlab, color = "#8B949E", gridcolor = "#21262D",
                  zerolinecolor = "#484F58"),
    yaxis  = list(title = ylab, color = "#8B949E", gridcolor = "#21262D",
                  zerolinecolor = "#484F58"),
    paper_bgcolor = "#0D1117",
    plot_bgcolor  = "#161B22",
    font          = list(color = "#C9D1D9"),
    legend        = list(bgcolor = "#161B22", bordercolor = "#21262D",
                         font = list(color = "#8B949E")),
    hoverlabel    = list(bgcolor = "#21262D", bordercolor = "#484F58",
                         font = list(color = "#E6EDF3"))
  )
}

## ══════════════════════════════════════════════════════════════════════════
##  UI
## ══════════════════════════════════════════════════════════════════════════
ui <- fluidPage(
  title = "Macro Explorer",
  tags$head(tags$style(HTML("
    body, .container-fluid { background:#0D1117; color:#C9D1D9; }
    .nav-tabs > li > a { background:#161B22; color:#8B949E; border-color:#21262D; }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:hover { background:#21262D; color:#E6EDF3; border-color:#21262D; }
    .well { background:#161B22; border-color:#21262D; }
    label { color:#8B949E; font-size:12px; font-weight:500; }
    select, input[type='number'] {
      background:#21262D !important; color:#C9D1D9 !important;
      border:1px solid #30363D !important; border-radius:4px;
    }
    .selectize-input { background:#21262D !important; color:#C9D1D9 !important;
                       border-color:#30363D !important; }
    .selectize-dropdown { background:#21262D !important; color:#C9D1D9 !important; }
    .irs--shiny .irs-bar { background:#2E86AB; }
    .irs--shiny .irs-handle { background:#2E86AB; border-color:#2E86AB; }
    .irs--shiny .irs-line, .irs--shiny .irs-grid-pol { background:#21262D; }
    .irs--shiny .irs-from, .irs--shiny .irs-to,
    .irs--shiny .irs-single { background:#2E86AB; }
    h4 { color:#E6EDF3; font-weight:600; border-bottom:1px solid #21262D;
         padding-bottom:6px; margin-bottom:14px; }
    .app-header { padding:18px 0 10px; border-bottom:1px solid #21262D; margin-bottom:18px; }
    .app-header h2 { color:#E6EDF3; font-weight:700; margin:0; }
    .app-header p  { color:#484F58; margin:4px 0 0; font-size:13px; }
    hr { border-color:#21262D; }
  "))),

  ## Header
  div(class = "app-header",
    div(style = "max-width:1200px; margin:0 auto; padding:0 16px;",
      h2("Macro Explorer"),
      p("40 Years · 30 Countries · Synthetic dataset calibrated to IMF, FRED, BIS")
    )
  ),

  div(style = "max-width:1200px; margin:0 auto; padding:0 16px;",
    tabsetPanel(id = "tabs",

      ## ── TAB 1: Country Trends ──────────────────────────────────────────
      tabPanel("Country Trends",
        br(),
        fluidRow(
          column(3,
            wellPanel(
              h4("Settings"),
              selectInput("ct_countries", "Countries (up to 8)",
                          choices = all_countries,
                          selected = c("USA","Germany","China","Brazil"),
                          multiple = TRUE, selectize = TRUE),
              selectInput("ct_var", "Macro Variable",
                          choices = all_macro_vars, selected = "gdp_growth"),
              sliderInput("ct_years", "Year Range",
                          min = 1985, max = 2024, value = c(1985, 2024), sep = ""),
              checkboxInput("ct_recession", "Shade recession episodes", value = FALSE),
              checkboxInput("ct_smooth", "Add smoothed trend", value = FALSE)
            )
          ),
          column(9,
            plotlyOutput("ct_plot", height = "480px"),
            br(),
            plotlyOutput("ct_distribution", height = "240px")
          )
        )
      ),

      ## ── TAB 2: Yield Curve & Recession ─────────────────────────────────
      tabPanel("Yield Curve Signal",
        br(),
        fluidRow(
          column(3,
            wellPanel(
              h4("Settings"),
              sliderInput("yc_years", "Year Range",
                          min = 1985, max = 2024, value = c(1985, 2024), sep = ""),
              selectInput("yc_overlay", "Secondary Series",
                          choices = monthly_vars,
                          selected = "vix"),
              checkboxInput("yc_shade", "Shade inverted curve periods", value = TRUE)
            )
          ),
          column(9,
            plotlyOutput("yc_plot", height = "380px"),
            br(),
            plotlyOutput("yc_secondary", height = "220px")
          )
        )
      ),

      ## ── TAB 3: Asset Returns ───────────────────────────────────────────
      tabPanel("Asset Returns",
        br(),
        fluidRow(
          column(3,
            wellPanel(
              h4("Settings"),
              selectInput("ar_region", "Region Filter",
                          choices = c("All", all_regions), selected = "All"),
              selectInput("ar_dev", "Development Level",
                          choices = c("All", "developed", "emerging"), selected = "All"),
              selectInput("ar_view", "Chart Type",
                          choices = c("Box Plot" = "box",
                                      "Violin Plot" = "violin",
                                      "Time Series" = "ts"),
                          selected = "box"),
              sliderInput("ar_years", "Year Range",
                          min = 1985, max = 2024, value = c(1985, 2024), sep = "")
            )
          ),
          column(9,
            plotlyOutput("ar_plot", height = "480px")
          )
        )
      ),

      ## ── TAB 4: Cross-Country Scatter ───────────────────────────────────
      tabPanel("Macro Scatter",
        br(),
        fluidRow(
          column(3,
            wellPanel(
              h4("Settings"),
              selectInput("sc_x", "X Axis",
                          choices = all_macro_vars, selected = "inflation"),
              selectInput("sc_y", "Y Axis",
                          choices = all_macro_vars, selected = "gdp_growth"),
              selectInput("sc_color", "Colour By",
                          choices = c("Region" = "region",
                                      "Dev Level" = "dev_level",
                                      "Shock Event" = "shock_event"),
                          selected = "region"),
              selectInput("sc_size", "Size By",
                          choices = c("None" = "none",
                                      "Debt/GDP" = "debt_to_gdp",
                                      "Unemployment" = "unemployment",
                                      "Financial Stress" = "fin_stress_idx"),
                          selected = "none"),
              sliderInput("sc_years", "Year Range",
                          min = 1985, max = 2024, value = c(2000, 2024), sep = ""),
              checkboxInput("sc_trend", "Show regression line", value = TRUE),
              checkboxInput("sc_label", "Label outliers", value = TRUE)
            )
          ),
          column(9,
            plotlyOutput("sc_plot", height = "520px")
          )
        )
      ),

      ## ── TAB 5: Correlation Heatmap ─────────────────────────────────────
      tabPanel("Correlation Matrix",
        br(),
        fluidRow(
          column(3,
            wellPanel(
              h4("Settings"),
              selectInput("cm_dev", "Market Type",
                          choices = c("Both", "developed", "emerging"), selected = "Both"),
              sliderInput("cm_years", "Decade / Year Range",
                          min = 1985, max = 2024, value = c(1985, 2024), sep = ""),
              hr(),
              p(style = "color:#484F58; font-size:11px;",
                "Correlations between macro variables and annual asset returns,
                averaged across all country-years in the selected window.")
            )
          ),
          column(9,
            plotlyOutput("cm_plot", height = "480px")
          )
        )
      )

    ) # end tabsetPanel
  ) # end container
)

## ══════════════════════════════════════════════════════════════════════════
##  SERVER
## ══════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  ## ── helpers ──────────────────────────────────────────────────────────────
  asset_colours <- c(
    equities    = "#2E86AB", bonds       = "#82E0AA",
    real_estate = "#5BA4CF", commodities = "#F4A261", cash = "#8B949E"
  )

  # Shock year reference lines for monthly plots
  shock_vlines <- function(p, df) {
    shocks <- df %>% filter(!is.na(shock_event) & shock_event != "") %>%
      group_by(shock_event) %>% slice_min(date, n = 1)
    for (i in seq_len(nrow(shocks))) {
      p <- p %>% add_segments(
        x = shocks$date[i], xend = shocks$date[i],
        y = -Inf, yend = Inf,
        line = list(color = "#484F58", dash = "dot", width = 1),
        showlegend = FALSE, hoverinfo = "none"
      )
    }
    p
  }

  ## ── TAB 1: Country Trends ────────────────────────────────────────────────
  ct_data <- reactive({
    req(input$ct_countries)
    macro %>%
      filter(country %in% input$ct_countries,
             year >= input$ct_years[1], year <= input$ct_years[2])
  })

  output$ct_plot <- renderPlotly({
    df  <- ct_data()
    var <- input$ct_var
    var_label <- names(all_macro_vars)[all_macro_vars == var]

    p <- plot_ly()

    countries <- unique(df$country)
    pal <- colorRampPalette(c("#2E86AB","#E84855","#F4A261","#82E0AA",
                               "#8E44AD","#27AE60","#E07B54","#5BA4CF"))(length(countries))

    for (i in seq_along(countries)) {
      d <- filter(df, country == countries[i])
      p <- p %>% add_trace(
        data = d, x = ~year, y = as.formula(paste0("~", var)),
        type = "scatter", mode = "lines+markers",
        name = countries[i],
        line    = list(color = pal[i], width = 2),
        marker  = list(color = pal[i], size = 5),
        hovertemplate = paste0("<b>", countries[i], "</b><br>",
                               "Year: %{x}<br>", var_label, ": %{y:.2f}<extra></extra>")
      )
      if (input$ct_smooth) {
        sm <- loess(as.formula(paste0(var, " ~ year")), data = d, span = 0.5)
        d$smoothed <- predict(sm)
        p <- p %>% add_trace(
          data = d, x = ~year, y = ~smoothed,
          type = "scatter", mode = "lines",
          name = paste(countries[i], "(trend)"),
          line = list(color = pal[i], width = 1, dash = "dot"),
          showlegend = FALSE, hoverinfo = "skip"
        )
      }
    }

    # Recession shading
    if (input$ct_recession) {
      rec_us <- rec %>% filter(country == "USA")
      for (i in seq_len(nrow(rec_us))) {
        p <- p %>% layout(shapes = c(
          lapply(seq_len(nrow(rec_us)), function(j) list(
            type = "rect", xref = "x", yref = "paper",
            x0 = rec_us$rec_start[j], x1 = rec_us$rec_end[j],
            y0 = 0, y1 = 1,
            fillcolor = "#C0392B", opacity = 0.08, line = list(width = 0)
          ))
        ))
      }
    }

    p %>% dark_layout(
      title = var_label,
      xlab  = "Year",
      ylab  = var_label
    ) %>% layout(hovermode = "x unified")
  })

  output$ct_distribution <- renderPlotly({
    df  <- ct_data()
    var <- input$ct_var
    var_label <- names(all_macro_vars)[all_macro_vars == var]

    p <- plot_ly()
    countries <- unique(df$country)
    pal <- colorRampPalette(c("#2E86AB","#E84855","#F4A261","#82E0AA",
                               "#8E44AD","#27AE60","#E07B54","#5BA4CF"))(length(countries))

    for (i in seq_along(countries)) {
      d <- filter(df, country == countries[i])
      p <- p %>% add_trace(
        data = d, x = as.formula(paste0("~", var)),
        type = "histogram", name = countries[i],
        marker = list(color = pal[i], opacity = 0.55),
        nbinsx = 20
      )
    }
    p %>% dark_layout(
      title = paste("Distribution of", var_label),
      xlab  = var_label, ylab  = "Count"
    ) %>% layout(barmode = "overlay")
  })

  ## ── TAB 2: Yield Curve ───────────────────────────────────────────────────
  yc_data <- reactive({
    monthly %>%
      filter(year >= input$yc_years[1], year <= input$yc_years[2])
  })

  output$yc_plot <- renderPlotly({
    df <- yc_data()

    p <- plot_ly(df, x = ~date)

    # Shading: inversion (negative spread)
    if (input$yc_shade) {
      p <- p %>%
        add_ribbons(ymin = ~pmin(yield_spread_10_2, 0), ymax = 0,
                    fillcolor = "rgba(200,57,43,0.25)", line = list(width = 0),
                    name = "Inverted (recession risk)", hoverinfo = "skip") %>%
        add_ribbons(ymin = 0, ymax = ~pmax(yield_spread_10_2, 0),
                    fillcolor = "rgba(39,174,96,0.2)", line = list(width = 0),
                    name = "Normal curve", hoverinfo = "skip")
    }

    p <- p %>%
      add_lines(y = ~yield_spread_10_2,
                line = list(color = "#E6EDF3", width = 1.5),
                name = "10Y–2Y Spread",
                hovertemplate = "Date: %{x}<br>Spread: %{y:.2f} pp<extra></extra>") %>%
      add_lines(y = ~rep(0, nrow(df)),
                line = list(color = "#484F58", width = 0.8, dash = "dash"),
                showlegend = FALSE, hoverinfo = "skip")

    p %>% dark_layout(
      title = "US Treasury Yield Curve: 10Y – 2Y Spread",
      xlab  = NULL, ylab = "Spread (percentage points)"
    )
  })

  output$yc_secondary <- renderPlotly({
    df      <- yc_data()
    var     <- input$yc_overlay
    vlabel  <- names(monthly_vars)[monthly_vars == var]

    plot_ly(df, x = ~date,
            y = as.formula(paste0("~", var)),
            type = "scatter", mode = "lines",
            line = list(color = "#F4A261", width = 1.5),
            name = vlabel,
            hovertemplate = paste0("Date: %{x}<br>", vlabel, ": %{y:.2f}<extra></extra>")) %>%
      dark_layout(title = vlabel, xlab = NULL, ylab = vlabel)
  })

  ## ── TAB 3: Asset Returns ─────────────────────────────────────────────────
  ar_data <- reactive({
    df <- returns %>%
      filter(year >= input$ar_years[1], year <= input$ar_years[2])
    if (input$ar_region != "All") df <- filter(df, region == input$ar_region)
    if (input$ar_dev    != "All") df <- filter(df, dev_level == input$ar_dev)
    df
  })

  output$ar_plot <- renderPlotly({
    df   <- ar_data()
    view <- input$ar_view

    assets <- c("equities","bonds","real_estate","commodities","cash")
    asset_labels <- c("Equities","Bonds","Real Estate","Commodities","Cash")
    pal <- unname(asset_colours[assets])

    if (view == "ts") {
      df_long <- df %>%
        pivot_longer(cols = all_of(assets), names_to = "asset", values_to = "return") %>%
        group_by(year, asset) %>%
        summarise(avg_return = mean(return, na.rm = TRUE), .groups = "drop") %>%
        mutate(asset_label = asset_labels[match(asset, assets)])

      p <- plot_ly()
      for (i in seq_along(assets)) {
        d <- filter(df_long, asset == assets[i])
        p <- p %>% add_trace(
          data = d, x = ~year, y = ~avg_return,
          type = "scatter", mode = "lines+markers",
          name = asset_labels[i],
          line   = list(color = pal[i], width = 2),
          marker = list(color = pal[i], size = 4),
          hovertemplate = paste0("<b>", asset_labels[i], "</b><br>",
                                 "Year: %{x}<br>Avg Return: %{y:.1f}%<extra></extra>")
        )
      }
      p %>% dark_layout(title = "Average Annual Asset Returns",
                        xlab  = "Year", ylab = "Return (%)")

    } else {
      df_long <- df %>%
        pivot_longer(cols = all_of(assets), names_to = "asset", values_to = "return") %>%
        mutate(asset_label = factor(asset_labels[match(asset, assets)],
                                    levels = asset_labels))

      p <- plot_ly()
      for (i in seq_along(assets)) {
        d <- filter(df_long, asset == assets[i])
        if (view == "box") {
          p <- p %>% add_trace(
            data = d, y = ~return, x = ~asset_label,
            type = "box", name = asset_labels[i],
            fillcolor = paste0(substr(pal[i],1,7),"55"),
            line   = list(color = pal[i]),
            marker = list(color = pal[i], size = 3, opacity = 0.3),
            boxpoints = "all", jitter = 0.3, pointpos = 0,
            hovertemplate = paste0("<b>", asset_labels[i], "</b><br>Return: %{y:.1f}%<extra></extra>")
          )
        } else {
          p <- p %>% add_trace(
            data = d, y = ~return, x = ~asset_label,
            type = "violin", name = asset_labels[i],
            fillcolor = paste0(substr(pal[i],1,7),"55"),
            line   = list(color = pal[i]),
            box    = list(visible = TRUE),
            meanline = list(visible = TRUE, color = "#E6EDF3"),
            points = FALSE
          )
        }
      }
      p %>% dark_layout(
        title = if (view == "box") "Asset Return Distributions"
                else "Asset Return Densities",
        xlab = NULL, ylab = "Annual Return (%)"
      ) %>% layout(showlegend = FALSE)
    }
  })

  ## ── TAB 4: Cross-Country Scatter ─────────────────────────────────────────
  sc_data <- reactive({
    macro %>%
      filter(year >= input$sc_years[1], year <= input$sc_years[2],
             !is.na(.data[[input$sc_x]]), !is.na(.data[[input$sc_y]])) %>%
      mutate(shock_label = ifelse(is.na(shock_event) | shock_event == "",
                                  "Normal", shock_event))
  })

  output$sc_plot <- renderPlotly({
    df      <- sc_data()
    xvar    <- input$sc_x
    yvar    <- input$sc_y
    cvar    <- input$sc_color
    svar    <- input$sc_size
    xlabel  <- names(all_macro_vars)[all_macro_vars == xvar]
    ylabel  <- names(all_macro_vars)[all_macro_vars == yvar]

    # Size mapping
    if (svar == "none") {
      df$dot_size <- 7
    } else {
      rng <- range(df[[svar]], na.rm = TRUE)
      df$dot_size <- rescale(df[[svar]], to = c(4, 20), from = rng)
      df$dot_size[is.na(df$dot_size)] <- 6
    }

    # Colour mapping
    colour_vals <- switch(cvar,
      region      = region_colours[df$region],
      dev_level   = ifelse(df$dev_level == "developed", "#2E86AB", "#E84855"),
      shock_event = {
        sc <- shock_colours
        sc["Normal"] <- "#484F58"
        unname(sc[df$shock_label])
      }
    )

    colour_vals[is.na(colour_vals)] <- "#484F58"

    # Outlier labels (top/bottom 2% by y)
    labels <- rep("", nrow(df))
    if (input$sc_label) {
      q <- quantile(df[[yvar]], c(0.02, 0.98), na.rm = TRUE)
      is_out <- df[[yvar]] < q[1] | df[[yvar]] > q[2]
      labels[is_out] <- df$country[is_out]
    }

    p <- plot_ly(df,
      x = as.formula(paste0("~", xvar)),
      y = as.formula(paste0("~", yvar)),
      type = "scatter", mode = "markers+text",
      marker = list(
        size    = ~dot_size,
        color   = colour_vals,
        opacity = 0.65,
        line    = list(width = 0.5, color = "#0D1117")
      ),
      text = labels,
      textposition = "top center",
      textfont = list(color = "#8B949E", size = 9),
      customdata = df[, c("country","year","region","dev_level")],
      hovertemplate = paste0(
        "<b>%{customdata[0]}</b> (%{customdata[1]})<br>",
        xlabel, ": %{x:.2f}<br>",
        ylabel, ": %{y:.2f}<br>",
        "Region: %{customdata[2]}<extra></extra>"
      )
    )

    # Regression line
    if (input$sc_trend) {
      fit <- tryCatch(
        lm(as.formula(paste0(yvar, " ~ ", xvar)), data = df),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        xseq <- seq(min(df[[xvar]], na.rm=TRUE), max(df[[xvar]], na.rm=TRUE), length.out=80)
        ypred <- predict(fit, newdata = setNames(data.frame(xseq), xvar))
        p <- p %>% add_lines(
          x = xseq, y = ypred,
          line = list(color = "#E6EDF3", width = 1.5, dash = "dot"),
          name = "OLS trend", hoverinfo = "skip", showlegend = FALSE
        )
      }
    }

    p %>% dark_layout(title = paste(ylabel, "vs", xlabel),
                      xlab = xlabel, ylab = ylabel)
  })

  ## ── TAB 5: Correlation Matrix ─────────────────────────────────────────────
  cm_data <- reactive({
    df <- cors %>%
      filter(year >= input$cm_years[1], year <= input$cm_years[2])
    if (input$cm_dev != "Both") df <- filter(df, dev_level == input$cm_dev)
    df %>%
      mutate(
        asset_label = recode(asset_class,
          equities    = "Equities",    bonds       = "Bonds",
          real_estate = "Real Estate", commodities = "Commodities",
          cash        = "Cash"),
        macro_label = recode(macro_var,
          gdp_growth     = "GDP Growth",   inflation    = "Inflation",
          policy_rate    = "Policy Rate",  unemployment = "Unemployment",
          fin_stress     = "Fin. Stress",  fin_stress_idx = "Fin. Stress",
          current_account= "Curr. Account",debt_to_gdp = "Debt/GDP",
          fx_change      = "FX Change",    biz_confidence = "Biz Confidence")
      ) %>%
      group_by(macro_label, asset_label) %>%
      summarise(avg_cor = mean(correlation, na.rm = TRUE), .groups = "drop")
  })

  output$cm_plot <- renderPlotly({
    df <- cm_data()
    if (nrow(df) == 0) return(plotly_empty())

    # Pivot to matrix
    mat <- df %>%
      pivot_wider(names_from = asset_label, values_from = avg_cor) %>%


    # Use tidyr to get matrix form
    wide <- df %>%
      pivot_wider(names_from = asset_label, values_from = avg_cor,
                  values_fill = 0)
    mat_vals <- as.matrix(wide[, -1])
    rownames(mat_vals) <- wide$macro_label

    # Build annotation text
    ann_text <- matrix(sprintf("%.2f", mat_vals), nrow = nrow(mat_vals))

    plot_ly(
      z         = mat_vals,
      x         = colnames(mat_vals),
      y         = rownames(mat_vals),
      type      = "heatmap",
      colorscale = list(
        list(0,   "#C0392B"),
        list(0.3, "#922B21"),
        list(0.5, "#2C2C2C"),
        list(0.7, "#1A4A6A"),
        list(1,   "#2E86AB")
      ),
      zmin = -0.6, zmax = 0.6,
      colorbar = list(
        title = "Correlation",
        tickfont = list(color = "#8B949E"),
        titlefont = list(color = "#C9D1D9")
      ),
      text = ann_text,
      texttemplate = "%{text}",
      textfont = list(color = "#E6EDF3", size = 13),
      hovertemplate = "Macro: %{y}<br>Asset: %{x}<br>Avg Corr: %{z:.3f}<extra></extra>"
    ) %>%
    dark_layout(
      title = "Macro Variable – Asset Class Correlation",
      xlab  = NULL, ylab = NULL
    ) %>%
    layout(
      yaxis = list(autorange = "reversed",
                   color = "#8B949E", gridcolor = "#21262D")
    )
  })

}

## ══════════════════════════════════════════════════════════════════════════
shinyApp(ui, server)
