# ============================================================
#  test Shiny app - Analyze of aproduction data
# ============================================================

library(shiny)
library(data.table)
library(plotly)
library(DT)
library(faosws)
library(faoswsUtil)
library(faoswsFlag)
library(faoswsModules)
library(SwsApiClient)

# -- Setting the environment ---------------------------------

if(CheckDebug()){

  library(faoswsModules)
  SETT <- ReadSettings("sws.yml")

  # SetClientFiles(SETT[["certdir"]])
  GetTestEnvironment(baseUrl = SETT[["server"]], token = SETT[["token"]])

}else{
  GetTestEnvironment('https://sws.qa.aws.fao.org:8181', '47f8ef3c-19c0-4305-b4f5-f1171b55e71b')
}

initialiseClient()

# -- Get data ----------------------------------------------

# Domain & dataset
domain_ = "baptiste_test_domain"
dataset_ = "agriculture_production_baptiste"

# Parameters
dataset_geo_areas <- c("250", "380")
dataset_elements <- c("5510")
dataset_items <- swsContext.datasets[[1]]@dimensions$measuredItemCPC@keys
dataset_years <- swsContext.datasets[[1]]@dimensions$timePointYears@keys

# Get the key
key_ <- DatasetKey(
  domain = domain_,
  dataset = dataset_,
  dimensions = list(
    Country = Dimension("geographicAreaM49", dataset_geo_areas),
    Element = Dimension("measuredElement", dataset_elements),
    Item = Dimension("measuredItemCPC", dataset_items),
    Year = Dimension("timePointYears", dataset_years)
  ))

# Get the data
dt <- GetData(key_)

# -- data transformation -------------------------------------------------------

# Convert the years in numeric format
dt[, timePointYears := as.numeric(timePointYears)]

# Add the names of the countries
## Get the geographicAreasM49 codelist
geoAreaCodelist <- getCodelistInfo("geographicAreaM49")$codes
geoAreaCodelist <- geoAreaCodelist[, .(id, label_en)]
setnames(geoAreaCodelist, c("id", "label_en"), c("geographicAreaM49", "geo_label"))
## Left join with dt
dt <- merge(dt, geoAreaCodelist, by = "geographicAreaM49", all.x = TRUE)
## Combine the code and name
dt[, geographicAreaM49 := paste0(geo_label," (", geographicAreaM49, ")")]
dt[, geo_label := NULL]

# Add the names of the items
## Get the measuredItemCPC codelist
measuredItemCPCCodelist <- getCodelistInfo("measuredItemCPC")$codes
measuredItemCPCCodelist <- measuredItemCPCCodelist[, .(id, label_en)]
setnames(measuredItemCPCCodelist, c("id", "label_en"), c("measuredItemCPC", "item_label"))
# Left join with dt
dt <- merge(dt, measuredItemCPCCodelist, by = "measuredItemCPC", all.x = TRUE)
# Combine the code and name
dt[, measuredItemCPC := paste0(item_label," (", measuredItemCPC, ")")]
dt[, item_label := NULL]

# Add the name of the elements
## Get the measuredElement codelist
measuredElementCodelist <- getCodelistInfo("measuredElement")$codes
measuredElementCodelist <- measuredElementCodelist[, .(id, label_en, unit)]
setnames(measuredElementCodelist, c("id", "label_en", "unit"), c("measuredElement", "element_label", "unit"))
# Left join with dt
dt <- merge(dt, measuredElementCodelist, by = "measuredElement", all.x = TRUE)
# Combine the code and name
dt[, measuredElement := paste0(element_label, " (", unit,") (", measuredElement, ")")]
dt[, element_label := NULL]
dt[, unit := NULL]

# dt <- fread("data.csv")

# Get the columns data
areas   <- sort(unique(dt$geographicAreaM49))
items   <- sort(unique(dt$measuredItemCPC))
flags   <- sort(unique(dt$flagObservationStatus))

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Roboto:ital,wght@0,100..900;1,100..900&display=swap');
 
    * { box-sizing: border-box; margin: 0; padding: 0; }
 
    body {
      background: #0d0f14;
      color: #e8e2d5;
      font-family: 'DM Mono', monospace;
      min-height: 100vh;
    }
 
    /* ── header ── */
    .app-header {
      background: linear-gradient(135deg, #1a1d24 0%, #0d0f14 100%);
      border-bottom: 1px solid #2a2e3a;
      padding: 24px 36px 20px;
      display: flex;
      align-items: baseline;
      gap: 16px;
    }
    .app-header h1 {
      font-family: 'Syne', sans-serif;
      font-size: 26px;
      font-weight: 800;
      color: #f0c060;
      letter-spacing: -0.5px;
    }
    .app-header span {
      font-size: 11px;
      color: #555b6e;
      letter-spacing: 2px;
      text-transform: uppercase;
    }
 
    /* ── layout ── */
    .main-grid {
      display: grid;
      grid-template-columns: 240px 1fr;
      gap: 0;
      min-height: calc(100vh - 70px);
    }
 
    /* ── sidebar ── */
    .sidebar-panel {
      background: #13161e;
      border-right: 1px solid #1e2230;
      padding: 28px 20px;
      display: flex;
      flex-direction: column;
      gap: 24px;
    }
    .filter-label {
      font-size: 10px;
      color: #f0c060;
      letter-spacing: 2.5px;
      text-transform: uppercase;
      margin-bottom: 8px;
      font-weight: 500;
    }
    .form-control, .selectize-input {
      background: #1a1d26 !important;
      border: 1px solid #2a2e3a !important;
      border-radius: 6px !important;
      color: #e8e2d5 !important;
      font-family: 'DM Mono', monospace !important;
      font-size: 12px !important;
    }
    .selectize-dropdown {
      background: #1a1d26 !important;
      border: 1px solid #2a2e3a !important;
      color: #e8e2d5 !important;
      font-family: 'DM Mono', monospace !important;
      font-size: 12px !important;
    }
    .selectize-dropdown .option:hover,
    .selectize-dropdown .active { background: #f0c06022 !important; }
 
    .irs--shiny .irs-bar { background: #f0c060 !important; border-top: 1px solid #f0c060 !important; border-bottom: 1px solid #f0c060 !important; }
    .irs--shiny .irs-handle { background: #f0c060 !important; border: 1px solid #c9a040 !important; }
    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: #f0c060 !important; color: #0d0f14 !important; font-size: 10px !important; }
    .irs--shiny .irs-line { background: #2a2e3a !important; border: none !important; }
    .irs--shiny .irs-grid-text { color: #555b6e !important; font-size: 9px !important; }
 
    .reset-btn {
      background: transparent;
      border: 1px solid #f0c060;
      color: #f0c060;
      font-family: 'DM Mono', monospace;
      font-size: 11px;
      letter-spacing: 1.5px;
      text-transform: uppercase;
      padding: 8px 14px;
      border-radius: 6px;
      cursor: pointer;
      width: 100%;
      transition: background 0.15s;
      margin-top: 4px;
    }
    .reset-btn:hover { background: #f0c06018; }
 
    /* ── content area ── */
    .content-panel {
      padding: 28px 32px;
      display: flex;
      flex-direction: column;
      gap: 28px;
    }
 
    /* ── stat strip ── */
    .stat-strip {
      display: flex;
      gap: 16px;
    }
    .stat-card {
      background: #13161e;
      border: 1px solid #1e2230;
      border-radius: 8px;
      padding: 14px 20px;
      flex: 1;
    }
    .stat-card .s-val {
      font-family: 'Syne', sans-serif;
      font-size: 22px;
      font-weight: 700;
      color: #f0c060;
    }
    .stat-card .s-lbl {
      font-size: 10px;
      color: #555b6e;
      letter-spacing: 1.5px;
      text-transform: uppercase;
      margin-top: 2px;
    }
 
    /* ── plot card ── */
    .plot-card {
      background: #13161e;
      border: 1px solid #1e2230;
      border-radius: 8px;
      padding: 20px 24px 12px;
    }
    .card-title {
      font-family: 'Syne', sans-serif;
      font-size: 13px;
      font-weight: 700;
      color: #e8e2d5;
      letter-spacing: 0.5px;
      margin-bottom: 14px;
      text-transform: uppercase;
    }
 
    /* ── DT table ── */
    .dataTables_wrapper { font-size: 12px; color: #aab0c0; }
    table.dataTable thead th {
      background: #1a1d26 !important;
      color: #f0c060 !important;
      border-bottom: 1px solid #2a2e3a !important;
      font-size: 10px !important;
      letter-spacing: 1.5px !important;
      text-transform: uppercase !important;
      padding: 10px 12px !important;
    }
    table.dataTable tbody tr { background: #13161e !important; }
    table.dataTable tbody tr:hover td { background: #1a1d26 !important; }
    table.dataTable tbody td { border-bottom: 1px solid #1e2230 !important; padding: 8px 12px !important; }
    .dataTables_info, .dataTables_length label, .dataTables_filter label { color: #555b6e !important; font-size: 11px !important; }
    .dataTables_filter input { background: #1a1d26 !important; border: 1px solid #2a2e3a !important; color: #e8e2d5 !important; border-radius: 4px !important; font-size: 11px !important; }
    .paginate_button { color: #aab0c0 !important; background: transparent !important; border: 1px solid #2a2e3a !important; border-radius: 4px !important; margin: 0 2px !important; font-size: 11px !important; }
    .paginate_button.current { background: #f0c06018 !important; color: #f0c060 !important; border-color: #f0c060 !important; }
    .paginate_button:hover { background: #f0c06018 !important; color: #f0c060 !important; }
  "))),
  
  # Header
  div(class = "app-header",
      h1("FAO Data Explorer"),
      span("measured element in tones (5510)")
  ),
  
  # Grid
  div(class = "main-grid",
      
      # ── Sidebar ──
      div(class = "sidebar-panel",
          div(
            div(class = "filter-label", "Geographic area"),
            selectInput("sel_area", NULL,
                        choices  = c("All" = "all", setNames(areas, areas)),
                        selected = "all", multiple = FALSE)
          ),
          div(
            div(class = "filter-label", "CPC Product"),
            selectInput("sel_item", NULL,
                        choices  = c("All" = "all", setNames(items, items)),
                        selected = "all", multiple = FALSE)
          ),
          div(
            div(class = "filter-label", "Period"),
            sliderInput("sl_year", NULL,
                        min = min(dt$timePointYears), max = max(dt$timePointYears),
                        value = c(min(dt$timePointYears), max(dt$timePointYears)),
                        step = 1, sep = "")
          ),
          div(
            div(class = "filter-label", "Flag observation"),
            selectInput("sel_flag", NULL,
                        choices  = c("Tous" = "all", setNames(flags, ifelse(flags == "", "(vide)", flags))),
                        selected = "all", multiple = FALSE)
          ),
          actionButton("btn_reset", "↺  Reset", class = "reset-btn")
      ),
      
      # ── Content ──
      div(class = "content-panel",
          
          # Stats
          div(class = "stat-strip",
              div(class = "stat-card", uiOutput("stat_rows")),
              div(class = "stat-card", uiOutput("stat_sum")),
              div(class = "stat-card", uiOutput("stat_mean"))
          ),
          
          # Plot
          div(class = "plot-card",
              div(class = "card-title", "Change in value"),
              plotlyOutput("plot_ts", height = "320px")
          ),
          
          # Table
          div(class = "plot-card",
              div(class = "card-title", "Filtered data"),
              DTOutput("tbl_data")
          )
      )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered <- reactive({
    d <- dt[timePointYears >= input$sl_year[1] & timePointYears <= input$sl_year[2]]
    if (input$sel_area != "all") d <- d[geographicAreaM49 == input$sel_area]
    if (input$sel_item != "all") d <- d[measuredItemCPC  == input$sel_item]
    if (input$sel_flag != "all") d <- d[flagObservationStatus == input$sel_flag]
    d
  })
  
  # Reset
  observeEvent(input$btn_reset, {
    updateSelectInput(session, "sel_area", selected = "all")
    updateSelectInput(session, "sel_item", selected = "all")
    updateSelectInput(session, "sel_flag", selected = "all")
    updateSliderInput(session, "sl_year",
                      value = c(min(dt$timePointYears), max(dt$timePointYears)))
  })
  
  # ── Stats ──
  output$stat_rows <- renderUI({
    n <- nrow(filtered())
    tagList(div(class = "s-val", n), div(class = "s-lbl", "Observations"))
  })
  output$stat_sum <- renderUI({
    s <- sum(filtered()$Value, na.rm = TRUE)
    tagList(div(class = "s-val", format(s, big.mark = " ", scientific = FALSE)),
            div(class = "s-lbl", "Total value"))
  })
  output$stat_mean <- renderUI({
    m <- mean(filtered()$Value, na.rm = TRUE)
    tagList(div(class = "s-val", format(round(m), big.mark = " ", scientific = FALSE)),
            div(class = "s-lbl", "Mean"))
  })
  
  # ── Plot ──
  output$plot_ts <- renderPlotly({
    d <- filtered()
    if (nrow(d) == 0) {
      return(plot_ly() |>
               layout(
                 paper_bgcolor = "#13161e", plot_bgcolor = "#13161e",
                 annotations = list(text = "No data found", showarrow = FALSE,
                                    font = list(color = "#555b6e", size = 14),
                                    xref = "paper", yref = "paper", x = 0.5, y = 0.5)
               ))
    }
    
    # Group by area + item for multiple lines
    d[, grp := paste0(geographicAreaM49, " · ", measuredItemCPC)]
    
    p <- plot_ly()
    for (g in unique(d$grp)) {
      sub <- d[grp == g][order(timePointYears)]
      p <- add_trace(p, data = sub,
                     x = ~timePointYears, y = ~Value, name = g,
                     type = "scatter", mode = "lines+markers",
                     line    = list(width = 2),
                     marker  = list(size = 7),
                     hovertemplate = paste0(
                       "<b>", g, "</b><br>",
                       "Year : %{x}<br>",
                       "Value : %{y:,.0f}<br>",
                       "<extra></extra>"
                     )
      )
    }
    
    p |> layout(
      paper_bgcolor = "#13161e",
      plot_bgcolor  = "#13161e",
      font    = list(family = "DM Mono, monospace", color = "#aab0c0", size = 11),
      xaxis   = list(title = "Year",
                     gridcolor = "#1e2230", zerolinecolor = "#1e2230",
                     tickfont = list(size = 10)),
      yaxis   = list(title = "Value",
                     gridcolor = "#1e2230", zerolinecolor = "#1e2230",
                     tickfont = list(size = 10)),
      legend  = list(bgcolor = "#0d0f14", bordercolor = "#2a2e3a",
                     borderwidth = 1, font = list(size = 10)),
      margin  = list(l = 50, r = 20, t = 10, b = 40),
      hovermode = "x unified"
    ) |>
      config(displayModeBar = FALSE)
  })
  
  # ── Table ──
  output$tbl_data <- renderDT({
    datatable(filtered(),
              rownames  = FALSE,
              selection = "none",
              options   = list(
                pageLength = 10,
                dom        = "ftip",
                scrollX    = TRUE,
                language   = list(
                  search      = "Search :",
                  info        = "Lines _START_ to _END_ on _TOTAL_",
                  paginate    = list(previous = "‹", `next` = "›"),
                  lengthMenu  = "Display _MENU_ lines"
                )
              )
    )
  })
}

app <- shinyApp(ui = ui, server = server)

tryCatch({
  runApp(app, host = '0.0.0.0', port = 8080)
}, error = function(e) {
  log_msg(paste("ERROR: Failed to start app -", e$message))
  stop(e)
})
