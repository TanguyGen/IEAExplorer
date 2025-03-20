library(shiny)
library(leaflet)
library(sf)
library(tidyr)
library(DT)
library(ggplot2)
library(gridExtra)
library(icesTAF)
library(tibble)
library(shinycssloaders)
library(shinyjs)

# Load necessary data
ecoregions <- readRDS("Data/Ecoregions.RData")
url_github<- "https://raw.githubusercontent.com/ices-eg/WGINOR/refs/heads/main/TAF_ATAC/output/tables.Rdata"
load(url(url_github))

# Extract unique categories excluding NAs for UI creation
category_choices <- unique(info$category)
category_choices <- category_choices[!is.na(category_choices)]

# Define UI
ui <- fluidPage(
  tags$head(includeCSS("www/styles.css")),
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Manrope:wght@300&family=Roboto:wght@100&display=swap"),
    tags$link(rel = "stylesheet", href = "https://cdn.leafletjs.com/leaflet/v0.7.7/leaflet.css"),
    tags$style(type = "text/css", "
      #graphs-container {
        overflow-y: auto;
        border: 1px solid #ccc;
        transition: height 0.5s ease;  /* Smooth transition when resizing */
      }
    ")
  ),
  withTags({
    div(
      class = "header",
      checked = NA,
      h1(
        img(
          width = 200,
          src = "ICES_logo.png", # Ensure the image is located in www/ directory
          class = "topleft"
        )
      ),
      h2("WGINOR Shiny")
    )
  }),
  navbarPage(
    "Menu",
    id = "menu",
    tabPanel("Ecoregions and Variables", sidebarLayout(
      sidebarPanel(
        leafletOutput("map", height = 400),
        selectInput("selected_shapefile", "Choose ecoregions", choices = unique(ecoregions$Ecoregion)),
        br(),
        checkboxGroupInput("selected_categories", "Select Categories:", choices = category_choices, selected = category_choices)
      ),
      mainPanel(
        DTOutput("Variables"),
        br(),
        actionButton("continue", "Continue", class = "btn-success"),
        br(),
        br(),
        textOutput("lastUpdate")
      )
    ))
  )
)


server <- function(input, output, session) {
  # Track original indices when filtering for selected categories
  filtered_data <- reactive({
    req(input$selected_categories)
    
    # Add original indices as an attribute but do not select it
    info_filtered <- info %>%
      mutate(original_index = row_number()) %>% # This preserves original indices
      filter(category %in% input$selected_categories & !is.na(category)) %>% 
      mutate(selectable = '<input type="checkbox" class="dt-checkbox" />')
    
    # Save original indices in a reactive value
    rv$original_indices <- info_filtered$original_index
    
    # Select columns for DT without showing original_index
    info_filtered %>% select(selectable, FullName, unit, category)  
  })
  
  rv <- reactiveValues(DataVariables = NULL, original_indices = NULL)
  
  output$lastUpdate <- renderText({
    paste("Last updated on:", extract_github_commit_date())
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = ecoregions, fillColor = ~factor(Ecoregion), color = "black", opacity = 0.7, fillOpacity = 0.3)
  })
  
  output$Variables <- renderDT({
    datatable(
      filtered_data(),
      escape = FALSE,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        autoWidth = TRUE,
        ordering = FALSE,
        initComplete = JS(
          "function(settings, json) {",
          "$('<button id=\"toggle-select-all\" class=\"btn-primary\" style=\"position: absolute; left: 0;margin-left: 10px;\">Select All/Unselect All</button>').prependTo($(settings.nTableWrapper).find('.dataTables_filter'));",
          "var selectedIDs = {};",
          "$('#toggle-select-all').click(function() {",
          "var checked = !(document.querySelector('#toggle-select-all').dataset.checked === 'true');",
          "document.querySelector('#toggle-select-all').dataset.checked = checked;",
          "$('.dt-checkbox').each(function(index) {",
          "if (checked) { $(this).prop('checked', true); selectedIDs[index] = true; }",
          "else { $(this).prop('checked', false); delete selectedIDs[index]; }",
          "});",
          "Shiny.setInputValue('selected_ids', Object.keys(selectedIDs));",
          "});",
          
          "$('.dt-checkbox').on('change', function() {",
          "var index = $(this).closest('tr').index();",
          "if ($(this).is(':checked')) {",
          "selectedIDs[index] = true;",
          "} else {",
          "delete selectedIDs[index];",
          "}",
          "Shiny.setInputValue('selected_ids', Object.keys(selectedIDs));",
          "});",
          
          "settings.oApi._fnCallbackReg(settings, 'aoDrawCallback', function(settings) {",
          "$('.dt-checkbox').each(function(index) {",
          "if (selectedIDs[index]) {",
          "$(this).prop('checked', true);",
          "} else {",
          "$(this).prop('checked', false);",
          "}",
          "});",
          "});",
          "}"
        ),
        columnDefs = list(
          list(orderable = FALSE, className = 'select-checkbox', targets = 0)
        )
      )
    )
  })
  
  observeEvent(input$continue, {
    req(input$selected_ids)
    
    # Convert selected row indices in DT table to original indices stored in rv
    selected_indices <- rv$original_indices[as.numeric(input$selected_ids) + 1]
    
    rv$DataVariables <- table.all[selected_indices]
    
    appendTab(inputId = "menu",
              tabPanel(
                "Graphs",
                plotOutput("Graphs")
              )
    )
    appendTab(inputId = "menu",
              tabPanel(
                "Download",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("year_selector", "Select Year:", choices = seq(from = 1980, to = 2023, by = 1), selected = 2023)
                  ),
                  mainPanel(
                    fluidRow(
                      downloadButton("ZIP", label = "Download ZIP file")
                    )
                  )
                )
              )
    )
    
    updateTabsetPanel(session, "menu", selected = "Graphs")
  })
  
  output$Graphs <- renderPlot({
    selected_data <- rv$DataVariables
    
    if (!is.null(selected_data) && length(selected_data) > 0) {
      plotlist <- list()
      for (i in seq_along(selected_data)) {
        data_item <- selected_data[[i]]
        j <- rv$original_indices[as.numeric(input$selected_ids)[i] + 1] # Original index mapping
        
        if (is_tibble(data_item) && ncol(data_item) >= 2) {
          plotlist[[i]] <- ggATAC(result = data_item) +
            xlim(c(1980, NA)) +
            ggtitle(paste0(info$FullName[j],
                           "\nData transformation: ", info$transformation[j],
                           "\nAutoregressive process: AR(", info$AR[j], ")")) +
            ylab(paste0("Units: ", info$unit[j])) +
            theme(plot.title = element_text(size = 16),
                  axis.title = element_text(size = 14, face = "bold"),
                  axis.text = element_text(size = 12))
          
          if (info$transformation[j] == TRUE) {
            plotlist[[i]] <- plotlist[[i]] + ylim(c(0, NA))
          }
        } else {
          plotlist[[i]] <- ggplot() +
            ggtitle(paste0(info$FullName[j],
                           "\nData transformation: ", info$transformation[j],
                           "\nAutoregressive process: AR(", info$AR[j], ")")) +
            theme(plot.title = element_text(size = 15),
                  axis.title = element_text(size = 14, face = "bold"),
                  axis.text = element_text(size = 12))
        }
      }
      
      gridExtra::grid.arrange(grobs = plotlist, ncol = 2)
    }
  })
  
  output$Download <- downloadHandler(
    filename = paste0("Data_", format(Sys.time(), "%s"), ".zip"),
    content = function(file) {
      zip(file, files = c(), extras = '-j')
    }
  )
}

shinyApp(ui = ui, server = server)
