#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import markdown
#' @importFrom DT datatable renderDataTable
#' @importFrom shinycssloaders withSpinner
#' @importFrom gridExtra grid.arrange
#' @importFrom tibble is_tibble
#' @importFrom data.table as.data.table rbindlist is.data.table
#' @importFrom xlsx write.xlsx
#' @noRd
app_server <- function(input, output, session) {
  # Load necessary data
  url_github <- "https://raw.githubusercontent.com/ices-eg/WGINOR/refs/heads/main/TAF_ATAC/output/tables.Rdata"
  load(url(url_github))
  
  # Extract unique categories excluding NAs for UI creation
  category_choices <- unique(info$Category)
  category_choices <- category_choices[!is.na(category_choices)]
  
  # Track original IDs when filtering for selected categories
  filtered_data <- reactive({
    req(input$selected_categories)
    
    info_filtered <- info %>%
      filter(Category %in% input$selected_categories &
               !is.na(Category))
    
    # Save IDs in a reactive value
    rv$selected_ids <- info_filtered$ID
    
    # Select columns for DT
    info_filtered %>% select(FullName, Unit, Category, Description, Source) %>%
      rename(`Full name` = FullName)
  })
  
  rv <- reactiveValues(
    DataVariables = NULL,
    selected_ids = NULL,
    tabsCreated = FALSE
  )
  
  output$lastUpdate <- renderText({
    paste("Last updated on:", extract_github_commit_date())
  })
  
  output$Variables <- renderDataTable(server = FALSE, {
    datatable(
      filtered_data(),
      escape = FALSE,
      extensions = "Select",
      selection = 'none',
      options = list(
        select = list(style = "multi"),
        initComplete = CodeJS,
        pageLength = 15,
        autoWidth = TRUE,
        ordering = FALSE,
        columnDefs = list(list(
          visible = FALSE, targets = c(4, 5)
        ))
      )
    )
  })
  
  observeEvent(input$continue, {
    req(input$Variables_rows_selected)
    
    # Use selected indices to get corresponding IDs
    selected_ids <- rv$selected_ids[as.integer(input$Variables_rows_selected)]
    rv$DataVariables <- table.all[selected_ids]  # Use IDs for data extraction
    
    Years <- unlist(lapply(rv$DataVariables, function(df)
      df$year))
    min_year <- min(Years)
    max_year <- max(Years)
    
    if (!rv$tabsCreated) {
      rv$tabsCreated <- TRUE
      
      insertTab(
        inputId = "menu",
        tabPanel("Graphs", fluidRow(
          column(
            12,
            plotOutput("Graphs", height = "600px") %>% withSpinner(
              type = 8,
              image = "rotating_fish.gif",
              id = "spinner-custom"
            )
          )
        )),
        target = "How to read the graph?",
        position = "before"
      )
      
      insertTab(
        inputId = "menu",
        tabPanel("Download", sidebarLayout(
          sidebarPanel(
            sliderInput(
              "yearRange",
              "Select Year Range:",
              min = min_year,
              max = max_year,
              value = c(min_year, max_year),
              step = 1,
              sep = "",
              width = "100%"
            ),
            width = "100%"
          ),
          mainPanel(fluidRow(
            downloadButton("CSV", label = "Download xlsx file")
          ))
        )),
        target = "How to read the graph?",
        position = "before"
      )
    }
    updateTabsetPanel(session, "menu", selected = "Graphs")
  })
  
  output$Graphs <- renderPlot({
    session$sendCustomMessage(type = "toggle-spinner", message = "hide")
    selected_data <- rv$DataVariables
    width <- session$clientData$output_Graphs_width
    title_size <- max(ceiling(width / 50), 16)
    axistitle_size <- max(ceiling(width / 60), 12)
    text_size <- max(ceiling(width / 80), 12)
    
    if (!is.null(selected_data) && length(selected_data) > 0) {
      plotlist <- lapply(names(selected_data), function(id) {
        data_item <- selected_data[[id]]
        info_item <- info[info$ID == id, ]
        
        if (is_tibble(data_item) && ncol(data_item) >= 2) {
          ggATAC(result = data_item, width = width) +
            xlim(c(1980, NA)) +
            ggtitle(paste0(info_item$FullName)) +
            ylab(paste0("Units: ", info_item$Unit)) +
            theme(
              plot.title = element_text(size = title_size),
              axis.title = element_text(size = axistitle_size, face = "bold"),
              axis.text = element_text(size = text_size)
            )
        } else {
          ggplot() +
            ggtitle(paste0(info_item$FullName)) +
            theme(
              plot.title = element_text(size = title_size),
              axis.title = element_text(size = axistitle_size, face = "bold"),
              axis.text = element_text(size = text_size)
            )
        }
      })
      
      if (!is.null(plotlist) && length(plotlist) > 0) {
        n <- length(plotlist)
        numrow <- ceiling(n / 2)
        do.call(gridExtra::grid.arrange,
                list(grobs = plotlist, nrow = numrow))
      }
    } else {
      print("No data selected for plotting.")
    }
  }, height = reactive({
    width <- session$clientData$output_Graphs_width
    num_plots <- length(rv$DataVariables)
    if (is.null(width))
      return(400)
    if (num_plots == 0)
      return(400)
    
    plot_height <- width * 1 / 3
    total_height <- plot_height * ceiling(num_plots / 2)
    return(total_height)
  }))
  
  output$CSV <- downloadHandler(
    filename = function() {
      paste0("Data_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xlsx")
    },
    content = function(file) {
      # Retrieve selected years
      years <- as.numeric(input$yearRange)
      
      selected_data <- rv$DataVariables
      
      # Initialize an empty list to store modified data.frames
      modified_data <- list()
      metadata <- list()
      
      for (id in names(selected_data)) {
        data_item <- selected_data[[id]]
        
        # Ensure the data_item is a data.table
        if (!is.data.table(data_item)) {
          data_item <- as.data.table(data_item)
        }
        
        # Filter data based on year range
        data_item <- data_item %>%
          filter(year >= years[1] & year <= years[2])
        
        # Add a column to the data.table
        data_item[, Variable := info$FullName[info$ID == id]]
        
        # Append the modified data.table to the list
        modified_data[[id]] <- data_item
        metadata[[id]] <- info[info$ID == id, ]
      }
      
      # Bind all data.tables row-wise and write to CSV
      Outputtable <- rbindlist(modified_data)
      Metadatatable <- rbindlist(metadata)
      write.xlsx(
        Metadatatable,
        file = file,
        sheetName = "Metadata",
        row.names = FALSE
      )
      write.xlsx(
        Outputtable,
        file = file,
        sheetName = "Table",
        append = TRUE,
        row.names = FALSE
      )
    }
  )
}
