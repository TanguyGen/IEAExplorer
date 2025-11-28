#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import markdown
#' @import tibble
#' @import leaflet
#' @importFrom DT datatable renderDataTable
#' @importFrom shinycssloaders withSpinner
#' @importFrom gridExtra grid.arrange
#' @importFrom tibble is_tibble
#' @importFrom data.table as.data.table rbindlist is.data.table
#' @importFrom xlsx write.xlsx
#' @noRd
#'
app_server <- function(input, output, session) {
  data <- reactiveValues(table.all = NULL, info = NULL)
  rv <- reactiveValues(DataVariables = NULL, selected_ids = NULL)
  Time_limit <- reactiveValues(min_year = NULL, max_year = NULL)
  
  selected_country <- map_panel_server(input, output, session)
  
  observeEvent(selected_country(), {
    
    region <- selected_country()
    
    # defer heavy work
    later::later(function() {
      
      if (region == "Norwegian Sea"){
        data_source <- url("https://raw.githubusercontent.com/ices-eg/WGINOR/refs/heads/main/TAF_ATAC/output/tables.Rdata")
      } else if (region == "Icelandic Waters"){
        data_source <- "inst/app/www/Iceland/tables.Rdata"
      }
      
      try({
        e <- new.env()
        load(data_source, envir = e)
        data$info <- e$info
        data$table.all <- e$table.all
      }, silent = TRUE)
      
    }, delay = 0)
    
  })
  
  # Update category checkbox choices
  observeEvent(data$info, {
    updateCheckboxGroupInput(session, "selected_categories",
                             choices = unique(na.omit(data$info$Category)),
                             selected = unique(na.omit(data$info$Category)))
  })
  
  observe({
    categories <- unique(data$info$Category)
    categories <- categories[!is.na(categories) & categories != ""]
    
    if (length(categories) == 0) {
      output$category_selector <- renderUI({ NULL })  # hide
    } else {
      output$category_selector <- renderUI({
        checkboxGroupInput(
          "selected_categories",
          "Select Categories:",
          choices = categories,
          selected = NULL
        )
      })
    }
  })
  
  # Reactive filtered info by selected categories
  filtered_data <- reactive({
    req(input$selected_categories)   # ensures at least 1
    
    filtered <- dplyr::filter(
      data$info,
      Category %in% input$selected_categories,
      !is.na(Category)
    )
    
    rv$selected_ids <- filtered$ID
    
    filtered %>%
      dplyr::select(FullName, Unit, Category, Description, Source) %>%
      dplyr::rename(`Full name` = FullName)
  })
  
  output$lastUpdate <- renderText({ 
    region <- selected_country()
    req(length(region)>0)
    extract_github_commit_date(region) 
    })
  
  output$Variables <- DT::renderDataTable({
    if(length(input$selected_categories>0)){
      dat <- filtered_data() %>% tibble::add_column(" " = "", .before = 1)
    }else{
      dat <- data.frame(FullName=NA, Unit=NA, Category=NA, Description=NA, Source=NA) %>% tibble::add_column(" " = "", .before = 1)
    }
    
    DT::datatable(
      dat,
      escape = FALSE,
      extensions = "Select",
      selection = 'none',
      options = list(
        select = list(style = "multi"),
        initComplete = DT::JS("initTooltipJS"),
        pageLength = 15,
        autoWidth = FALSE,
        ordering = FALSE,
        rownames = FALSE,
        columnDefs = list(
          list(visible = FALSE, targets = c(0, 4, 5)),
          list(orderable = TRUE, className = 'select-checkbox', targets = 1)
        )
      )
    )
  }, server = FALSE)
  
  # Update selected data and year range limits when rows are selected
  observe({
    req(input$Variables_rows_selected)
    selected_ids <- rv$selected_ids[as.integer(input$Variables_rows_selected)]
    rv$DataVariables <- data$table.all[selected_ids]
    
    years <- unlist(lapply(rv$DataVariables, function(df) df$year))
    Time_limit$min_year <- min(years)
    Time_limit$max_year <- max(years)
  })
  
  # Insert tabs on continue button
  observeEvent(input$continue, {
    req(input$Variables_rows_selected)
    
    updateTabsetPanel(session, "menu", selected = "Graphs")
  })
  
  
  # Update slider input dynamically
  observeEvent(list(Time_limit$min_year, Time_limit$max_year), {
    req(Time_limit$min_year, Time_limit$max_year)
    updateSliderInput(session, "yearRange",
                      min = Time_limit$min_year,
                      max = Time_limit$max_year,
                      value = c(Time_limit$min_year, Time_limit$max_year))
  })
  
  # Render graphs based on selected data
  output$Graphs <- renderPlot({
    req(rv$DataVariables)
    selected_data <- rv$DataVariables
    req(selected_data)
    
    width <- session$clientData$output_Graphs_width
    title_size <- max(ceiling(width / 50), 16)
    axistitle_size <- max(ceiling(width / 60), 12)
    text_size <- max(ceiling(width / 80), 12)
    info <- data$info
    
    plotlist <- lapply(names(selected_data), function(id) {
      df <- selected_data[[id]]
      info_item <- info[info$ID == id, ]
      
      if (tibble::is_tibble(df) && ncol(df) >= 2) {
        ggATAC(result = df, width = width) +
          xlim(c(1980, NA)) +
          ggtitle(stringr::str_wrap(info_item$FullName, 30)) +
          ylab(info_item$Unit) +
          xlab("Year") +
          theme(
            plot.title = element_text(size = title_size, hjust = 0.5),
            axis.title = element_text(size = axistitle_size, face = "bold"),
            axis.text = element_text(size = text_size)
          )
      } else {
        ggplot() +
          ggtitle(stringr::str_wrap(info_item$FullName, 30)) +
          theme(
            plot.title = element_text(size = title_size, hjust = 0.5),
            axis.title = element_text(size = axistitle_size, face = "bold"),
            axis.text = element_text(size = text_size)
          ) +
          xlab("Year")
      }
    })
    
    if (length(plotlist) > 0) {
      do.call(gridExtra::grid.arrange, list(grobs = plotlist, nrow = ceiling(length(plotlist) / 2)))
    }
  }, height = reactive({
    width <- session$clientData$output_Graphs_width
    n <- length(rv$DataVariables)
    if (is.null(width) || n == 0) return(400)
    width / 3 * ceiling(n / 2)
  }))
  
  # Download handler for exporting filtered data to Excel
  output$CSV <- downloadHandler(
    filename = function() {
      paste0("Data_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xlsx") #Give a unique name to the file
    },
    content = function(file) {
      years <- as.numeric(input$yearRange)
      selected_data <- rv$DataVariables
      info<-data$info
      modified_data <- list()
      metadata <- list()
      
      for (id in names(selected_data)) {
        data_item <- selected_data[[id]]
        if (!data.table::is.data.table(data_item)) {
          data_item <- data.table::as.data.table(data_item)
        }
        data_item <- filter(data_item, year >= years[1] &
                              year <= years[2]) # Filter data by selected year range
        data_item[, Variable := info$FullName[info$ID == id]]
        modified_data[[id]] <- data_item
        metadata[[id]] <- info[info$ID == id, ]
      }
      
      # Combine and export to Excel with metadata
      Outputtable <- data.table::rbindlist(modified_data)
      Metadatatable <- data.table::rbindlist(metadata)
      xlsx::write.xlsx(
        Metadatatable,
        file = file,
        sheetName = "Metadata",
        row.names = FALSE
      )
      xlsx::write.xlsx(
        Outputtable,
        file = file,
        sheetName = "Table",
        append = TRUE,
        row.names = FALSE
      )
    }
  )
  
  # Render a table summarizing selected variables' metadata
  output$selectedVarsTable <- DT::renderDataTable({
    req(rv$DataVariables)
    data$info %>%
      filter(ID %in% names(rv$DataVariables)) %>%
      select(FullName, Unit, Description, Source) %>%
      rename(
        `Full name` = FullName,
        `Units` = Unit,
        `Description` = Description,
        `Source` = Source
      )
  })
  
  #----TUTORIAL----
  
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Continue",
                                               "prevLabel"="Back",
                                               "skipLabel"="Skip the tutorial"),
                       events = list("onbeforechange" = readCallback("switchTabs")))
  )
}
