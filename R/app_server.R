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
#' @importFrom DT datatable renderDataTable
#' @importFrom shinycssloaders withSpinner
#' @importFrom gridExtra grid.arrange
#' @importFrom tibble is_tibble
#' @importFrom data.table as.data.table rbindlist is.data.table
#' @importFrom xlsx write.xlsx
#' @noRd
#'
app_server <- function(input, output, session) {
  data <- reactiveValues(table.all = NULL, info = NULL) #Create a reactive value to store the data from the RData file
  
  observe({
    #Load the data
    if (is.null(data$table.all)) {
      #If the data is not already loaded
      e <- new.env()#Create a new environment
      url_github <- "https://raw.githubusercontent.com/ices-eg/WGINOR/refs/heads/main/TAF_ATAC/output/tables.Rdata"
      load(url(url_github), envir = e) #Load the RData file from the URL into the environment
      
      loaded_objs <- ls(envir = e) # Get all the variables from the environment
      
      #Check that there are 2 object in the environment
      if (length(loaded_objs) != 2) {
        showNotification("Default RData must contain two objects.", type = "error")
        return()
      }
      
      data$info <- e$info #Get the info object from the environment
      data$table.all <- e$table.all #Get the table.all object from the environment
      
    } else {
      return() #Avoid loading the data in repeat
    }
  })
  
  #Update the checkbox category options with the categories from the info object
  updateCheckboxGroupInput(
    session,
    "selected_categories",
    choices = unique(stats::na.omit(info$Category)),
    selected = unique(stats::na.omit(info$Category))
  )
  
  
  rv <- reactiveValues(
    DataVariables = NULL,
    selected_ids = NULL,
    tabsCreated = FALSE
  ) #Create a reactive value to store the selected data
  
  # Observe the selected categories and update the table accordingly
  filtered_data <- reactive({
    info_filtered <- filter(data$info,
                            Category %in% input$selected_categories &
                              !is.na(Category)) #Select only the data with the chosen category
    rv$selected_ids <- info_filtered$ID #Save the selected IDs
    select(info_filtered,
           FullName,
           Unit,
           Category,
           Description,
           Source) %>% rename(`Full name` = FullName)
  })
  
  output$lastUpdate <- renderText({
    extract_github_commit_date()
  }) #Print the time the RData file has been last updated
  
  output$Variables <- DT::renderDataTable(server = FALSE, {
    #Display the filtered by category data table
    
    data <- filtered_data()
    
    data <- data %>%
      add_column(" " = "", .before = 1) #Add an empty column to display the checkboxes
    
    DT::datatable(
      data,
      escape = FALSE,
      extensions = "Select",
      #Enable selection
      selection = 'none',
      #Disable DT row selection display
      options = list(
        select = list(style = "multi"),
        #Allow multi-selection
        initComplete = DT::JS("initTooltipJS"),
        #Add JavaScript to add select all button and tooltip
        pageLength = 15,
        #Max results per page
        autoWidth = TRUE,
        ordering = FALSE,
        rownames = FALSE,
        columnDefs = list(
          list(visible = FALSE, targets = c(0, 4, 5)),
          #Only display columns Checkboxes, Full Name, Units and Description
          list(
            orderable = TRUE,
            className = 'select-checkbox',
            targets = 1
          ) #Make chckboxes clickabble
        )
      )
    )
  })
  
  observeEvent(input$continue, {
    #After selecting the wanted variables and clicking continue
    req(input$Variables_rows_selected) #Check that the user has selected at least one variable*
    selected_ids <- rv$selected_ids[as.integer(input$Variables_rows_selected)] # Get the row indices of the corresponding selected variables
    rv$DataVariables <- data$table.all[selected_ids] #Get the data from the table.all object using the selected IDs
    
    # Determine the min and max year across selected variables
    Years <- unlist(lapply(rv$DataVariables, function(df)
      df$year))
    min_year <- min(Years)
    max_year <- max(Years)
    
    # If tabs haven't been added yet, add them once
    if (!rv$tabsCreated) {
      rv$tabsCreated <- TRUE
      # Insert "Graphs" tab
      insertTab(
        "menu",
        tabPanel("Graphs", fluidRow(
          column(12, shinycssloaders::withSpinner(
            plotOutput("Graphs", height = "600px"), type = 6
          ))  #Add the graphs and a loader before
        )),
        target = "Info",
        position = "before"
      ) #Put it between the tabs Variables and the tab Info
      
      # Insert "Download" tab
      insertTab(
        "menu",
        tabPanel(
          "Download",
          fluidRow(
            column(
              width = 2,
              br(),
              downloadButton(
                "CSV",
                label = "Download",
                class = "btn btn-lg btn-primary",
                style = "width: 100%"
              ) #Add a download button
            ),
            column(
              width = 10,
              sliderInput(
                #Add a slider to select the years
                "yearRange",
                "Select Year Range:",
                min = min_year,
                max = max_year,
                value = c(min_year, max_year),
                step = 1,
                sep = "",
                width = "100%"
              )
            )
          ),
          br(),
          fluidRow(column(
            width = 8,
            offset = 2,
            DT::dataTableOutput("selectedVarsTable") #Display the selected variables
          ))
        ),
        target = "Info",
        position = "before"
      )
    }
    updateTabsetPanel(session, "menu", selected = "Graphs") #Switch to the Graphs tab
  })
  
  output$Graphs <- renderPlot({
    #Display the graphs
    selected_data <- rv$DataVariables #Get the selected data
    width <- session$clientData$output_Graphs_width #Get the width of the user's display
    title_size <- max(ceiling(width / 50), 16) #Set the title size depending on the display
    axistitle_size <- max(ceiling(width / 60), 12) #Set the axis title size depending on the display
    text_size <- max(ceiling(width / 80), 12) #Set the text size depending on the display
    
    info <- data$info
    
    # If selected data is not empty, generate plots
    if (!is.null(selected_data) && length(selected_data) > 0) {
      plotlist <- lapply(names(selected_data), function(id) {
        data_item <- selected_data[[id]]
        info_item <- info[info$ID == id, ]
        
        if (tibble::is_tibble(data_item) && ncol(data_item) >= 2) {
          ggATAC(result = data_item, width = width) +  # Use ggATAC function to generate the main plot
            xlim(c(1980, NA)) +
            ggtitle(stringr::str_wrap(info_item$FullName, width = 30)) +
            ylab(info_item$Unit) +
            xlab("Year") +
            theme(
              plot.title = element_text(size = title_size, hjust = 0.5),
              axis.title = element_text(size = axistitle_size, face = "bold"),
              axis.text = element_text(size = text_size)
            )
        } else {
          # Fallback for missing or malformed data
          ggplot() +
            ggtitle(stringr::str_wrap(info_item$FullName, width = 30)) +
            theme(
              plot.title = element_text(size = title_size, hjust = 0.5),
              axis.title = element_text(size = axistitle_size, face = "bold"),
              axis.text = element_text(size = text_size)
            ) +
            xlab("Year")
        }
      })
      # Arrange the generated plots in a grid layout
      if (!is.null(plotlist) && length(plotlist) > 0) {
        n <- length(plotlist)
        numrow <- ceiling(n / 2)
        do.call(gridExtra::grid.arrange,
                list(grobs = plotlist, nrow = numrow))
      }
    }
  }, height = reactive({
    # Dynamic plot height based on number of plots and available width
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
  
  # Download handler for exporting filtered data to Excel
  output$CSV <- downloadHandler(
    filename = function() {
      paste0("Data_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xlsx") #Give a unique name to the file
    },
    content = function(file) {
      years <- as.numeric(input$yearRange)
      selected_data <- rv$DataVariables
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
}
