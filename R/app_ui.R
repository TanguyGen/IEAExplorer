#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  golem_add_external_resources()# Load external styles and scripts
  
  fluidPage(
    # HTML <head> content: CSS, JS, and fonts
    tags$head(
      includeCSS(app_sys("app/www/styles.css")), # Load custom CSS
      tags$script(src = "www/initComplete.js"), # Load custom JavaScript
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700&display=swap")  # Add nice font
    ),
    
    withTags({
      div(class = "header",
          checked = NA,
          h1(), #Let some space for visual
          h2("IEA Explorer")) #Title
    }),
    
    # Navigation bar layout with multiple tabs
    navbarPage(
      "Menu", #Navigation bar title
      id = "menu",
      
      # --- First Tab: Variables ---
      tabPanel(
        "Variables",
        sidebarLayout(
          sidebarPanel(
            img(
              # Map image shown in sidebar
              src = "www/Map_Norwegian_Sea.svg",
              height = 400,
              width = "100%",
              class = "responsive-img"
            ),
            br(),
            checkboxGroupInput("selected_categories", "Select Categories:", choices = NULL)  # Dynamic checkboxes for selecting categories
          ),
          mainPanel(
            DT::DTOutput("Variables"), # Add interactive table of variables
            br(),
            actionButton("continue", "Continue", class = "btn-success", width = 200), #Button Continue
            br(),
            br(),
            textOutput("lastUpdate")  # Displays the last update date
          )
        )
      ),
      
      # --- Last Tab: Info ---
      tabPanel(
        "Info",
        fluidRow(
          htmltools::includeMarkdown("inst/app/www/ATAC_description.Rmd"), # Include Markdown description
          br(),
          br(),
          img(
            # Series interpretation image
            id = "img11",
            src = "www/ATAC_series_interpretation2023.png",
            height = 600,
            width = "100%",
            class = "responsive-img"
          ),
          br(),
          br()
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  
  tags$head(favicon(),
            bundle_resources(path = app_sys("app/www"), app_title = "ieaexplorer"))
}
