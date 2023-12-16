mpsle_ui <- function(){
  shiny::fluidRow(
    column(width = 9,
           # Result display box
          shinydashboard::box(width = NULL, solidHeader = TRUE,
                              div(
                                style = "display: flex; justify-content: center;",  # Center content horizontally
          tableOutput('sle_stats')
                              ),
          div(
            style = "display: flex; justify-content: center;",  # Center content horizontally
            tableOutput('sle_infection_rate')
            
          ),
          div(
            style = "display: flex; justify-content: center;",  # Center content horizontally
            tableOutput('sle_initial_inoculum')
            
          ),
          div(
            style = "display: flex; justify-content: center;",  # Center content horizontally
            tableOutput('sle_AUDPC')
            
          ),
               tags$br()
           ),
          # plot display box
          shinydashboard::box(width = NULL, solidHeader = TRUE,
                              shiny::plotOutput('sle_basicplot')%>%  shinycssloaders::withSpinner(color="#0dc5c1"),
                              tags$br()
          ),
          # control panel
          shinydashboard::box(width = NULL, 
                              title = "Control Panel for basic plot",
                              shiny::uiOutput('cp_sle')              
                              
          ),
          # model fitted plot display box
          shinydashboard::box(width = NULL, solidHeader = TRUE,
                              shiny::plotOutput('sle_modelplot')%>%  shinycssloaders::withSpinner(color="#0dc5c1"),
                              tags$br()
          ),
          # control panel for model plot
          shinydashboard::box(width = NULL, 
                              title = "Control Panel for model plot",
                              shiny::uiOutput('cp_modplot')
          )
    ),
    column(width = 3,
           # file upload box
           shinydashboard::box(width = NULL, status = "warning",
              shiny::fileInput("file1_sle", "CSV File (upload in csv format)", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
               shiny::checkboxInput("header_sle", "Header", TRUE),
              shiny::uiOutput('var_sle'),
               tags$br(),
               p(
                 class = "text-muted",
                 paste("Note: Upload a csv file from your system by clicking on Browse. you can download model dataset to see the format. Read the instruction to know more"
                 )
               )
           ),
           #Model dataset box
           shinydashboard::box(width = NULL, status = "warning",
               tags$br(),
               p(
                 class = "text-muted",
               paste("Note: Download the dataset here for testing")),
               shiny::uiOutput('data_set_sle'),
               tags$br()


           )
    )
  )
}
