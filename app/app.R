
library(shiny)
library(stargazer)

load("data/models_lists.RData")

ui <- fluidPage(
  titlePanel("Appendix: Regression results"),
  
  tabsetPanel(
    
    tabPanel("Parents' expectations and attitudes",
             tags$h3("France"),
             # FR Attitudes
             sidebarLayout(
               sidebarPanel(
                 selectInput("attitudefr_outcome", "Select outcome:", choices = names(attitudefr_models_list))
               ),
               mainPanel(
                 htmlOutput("attitudefr_table")
               )
             ),
             tags$h3("UK"),
             # UK Attitudes
             sidebarLayout(
               sidebarPanel(
                 selectInput("attitudeuk_outcome", "Select outcome:", choices = names(attitudeuk_models_list))
               ),
               mainPanel(
                 htmlOutput("attitudeuk_table")
               )
             )
    ),
    
    tabPanel("Parents' role modelling",
             tags$h3("France"),
             # FR Modelling
             sidebarLayout(
               sidebarPanel(
                 selectInput("modellingfr_outcome", "Select outcome:", choices = names(modellingfr_models_list))
               ),
               mainPanel(
                 htmlOutput("modellingfr_table")
               )
             ),
             tags$h3("UK"),
             # UK Modelling
             sidebarLayout(
               sidebarPanel(
                 selectInput("modellinguk_outcome", "Select outcome:", choices = names(modellinguk_models_list))
               ),
               mainPanel(
                 htmlOutput("modellinguk_table")
               )
             )
    ),
    
    tabPanel("Children’s access and exposure to family resources",
             tags$h3("France"),
             # FR Resources
             sidebarLayout(
               sidebarPanel(
                 selectInput("resourcesfr_outcome", "Select outcome:", choices = names(resourcesfr_models_list))
               ),
               mainPanel(
                 htmlOutput("resourcesfr_table")
               )
             ),
             tags$h3("UK"),
             # UK Resources
             sidebarLayout(
               sidebarPanel(
                 selectInput("resourcesuk_outcome", "Select outcome:", choices = names(resourcesuk_models_list))
               ),
               mainPanel(
                 htmlOutput("resourcesuk_table")
               )
             )
    )
  )
)


server <- function(input, output){
                   
  output$attitudefr_table <- renderUI({
    selected_attitudefr_models <- attitudefr_models_list[[input$attitudefr_outcome]]
    stargazer_output <- capture.output(
      stargazer(selected_attitudefr_models, type = "html", title = paste("Linear regression results for", input$attitudefr_outcome))
    )
    HTML(paste(stargazer_output, collapse = "\n"))
  })
                   
  output$attitudeuk_table <- renderUI({
    selected_attitudeuk_models <- attitudeuk_models_list[[input$attitudeuk_outcome]]
    stargazer_output <- capture.output(
      stargazer(selected_attitudeuk_models, type = "html", title = paste("Linear regression results for", input$attitudeuk_outcome))
    )
    HTML(paste(stargazer_output, collapse = "\n"))
  })
  
  output$modellingfr_table <- renderUI({
    selected_modellingfr_models <- modellingfr_models_list[[input$modellingfr_outcome]]
    stargazer_output <- capture.output(
      stargazer(selected_modellingfr_models, type = "html", title = paste("Linear regression results for", input$modellingfr_outcome))
    )
    HTML(paste(stargazer_output, collapse = "\n"))
  })
  
  output$modellinguk_table <- renderUI({
    selected_modellinguk_models <- modellinguk_models_list[[input$modellinguk_outcome]]
    stargazer_output <- capture.output(
      stargazer(selected_modellinguk_models, type = "html", title = paste("Linear regression results for", input$modellinguk_outcome))
    )
    HTML(paste(stargazer_output, collapse = "\n"))
  })
  
  output$resourcesfr_table <- renderUI({
    selected_resourcesfr_models <- resourcesfr_models_list[[input$resourcesfr_outcome]]
    stargazer_output <- capture.output(
      stargazer(selected_resourcesfr_models, type = "html", title = paste("Linear regression results for", input$resourcesfr_outcome))
    )
    HTML(paste(stargazer_output, collapse = "\n"))
  })
  
  output$resourcesuk_table <- renderUI({
    selected_resourcesuk_models <- resourcesuk_models_list[[input$resourcesuk_outcome]]
    stargazer_output <- capture.output(
      stargazer(selected_resourcesuk_models, type = "html", title = paste("Linear regression results for", input$resourcesuk_outcome))
    )
    HTML(paste(stargazer_output, collapse = "\n"))
  })
}

shinyApp(ui = ui, server = server)
