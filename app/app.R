
library(shiny)
library(stargazer)

load("./data/models_lists.RData")

# Replace with your actual model list
modellingfr_models_list <- list(
  "doctor2m3" = list(em1a, em2a, em3a, em4a),
  "nappies2m3" = list(em1b, em2b, em3b, em4b),
  "laundry2m3" = list(em1c, em2c, em3c, em4c),
  "night2m3" = list(em1d, em2d, em3d, em4d),
  "clean2m3" = list(em1e, em2e, em3e, em4e),
  "cook2m3" = list(em1f, em2f, em3f, em4f),
  "diy2m3" = list(em1g, em2g, em3g, em4g)
)

modellinguk_models_list <- list(
  "doctor9m3" = list(mm1a, mm2a, mm3a, mm4a),
  "nappies9m3" = list(mm1b, mm2b, mm3b, mm4b),
  "laundry9m3" = list(mm1c, mm2c, mm3c, mm4c),
  "night9m3" = list(mm1d, mm2d, mm3d, mm4d),
  "clean9m3" = list(mm1e, mm2e, mm3e, mm4e),
  "cook9m3" = list(mm1f, mm2f, mm3f, mm4f),
  "diy9m3" = list(mm1g, mm2g, mm3g, mm4g)
)

attitudefr_models_list <- list(
  "socialsuccess2m" = list(em1h, em2h, em3h, em4h),
  "lovelife2m" = list(em1i, em2i, em3i, em4i),
  "interestingjob2m" = list(em1j, em2j, em3j, em4j),
  "passion2m" = list(em1k, em2k, em3k, em4k),
  "calmlife2m" = list(em1l, em2l, em3l, em4l),
  "bigfamily2m" = list(em1m, em2m, em3m, em4m),
  "lotsoffriends2m" = list(em1n, em2n, em3n, em4n),
  "fairerworld2m" = list(em1o, em2o, em3o, em4o),
  "goodhealth2m" = list(em1p, em2p, em3p, em4p)
  )

attitudeuk_models_list <- list(
  "independence3y" = list(mm1h, mm2h, mm3h, mm4h),
  "obedience3y" = list(mm1i, mm2i, mm3i, mm4i),
  "negotiation3y" = list(mm1j, mm2j, mm3j, mm4j),
  "respectelders3y" = list(mm1k, mm2k, mm3k, mm4k),
  "dowellatschool3y" = list(mm1l, mm2l, mm3l, mm4l),
  "instillreligiousvalues3y" = list(mm1m, mm2m, mm3m, mm4m),
  "bewellliked3y" = list(mm1n, mm2n, mm3n, mm4n),
  "thinkforself3y" = list(mm1o, mm2o, mm3o, mm4o),
  "workhard3y" = list(mm1p, mm2p, mm3p, mm4p),
  "helpothers3y" = list(mm1q, mm2q, mm3q, mm4q),
  "obeyparents3y" = list(mm1r, mm2r, mm3r, mm4r),
  "qualityreligiousvalues3y" = list(mm1s, mm2s, mm3s, mm4s)
  )

resourcesfr_models_list <- list(
  "frpaint3y" = list(em1q, em2q, em3q, em4q),
  "frread3y" = list(em1r, em2r, em3r, em4r),
  "music3y" = list(em1s, em2s, em3s, em4s),
  "readplus3y" = list(em1t, em2t, em3t, em4t),
  "frcounting3y" = list(em1u, em2u, em3u, em4u),
  "writing3y" = list(em1v, em2v, em3v, em4v),
  "puzzle3y" = list(em1w, em2w, em3w, em4w),
  "anyactivity3y" = list(em1x, em2x, em3x, em4x),
  "swimming3y" = list(em1y, em2y, em3y, em4y),
  "gymnastics3y" = list(em1z, em2z, em3z, em4z),
  "circus3y" = list(em1aa, em2aa, em3aa, em4aa),
  "sportsinit3y" = list(em1ab, em2ab, em3ab, em4ab),
  "musicclass3y" = list(em1ac, em2ac, em3ac, em4ac),
  "danceclass3y" = list(em1ad, em2ad, em3ad, em4ad),
  "visualarts3y" = list(em1ae, em2ae, em3ae, em4ae),
  "horseriding3y" = list(em1af, em2af, em3af, em4af)
)

resourcesuk_models_list <- list(
  "read3y" = list(mm1t, mm2t, mm3t, mm4t),
  "library3y" = list(mm1u, mm2u, mm3u, mm4u),
  "counting3y" = list(mm1v, mm2v, mm3v, mm4v),
  "songs3y" = list(mm1w, mm2w, mm3w, mm4w),
  "alphabet3y" = list(mm1x, mm2x, mm3x, mm4x),
  "paint3y" = list(mm1y, mm2y, mm3y, mm4y),
  "physical3y" = list(mm1z, mm2z, mm3z, mm4z),
  "read5y" = list(mm1aa, mm2aa, mm3aa, mm4aa),
  "stories5y" = list(mm1ab, mm2ab, mm3ab, mm4ab),
  "songs5y" = list(mm1ac, mm2ac, mm3ac, mm4ac),
  "paint5y" = list(mm1ad, mm2ad, mm3ad, mm4ad),
  "physical5y" = list(mm1ae, mm2ae, mm3ae, mm4ae),
  "indoor5y" = list(mm1af, mm2af, mm3af, mm4af),
  "park5y" = list(mm1ag, mm2ag, mm3ag, mm4ag)
)


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
