
library(shiny)
library(shinythemes)


ui <- fluidPage(
  titlePanel(h2("Pro-Sequential Organ Failure Assessment (Pro-SOFA) Score Calculator", style = "font-weight: bold;")),
  tags$style(HTML("
  
    body {
        background-color: #f0f0f0;
      }
    #calculate {
      background-color: #527a85; 
      color: white; 
      border: 2px solid #344d54; 
      border-radius: 5px; 
      padding: 10px 20px;
      font-weight: bold;
    }

    #calculate:hover {
      background-color: #344d54; 
      border-color: #344d54; 
    }


    .sidebar-panel {
      background-color:   #e2e2e2;
      padding: 10px;
      border: 1px solid #344d54;
      border-radius: 5px;
      margin-bottom: 10px;
    }
    
  
    .well-panel {
      background-color:  #f0f2f5; 
      border: 1px solid #344d54;
      border-radius: 5px;
      padding: 10px;
    }
    
    .output-box {
      background-color: #e2e2e2;
      color: white;
      border: 2px solid #555;
      border-radius: 8px;
      padding: 10px;
      font-weight: bold;
    }

  ")),
  sidebarLayout(
    sidebarPanel(
      class = 'sidebar-panel',
      wellPanel(
        title = 'Patient Information',
        class = 'well-panel',
        numericInput("paO2", "PaO2 (mmHg)", value = 27, min = 0),
        numericInput("fiO2", "FiO2 (%)", value = 21, min = 21, max = 100),
        radioButtons("ventilation", "Is the patient mechanically ventilated?",
                     choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE),
        numericInput("platelets", "Platelets (x10^3/µL)", value = 0, min = 0),
        numericInput("bilirubin", "Bilirubin (mg/dL)", value = 0, min = 0),
        numericInput("creatinine", "Creatinine (mg/dL)", value = 0, min = 0),
        numericInput("mean_arterial_pressure", "Mean Arterial Pressure (mmHg)", value = 0, min = 0),
        numericInput("glasgow_coma_scale", "Glasgow Coma Scale", value = 15, min = 3, max = 15),
        numericInput("dopamine", "Dopamine (mcg/kg/min)", value = 0, min = 0),
        numericInput("dobutamine", "Dobutamine (mcg/kg/min)", value = 0, min = 0),
        numericInput("epinephrine", "Epinephrine (mcg/kg/min)", value = 0, min = 0),
        numericInput("norepinephrine", "Norepinephrine (mcg/kg/min)", value = 0, min = 0),
        numericInput("pct", 'Procalcitonin (ng/ml)', value = 0, min = 0),
        numericInput("nlr", 'NLR', value = 0, min = 0),
        numericInput("crp", 'CRP (mg/L)', value = 0, min = 0)
      ),
      actionButton("calculate", "Calculate the Score", style = "width: 100%; margin-top: 10px;")
    ),
    mainPanel(
      fluidRow(
        column(
          width = 8,
          div(style = "background-color: #e2e2e2; padding: 10px; border: 2px solid #555; border-radius: 8px; text-align: center; font-weight:bold",
              HTML("<p style='margin: 0;'>Pro-SOFA Score: &nbsp;&nbsp;&nbsp;&nbsp;7</p>")
          )
        )
      )
    )
  )
)


server <- function(input, output) {
  calculateproSOFA <- function(paO2, fiO2, platelets, bilirubin, mean_arterial_pressure, glasgow_coma_scale, creatinine, ventilation, dopamine, dobutamine, epinephrine, norepinephrine, pct, nlr, crp) {
    

    print(paste("Input PaO2: ", paO2))
    print(paste("Input FiO2: ", fiO2))
    
    pfr <- (paO2 / fiO2)*100
    print(paste("Calculated PFR: ", pfr))
    
    print(paste("Ventilation: ", ventilation))
    
    if (ventilation == TRUE) {
      if (pfr < 100) {
        resp <- 4
      } else if (pfr < 200) {
        resp <- 3
      } else {
        resp <- 0
      }
    } else {
      if (pfr < 200) {
        resp <- 2
      } else if (pfr < 300) {
        resp <- 1
      } else if (pfr < 400) {
        resp <- 1
      } else if (pfr >= 400) {
        resp <- 0
      }
    }
    
    
    
    print(paste("Respiratory Score: ", resp))
    
    plt <- ifelse(platelets < 20, 4, ifelse(platelets < 50, 3, ifelse(platelets < 100, 2, ifelse(platelets < 150, 1, 0))))
    print(paste("Platelet Score: ", plt))
    
    bil <- ifelse(bilirubin >= 12.0, 4, ifelse(bilirubin >= 6.0, 3, ifelse(bilirubin >= 2.0, 2, ifelse(bilirubin > 1.2, 1, 0))))
    print(paste("Bilirubin Score: ", bil))
    
    cardio <- 0
    if (mean_arterial_pressure >= 70){
      cardio <- 0
    }else if (mean_arterial_pressure < 70) {
      cardio <- 1
    } else if (dopamine <= 5 || dobutamine > 0) {
      cardio <- 2
    } else if (dopamine > 15 || epinephrine > 0.1 || norepinephrine > 0.1) {
      cardio <- 4
    } else if (dopamine > 5 || epinephrine <= 0.1 || norepinephrine <= 0.1) {
      cardio <- 3
    }
    print(paste("Cardiovascular Score: ", cardio))
    
    gcs <- ifelse(glasgow_coma_scale < 6, 4, ifelse(glasgow_coma_scale < 10, 3, ifelse(glasgow_coma_scale < 13, 2, ifelse(glasgow_coma_scale < 15, 1, 0))))
    print(paste("GCS Score: ", gcs))
    
    cr <- ifelse(creatinine >= 5, 4, ifelse(creatinine >= 3.5, 3, ifelse(creatinine >= 2, 2, ifelse(creatinine >= 1.2, 1, 0))))
    print(paste("Creatinine Score: ", cr))
    

    if (pct >= 40) {
      pro <- 4
    } else if(pct<40 && pct >= 30){
      pro <- 3
    }else if(pct<30 && pct >= 20){
      pro <- 2
    }else if(pct<20 && pct >= 10){
      pro <- 1
    }else {
      pro <- 0
    }
    print(paste("pro Score: ", pro))
    
    nr <- 0
    if (crp > 100 && nlr >=0 && nlr <= 9) {
      nr <- 3
    } else if(nlr >=10 && nlr <= 19){
      nr <- 4
    }else if(nlr >= 20){ 
      nr <- 7
    }
    print(paste("nlr Score: ", nr))
    
    
    total_score <- resp + plt + bil + cardio + gcs + cr + pro + nr
    print(paste("Total Pro-SOFA Score: ", total_score))
    
    return(total_score)
  }
  
  output$pro_sofa_score <- renderText({
    input$calculate
    isolate({
      score <- calculateproSOFA(input$paO2, input$fiO2, input$platelets, input$bilirubin, 
                             input$mean_arterial_pressure, input$glasgow_coma_scale, 
                             input$creatinine, input$ventilation, input$dopamine, 
                             input$dobutamine, input$epinephrine, input$norepinephrine,
                             input$pct, input$nlr, input$crp)
      paste("Pro-SOFA Score           :", score)
    })
  })
}

shinyApp(ui = ui, server = server)



