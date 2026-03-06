options(repos = c(CRAN = "https://cran.r-project.org"))

library(shiny)
library(bslib)
library(gbm)
library(caret)

# ---------- Lifestyle suggestions ----------
get_suggestions <- function(x) {
  s <- character()
  if (x$smoking == 1) s <- c(s, "Reducing or quitting smoking can lower heart risk.")
  if (x$obesity == 1) s <- c(s, "Aim for gradual weight management with balanced diet and activity.")
  if (x$high_bp == 1) s <- c(s, "Consider monitoring blood pressure and reducing salt intake.")
  if (x$high_cholesterol == 1) s <- c(s, "Focus on heart-healthy fats and fibre-rich foods.")
  if (x$diabetes == 1) s <- c(s, "Maintain stable blood sugar with regular monitoring.")
  if (x$chronic_stress == 1) s <- c(s, "Try stress-reduction habits like better sleep and relaxation.")
  if (x$shortness_of_breath == 1) s <- c(s, "Persistent breathlessness should be discussed with a professional.")
  if (x$chest_pain == 1) s <- c(s, "Sudden or severe chest pain requires urgent medical attention.")
  s <- c(s, "Regular physical activity and balanced nutrition support heart health.")
  unique(s)
}

# ---------- UI ----------
ui <- page_navbar(
  title = "Heart Risk Predictor",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#0d6efd"),
  
  nav_panel("Home",
            h2("Heart Risk Prediction Tool"),
            p("Educational tool to estimate heart disease risk using machine learning."),
            p("This system provides preventive lifestyle recommendations."),
            tags$br(),
            tags$div(
              style="background:#f8f9fa; padding:15px; border-radius:10px;",
              strong("Disclaimer: "),
              "This tool is for educational purposes and not a medical diagnosis."
            )
  ),
  
  nav_panel("Login",
            textInput("login_user", "Username"),
            passwordInput("login_pass", "Password"),
            actionButton("login_btn", "Login", class="btn-primary"),
            uiOutput("login_msg")
  ),
  
  nav_panel("Sign Up",
            textInput("signup_user", "Username"),
            passwordInput("signup_pass", "Password"),
            passwordInput("signup_pass2", "Confirm Password"),
            actionButton("signup_btn", "Sign Up", class="btn-success"),
            uiOutput("signup_msg")
  ),
  
  nav_panel("Predict Risk",
            uiOutput("predict_page")
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  
  users <- reactiveVal(data.frame(username=character(), password=character()))
  logged_in <- reactiveVal(FALSE)
  
  final_model <- readRDS("models/final_model.rds")
  
  train_data <- final_model$trainingData
  age_mean <- mean(train_data$age, na.rm = TRUE)
  age_sd   <- sd(train_data$age, na.rm = TRUE)
  
  # ----- Signup -----
  observeEvent(input$signup_btn, {
    if (nchar(input$signup_user) < 4) { output$signup_msg <- renderText("Username must be at least 4 characters."); return() }
    if (nchar(input$signup_pass) < 6) { output$signup_msg <- renderText("Password must be at least 6 characters."); return() }
    if (input$signup_pass != input$signup_pass2) { output$signup_msg <- renderText("Passwords do not match."); return() }
    df <- users()
    if (input$signup_user %in% df$username) { output$signup_msg <- renderText("Username already exists."); return() }
    users(rbind(df, data.frame(username=input$signup_user, password=input$signup_pass)))
    output$signup_msg <- renderText("Account created successfully. Please login.")
  })
  
  # ----- Login -----
  observeEvent(input$login_btn, {
    df <- users()
    ok <- nrow(df) > 0 && input$login_user %in% df$username &&
      df$password[df$username==input$login_user] == input$login_pass
    
    if (ok) {
      logged_in(TRUE)
      output$login_msg <- renderText("Login successful")
    } else {
      output$login_msg <- renderText("Login failed")
    }
  })
  
  # ----- Prediction Page -----
  output$predict_page <- renderUI({
    if (!logged_in()) return(h4("Please login to access predictions."))
    
    fluidPage(
      sidebarLayout(
        
        sidebarPanel(
          
          h4("Patient Information"),
          
          tags$div(
            style="background:#eef5ff; padding:12px; border-radius:10px;",
            strong("Note: "),
            "Select Yes or No for each risk factor."
          ),
          
          sliderInput("age", "Age", 18, 90, 30),
          
          selectInput("gender","Gender",
                      choices=c("Female"=0,"Male"=1)),
          
          selectInput("smoking","Smoking",c("No"=0,"Yes"=1)),
          selectInput("obesity","Obesity",c("No"=0,"Yes"=1)),
          selectInput("high_bp","High Blood Pressure",c("No"=0,"Yes"=1)),
          selectInput("high_cholesterol","High Cholesterol",c("No"=0,"Yes"=1)),
          selectInput("diabetes","Diabetes",c("No"=0,"Yes"=1)),
          selectInput("family_history","Family History",c("No"=0,"Yes"=1)),
          selectInput("chronic_stress","Chronic Stress",c("No"=0,"Yes"=1)),
          selectInput("chest_pain","Chest Pain",c("No"=0,"Yes"=1)),
          selectInput("shortness_of_breath","Shortness of Breath",c("No"=0,"Yes"=1)),
          selectInput("fatigue","Fatigue",c("No"=0,"Yes"=1)),
          selectInput("palpitations","Palpitations",c("No"=0,"Yes"=1)),
          
          actionButton("predict_btn", "Predict Risk", class="btn-danger")
        ),
        
        mainPanel(
          uiOutput("risk_card"),
          plotOutput("prob_plot", height="350px"),
          tags$br(),
          uiOutput("suggestions_out")
        )
      )
    )
  })
  
  pred_state <- reactiveVal(NULL)
  
  # ----- Prediction -----
  observeEvent(input$predict_btn, {
    req(logged_in())
    
    raw_age <- as.numeric(input$age)
    age_scaled <- (raw_age - age_mean) / age_sd
    
    new_user <- data.frame(
      chest_pain = as.numeric(input$chest_pain),
      shortness_of_breath = as.numeric(input$shortness_of_breath),
      fatigue = as.numeric(input$fatigue),
      palpitations = as.numeric(input$palpitations),
      high_bp = as.numeric(input$high_bp),
      high_cholesterol = as.numeric(input$high_cholesterol),
      diabetes = as.numeric(input$diabetes),
      smoking = as.numeric(input$smoking),
      obesity = as.numeric(input$obesity),
      family_history = as.numeric(input$family_history),
      chronic_stress = as.numeric(input$chronic_stress),
      gender = as.numeric(input$gender),
      age = raw_age,
      age_scaled = age_scaled
    )
    
    p <- predict(final_model, new_user, type="prob")
    prob <- as.numeric(p[,"Yes"])
    
    risk_cat <- if (prob < 0.33) "Low Risk"
    else if (prob < 0.66) "Moderate Risk"
    else "High Risk"
    
    pred_state(list(prob=prob, risk_cat=risk_cat, new_user=new_user))
  })
  
  # Risk card
  output$risk_card <- renderUI({
    st <- pred_state()
    if (is.null(st)) return(NULL)
    
    colour <- if (st$risk_cat=="Low Risk") "#198754"
    else if (st$risk_cat=="Moderate Risk") "#fd7e14"
    else "#dc3545"
    
    tags$div(
      style=paste0("background:",colour,
                   "; color:white; padding:20px; border-radius:12px;
                   font-size:20px; font-weight:600;"),
      paste0(st$risk_cat,
             " • Probability: ",
             round(st$prob*100,1), "%")
    )
  })
  
  #  Pie 
  output$prob_plot <- renderPlot({
    st <- pred_state()
    if (is.null(st)) return(NULL)
    
    vals <- c(st$prob, 1 - st$prob)
    labels <- c("Heart Risk", "Low Risk")
    
    par(mar=c(2,2,2,2))
    pie(vals,
        labels=paste0(labels," (",round(vals*100,1),"%)"),
        main="Risk Distribution")
  })
  
  # ----- Suggestions -----
  output$suggestions_out <- renderUI({
    st <- pred_state()
    if (is.null(st)) return(NULL)
    
    suggestions <- get_suggestions(st$new_user)
    
    tags$div(
      style="background:black; color:white; padding:20px; border-radius:12px;",
      tags$h4("Lifestyle Improvement Suggestions"),
      tags$table(
        lapply(suggestions, function(s){
          tags$tr(tags$td(s, style="padding:10px; border-bottom:1px solid #444;"))
        })
      )
    )
  })
}

shinyApp(ui, server)
