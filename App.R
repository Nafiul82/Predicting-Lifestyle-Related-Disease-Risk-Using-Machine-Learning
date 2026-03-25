options(repos = c(CRAN = "https://cran.r-project.org"))

library(shiny)
library(bslib)
library(gbm) 
library(caret)
library(digest)

# Lifestyle suggestions
get_suggestions <- function(x) {
  s <- character()
  if (x$smoking == 1) s <- c(s, "Reduce or quit smoking.")
  if (x$obesity == 1) s <- c(s, "Maintain a healthy weight.")
  if (x$high_bp == 1) s <- c(s, "Monitor blood pressure.")
  if (x$high_cholesterol == 1) s <- c(s, "Improve diet.")
  if (x$diabetes == 1) s <- c(s, "Manage blood sugar.")
  if (x$chronic_stress == 1) s <- c(s, "Reduce stress.")
  if (x$chest_pain == 1) s <- c(s, "Seek medical advice immediately.")
  unique(c(s, "Exercise regularly and maintain a balanced diet."))
}

#  UI 
ui <- page_navbar(
  title = "CardioSense AI",
  
  theme = bs_theme(
    version = 5,
    bg = "#0b0b0b",
    fg = "#ffffff",
    primary = "#5b9cff"
  ),
  
  tags$head(
    tags$style(HTML("
      body { background-color: #0b0b0b; }
      .navbar { background-color: #000 !important; }
      
      .card {
        background: #111;
        border-radius: 15px;
        padding: 20px;
        border: 1px solid #222;
      }
      
      .btn-modern {
        background: linear-gradient(90deg,#5b9cff,#7c4dff);
        border: none;
        color: white;
        border-radius: 10px;
      }
      
      .user-box {
        position: absolute;
        top: 20px;
        right: 30px;
        background: #111;
        padding: 8px 14px;
        border-radius: 10px;
        border: 1px solid #333;
      }
      
      .placeholder-box {
        text-align: center;
        padding: 40px;
        border-radius: 15px;
        background: #111;
        border: 1px dashed #444;
        color: #aaa;
      }
    "))
  ),
  
  #  HOME 
  nav_panel("Home",
            div(style="text-align:center; margin-top:40px;",
                
                h1("AI-Based Heart Risk Prediction System",
                   style="font-weight:700; font-size:42px;"),
                
                p("Empowering preventive healthcare through intelligent machine learning insights",
                  style="color:#aaa; font-size:18px;"),
                
                tags$br(),
                
                div(class="card",
                    style="max-width:800px; margin:auto;",
                    
                    h3("Why This System Matters"),
                    p("Cardiovascular diseases are a leading cause of death globally. Early detection can significantly improve outcomes.",
                      style="color:#bbb;"),
                    p("This system uses machine learning to predict heart disease risk and provide personalised recommendations.",
                      style="color:#bbb;")
                ),
                
                tags$br(),
                
                fluidRow(
                  column(4,
                         div(class="card",
                             h4("Accurate"),
                             p("Advanced ML models ensure reliable predictions.", style="color:#bbb;")
                         )
                  ),
                  column(4,
                         div(class="card",
                             h4("Simple"),
                             p("Designed for ease of use.", style="color:#bbb;")
                         )
                  ),
                  column(4,
                         div(class="card",
                             h4("Actionable"),
                             p("Provides useful lifestyle guidance.", style="color:#bbb;")
                         )
                  )
                )
            )
  ),
  
  # LOGIN 
  nav_panel("Login",
            div(style="max-width:400px; margin:auto; margin-top:50px;",
                div(class="card",
                    h3("Login - CardioSense AI"),
                    textInput("login_user", "Username"),
                    passwordInput("login_pass", "Password"),
                    actionButton("login_btn", "Login", class="btn-modern", style="width:100%"),
                    uiOutput("login_msg")
                )
            )
  ),
  
  # SIGNUP 
  nav_panel("Sign Up",
            div(style="max-width:400px; margin:auto; margin-top:50px;",
                div(class="card",
                    h3("Create Account - CardioSense AI"),
                    textInput("signup_user", "Username"),
                    passwordInput("signup_pass", "Password"),
                    passwordInput("signup_pass2", "Confirm Password"),
                    actionButton("signup_btn", "Sign Up", class="btn-modern", style="width:100%"),
                    uiOutput("signup_msg")
                )
            )
  ),
  
  nav_panel("Predict Risk", uiOutput("predict_page"))
)

#  SERVER 
server <- function(input, output, session) {
  
  users <- reactiveVal(data.frame(username=character(), password=character()))
  logged_in <- reactiveVal(FALSE)
  current_user <- reactiveVal("")
  
  final_model <- readRDS("models/final_model.rds")
  train_data <- final_model$trainingData
  age_mean <- mean(train_data$age)
  age_sd   <- sd(train_data$age)
  
  # SIGNUP
  observeEvent(input$signup_btn, {
    if (nchar(input$signup_user) < 4) {
      output$signup_msg <- renderText("Username too short"); return()
    }
    if (nchar(input$signup_pass) < 6) {
      output$signup_msg <- renderText("Password too short"); return()
    }
    if (input$signup_pass != input$signup_pass2) {
      output$signup_msg <- renderText("Passwords do not match"); return()
    }
    
    hashed <- digest(input$signup_pass, "sha256")
    users(rbind(users(), data.frame(username=input$signup_user, password=hashed)))
    output$signup_msg <- renderText("Account created")
  })
  
  # LOGIN
  observeEvent(input$login_btn, {
    df <- users()
    hashed <- digest(input$login_pass, "sha256")
    
    ok <- input$login_user %in% df$username &&
      df$password[df$username==input$login_user] == hashed
    
    if (ok) {
      logged_in(TRUE)
      current_user(input$login_user)
      output$login_msg <- renderText("Login successful")
    } else {
      output$login_msg <- renderText("Login failed")
    }
  })
  
  # PREDICTION PAGE 
  output$predict_page <- renderUI({
    if (!logged_in()) return(h4("Please login first"))
    
    fluidPage(
      div(class="user-box", paste("User:", current_user())),
      
      sidebarLayout(
        
        sidebarPanel(
          h4("Patient Info"),
          
          sliderInput("age", "Age", 18, 90, 30),
          selectInput("gender","Gender",c("Female"=0,"Male"=1)),
          selectInput("smoking","Smoking",c("No"=0,"Yes"=1)),
          selectInput("obesity","Obesity",c("No"=0,"Yes"=1)),
          selectInput("high_bp","High BP",c("No"=0,"Yes"=1)),
          selectInput("high_cholesterol","Cholesterol",c("No"=0,"Yes"=1)),
          selectInput("diabetes","Diabetes",c("No"=0,"Yes"=1)),
          selectInput("family_history","Family History",c("No"=0,"Yes"=1)),
          selectInput("chronic_stress","Stress",c("No"=0,"Yes"=1)),
          selectInput("chest_pain","Chest Pain",c("No"=0,"Yes"=1)),
          selectInput("shortness_of_breath","Breathing difficulty",c("No"=0,"Yes"=1)),
          selectInput("fatigue","Fatigue",c("No"=0,"Yes"=1)),
          selectInput("palpitations","Palpitations",c("No"=0,"Yes"=1)),
          
          actionButton("predict_btn", "Predict Risk", class="btn-modern")
        ),
        
        mainPanel(
          uiOutput("risk_card"),
          uiOutput("placeholder"),
          plotOutput("prob_plot"),
          uiOutput("suggestions_out")
        )
      )
    )
  })
  
  pred_state <- reactiveVal(NULL)
  
  observeEvent(input$predict_btn, {
    age_scaled <- (input$age - age_mean)/age_sd
    
    new_user <- data.frame(
      chest_pain=as.numeric(input$chest_pain),
      shortness_of_breath=as.numeric(input$shortness_of_breath),
      fatigue=as.numeric(input$fatigue),
      palpitations=as.numeric(input$palpitations),
      high_bp=as.numeric(input$high_bp),
      high_cholesterol=as.numeric(input$high_cholesterol),
      diabetes=as.numeric(input$diabetes),
      smoking=as.numeric(input$smoking),
      obesity=as.numeric(input$obesity),
      family_history=as.numeric(input$family_history),
      chronic_stress=as.numeric(input$chronic_stress),
      gender=as.numeric(input$gender),
      age=input$age,
      age_scaled=age_scaled
    )
    
    prob <- predict(final_model, new_user, type="prob")[,"Yes"]
    
    risk_cat <- if (prob < 0.33) "Low Risk"
    else if (prob < 0.66) "Moderate Risk"
    else "High Risk"
    
    pred_state(list(prob=prob, risk_cat=risk_cat, new_user=new_user))
  })
  
  # Risk card
  output$risk_card <- renderUI({
    st <- pred_state()
    if (is.null(st)) return(NULL)
    
    col <- if (st$risk_cat=="Low Risk") "#198754"
    else if (st$risk_cat=="Moderate Risk") "#fd7e14"
    else "#dc3545"
    
    div(style=paste0("background:",col,"; padding:15px; border-radius:10px; text-align:center;"),
        paste(st$risk_cat, "-", round(st$prob*100,1), "%"))
  })
  
  # Placeholder
  output$placeholder <- renderUI({
    if (!is.null(pred_state())) return(NULL)
    
    div(class="placeholder-box",
        h4("Enter details and click Predict"),
        p("Your results will appear here")
    )
  })
  
  # Pie chart
  output$prob_plot <- renderPlot({
    st <- pred_state()
    if (is.null(st)) return(NULL)
    
    high <- round(st$prob*100,1)
    low  <- round((1-st$prob)*100,1)
    
    pie(
      c(st$prob,1-st$prob),
      labels=c(
        paste0("High Risk (",high,"%)"),
        paste0("Low Risk (",low,"%)")
      )
    )
  })
  
  # Suggestions + LINKS
  output$suggestions_out <- renderUI({
    st <- pred_state()
    if (is.null(st)) return(NULL)
    
    div(class="card",
        
        h4("Lifestyle Recommendations"),
        tags$ul(lapply(get_suggestions(st$new_user), tags$li)),
        
        tags$hr(),
        
        h4("Learn More & Get Support"),
        
        tags$ul(
          tags$li(tags$a("NHS Heart Disease Guide",
                         href="https://www.nhs.uk/conditions/cardiovascular-disease/",
                         target="_blank")),
          
          tags$li(tags$a("World Health Organization (WHO)",
                         href="https://www.who.int/health-topics/cardiovascular-diseases",
                         target="_blank")),
          
          tags$li(tags$a("British Heart Foundation",
                         href="https://www.bhf.org.uk",
                         target="_blank")),
          
          tags$li(tags$a("Find a GP (NHS)",
                         href="https://www.nhs.uk/service-search/find-a-gp",
                         target="_blank"))
        )
    )
  })
}

shinyApp(ui, server)
