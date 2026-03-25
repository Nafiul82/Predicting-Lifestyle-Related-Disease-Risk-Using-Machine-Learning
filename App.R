library(shiny)
library(bslib)

# ---------- Load model & scaler ----------

# ---------- Load model & scaler safely ----------
load_assets <- function() {
  possible_model_paths <- c("models/final_model.rds", "final_model.rds")
  possible_scaler_paths <- c("models/age_scaler.rds", "age_scaler.rds")
  
  model_path <- possible_model_paths[file.exists(possible_model_paths)][1]
  scaler_path <- possible_scaler_paths[file.exists(possible_scaler_paths)][1]
  
  if (is.na(model_path) || is.na(scaler_path)) {
    message("Model or scaler file not found at startup.")
    return(NULL)
  }
  
  list(
    model = readRDS(model_path),
    scaler = readRDS(scaler_path)
  )
}

assets <- load_assets()

final_model <- if (!is.null(assets)) assets$model else NULL
age_scaler  <- if (!is.null(assets)) assets$scaler else NULL



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
  title = "Health Risk Predictor",
  theme = bs_theme(version = 5, bootswatch = "flatly", base_font = font_google("Poppins")),
  
  nav_panel("Home",
            h2("Heart Risk Prediction Tool"),
            p("Educational tool to estimate heart risk using machine learning."),
            p("Not a medical diagnosis.")
  ),
  
  nav_panel("Login",
            textInput("login_user", "Username"),
            passwordInput("login_pass", "Password"),
            actionButton("login_btn", "Login"),
            uiOutput("login_msg")
  ),
  
  nav_panel("Sign Up",
            textInput("signup_user", "Username"),
            passwordInput("signup_pass", "Password"),
            passwordInput("signup_pass2", "Confirm Password"),
            actionButton("signup_btn", "Sign Up"),
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
  
  # ----- Signup -----
  observeEvent(input$signup_btn, {
    validate(
      need(nchar(input$signup_user) >= 4, "Username too short"),
      need(nchar(input$signup_pass) >= 6, "Password too short"),
      need(input$signup_pass == input$signup_pass2, "Passwords do not match")
    )
    
    df <- users()
    if (input$signup_user %in% df$username) {
      output$signup_msg <- renderText("Username already exists.")
    } else {
      users(rbind(df, data.frame(username=input$signup_user, password=input$signup_pass)))
      output$signup_msg <- renderText("Account created. Please login.")
    }
  })
  
  # ----- Login -----
  observeEvent(input$login_btn, {
    df <- users()
    ok <- nrow(df) > 0 &&
      input$login_user %in% df$username &&
      df$password[df$username==input$login_user] == input$login_pass
    
    if (ok) {
      logged_in(TRUE)
      output$login_msg <- renderText("Login successful")
    } else {
      output$login_msg <- renderText("Login failed")
    }
  })
  
  # ----- Protected Prediction Page -----
  output$predict_page <- renderUI({
    if (!logged_in()) {
      return(h4("Please login to access predictions."))
    }
    
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          sliderInput("age", "Age", 18, 90, 30),
          selectInput("gender","Gender",c("0","1")),
          selectInput("smoking","Smoking",c("0","1")),
          selectInput("obesity","Obesity",c("0","1")),
          selectInput("high_bp","High BP",c("0","1")),
          selectInput("high_cholesterol","High Cholesterol",c("0","1")),
          selectInput("diabetes","Diabetes",c("0","1")),
          selectInput("family_history","Family History",c("0","1")),
          selectInput("chronic_stress","Chronic Stress",c("0","1")),
          selectInput("chest_pain","Chest Pain",c("0","1")),
          selectInput("shortness_of_breath","Shortness of Breath",c("0","1")),
          selectInput("fatigue","Fatigue",c("0","1")),
          selectInput("palpitations","Palpitations",c("0","1")),
          actionButton("predict_btn", "Predict Risk")
        ),
        mainPanel(
          uiOutput("risk_summary"),
          plotOutput("prob_plot"),
          uiOutput("suggestions_out")
        )
      )
    )
  })
  
  pred_state <- reactiveVal(NULL)
  
  # ----- Prediction -----
  
  
  
  observeEvent(input$predict_btn, {
    req(logged_in())
    
    validate(
      need(!is.null(final_model), "Model file not loaded."),
      need(!is.null(age_scaler), "Scaler file not loaded.")
    )
    
    
    # Raw age (model expects this too)
    raw_age <- as.numeric(input$age)
    
    # Scaled age (same transformation used in training)
    age_df <- data.frame(age = raw_age)
    scaled_age <- predict(age_scaler, age_df)
    
    new_user <- data.frame(
      chest_pain = as.integer(input$chest_pain),
      shortness_of_breath = as.integer(input$shortness_of_breath),
      fatigue = as.integer(input$fatigue),
      palpitations = as.integer(input$palpitations),
      high_bp = as.integer(input$high_bp),
      high_cholesterol = as.integer(input$high_cholesterol),
      diabetes = as.integer(input$diabetes),
      smoking = as.integer(input$smoking),
      obesity = as.integer(input$obesity),
      family_history = as.integer(input$family_history),
      chronic_stress = as.integer(input$chronic_stress),
      gender = as.integer(input$gender),
      age = raw_age,                 # ✅ required
      age_scaled = scaled_age[,1]    # ✅ required
    )
    
    res <- tryCatch({
      prob <- predict(final_model, new_user, type="prob")[,"Yes"]
      prob <- as.numeric(prob)
      
      risk_cat <- if (prob < 0.33) "Low Risk"
      else if (prob < 0.66) "Moderate Risk"
      else "High Risk"
      
      list(prob = prob, risk_cat = risk_cat, new_user = new_user)
      
    }, error = function(e) {
      showNotification(paste("Prediction error:", e$message), type = "error")
      NULL
    })
    
    if (!is.null(res)) pred_state(res)
  })
  
  
  
  
  
  
  
 
  
  
  # ----- Outputs -----
  output$risk_summary <- renderUI({
    st <- pred_state()
    if (is.null(st)) return("No prediction yet.")
    paste0(st$risk_cat, " • Probability: ", round(st$prob*100,1), "%")
  })
  
  output$prob_plot <- renderPlot({
    st <- pred_state()
    if (is.null(st)) return(NULL)
    barplot(st$prob, horiz=TRUE, xlim=c(0,1), main="Predicted Heart Risk Probability")
  })
  
  output$suggestions_out <- renderUI({
    st <- pred_state()
    if (is.null(st)) return("Suggestions will appear after prediction.")
    tags$ul(lapply(get_suggestions(st$new_user), tags$li))
  })
}

shinyApp(ui, server)

getwd()
list.files()





