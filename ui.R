library(shiny)

shinyUI(pageWithSidebar(
  headerPanel(list(tags$head(tags$style(".span12 {background-color: white;}")),
                    HTML('<img src="logo.jpg", height="50", width = "100",    
style="float:bottom"/>','<p style="color:black"> Food Waste Calculator  </p>' ))
  ),
  sidebarPanel(
    numericInput("poundsOfFood", "Pounds of Food", 0),
    selectInput("Type",label = "Type",
                c("", "Meat & Poultry", "Fish & Shellfish", "Cheese", "Dairy Products", 
                  "Grains", "Sugar", "Vegetables", "Beans & Pulses", "Fruits", "Fats & Oils")),
    checkboxInput("General", label = "", FALSE),
    selectInput("Type1", label = "Specific Types",
                c("", "Beef", "Chicken", "Turkey", "Pork", "Eggs", "Tuna", "Salmon", "Shrimp", "Milk", "Yogurt",
                  "Flour", "Rice", "Cabbage", "Carrots", "Lettuce", "Potatoes", "Tomatoes", "Broccoli", 
                  "Nuts", "Dry Beans", "Tofu", "Oranges", "Peaches", "Strawberries", "Apples", "Bananas", "Grapes",  
                  "Butter", "Oils")),
    checkboxInput("Specific", label = "", FALSE),
    submitButton(text = "Calculate"),
    br(),
    br(),
    h5("INFORMATION:"),
    p("The calculator can calculate the following:"),
    p("1. Generalized food (based on the average American diet)"),
    p("2. General types of food (such as Meat and Poultry, Vegetables, etc.)"),
    p("3. Even more specific type of foods (such as Beef, Tomatoes, Tofu, etc.)"),
    p("If no boxes are checked, it will calculate generalized food."),
    p("If both are checked if will calculate data for the Specific Types"),
    
    
    h5("NOTE:"),
    p("All food carbon emissions are reported above in Kg of CO2e, including major greenhouse gases such as methane and nitrous oxide."),
    p("The produced data UNDERESTIMATE the negative effects of food waste."),
    h5("For more details and sources:", a("http://tinyurl.com/mejob7y", href = "http://tinyurl.com/mejob7y")),
    br(),
    div("Copyright: Tanin Phongpandecha, UC Berkeley BEACN Metrics Team", style = "color:grey")
    
  ),
  mainPanel(
    h4(textOutput("text1")),
    br(),
    h4(textOutput("text2")),
    br(),
    h4(textOutput("text3")),
    br(),
    h4(textOutput("text4")),
    br(),
    h4(textOutput("text5")),
    br(),
    h4(textOutput("text6")),
    br(),
    h4(textOutput("text7"))
    )
))
