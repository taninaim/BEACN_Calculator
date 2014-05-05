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
    p("NOTE: The calculator works with the more specific type of food. If both checkboxes are checked, it calculates the specific type. 
      If both are unchecked, it calculates the emissions from food in general.")
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
    h4(textOutput("text7")),
    p("Copyright: Tanin Phongpandecha, UC Berkeley BEACN Metrics Team", style="position: absolute; bottom: 0; right: 0; width: 500px; text-align:right; color:grey")
    )
))
