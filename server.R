library(shiny)

shinyServer(function(input, output) {
  #The Average American Diet
  ##Meat & Poultry
  ##Fish & Shellfish
  ##Eggs
  ##Cheese
  ##Dairy Products
  ##Flour & Cereal Products
  ##Caloric Sweeteners
  ##Vegetables
  ##Coffee, Cocoa & Nuts
  ##Fruits
  ##Fats & Oils
  YearlyAverage = 1996.3 #in lbs
  Weight = c(216.3, 16.1, 31.4, 600.5, 192.3, 141.6, 415.4, 24, 273.2, 85.5) #in lbs
  Type = c("Meat&Poultry", "Fish&Shellfish", "Cheese", "DairyProducts", "Grains", "Sweetener", "Vegetables", "Beans&Pulses", "Fruits", "Fats&Oils")
  ###2013 Statistic
  AverageAmerican = data.frame(Type, Weight)
  emission.car = 423 #gramsCO2 per mile
  
  #Amounts from Different Types
  ##Meat and Poultry##
  MeatPoultry.emissions = function(x = 0, arg = NULL) {
    slope.beef = 7.374 #KgCO2e per pound of Beef
    slope.chicken = 1.70 
    slope.eggs = 0.96
    slope.pork = 2.52
    slope.turkey = 2.09
    slope = slope.beef + slope.chicken + slope.eggs + slope.pork + slope.turkey
    if (is.null(arg)) {
      return(slope*x/5)
    } else if (arg == "Beef" | arg == "beef") {
      return(slope.beef*x)
    } else if (arg == "Chicken" | arg == "chicken") {
      return(slope.chicken*x)
    } else if (arg == "Eggs" | arg == "eggs") {
      return(slope.eggs*x)
    } else if (arg == "Pork" | arg == "pork") {
      return(slope.pork*x)
    } else if (arg == "Turkey" | arg == "turkey") {
      return(slope.turkey*x)
    }
  }
  
  ##Fish and Shellfish##
  FishShellfish.emissions = function(x = 0, arg = NULL) {
    slope.salmon = 2.21
    slope.shrimp = 3.2
    slope.tuna = 1.85
    slope = slope.salmon + slope.shrimp + slope.tuna
    if (is.null(arg)) {
      return(slope*x/3)
    } else if(arg == "Salmon" | arg == "salmon") {
      return(slope.salmon*x)
    } else if(arg == "Shrimp" | arg == "shrimp") {
      return(slope.shrimp*x)
    } else if(arg == "Tuna" | arg == "tuna") {
      return(slope.tuna*x)
    }
  }
  
  ##Cheese##
  Cheese.emissions = function(x = 0) {
    slope = 4.46
    return(slope*x)
  }
  
  ##Dairy Products##
  DairyProducts.emissions = function(x = 0, arg = NULL) {
    slope.milk = 0.36
    slope.yogurt = 0.41
    slope = slope.milk + slope.yogurt
    if (is.null(arg)) {
      return(slope*x/2)
    } else if (arg == "Milk" | arg == "milk") {
      return(slope.milk*x)
    } else if (arg == "Yogurt" | arg == "yogurt") {
      return(slope.yogurt*x)
    }
  }
  
  ##Flour and Cereal (Grains)##
  FlourCereal.emissions = function(x = 0, arg = NULL) {
    slope.flour = 0.30
    slope.rice = 1.20
    slope = slope.flour + slope.rice
    if (is.null(arg)) {
      return(slope*x/2)
    } else if (arg == "Flour" | arg == "flour") {
      return(slope.flour*x)
    } else if (arg == "Rice" | arg == "rice") {
      return(slope.rice*x)
    }
  }
  
  ##Sweetener (Sugar)##
  Sweetener.emissions = function(x = 0) { #Sugar
    slope = 0.9072
    return(slope*x)
  }
  
  ##Vegetables##
  Vegetables.emissions = function(x = 0, arg = NULL) {
    slope.broccoli = 0.16
    slope.cabbage = 0.06
    slope.carrots = 0.05
    slope.lettuce = 0.06
    slope.potatoes = 0.12
    slope.tomatoes = 0.08
    slope = slope.broccoli + slope.cabbage + slope.carrots + slope.lettuce + slope.potatoes + slope.tomatoes
    if (is.null(arg)) {
      return(slope*x/6)
    } else if (arg == "Broccoli" | arg == "broccoli") {
      return(slope.broccoli*x)
    } else if (arg == "Cabbage" | arg == "cabbage") {
      return(slope.cabbage*x)
    } else if (arg == "Carrots" | arg == "carrots") {
      return(slope.carrots*x)
    } else if (arg == "Lettuce" | arg == "lettuce") {
      return(slope.lettuce*x)
    } else if (arg == "Potatoes" | arg == "potatoes") {
      return(slope.potatoes*x)
    } else if (arg == "Tomatoes" | arg == "tomatoes") {
      return(slope.tomatoes*x)
    }
  }
  
  ##Beans and Pulses##
  BeanPulses.emissions = function(x = 0, arg = NULL) {
    slope.drybeans = 0.40
    slope.tofu = 0.32
    slope = slope.drybeans + slope.tofu
    if (is.null(arg)) {
      return(slope*x/2)
    } else if (arg == "Dry Beans" | arg == "dry beans" | arg == "Dry beans") {
      return(slope.drybeans*x)
    } else if (arg == "Tofu" | arg == "tofu") {
      return(slope.tofu*x)
    }
  }
  
  ##Fruits##
  Fruits.emissions = function(x = 0, arg = NULL) {
    slope.oranges = 0.07
    slope.peaches = 0.09
    slope.strawberries = 0.14
    slope.apples = 0.07
    slope.bananas = 0.12
    slope.grapes = 0.09
    slope = slope.oranges + slope.peaches + slope.strawberries + slope.apples + slope.bananas + slope.grapes
    if(is.null(arg)) {
      return(slope*x/6)
    } else if(arg == "Oranges" | arg == "oranges") {
      return(slope.oranges*x)
    } else if(arg == "Peaches" | arg == "peaches") {
      return(slope.peaches*x)
    } else if(arg == "Strawberries" | arg == "strawberries") {
      return(slope.strawberries*x)
    } else if(arg == "Apples" | arg == "apples") {
      return(slope.apples*x)
    } else if(arg == "Bananas" | arg == "bananas") {
      return(slope.bananas*x)
    } else if(arg == "Grapes" | arg == "grapes") {
      return(slope.grapes*x)
    }
  }
  
  ##Fats and Oils##
  FatsOils.emissions = function(x = 0, arg = NULL) {
    slope.butter = 0.53
    slope.vegetableOils = 0.71
    slope = slope.butter + slope.vegetableOils
    if (is.null(arg)){
      return(slope*x/2)
    } else if(arg == "Butter" | arg == "butter") {
      return(slope.butter*x)
    } else if(arg == "Vegetable Oils" | arg == "Vegetable oils" | arg == "vegetable oils") {
      return(slope.vegetableOils*x)
    }
  }
  
  ##Converter (for convenience)
  PoundsToGrams = function(x) {
    return(x*453.592)
  }
  PoundsToKg = function(x) {
    return(x*0.453592)
  }
  PoundsToTons = function(x) {
    return(x*0.000453592)
  }
  GramsToPounds = function(x) {
    return(x*0.00220462)
  }
  KgToPounds = function(x) {
    return(x*2.20462)
  }
  TonsToPounds = function(x) {
    return(x*2204.62)
  }
  
  ##Calculator
  ###HOW TO USE###
  ###The output is either a comparison statement or an amount of KgCO2e (Kilograms of Carbon Dioxide Equivalent)
  ###THE FIRST ARGUMENT x must be a number (in pounds). The second and third arguments are optional (foodToEmissions(1000) would give the emission of 1000 lbs of "food" in general)
  ###THE SECOND ARGUMENT: I've taken care of capitalization, so Beef and beef both work. But be sure to put quotation marks as in "beef" and not just beef
  
  ###Valid inputs to the second argument are: c("Beef", "beef", "Chicken", "chicken", "Eggs", "eggs", "Pork", "pork", "Turkey", "turkey",
  ###"Salmon", "salmon", "Shrimp", "shrimp","Tuna","tuna",
  ###"Milk", "milk","Yogurt","yogurt", "Flour","flour","Rice","rice",
  ###"Broccoli", "broccoli","Cabbage", "cabbage", "Carrots", "carrots","Lettuce","lettuce", "Potatoes", "potatoes", "Tomatoes", "tomatoes",
  ###"Dry Bean", "dry bean", "Dry bean","Tofu", "tofu",  
  ###"Oranges", "oranges","Peaches", "peaches", "Strawberries", "strawberries", "Apples", "apples", "Bananas", "bananas","Grapes", "grapes",
  ###"Butter", "butter", "Vegetable Oils", "Vegetable oils", "vegetable oils",
  ###"Meat", "Poultry", "Meat and Poultry", "Meat & Poultry", "meat", "poultry",
  ###"Fish and Shellfish", "Fish & Shellfish", "Fish", "Shellfish", "fish", "shellfish",
  ###"Cheese", "cheese", "Dairy Products", "dairy products", "Flour & Cereal", "Grains", "Flour and Cereal", "Flour", "Cereal", "flour", "cereal",
  ###"flour and cereal", "Beans", "Pulses", "Beans & Pulses", "Beans and Pulses", "beans", "pulses", "beans and pulses", "beans & pulses",
  ###"Fats & Oils", "Fats and Oils", "Fats", "Oils", "oils", "fats", "fats & oils", "fats and oils")
  
  ###THE THIRD ARGUMENT: input compare = TRUE or simply just TRUE to get a comparison with how many miles of an average vehicle you have to drive to cause the same 
  ###damage as the food waste
  foodToEmissions = function(x = 0, Type = "", compare = FALSE) { #in KgCO2e
    #case where we just want food in general
    dietPercentage = AverageAmerican$Weight/sum(AverageAmerican$Weight)
    if (Type == "") {
      productionCost = dietPercentage[1] * MeatPoultry.emissions(x)
      + dietPercentage[2] * FishShellfish.emissions(x)
      + dietPercentage[3] * Cheese.emissions(x)
      + dietPercentage[4] * DairyProducts.emissions(x)
      + dietPercentage[5] * FlourCereal.emissions(x)
      + dietPercentage[6] * Sweetener.emissions(x)
      + dietPercentage[7] * Vegetables.emissions(x)
      + dietPercentage[8] * BeanPulses.emissions(x)
      + dietPercentage[9] * Fruits.emissions(x)
      + dietPercentage[10] * FatsOils.emissions(x) 
      if (compare) {
        print(paste(x, "lbs of food is equivalent to driving",  round(((productionCost*1000) / emission.car), 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(productionCost,2))
    } else if (!any(Type == c("nuts", "Nuts", "Beef", "beef", "Chicken", "chicken", "Eggs", "eggs", "Pork", "pork", "Turkey", "turkey", "Sugar",
                              "Salmon", "salmon", "Shrimp", "shrimp","Tuna","tuna", "Dry Beans",
                              "Milk", "milk","Yogurt","yogurt", "Flour","flour","Rice","rice", "Vegetables", "vegetables",
                              "Broccoli", "broccoli","Cabbage", "cabbage", "Carrots", "carrots","Lettuce","lettuce", "Potatoes", "potatoes", "Tomatoes", "tomatoes",
                              "Dry Beans", "dry beans", "Dry beans","Tofu", "tofu", "Fruits",
                              "Oranges", "oranges","Peaches", "peaches", "Strawberries", "strawberries", "Apples", "apples", "Bananas", "bananas","Grapes", "grapes",
                              "Butter", "butter", "Vegetable Oils", "Vegetable oils", "vegetable oils",
                              "Meat", "Poultry", "Meat and Poultry", "Meat & Poultry", "meat", "poultry",
                              "Fish and Shellfish", "Fish & Shellfish", "Fish", "Shellfish", "fish", "shellfish",
                              "Cheese", "cheese", "Dairy Products", "dairy products", "Flour & Cereal", "Grains", "Flour and Cereal", "Flour", "Cereal", "flour", "cereal",
                              "flour and cereal", "Beans", "Pulses", "Beans & Pulses", "Beans and Pulses", "beans", "pulses", "beans and pulses", "beans & pulses",
                              "Fats & Oils", "Fats and Oils", "Fats", "Oils", "oils", "fats", "fats & oils", "fats and oils"))) {
      stop(paste(Type, "is not a valid Type"))
    }
    
    #case where we want to be specific less specific (e.g. Meat and Poultry, Vegetables, etc.)
    if (Type == "Meat" | Type == "Poultry" | Type == "Meat and Poultry" | Type == "Meat & Poultry" | Type == "meat" | Type == "poultry") {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(MeatPoultry.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(MeatPoultry.emissions(x), 2))
    } else if (Type == "Fish and Shellfish" | Type == "Fish & Shellfish" | Type == "Fish" | Type == "Shellfish" | Type == "fish" | Type == "shellfish") {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(FishShellfish.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(FishShellfish.emissions(x),2))
    } else if (Type == "Cheese" | Type == "cheese") {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(Cheese.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(Cheese.emissions(x),2))
    } else if(Type == "Dairy Products" | Type == "dairy products" | Type == "dairy") {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(DairyProducts.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(DairyProducts.emissions(x),2))
    } else if(Type == "Flour & Cereal" | Type == "Grains" | Type == "Flour and Cereal") {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(FlourCereal.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(FlourCereal.emissions(x),2))
    } else if(Type == "Sweetener" | Type == "Sugar" | Type == "sugar" | Type == "sweetener") {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(Sweetener.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(Sweetener.emissions(x),2))
    } else if(Type == "Vegetables" | Type == "vegetables") {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(Vegetables.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(Vegetables.emissions(x),2))
    } else if(Type == "Beans" | Type == "Pulses" | Type == "Beans & Pulses" | Type == "Dry Beans" | Type == "Beans and Pulses" | Type == "beans" | Type == "pulses" | Type == "beans and pulses" | Type == "beans & pulses" | Type == "nuts" | Type == "Nuts") {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(BeanPulses.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(BeanPulses.emissions(x),2))
    } else if(Type == "Fruits") {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(Fruits.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(Fruits.emissions(x),2))
    } else if(Type == "Fats & Oils" | Type == "Fats and Oils" | Type == "Fats" | Type == "Oils" | Type == "oils" | Type == "fats" | Type == "fats & oils" | Type == "fats and oils") {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(FatsOils.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(FatsOils.emissions(x),2))
    } #case where we want to be specific (e.g. emissions from beef)
    else if(any(Type == c("Beef", "beef", "Chicken", "chicken", "Eggs", "eggs", "Pork", "pork", "Turkey", "turkey"))) {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(MeatPoultry.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(MeatPoultry.emissions(x, Type),2))
    } else if(any(Type == c("Salmon", "salmon", "Shrimp", "shrimp","Tuna","tuna"))) {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(FishShellfish.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(FishShellfish.emissions(x, Type),2))
    } else if(any(Type == c("Milk", "milk","Yogurt","yogurt"))) {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(DairyProducts.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(DairyProducts.emissions(x, Type),2))
    } else if(any(Type == c("Flour","flour","Rice","rice"))) {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(FlourCereal.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(FlourCereal.emissions(x, Type), 2))
    }  else if(any(Type == c("Broccoli", "broccoli","Cabbage", "cabbage", "Carrots", "carrots","Lettuce","lettuce", "Potatoes", "potatoes", "Tomatoes", "tomatoes" ))) {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(Vegetables.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(Vegetables.emissions(x, Type),2))
    } else if(any(Type == c("Dry Bean", "dry bean", "Dry bean","Tofu", "tofu" ))) {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(BeanPulses.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(BeanPulses.emissions(x, Type),2))
    } else if(any(Type == c("Oranges", "oranges","Peaches", "peaches", "Strawberries", "strawberries", "Apples", "apples", "Bananas", "bananas","Grapes", "grapes"))) {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(Fruits.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(Fruits.emissions(x, Type),2))
    }  else if(any(Type == c("Butter", "butter", "Vegetable Oils", "Vegetable oils", "vegetable oils", "oils", "Oils"))) {
      if (compare) {
        print(paste(x, "lbs of", Type, "is equivalent to driving", round(FatsOils.emissions(x)*1000/emission.car, 2), "miles on an average vehicle"))
        return(invisible())
      }
      return(round(FatsOils.emissions(x, Type),2))
    } 
  }
  
  ### Water Footprint (liter/kg)
  Grains = 5699 #combine Grains/Cereals and Pulses as one category
  Dairy = 1125
  Fish = 0
  Fruits = 967
  Meat = 7273
  Nuts = 9063
  Oils = 3958.5
  Sugar = 197
  Vegetables = 322
  Weights = c(192.3, 600.5, 16.1, 273.2, 216.3, 24, 85.5, 141.6, 415.4)
  Weights = Weights/sum(Weights)
  
  WFootprint = c(Grains, Dairy, Fish, Fruits, Meat, Nuts, Oils, Sugar, Vegetables)
  WFootprint = WFootprint / 2.20462 ##Convert to pounds
  
  
  
  
  waterFootprint = function (poundsOfFood = 0, Type = "") { #returns liters of water used
    if (Type == "") {
      return(sum(Weights*poundsOfFood*WFootprint))
    } else if (!any(Type == c("nuts", "Nuts", "Beef", "beef", "Chicken", "chicken", "Eggs", "eggs", "Pork", "pork", "Turkey", "turkey", "Sugar",
                              "Salmon", "salmon", "Shrimp", "shrimp","Tuna","tuna", "Dry Beans",
                              "Milk", "milk","Yogurt","yogurt", "Flour","flour","Rice","rice", "Vegetables", "vegetables",
                              "Broccoli", "broccoli","Cabbage", "cabbage", "Carrots", "carrots","Lettuce","lettuce", "Potatoes", "potatoes", "Tomatoes", "tomatoes",
                              "Dry Beans", "dry beans", "Dry beans","Tofu", "tofu", "Fruits",
                              "Oranges", "oranges","Peaches", "peaches", "Strawberries", "strawberries", "Apples", "apples", "Bananas", "bananas","Grapes", "grapes",
                              "Butter", "butter", "Vegetable Oils", "Vegetable oils", "vegetable oils",
                              "Meat", "Poultry", "Meat and Poultry", "Meat & Poultry", "meat", "poultry",
                              "Fish and Shellfish", "Fish & Shellfish", "Fish", "Shellfish", "fish", "shellfish",
                              "Cheese", "cheese", "Dairy Products", "dairy products", "Flour & Cereal", "Grains", "Flour and Cereal", "Flour", "Cereal", "flour", "cereal",
                              "flour and cereal", "Beans", "Pulses", "Beans & Pulses", "Beans and Pulses", "beans", "pulses", "beans and pulses", "beans & pulses",
                              "Fats & Oils", "Fats and Oils", "Fats", "Oils", "oils", "fats", "fats & oils", "fats and oils"))) {
      stop((paste(Type,"is an invalid type")))
    } else if (any(Type == c("Flour & Cereal", "Grains", "Flour and Cereal", "Flour", "Cereal", "flour", "cereal",
                             "flour and cereal", "Beans", "Pulses", "Beans & Pulses", "Beans and Pulses", "beans", "pulses", "beans and pulses", "beans & pulses","Rice","rice", "Dry Beans", "dry beans", "Dry beans","Tofu", "tofu"))) {
      return(poundsOfFood*WFootprint[1])
    } else if (any(Type == c("Cheese", "cheese", "Dairy Products", "dairy products", "Milk", "milk","Yogurt","yogurt"))) {
      return(poundsOfFood*WFootprint[2])
    } else if (any(Type == c("Salmon", "salmon", "Shrimp", "shrimp","Tuna","tuna", "Fish and Shellfish", "Fish & Shellfish", "Fish", "Shellfish", "fish", "shellfish"))) {
      return(poundsOfFood*WFootprint[3])
    } else if (any(Type == c("Fruits", "fruits", "Oranges", "oranges","Peaches", "peaches", "Strawberries", "strawberries", "Apples", "apples", "Bananas", "bananas","Grapes", "grapes"))) {
      return(poundsOfFood*WFootprint[4])
    } else if (any(Type == c("Beef", "beef", "Chicken", "chicken", "Eggs", "eggs", "Pork", "pork", "Turkey", "turkey", "Meat", "Poultry", "Meat and Poultry", "Meat & Poultry", "meat", "poultry"))) {
      return(poundsOfFood*WFootprint[5])
    } else if (any(Type == c("Nuts", "nuts"))) {
      return(poundsOfFood*WFootprint[6])
    } else if (any(Type == c("Vegetable Oils", "Vegetable oils", "vegetable oils",
                             "Fats & Oils", "Fats and Oils", "Fats", "Oils", "oils", "fats", "fats & oils", "fats and oils", "Butter", "butter"))) {
      return(poundsOfFood*WFootprint[7])
    } else if (any(Type == c("Sweetener", "Sugar", "sweetener", "sugar"))) {
      return(poundsOfFood*WFootprint[8])
    } else if (any(Type == c("Vegetables", "vegetables",
                             "Broccoli", "broccoli","Cabbage", "cabbage", "Carrots", "carrots","Lettuce","lettuce", "Potatoes", "potatoes", "Tomatoes", "tomatoes"))) {
      return(poundsOfFood*WFootprint[9])
    }
  }
  
  Cost = function(poundsOfFood, type = NULL) {
    ## Implicit Cost = The tax value from this much CO2 emissions
    ## Explicit Cost = Financial cost (the cost on the food that goes to waste)
    x = foodToEmissions(poundsOfFood) / 2.35 * 0.028
    averageMealSize = 1.984 #lbs
    averageCACost = 2.73/1.984 #dollars per pound 
    averageNationCost = 2.67/1.984 #dollars per pound
    print(paste(foodToEmissions(poundsOfFood, Type = type), " KgCO2e, which translates to:", collapse = " ", sep = ""))
    print(paste("Implicit Cost (based on gasoline tax): $", x, collapse = " ", sep ="")) 
    print(paste("Dollar Value of Food Wasted (based on meal cost in CA): $", round(poundsOfFood * averageCACost, 2), collapse = " ", sep = ""))
    print(paste("Dollar Value of Food Wasted (based on nationwide meal cost): $", round(poundsOfFood * averageNationCost, 2), collapse = " ", sep = ""))
    print(paste("This much food used up", round(waterFootprint(poundsOfFood, Type = type),2), "liters of water in the production."))
    foodToEmissions(poundsOfFood, Type = type, compare = TRUE)
  }
   
  output$text1 = renderText({
    if (input$Specific == TRUE) { 
      paste(foodToEmissions(input$poundsOfFood, Type = input$Type1), " KgCO2e, which translates to:")
  } else if (input$General == TRUE) {
      paste(foodToEmissions(input$poundsOfFood, Type = input$Type), " KgCO2e, which translates to:")
  } else {
    paste(foodToEmissions(input$poundsOfFood, Type = ""), " KgCO2e, which translates to:")
  }
})

  output$text2 = renderText ({
    if (input$Specific == TRUE){
      print(paste("Implicit Cost (based on gasoline tax): $", round(foodToEmissions(input$poundsOfFood, Type = input$Type1)/ 2.35 * 0.028,2), collapse = " ", sep =""))  
    } else if (input$General == TRUE) {
      print(paste("Implicit Cost (based on gasoline tax): $", round(foodToEmissions(input$poundsOfFood, Type = input$Type)/ 2.35 * 0.028,2), collapse = " ", sep ="")) 
    } else {
      print(paste("Implicit Cost (based on gasoline tax): $", round(foodToEmissions(input$poundsOfFood, Type = "")/ 2.35 * 0.028,2) , collapse = " ", sep =""))
    } 
  })

  output$text3 = renderText ({
    averageCACost = 2.73/1.984 #dollars per pound 
      print(paste("Dollar Value of Food Wasted (based on meal cost in CA): $", round(input$poundsOfFood * averageCACost, 2), collapse = " ", sep = ""))
  })

  output$text4 = renderText ({
    averageNationCost = 2.67/1.984 #dollars per pound
    print(paste("Dollar Value of Food Wasted (based on nationwide meal cost): $", round(input$poundsOfFood * averageNationCost, 2), collapse = " ", sep = ""))
  })

  output$text5 = renderText ({
    if (input$Specific == TRUE) {
      print(paste("This much food used up", round(waterFootprint(input$poundsOfFood, Type = input$Type1),2), "liters of water in the production."))
      } else if (input$General == TRUE) {
      print(paste("This much food used up", round(waterFootprint(input$poundsOfFood, Type = input$Type),2), "liters of water in the production."))
    } else {
      print(paste("This much food used up", round(waterFootprint(input$poundsOfFood, Type = ""),2), "liters of water in the production."))
    }
  })

  output$text6 = renderText({
    if (input$Specific == TRUE) {
      print(paste("The water footprint of ",  input$poundsOfFood, " lb food waste is", round(waterFootprint(input$poundsOfFood, Type = input$Type1)/2500000 ,2),  "Olympic sized swimming pool."))
    } else if (input$General == TRUE) {
      print(paste("The water footprint of ",  input$poundsOfFood, " lb food waste is", round(waterFootprint(input$poundsOfFood, Type = input$Type)/2500000 ,2),  "Olympic sized swimming pool."))    
      } else {
        print(paste("The water footprint of ",  input$poundsOfFood, " lb food waste is", round(waterFootprint(input$poundsOfFood, Type = input$Type1)/2500000 ,2),  "Olympic sized swimming pool."))    
      }
  })

  output$text7 = renderText ({
    if (input$Specific == TRUE) {
      paste("The carbon footprint of", input$poundsOfFood, "lb of", input$Type1, "is equivalent to driving", round(foodToEmissions(input$poundsOfFood, input$Type1)*1000/emission.car,2), "miles on an average vehicle")
    } else if (input$General == TRUE) {
      paste("The carbon footprint of", input$poundsOfFood, "lb of", input$Type, "is equivalent to driving", round(foodToEmissions(input$poundsOfFood, input$Type)*1000/emission.car,2), "miles on an average vehicle")
    } else {
      paste("The carbon footprint of", input$poundsOfFood, "lb of food waste is equivalent to driving", round(foodToEmissions(input$poundsOfFood)*1000/emission.car,2), "miles on an average vehicle")
    }
  })
})