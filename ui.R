shinyUI(
  fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("GREEN FOOTPRINT"),
    
    navbarPage(
      "Calculate Your Daily Carbon Footprint since today!",
      tabPanel("Introduction", 
               sidebarPanel(img(src="carbon-footprint.png", height = 400, width = 250),
                            em(textOutput("carbon_credit")),
                            a("www.imgbin.com/download-png/uPQk6ZLC")
                            ),
               mainPanel(
                 h3("What is Green Footprint?"),
                 textOutput("overall_intro"),
                 h3("How to use Green Footprint?"),
                 p("Under the Calculator section, begin by choosing your diet types for breakfast, lunch, and dinner. You can select from vegan, vegetarian, chicken, pork, seefood, beef, lamb, or omnivorous average for types of diet. An 'omnivorous average' means you ate mutiple types of meat for that meal. "),
                 
                 p("Then, enter up to three types of transportation you took and the distance you travelled with them respectively. For transportation, you can choose from walking, biking, taking bus, and driving cars, where cars are divided into three categories: gasoline, hybrid, and electric. For other types of public transportations, you may choose bus. If you want to leave any opinions blank, select '--Choose--' for Diet and 'No transportation' for Transportation. ")
                 ) 
               ),
      
      tabPanel("Calculator",
               tabsetPanel(
                   tabPanel("Diet",
                            sidebarPanel(
                              selectInput(inputId = "diet_type1",
                                          label = "Breakfast type",
                                          choices = c("-- Choose --", unique(diet_carbon$type))
                              ),
                              selectInput(inputId = "diet_type2",
                                            label = "Lunch type",
                                            choices = c("-- Choose --", unique(diet_carbon$type))
                              ),
                              selectInput(inputId = "diet_type3",
                                              label = "Dinner type",
                                              choices = c("-- Choose --", unique(diet_carbon$type))
                              ),
                              actionButton("action1", "Calculate", class = "btn-primary")
                            ),
                            
                            tableOutput(outputId = "diet_table"),
                            
                            p("Your total amount of carbon produced (kilograms) through daily diet is:"),
                            span(textOutput(outputId = "diet_carbon_total"), style = "font-size:30px")
                            ),
                   
                   tabPanel("Transportation", 
                            sidebarPanel(
                              selectInput("trans_select_1", 
                                          label = "Transportation type 1 ", 
                                          choices = unique(transport_gram_per_mile$trans_type)
                              ),
                              numericInput("distance_1", 
                                           label = "Distance 1 (miles)", 
                                           min = 0,
                                           value = 0
                              ),
                              selectInput("trans_select_2", 
                                          label = "Transportation type 2", 
                                          choices = unique(transport_gram_per_mile$trans_type)
                              ),
                              numericInput("distance_2", 
                                           label = "Distance 2 (miles)", 
                                           min = 0,
                                           value = 0
                              ),
                              selectInput("trans_select_3", 
                                          label = "Transportation type 3", 
                                          choices = unique(transport_gram_per_mile$trans_type)
                              ),
                              numericInput("distance_3", 
                                           label = "Distance 3 (miles)", 
                                           min = 0,
                                           value = 0
                              ),
                              actionButton("action2", "Calculate", class = "btn-primary")
                            
                            ),
                            
                            mainPanel(
                              tableOutput(outputId = "trans_carbon"),
                              p("Your total amount of carbon produced (kilograms) through transport is:"),
                              span(textOutput(outputId = "trans_carbon_total"), style = "font-size:30px")
                            )
                          ),
                  
                   tabPanel("Summary and Next Steps", 
                            
                            mainPanel(
                              h4("You emitted this amount of carbon dioxide (kilograms) today, WHOA!"),
                              span(textOutput("carbon_total"), style = "font-size:30px"),
                              h4("YOUR Diet vs. Transportation:"),
                              plotOutput("diet_trans"),
                              h4("Your carbon emission today (green line) in GLOBAL scale:"),
                              plotOutput(outputId = "global_co2"),
                              h4("Great job! But you can reduce more through the next steps:"),
                              p("1. You can always reduce your carbon emission by choosing a vegan/vegetarian diet or consuming less beef or lamb."),
                              p("2. You can walk, bike or taking public transit to work or school. "),
                              p("3. If you need a car, you can save two thirds of your carbon dioxide emission by choosing a hybrid over a regular gasoline car. "),
                              p("4. An electric car can further reduce your emission by half."),
                              p("If you are already optimizing your carbon emissions, CHEERS! You ARE the winner and leader!"),
                              p("For more detailed information, please refer to the Data Sources section!")
                            )
                  )
               )
      ),
      
      tabPanel("Data Sources", 
               mainPanel(
                 textOutput("source_intro"),
                 h3("Carbon dioxide emission data for different types of diet"),
                 tableOutput("diet_original_data"),
                 h3("Carbon dioxide emission data for different types of transportation"),
                 tableOutput("transport_original_data"),
                 h4("Source links:"),
                 textOutput("diet_link"),
                 textOutput("car_link"),
                 textOutput("not_car_link"),
                 textOutput("bus_link"),
                 textOutput("worldbank_link"),
                 textOutput("worldbank_pop_link"),
                 textOutput("scale"),
                 textOutput("other_link")
               )
               ),
      
      tabPanel("Team", 
               mainPanel(
                 h2("Shuhan Song and Ruoyu Wang"),
                 h3("宋舒涵，王若宇"),
                 textOutput("our_team"),
                 span(textOutput("thank_note"), style = "font-size:30px; color:darkgreen")),
                 img(src="BrenArtLogo.jpg", height = 400, width = 400),
               em(textOutput("art_credit")),
               a("www.brengarb.org/products/bren-logo-mural-signed-by-artist-alison-horst")

               )
    )
  )
)

