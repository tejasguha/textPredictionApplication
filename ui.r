library(shiny)
library(shinythemes)
library(shinyalert)
ui <- fluidPage(theme = shinytheme("cosmo"), useShinyalert(),
                tags$head(tags$style(HTML('.shiny-html-output table td {
                               width: 25%;
                                          }'))),
                tags$head(tags$style("#knsOutput table {background-color: #222222; color: white; font-size: 150%}", media="screen", type="text/css")),
                tags$head(tags$style("#markovOutput table {background-color: #222222; color: white; font-size: 150%}", media="screen", type="text/css")),
                tags$head(tags$style("label {font-size: 200%;}")),
                tags$head(tags$style("#text {font-size: 150%; text-align:center;}")),
                tags$head(tags$style("h3 {font-size: 200%; font-weight: normal;}")),
                tags$head(tags$style("hr {border: 2px solid #222222; width: 75%;}")),
                tags$head(tags$style('.navbar-brand {font-size:225%; font-weight:lighter; width: 475px; text-align:left;}')),
                tags$head(tags$style('.navbar-nav {font-size: 150%;}')),
                HTML("<style>.navbar-default .navbar-nav > .active > a, .navbar-default .navbar-nav > .active > a:hover, .navbar-default .navbar-nav > .active > a:focus {font-weight:normal; background-color: #222; }</style>"),
                HTML("<style>.navbar-default .navbar-nav > li > a:hover {background-color: #222; font-weight: normal;}</style>"),
                HTML("<style>.navbar-default .navbar-nav > li > a {font-weight:lighter}</style>"),
                HTML("<style>.navbar {height:150% !important;}</style>"),
  navbarPage("Text Prediction Application", 
             tabPanel("Interact",
                  fluidRow(
                    column(12, align="center",
                      textInput(inputId = "text",
                                  label = "Partial phrase/sentence:",
                                  value = "",
                                  width = "50%"
                                )
                    )
                  ), 
                  fluidRow(column=3, align="center", hr()),
                  fluidRow(
                    column(6, align="center",
                           h3("Modified Kneser-Ney Smoothing Model"),
                           tableOutput(outputId = "knsOutput")
                    ),
                    column(6, align="center",
                           h3("Markov Chain Model"),
                           tableOutput(outputId = "markovOutput")
                    )
                )
             ),
             tabPanel("Technical Information",
                      HTML('
                <p style="display: inline-block; text-align:center; background-color: #09814A; color: white; font-family: Candara; font-size: 150%; font-weight: normal; border-radius: 10px; position: absolute; left: 20%; right: 20%; padding: 1%;line-height:1.5;">
                This web application was made with the Shiny library on R. &nbsp;The models you see from the <b>Interact</b> tab were built with a corpus of tweets, blogs, and news articles. &nbsp;
                Obviously, I did not want my model to predict profanity, so I applied a profanity filter and did some other cleaning to the dataset. &nbsp;<br><br>
                The <b>Markov Model</b> I built is based on the concept of n-Grams: the probability a specific word or phrase follow a preceding one. &nbsp;
                However, the first version of the model I built had a major flaw; it almost always predicted words like <b>the, at, or it</b>. &nbsp;
                In the Natural Language Processing these words are known as <b>stop words;</b> words so frequent there occurence is rarely of any value.
                Thus, to resolve this issue I built the <b>Markov Model</b> such that it eliminates stop words from its analysis of the dataset and the user input.
                <br><br>
                However, I did not want to stop there. &nbsp;After some research and reading articles, I found out about a popular method in natural language processing
                for text prediction: <b>Kneser-Ney Smoothing</b>. &nbsp;The beauty of such an algorithm is that it takes the relative frequency of a phrase in the language
                into account as well as the probability of that phrase following another phrase.  Thus, <b>KNS</b> does not overpredict stop words.  However,
                there are two drawbacks of the <b>KNS method</b>: its lower efficiency and dependence on large datasets. &nbsp;The first issue - efficiency - I addressed through my programming and cautious use
                of complicated functions that eat up RAM.  The second issue, needing large datasets, I addressed by modifying the algorithm so that a word/phrase can get a score even if it is not explicitly present in the dataset.

                
                </p>')
                      
             ),
             tabPanel("About",
                      HTML('
                      <p style="display: inline-block; text-align:center; background-color: #007bff; color: white; font-family: Candara; font-size: 150%; font-weight: normal; border-radius: 10px; position: absolute; left: 20%; right: 20%; padding: 1%;line-height:1.5;">
                            &nbsp; <b>My name is Tejas Guha and I am a rising freshman to the University of Maryland, College Park with a Banneker/Key scholarship (full ride) planning to study electrical engineering with a computer engineering minor.</b><br><br>
                           This was my capstone project for the "Data Science Specialization"
                           by Johns Hopkins University on Coursera. &nbsp;A ten course sequence (each about a month long) starting from an introduction to R programming then quickly diving to more advanced topics like 
                           statistical inference and machine learning. &nbsp;Over a period of six summers, I have taken these courses between playing soccer with friends
                           and licking ice cream.&nbsp; Initially, it was just a way to combat boredom.&nbsp; Now, it sprouted an interest in computer science and the idea of big data.&nbsp; Making sense out of the nonsense.&nbsp;
                           Without these courses, I would not have pursued a research internship at Johns Hopkins School of Medicine to help in the fight to
                           end cancer. &nbsp;Nor would I be where I am today. &nbsp;These courses partially define me because I grew up academically with them.
                           
                           </p>')
                      
                      )
             )
           
  )
    
