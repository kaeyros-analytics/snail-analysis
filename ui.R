dashboardPage(
  dashboardHeader(title = "Snail analysis"),
  dashboardSidebar(
    #tags$style(".well {background-color:#186A3B;}"),
    selectInput("loc", "Localisation", choices = c("All",as.character(unique(data_f$locality)))),
    selectInput("season", "Season", choices = c("All",as.character(unique(data_f$Season)))),
    actionButton("action", label = "Apply filters")
  ),
  dashboardBody(
   tabsetPanel(
     tabPanel("Collectors",
              br(),
              br(),
              fluidRow(
                tags$head(tags$style(HTML(".small-box {height: 100px}"))),
                valueBoxOutput("nb_col", width = 2),
                valueBoxOutput("nb_house", width = 2),
                valueBoxOutput("coll_exp", width=3.),
              ),
              fluidRow(
                box(width = 6, title= "", plotlyOutput("gender") ),
                box(width = 6, title= "", plotlyOutput("age") ),
                box(width = 6, title= "", plotlyOutput("mar_sta") ),
                box(width = 6, title= "", plotlyOutput("edu_level") ),
                box(title= "", plotlyOutput("main_ac"), width=6,  height = 465 ),
                box(title= "", tags$img(src="escargot5.avif", height=400), width=6, height = 465)
                #tags$img(src="escargot1.jpg", height=465, width="50%")
                #box(width = 6, title= "ras", plotOutput("region") ),

              )

              ),
     tabPanel("Snails",
              br(),
              br(),
              fluidRow(
                box(width = 6, title="", plotlyOutput("best_zone")),
                box(width = 6, title="", plotlyOutput("col_zone")),
                box(width = 6, title="", plotlyOutput("month")),
                box(width = 6, title="", plotlyOutput("time")),
                box(width = 6, title="", plotlyOutput("pestqty")),
                box(width = 6, title="", plotlyOutput("pestsize")),
              )
              ),
     tabPanel("More graphics",
              br(),
              br(),
              fluidRow(
                box(width = 6, title="", plotlyOutput("buckets_bzone")),
                box(width = 6, title="", plotlyOutput("genderbucket")),
                box(width = 6, title="", plotlyOutput("regionbucket")),
                box(width = 6, title="", plotlyOutput("mainactbucket")),
              )),
     tabPanel("Statistics",
              br(),
              br(),
              fluidRow(
                box(width = 6, title="Table of correlation matrix", dataTableOutput("cor_mat"), height = 465),
                box(width = 6, title="Visualization of correlation matrix", plotOutput("corr"), height = 465),
                box(width = 12, title="Independence test between level of education and opinion on the effect
                    of chemicals on quantity of snail", textOutput("test1")),
                box(width = 12, title="Independence test between level of education and opinion on the effect
                    of chemicals on snail size", textOutput("test2")),
              )
              ),
     tabPanel("Conclusions",
              br(),
              br(),
              fluidRow(
                box(width = 12, title="Conclusions", htmlOutput("conclusion")) #, height = 465
              )
              )
   ),
   # setBackgroundImage(
   #   src = "escargot1.jpg",
   #   shinydashboard = TRUE
   # ),

  ),
  skin = "blue"

)
