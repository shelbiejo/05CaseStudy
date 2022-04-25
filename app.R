library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)

# check to see if data exists in environment
if (!exists("injuries")) {
    injuries <-vroom("injuries.tsv.gz")
    products <- vroom("products.tsv")
    population <-vroom("population.tsv")
}

#renaming columns so the tables can have better titles

injuries <- injuries %>%
  rename("Diagnosis" = diag,
         "Body Part"= body_part,
         "Location of incident"=location)




# code to get the products
prod_codes <- setNames(products$prod_code, products$title)

#useful factor lumping function

count_top <- function(df, var, n = 5) {
    df %>%
        mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
        group_by({{ var }}) %>%
        summarise(n = as.integer(sum(weight)))
}

# start the ui
ui <- fluidPage(
  #adding title
  titlePanel("Injuries in 2017"),
    #select product user input
    fluidRow(
        column(8,
               selectInput("code", "Source of injury",
                           choices = setNames(products$prod_code, products$title),
                           width = "100%"
               )
        ),
        #user input to select variable plot
        column(2, selectInput("y", "Y axis", c("rate", "count")))
    ),
    
    fluidRow(
        #output from server as tables
        column(4, tableOutput("Diagnosis")),
        column(4, tableOutput("Body Part")),
        column(4, tableOutput("Location of incident"))
    ),
    fluidRow(
        # output from server as plot
        column(12, plotOutput("age_sex"))
    ),
    fluidRow(
        column(2, actionButton("story", "Tell me a story")),
        column(10, textOutput("narrative"))
    )
)
#>>

#<< server
server <- function(input, output, session) {
    # make a reactive to select product code
    selected <- reactive(injuries %>% filter(prod_code == input$code))
    
    # output table for diagnosis
    output$Diagnosis <- renderTable(count_top(selected(), Diagnosis), width = "100%")
    
    # output table for body part
    output$`Body Part` <- renderTable(count_top(selected(), `Body Part`), width = "100%")
    
    # output table for location
    output$`Location of incident`<- renderTable(count_top(selected(), `Location of incident`), width = "100%")
    
    # reactive for getting rate of injury per 10,000 and raw number
    summary <- reactive({
        selected() %>%
            count(age, sex, wt = weight) %>%
            left_join(population, by = c("age", "sex")) %>%
            mutate(rate = n / population * 1e4)
    })
    # output a plot
    output$age_sex <- renderPlot({
        if (input$y == "count") {
            summary() %>%
                ggplot(aes(age, n, colour = sex)) +
                geom_line() +
            theme_bw()+#changing theme of plot
                labs(y = "Estimated number of injuries")
        } else {
            summary() %>%
                ggplot(aes(age, rate, colour = sex)) +
                geom_line(na.rm = TRUE) +
            theme_bw()+ #changing theme of plot
                labs(y = "Injuries per 10,000 people")
        }
    }, res = 96)
    
    #tell a story based on an action button
    narrative_sample <- eventReactive(
        list(input$story, selected()),
        selected() %>% pull(narrative) %>% sample(1)
    )
    output$narrative <- renderText(narrative_sample())
}
#>>

shinyApp(ui, server)
