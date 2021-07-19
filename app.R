
# Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ganttrify)
library(here)
library(readxl)
library(rvest)
library(xml2)
library(janitor)
library(DT)
library(googledrive)
library(googlesheets4)

# Loading data from googledrive 
#drive_auth(cache = ".secrets", email = "easa90657@gmail.com", use_oob = TRUE, scopes = "https://www.googleapis.com/auth/drive")
# drive_auth(email = "easa90657@gmail.com")
# gs4_auth(email = "easa90657@gmail.com")
# #drive_deauth()
# options(
#     gargle_oauth_cache = ".secrets",
#     gargle_oauth_email = TRUE
# )
# deadlines in current year 
# gd <- drive_get("current_year")
# dat <- read_sheet(gd, col_names = TRUE, col_types = "cccccccdddDcDcDcccccccccccddc")
#write_csv(dat, "Data/dat.csv")

dat <- read_csv("Data/dat.csv")

# assessments to be included next year 
# gda <- drive_get("assess_dat")
# assess_dat <- read_sheet(gda, col_names = TRUE, col_types = "ccccccdddd") %>% 
#   unique()
# write_csv(assess_dat, "Data/assess_dat.csv")
 
#module programme information
# gdp <- drive_get("prog_dat")
# prog_dat <- read_sheet(gdp, col_names = TRUE, col_types = "ccccd")
#write_csv(prog_dat, "Data/prog_dat.csv")
prog_dat <- read_csv("Data/prog_dat.csv")


ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "flatly"),
    titlePanel("Assessment Planner"),
    tags$hr(),
    tags$p( " This app can be used as a rough guide to visualise your workload based on your optional module choices. 
          Please note that the Assessment deadlines shown are those being used in the current teaching year and may be subject to change."),
    tags$p(" To view the options available to you first select your programme of study from the dropdown menu.
          Select the degree stage for which you are planning to see the deadlines for compulsory modules.
         Add optional modules from the dropdown box to see where their assessments would fall and to plan your workload."),
    tags$p(" The start of the coloured bar denotes the date that an assignment is set, the end denotes the due date of the assignment.
          The width of the bar denotes the weighting of the assignment in terms of the number of credits that the assignment is worth, 
         for example an assignment that comprises 50% of a 20 credit module will be the same width as an assignment that is worth 100% of a 10 credit module."),
    
    fluidRow(
        column(width = 3, 
               selectInput(inputId = "course",
                           label = "Degree Programme",
                           selected = prog_dat$programme_title[1],
                           choices= unique(prog_dat$programme_title))),
        
        column(width = 3,
               radioButtons(inputId = "stage", 
                            label = "Stage",
                            choiceNames = c("Stage 1", "Stage 2", "Stage 3", "Stage4/Msc"),
                            choiceValues = c("1", "2", "3", "8"),
                            selected = 1)),
        column(width = 3,
               uiOutput("mods"))
    ),
    
    plotOutput("gantt"),
    
    fluidRow(
        column(width = 12, 
               dataTableOutput(outputId = "gtable"))),
    
    fluidRow(tags$p(""))
) # end of fluidPage


server <- function(input, output){
    thematic::thematic_shiny()
    dat <- read_csv("Data/dat.csv")
    prog_dat <- read_csv("Data/prog_dat.csv")
    
    course_mods <- reactive({
        prog_dat %>% filter(programme_title %in% input$course) %>% 
            rename("module_code" = mod_code) 
    })
    
    
    course_options <- reactive({
        course_mods() %>%
            filter(comp =="No") %>% 
            mutate("mod_num"= as.numeric(str_extract(module_code, "\\d+"))) %>% 
            separate(mod_num, into = c("Stage", "Junk"), sep = 1) %>% 
            select(-Junk) %>% 
            filter(Stage %in% input$stage)
    })
    
    output$mods <- renderUI({
        selectInput(inputId = "mods",
                    label = "Add optional modules",
                    # choices = prog_dat %>% filter(Course == input$course) %>% select(Module) %>% unique(), 
                    choices = course_options()$module_code,
                    multiple = TRUE)
    })
    
    disp_mods <- reactive({
        c(pull(course_mods() %>% filter(comp == "Yes") %>% select(module_code)), input$mods)
        print(c(pull(course_mods() %>% filter(comp == "Yes") %>% select(module_code)), input$mods))
    })
    gant_dat <- reactive({
        
        dat %>% 
            left_join(., course_mods(), by = "module_code") %>% 
            mutate("mod_num"= as.numeric(str_extract(module_code, "\\d+"))) %>% 
            separate(mod_num, into = c("Stage", "Junk"), sep = 1) %>% 
            select(-Junk) %>% 
            filter(Stage %in% input$stage) %>% 
            filter(module_code %in% disp_mods()) %>% 
            mutate(tot_cred = sem_1_cred + sem_2_cred) %>% 
            mutate(tot_worth = (tot_cred/100)*assm_percent) %>% 
            select(-end_date) %>% 
            rename(end_date = spot_date, wp = comp) 
        
        
        
    })
    
    # spot_dat <- reactive({
    #   gant_dat()%>% select(activity, spot_date) %>% 
    #     mutate("spot_type" = substr(gant_dat()$spot_date, start = 9, stop = 10))
    # })
    
    output$gantt  <- renderPlot( {
        print(gant_dat() %>% select(activity, start_date, end_date, wp, tot_worth))
        
        ganttrify(project = gant_dat(),
                  # spots = spot_dat(),
                  by_date = TRUE,
                  exact_date = TRUE,
                  hide_wp = TRUE,
                  month_number_label = FALSE,
                  alpha_wp = 0.5,
                  line_end = "butt",
                  size_activity = gant_dat()$tot_worth,
                  axis_text_align = "left",
                  colour_palette = c("#31688EFF","#35B779FF"))+
            theme(legend.position = "none",legend.title=element_blank())+
            theme(axis.text.y = element_text (size = 10))
    })
    
    
    output$gtable <- renderDataTable({
        gant_dat() %>%
            select(activity, wp, start_date, end_date, tot_worth)%>% 
            rename("Assesment" = "activity", "Compulsory Module" = "wp", "Assessment released" = "start_date", 
                   "Assessment Due" = "end_date", "Assessment credit value" = "tot_worth")})
} # end of server 




# Run the application 
shinyApp(ui = ui, server = server)
