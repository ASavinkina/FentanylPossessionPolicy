library(scales)
library(plotly)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(markovchain)
library(diagram)
library(ggplot2)
library(tidyverse)
library(kableExtra)
library(priceR)



# UI for shiny app looking at felonization of fentanyl posession 

input_element_color <- "primary" 
highlight_color <- "navy" 
regular_color <- "navy"


header <- dashboardHeader(
    tags$li(
        class = "dropdown"),
    title = "Effects of policy felonizing fentanyl possession", titleWidth = 500
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "sidebar",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Source Code", icon = icon("file-code-o"), 
                 href = "https://github.com/ASavinkina/Fentanyl-Policy"),
        menuItem("References", tabName = "references", icon = icon("book"))
    )
)

body <- dashboardBody(
    #     tags$head(tags$style(HTML('
    #         /* logo */
    #         .skin-blue .main-header .logo {
    #                               background-color: #08306b;
    #                               }
    # 
    #         /* logo when hovered */
    #         .skin-blue .main-header .logo:hover {
    #                               background-color: #08306b;
    #                               }
    # 
    #         /* navbar (rest of the header) */
    #         .skin-blue .main-header .navbar {
    #                               background-color: #deebf7;
    #                               }        
    # 
    #         /* main sidebar */
    #         .skin-blue .main-sidebar {
    #                               background-color: #08306b;
    #                               }
    # 
    #         /* active selected tab in the sidebarmenu */
    #         .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
    #                               color: #eff3ff;
    #                               background-color: #084594;
    #                               }
    # 
    #         /* other links in the sidebarmenu */
    #         .skin-blue .main-sidebar .sidebar .sidebar-menu a{
    #                               background-color: #c6dbef;
    #                               color: #000000;
    #                               }
    # 
    #         /* other links in the sidebarmenu when hovered */
    #          .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
    #                               background-color: #fcfbfd;
    #                               }
    #         /* toggle button when hovered  */                    
    #          .skin-blue .main-header .navbar .sidebar-toggle:hover{
    #                               background-color: #084594;
    #          }
    #                               
    #            .box.box-solid.box-primary>.box-header {
    #   #color:#000000;
    #   background:#08306b
    #                     }
    # 
    # .box.box-solid.box-primary{
    # border-bottom-color:#807dba;
    # border-left-color:#c6dbef;
    # border-right-color:#c6dbef;
    # border-top-color:#c6dbef;
    # 
    # 
    # }                   
    #                               
    #                               '))),
    #     
    
    
    
    tabItems(
        tabItem(
            # MAIN DASHBOARD ---------------------------------------------------
            tabName = "dashboard",
            ## INPUTS --------
            column(width = 4,
                   ## Population
                   box(title = "Inputs", width = NULL, solidHeader = TRUE, status = input_element_color,
                       collapsible = TRUE, collapsed = FALSE,
                       selectInput("costarrest",
                                   ("State"),
                                   choices= c(paste(as.character(state.name)),"US Average"), selected="Colorado") ,
                       
                       selectInput("atRisk", ("How much fentanyl is a felony?"), 
                                   choices = list("Greater than 4g" = "1", "Greater than 1g" = "2",
                                                  "Any amount" = "3"), selected = "1"),
                       
                       selectInput("timeframe", ("Length of simulation?"),
                                   choices = list("One year" = 1, "Five years" = 5,
                                                  "Ten years" = 10), selected = 5),
                       
                       sliderInput("propFentanyl", 
                                   ("Proportion of the illicit drug supply that contains fentanyl"), 
                                   min=0, max=1, value=0.5),
                       
                       sliderInput("MOUDJail", 
                                   ("Proportion of incarcerated who start MOUD/are linked to MOUD post-care"), 
                                   min=0, max=1, value=0),
                       
                       sliderInput("RecidProgram", 
                                   ("Proportion of those with first charge who are sent to a diversion program"), 
                                   min=0, max=1, value=0),
                       
                       
                       selectInput("Arrests", 
                                   ("Policing strategy"), 
                                   choices= list("Standard Policing"= 0.13, "Aggressive Policing"= 0.3,
                                                 "Treatment-informed Policing"=0.05), selected= "Standard Policing"),
                       
                       selectInput("costdeath", 
                                   ("Costs associated with a death"), 
                                   choices= list("Healthcare costs only", "Lost productivity costs only",
                                                 "Value of a statistical life"), selected= "Healthcare costs only")
                   )
            ),
            
            ## OUTPUT: plot and metrics --------
            
            column(width = 8,
                   fluidRow(box(textOutput("StateText"), 
                                width=10,
                                background="navy")),
                   
                   
                   fluidRow(valueBoxOutput("Deaths", width=5),
                            valueBoxOutput("Arrests", width=5),
                            valueBoxOutput("Price", width=5),
                            valueBoxOutput("Overdose", width=5),
                            valueBoxOutput("MOUD", width=5),
                            valueBoxOutput("Diversion", width=5))
            )),
        
        # ,
        ## References ----------------------------------------------------------
        tabItem(
            tabName = "references",
            h2("Sources"),
            p(strong("Population in model:"),"The model was populated according to estimates
                of people with OUD in each state and the US as a whole, as estimated by Krawczyk et al. 2022 
                with data from the National Survey on Drug Use and Health. See:", 
              a("Krawczyk et al. 2022",
                href = "https://www.sciencedirect.com/science/article/pii/S0955395922002031?via%3Dihub")),
            p(strong("Percent of people with OUD who unknowingly possess fentanyl:"),
              "Estimated at 60% based on literature sources. See:", 
              a("Macmadu et al. 2017",
                href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5291510/"),
              a("Carroll et al. 2017",
                href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5560423/")),
            p(strong("Amount of drug in possession:"),
              "The proportion of the population that possess >4 grams of drugs, between 1 and 4 grams,
                    and below 1 gram. Based on drug arrest data from 2004, 2008, and 2012. See:", 
              a("Kennedy et al. 2018.",
                href = "https://lawreview.law.ucdavis.edu/issues/52/2/Articles/52-2_Kennedy.pdf")),
            p(strong("Annual overdose probability:"),"Estimated at around 0.8% annually, based on data from 
                CDC on number of overdoses annually and NSDUH for number of people with OUD in the US. See ", 
              a("CDC SARS-CoV-2 Diagnostic, Screening, and Surveillance Testing.",
                href = "https://www.cdc.gov/nchs/pressroom/nchs_press_releases/2021/20211117.htm")),
            p(strong("Overdose multiplier following incarceration:"),"For first month post-release: 40x.
                For the next 11 months: 10x. Then back to baseline. Based on literature estimates. See:",
              a("Ranapurwala et al. 2018",
                href ="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6085027/")),
            p(strong("Annual arrest probability:"),"Estimated at 13% based on arrest numbers for possession
              and population with drug use in the US. See:",
              a("PEW Charitable Trusts",
                href = " https://www.pewtrusts.org/-/media/assets/2022/02/drug-arrests-stayed-high-even-as-imprisonment-fell-from-2009-to-2019.pdf")),
            p(strong("Multiplier on subsequent arrests:"),"2.43x, based on literature estimates. See:",
              a("Belenko et al. 2013",
                href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3859122/")),
            p(strong("Cost of incarceration, by state:"),"Estimated from Vera research on prison spending. See:",
              a("Vera, Prison Spending in 2015",
                href = "https://www.vera.org/publications/price-of-prisons-2015-state-spending-trends/price-of-prisons-2015-state-spending-trends/price-of-prisons-2015-state-spending-trends-prison-spending")),
            p(strong("Cost of an overdose death:"),"Estimated from literature sources. Healthcare only: $5,500,
               lost productivity: $1.4 million, value of a statistical life: $10 million. See:",
              a("Florence et al. 2021",
                href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8091480/")))
        
    )
)



ui <- dashboardPage(header, sidebar, body)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ## Reactive elements
    
    Data_People <- reactive({
        req(input$costarrest,
            input$atRisk,
            input$timeframe,
            input$propFentanyl,
            input$MOUDJail,
            input$RecidProgram,
            input$Arrests,
            input$costdeath,
            cancelOutput = TRUE
        )
        
        
        #Inputs:
        
        atRisk <- input$atRisk
        
        PropHigh <- 0.17
        PropMed <- 0.18
        PropLow <- 0.65
        
        PropFentanyl <- input$propFentanyl
        
        
        Prop_AtRisk <- ifelse(atRisk=="1", PropHigh,
                              ifelse(atRisk=="2", PropHigh+PropMed,
                                     ifelse(atRisk=="3", PropHigh+PropMed+PropLow,)))
        
        
        Duration_MOUD <- 6 # months
        
        #   Death rate, monthly, from opioid overdose. Number of opioid deaths in 2021 according to CDC.
        #     Divided by population with opioid possession. Divided by 12 for monthly data.
        #     80,000 is the number of opioid overdoses seen in the US in 2021. Krawczyk paper estimates
        #     0.02 of US drug use in Colorado. 0.02* 80,000 ~ 1,600 in 2021. CDC estimates ~ 1,500 drug deaths in Colorado in 2020.
        #     We will use 1500 here as drug deaths for opioids have gone up.
        
        CDC_deaths <- read.csv("CDC_Deaths.csv")
        CDC_deaths <- CDC_deaths[which(CDC_deaths$YEAR==2020),]
        CDC_deaths$State2 <-state.name[match(CDC_deaths$STATE, state.abb)]
        
        drug_use_pop_state <- read.csv(file="drugusestate.csv")
        
        #State = input$costarrest
        
        N_pop1 = drug_use_pop_state[which(drug_use_pop_state$State==input$costarrest),2]
        N_pop <- as.numeric(drug_use_pop_state[which(drug_use_pop_state$State==input$costarrest),2])*input$propFentanyl
        
        
        Death_Rate<- 1-exp(-(-log(1-(as.numeric(CDC_deaths[which(CDC_deaths$State2==input$costarrest),"DEATHS"])/N_pop1)))/12) #0.0008981294 for CO
        
        Death_Rate_Jail <- 0.00001/12
        
        Jail_Rate <- 1-exp(-(-log(1-as.numeric(input$Arrests)))/12)
        
        Arrests = input$Arrests
        # 
        # Arrest_text1 <- ifelse(input()$Arrests==0.13, "Standard policing",
        #                        ifelse(input()$Arrests==0.05, "Treatment-informed Policing", "Aggressive Policing"))
        # 
        # Arrest_text2 <- ifelse(input()$Arrests==0.13, "13%",
        #                        ifelse(input()$Arrests==0.05, "5%", "20%"))
        
        
        Arrest_Mult_Arrest <- 2.43
        Arrest_Mult_Death1 <- 40
        Arrest_Mult_Death2 <- 10
        
        
        recidProgram <- input$RecidProgram
        #   Duration of diversion program, 12 months.
        recidDuration <- 12 # months
        
        #   Likelihood of incarceration given recidivism program. Normal arrest rate: 13%. Arrest rate with diversion program:
        #     7%, so around 1/2.
        recidIncarcMulti <- 0.5
        
        recidJail <-1-exp(-(-log(1-0.11))/12)
        
        recid_Cost <- 500/12
        
        MOUD_Jail <- input$MOUDJail*0.48
        
        
        state_cost <- read.csv(file='prisoncosts_state.csv')
        state_cost <- state_cost[,c(1,5)]
        
        
        Cost_MOUD <- 5980/12
        
        Cost_Jail <- as.numeric(state_cost[which(state_cost$State==input$costarrest),2])
        
        Cost_Death <- ifelse(input$costdeath == "Value of a statistical life",  10099517 ,
                             ifelse(input$costdeath == "Healthcare costs only",  5462,
                                    ifelse(input$costdeath == "Lost productivity costs only",  1443151,0 )))
        
        
        MOUD_pop <- read.csv(file="MOUDstats.csv")
        

        MOUD_pop1 <- MOUD_pop[which(MOUD_pop$States==input$costarrest),9]
        
        MOUD_Community1 <- (MOUD_pop1/N_pop1)
        
        MOUD_Community <- 1-exp(-(-log(1-MOUD_Community1))/12)
        
        # Model parameters
        
        # Rate of first felony:
        
        Felony_1 <-Jail_Rate*Prop_AtRisk
        
        # Rate of second felony
        
        Felony_2 <-Jail_Rate*Prop_AtRisk*Arrest_Mult_Arrest
        
        
        # Rate of death, not directly after incarceration
        
        Death_1 <- Death_Rate
        
        # Rate of death directly following incarceration
        
        Death_2 <- Death_Rate*Arrest_Mult_Death1
        
        Death_3 <- Death_Rate*Arrest_Mult_Death2
        
        # Model
        # Model
        
        trans_mat <- matrix(c(1-Felony_1-Death_1-MOUD_Community,MOUD_Community,Felony_1*recidProgram,0,Felony_1*(1-recidProgram),0,0,0,0,Death_1, # Not Jailed
                              1/Duration_MOUD,1-1/Duration_MOUD,0,0,0,0,0,0,0,0, #Community MOUD
                              0,MOUD_Community,1-1/recidDuration-recidJail-Death_1-MOUD_Community, 1/recidDuration,recidJail,0,0,0,0,Death_1, #in recidivism program
                              1/12,MOUD_Community,0,1-(1/12)-MOUD_Community-Felony_1*recidIncarcMulti-Death_1,Felony_1*recidIncarcMulti,0,0,0,0,Death_1, # Year post-recidivism program: less likely to be inarcerated
                              0,0,0,0,(1-Death_Rate_Jail-1/6),(1/6)*MOUD_Jail,1/6*(1-MOUD_Jail),0,0,Death_Rate_Jail, # Jail
                              0,0,0,0,0,1-1/Duration_MOUD,0,1/Duration_MOUD,0,0, #MOUD from jail
                              0,0 ,0,0,Felony_2, MOUD_Community, 0, 1-Death_3-Felony_2-MOUD_Community,0, Death_3,# Within one month of release
                              0,0,0,0,Felony_2, MOUD_Community, 0, 1-Felony_2-Death_2-1/11-MOUD_Community, 1/11, Death_2,# First year within release
                              0,0,0,0,Felony_2, MOUD_Community, 0, 0,1-Felony_2-Death_1-MOUD_Community, Death_1,# More than 1 post-release
                              0,0,0,0,0,0,0,0,0,1 # Death
        ),nrow = 10, byrow = TRUE)
        trans_mat
        
        
        disc_trans <- new("markovchain",transitionMatrix=trans_mat, states=c("Not arrested","Community MOUD", "Recidivism Program","Year post-recid program", "Arrested","MOUD Jail", "Released, Immediate","Released, within year","Released, post one year","Dead"), name="MC 1") 
        
        #plot(disc_trans)
        
        Current_state<-c(1,0,0,0,0,0,0,0,0,0)
        steps<-as.numeric(input$timeframe)*12
        
        Data <- data.frame(matrix(0,nrow=steps,ncol=11))
        colnames(Data) <- c("month","Not arrested","Community MOUD","Recidivism Program","Year post-recid program", "Arrested","MOUD Jail","Released_immediate","Released_1yr","Released_post1yr","Dead")
        Data$month <- c(1:steps)
        
        for (i in 1:steps) {
            
            finalState<-Current_state*disc_trans^i #using power operator
            Data[i,2:11] <- finalState
            
        }
        
        
        Data_People <- Data %>%
            mutate(across(where(is.numeric), ~ .x * N_pop)) %>%
            mutate(month= month/N_pop,
                ChargeCost = Arrested*Cost_Jail,
                DeathCost = Dead*Cost_Death,
                MOUDCost = `Community MOUD`+ `MOUD Jail`* Cost_MOUD,
                RecidCost = `Recidivism Program`* recid_Cost,
                N_pop1 = N_pop1,
                MOUD_Community1 = MOUD_Community1,
                Cost_Jail = Cost_Jail
                
                )
        
    })
    
    Data_Summary <- reactive({
        
        Data_Summary <- Data_People() %>%
            mutate(ChargeCostCum = cumsum(ChargeCost),
                   RecidCostCum = cumsum(RecidCost),
                   MOUDCostCum = cumsum(MOUDCost),
                   Incarcerated = Arrested + Released_immediate + Released_1yr + Released_post1yr) %>%
                   slice(n())
        
        #Data_Summary <- Data_Summary[steps,]
        #Data_Summary <- Data_Summary[steps,]
        
        
        Data_Summary <- list(
            ## Expected outputs
            Incarcerated = Data_Summary$Incarcerated,
            Dead = Data_Summary$Dead,
            ChargeCostCum = Data_Summary$ChargeCostCum,
            RecidCostCum = Data_Summary$RecidCostCum,
            MOUDCostCum = Data_Summary$MOUDCostCum,
            DeathCost = Data_Summary$DeathCost,
            Population = Data_Summary$N_pop1,
            MOUDpop = Data_Summary$MOUD_Community1,
            CostJail = Data_Summary$Cost_Jail
        )
        
        
    })
    
    
    output$StateText<- renderText({
        paste("Estimated population with opioid use disorder in ", input$costarrest,  " is",
              prettyNum(Data_Summary()$Population, big.mark = ","), ", the average cost of a month in prison in ", input$costarrest, "is ", format_dollars(Data_Summary()$CostJail,2), ".",
              "Annual community MOUD level in ",input$costarrest,  " is ", round(Data_Summary()$MOUDpop,2)*100, "%.")
    })
    
    
    
    output$Deaths <- renderValueBox({
        valueBox(prettyNum(trunc(Data_Summary()$Dead), big.mark = ","), "Total opioid overdose deaths",
                 #icon = icon("skull"),
                 color = "red")
    })
    
    output$Arrests <- renderValueBox({
        valueBox(prettyNum(trunc(Data_Summary()$Incarcerated), big.mark = ","), "Total Incarcerations",
                 #icon = icon("trailer"),
                 color = "orange")
    })
    
    output$Price <- renderValueBox({
        valueBox(prettyNum(trunc(Data_Summary()$ChargeCostCum), big.mark = ","), "Total Cost of incarcerations",
                 #icon = icon("trailer"),
                 color = "green")
    })
    
    output$Overdose <- renderValueBox({
        valueBox(prettyNum(trunc(Data_Summary()$DeathCost), big.mark = ","), "Total Cost of Deaths",
                 #icon = icon("trailer"),
                 color = "green")
    })
    
    output$MOUD <- renderValueBox({
        valueBox(prettyNum(trunc(Data_Summary()$MOUDCostCum), big.mark = ","), "Total Cost of MOUD",
                 #icon = icon("trailer"),
                 color = "green")
    })
    
    output$Diversion <- renderValueBox({
        valueBox(prettyNum(trunc(Data_Summary()$RecidCostCum), big.mark = ","), "Total Cost of diversion program",
                 #icon = icon("trailer"),
                 color = "green")
    })
    
    
    
}

# Run the application 
shinyApp(ui= ui, server= server)
