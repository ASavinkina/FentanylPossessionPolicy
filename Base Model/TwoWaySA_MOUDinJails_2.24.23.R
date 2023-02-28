library(markovchain)
library(diagram)
library(ggplot2)
library(tidyverse)
library(kableExtra)
library(priceR)


# Alexandra Savinkina
# 2/24/23

# Code to run threshhold analyses looking at:

# 1: Amount of MOUD linkage that leads to it being more effective
# 2: MOUD retention post-incarceration that would lead to it being more effetive
# 3: Post-incarceration death multipliers

# Key Inputs:



# Key Inputs:


#   Time frame of analysis

years= 5 # 1,3,5,10

#  How much fentanyl is a felony? These are the three major scenarios that need to be selected for analysis.

#"Greater than 4g" = "1", 
#"Greater than 1g" = "2",
#"Any amount" = "3"

atRisk <- "1"

#     Likelihood of diversion program instead of incarceration given arrest. 
#       Baseline either 0 if no diversion program or 10% if diversion program exists. Range 0-20%
diversionProgram <- 0

#     Likelihood of being offered MOUD in jails. Baseline either 0 if no MOUD
#       Offered in prisons, or 100% if MOUD offered in prisons. Range 0-1.
MOUD_Jail1 <- 0

#     Multi-way sensitivity analysis threshholds:

#     Likelihood of continuing on MOUD (linking to care) upon exit from prison if started
#       Treatment in prison. Baseline 48%, range 25%-75%.
MOUD_Jail_list2 <- rep(seq(from = 0, to = 1, by = 0.05),21)
Duration_MOUD_Jail_list <- rep(seq(from=6, to=26, by=1), each=21)
PostJailRiskMultiplier <- rep(seq(from=0, to=1, by = 0.05),21)

MOUD_list <- data.frame(MOUD_Jail_list2,Duration_MOUD_Jail_list,PostJailRiskMultiplier)

All_Data <- data.frame(matrix(NA, nrow = length(MOUD_Jail_list2), ncol = 6))

colnames(All_Data) <- c("MOUD_jail2", "Duration_MOUD_Jail","Deaths Multiplier 1","Deaths Multiplier 2","Deaths","Incarcerations")

#     Multiplier for death given incarceration in last month. Baseline 40x   
#Arrest_Mult_Death1 <- seq(from=0, to=40, by=2)

#     Multiplier for death given incarceration in last year. Baseline 10x 
#Arrest_Mult_Death2 <- seq(from=0, to=20, by=1)



# Function that will run all code to determine deaths, incarcerations, and costs of fentanyl felonization.
#   This function takes 4 parameters:
#   years = years of simulation. Can take any number, suggested 1,3,5,10.
#   atRisk = what is the felony policy? 
#         "Greater than 4g" = "1", 
#         "Greater than 1g" = "2",
#         "Any amount" = "3"
#   recidProgram = is there a diversion program? (0 no, 1 yes) - if yes, 10% arrested go to diversion
#   MOUD_Jail = are MOUD offered in prison? (O no, 1 yes) - if yes, 100% get MOUD in prison, 48% link to MOUD post-prison


  
  #  PSA inputs than move through a range
  
for (i in 1:length(Duration_MOUD_Jail_list)) {
    
  MOUD_Jail2 <- MOUD_list[i,1]
  Duration_MOUD_Jail <- MOUD_list[i,2]
  
  #     Multiplier for death given incarceration in last month. Baseline 40x   
  Arrest_Mult_Death1 <- 40* MOUD_list[i,3]
  
  Arrest_Mult_Death1 <- ifelse(Arrest_Mult_Death1==0,1,Arrest_Mult_Death1)
  
  #     Multiplier for death given incarceration in last year. Baseline 10x 
  Arrest_Mult_Death2 <- 10* MOUD_list[i,3]
  
  Arrest_Mult_Death2 <- ifelse(Arrest_Mult_Death2==0,1,Arrest_Mult_Death2)
  
  #  PSA inputs than move through a range
  
  # if diversion program exists, set likelihood to 0.1 upon arrest:
  
  recidProgram <- ifelse(diversionProgram==1, 0.1, 0)
  
  #   Duration of MOUD, community and post-incarceration (if available), in months. 
  #     The base analysis uses an estimate of 6 months, PSA ranges from 3 to 9.
  Duration_MOUD <- 6
  
  #   Rate of arrest for drug posession. We start at a baseline of 13% from literature. 
  #     We then range from 6.5% to 18% annually (converted to monthly).
  Arrests <- 1-exp(-(-log(1-0.13))/12)
  Jail_Rate <- Arrests
  
  #   Multiplier for negative outcomes post-incarceration. 
  
  #     Multiplier for subsequent arrest given prior incarceration. Baseline 2.43x
  Arrest_Mult_Arrest <- 2.43

  
  
  #     Cost of diversion program. Baseline $500/yr/person, range of $250-$750/year/person.
  recid_Cost <- 500/12
  
  #   Likelihood of not completing recidivism program and going to prison, annual.
  #     Max from 2017 data from Colorado, min from 2021 data.
  recidJail <- 1-exp(-(-log(1-0.11))/12)
  
  
  #     Likelihood of continuing on MOUD (linking to care) upon exit from prison if started
  #       Treatment in prison. Baseline 48%, range 25%-75%.
  #MOUD_Jail2 <- 0.48
  
  #     Population size for those with opioid use disorder/opioid posession in Colorado.
  #       Baseline estimate 139,867. Range 37,000-166,000
  N_pop1 <- 139867
  
  #   Population on MOUD in the community annually. Baseline estimate from Colorado around 9,000.
  MOUD_pop <- read.csv(file="MOUDstats.csv")
  MOUD_pop1 <- MOUD_pop[which(MOUD_pop$States=="Colorado"),9]
  
  #   Proportion of population with OUD/opioid possession in possession of fentanyl. 
  #     Baseline estimate 50%, range 0-1.
  PropFentanyl <-0.5
  
  #   Death rate, monthly, from opioid overdose. Number of opioid deaths in 2021 according to CDC.
  #     Divided by population with opioid possession. Divided by 12 for monthly data.
  #     80,000 is the number of opioid overdoses seen in the US in 2021. Krawczyk paper estimates
  #     0.02 of US drug use in Colorado. 0.02* 80,000 ~ 1,600 in 2021. CDC estimates ~ 1,500 drug deaths in Colorado in 2020.
  #     We will use 1500 here as drug deaths for opioids have gone up.
  Death_Rate<- 1-exp(-(-log(1-(1500/N_pop1)))/12) #0.0008981294
  
  #   Proportion of people within high, medium, and low possession levels. 
  #     High: >=4g, medium 1-4g, low: under 1g
  PropHigh <- 0.17
  PropMed <- 0.18
  PropLow <- 0.65
  
  
  #   Proportion at risk is used to determine what proportion of the population is at risk of
  #   arrest and incarceration given different felonization laws and proportion of population in
  #   each possession amount group.
  Prop_AtRisk <- ifelse(atRisk=="1", PropHigh,
                        ifelse(atRisk=="2", PropHigh+PropMed,
                               ifelse(atRisk=="3", PropHigh+PropMed+PropLow,)))
  
  
  
  #   Death rate, monthly, from opioid overdose in prisons. We set this estimate low, but not impossibly low.
  Death_Rate_Jail <- 0.00001/12
  
  
  #   Duration of diversion program, 12 months.
  recidDuration <- 12 # months
  
  #   Likelihood of incarceration given recidivism program. Normal arrest rate: 13%. Arrest rate with diversion program:
  #     7%, so around 1/2.
  recidIncarcMulti <- 0.5
  
  #   In model, in-prison MOUD doesn't matter, only linkage to care does, so we multiply the two
  #     Probabilities into one.
  MOUD_Jail <- MOUD_Jail1 * MOUD_Jail2
  
  #   For jail costs by state, we read-in data for all states, to use Colorado.
  state_cost <- read.csv(file='prisoncosts_state.csv')
  state_cost <- state_cost[,c(1,5)]
  Cost_Jail <- as.numeric(state_cost[which(state_cost$State=="Colorado"),2])
  
  #   For cost of MOUD we use cost of buprenorphine from NSDUH.
  Cost_MOUD <- 5980/12
  
  #   For cost of death we use the medical system costs of a fatal overdose.
  Cost_Death <- 5462 
  
  #   Calculate MOUD community rate of starting MOUD treatment: number on MOUD (annual)
  #     in Colorado / OUD population Colorado. From annual to monthly.
  MOUD_Community1 <- (MOUD_pop1/N_pop1)
  MOUD_Community <- 1-exp(-(-log(1-MOUD_Community1))/12)
  
  
  # Create dataframe to store PSA results.
  
  Data_Full <- data.frame(matrix(nrow=0,ncol=18))
  
  # Run PSA
  
  N_pop <- N_pop1 * PropFentanyl
  
  # Rate of first felony:
  
  Felony_1 <-1-exp(-(-log(1-Jail_Rate)*Prop_AtRisk))
  
  # Rate of second felony
  
  Felony_2 <- 1-exp(-(-log(1-Jail_Rate)*Prop_AtRisk*Arrest_Mult_Arrest))
  
  
  # Rate of death, not directly after incarceration
  
  Death_1 <- Death_Rate
  
  # Probability of death directly following incarceration (converting to and from rates)
  
  Death_2 <- 1-exp(-(-log(1-Death_Rate)*Arrest_Mult_Death1))
  
  Death_3 <- 1-exp(-(-log(1-Death_Rate)*Arrest_Mult_Death2))
  
  # Model
  
  # Model
  
  trans_mat <- matrix(c(1-Felony_1-Death_1-MOUD_Community,MOUD_Community,Felony_1*recidProgram,0,Felony_1*(1-recidProgram),0,0,0,0,Death_1, # Not Jailed
                        1/Duration_MOUD,1-1/Duration_MOUD,0,0,0,0,0,0,0,0, #Community MOUD
                        0,MOUD_Community,1-1/recidDuration-recidJail-Death_1-MOUD_Community, 1/recidDuration,recidJail,0,0,0,0,Death_1, #in recidivism program
                        1/12,MOUD_Community,0,1-(1/12)-MOUD_Community-Felony_1*recidIncarcMulti-Death_1,Felony_1*recidIncarcMulti,0,0,0,0,Death_1, # Year post-recidivism program: less likely to be inarcerated
                        0,0,0,0,(1-Death_Rate_Jail-1/6),(1/6)*MOUD_Jail,1/6*(1-MOUD_Jail),0,0,Death_Rate_Jail, # Jail
                        0,0,0,0,0,1-1/Duration_MOUD_Jail,0,1/Duration_MOUD_Jail,0,0, #MOUD from jail
                        0,0,0,0,Felony_2, MOUD_Community, 0, 1-Death_3-Felony_2-MOUD_Community,0, Death_3,# Within one month of release
                        0,0,0,0,Felony_2, MOUD_Community, 0, 1-Felony_2-Death_2-1/11-MOUD_Community, 1/11, Death_2,# First year within release
                        0,0,0,0,Felony_2, MOUD_Community, 0, 0,1-Felony_2-Death_1-MOUD_Community, Death_1,# More than 1 post-release
                        0,0,0,0,0,0,0,0,0,1 # Death
  ),nrow = 10, byrow = TRUE)
  trans_mat
  
  
  disc_trans <- new("markovchain",transitionMatrix=trans_mat, states=c("Not arrested","Community MOUD", "Recidivism Program","Year post-recid program", "Arrested","MOUD Jail", "Released, Immediate","Released, within year","Released, post one year","Dead"), name="MC 1") 
  
  #plot(disc_trans)
  
  Current_state<-c(1,0,0,0,0,0,0,0,0,0)
  steps<-years*12
  
  Data <- data.frame(matrix(0,nrow=steps,ncol=11))
  colnames(Data) <- c("month","Not arrested","Community MOUD","Recidivism Program","Year post-recid program", "Arrested","MOUD Jail","Released_immediate","Released_1yr","Released_post1yr","Dead")
  Data$month <- c(1:steps)
  
  for (j in 1:steps) {
    
    finalState<-Current_state*disc_trans^j #using power operator
    Data[j,2:11] <- finalState
    
  }
  
  Data_People1 <- Data
  Data_People <- Data*N_pop
  Data_People$month <- Data_People1$month
  
  Data_People$ChargeCost <- Data_People$Arrested*Cost_Jail
  Data_People$DeathCost <- Data_People$Dead*Cost_Death
  Data_People$MOUDCost <- (Data_People$`Community MOUD`+ Data_People$`MOUD Jail`)* Cost_MOUD
  Data_People$RecidCost <- (Data_People$`Recidivism Program`)* recid_Cost
  
  Data_People$ChargeCostCum <- cumsum(Data_People$ChargeCost)
  Data_People$RecidCostCum <- cumsum(Data_People$RecidCost)
  Data_People$MOUDCostCum <- cumsum(Data_People$MOUDCost)
  
  Data_People2 <- Data_People[,c(1,3:15)]
  Data_Long <- gather(Data_People2, key="observation", value="value",-month)
  Data_Long2 <- gather(Data_People, key="observation", value="value",-month)
  
  
  Data_Cost_Long <- Data_Long2[which((Data_Long2$observation=="ChargeCost"|Data_Long2$observation=="DeathCost"
                                      |Data_Long2$observation=="MOUDCost")),]
  Data_Cost_Long$names <- ifelse(Data_Cost_Long$observation=="ChargeCostCum","Incarceration costs",
                                 ifelse(Data_Cost_Long$observation=="MOUDCost", "MOUD Costs", "Death costs"))
  Data_Cost_Long$value <- Data_Cost_Long$value/1000000
  
  Data_Full[1,1:18]<- c(tail(Data_People,1))
  colnames(Data_Full) <- colnames(Data_People)
  Data_Full$Category <- paste("Years=",years,"atRisk=",atRisk,"diversionProgam=",diversionProgram,"MOUD_Jail=",MOUD_Jail1)
  
  All_Data[i,1] <- MOUD_Jail2
  All_Data[i,2] <- Duration_MOUD_Jail
  All_Data[i,3] <- Arrest_Mult_Death1
  All_Data[i,4] <- Arrest_Mult_Death2
  All_Data[i,5] <- Data_Full$Dead
  All_Data[i,6] <- Data_Full$Arrested + Data_Full$Released_immediate + Data_Full$Released_1yr + Data_Full$Released_post1yr
  
  }


All_Data$Deaths_base <- All_Data$Deaths
All_Data$Deaths_Incarc <- All_Data$Incarcerations

All_Data_Full <- cbind(All_Data_Scenario2, All_Data[,c(7,8)])

All_Data_Full$Deaths_fewer <- ifelse(All_Data_Full$Deaths<=All_Data_Full$Deaths_base,1,0 )
sum(All_Data_Full$Deaths_fewer)

All_Data_Full2 <- All_Data_Full[which(All_Data_Full$Deaths_fewer==1 & All_Data_Full$`Deaths Multiplier 2`>=1),]


All_Data$Deaths_basecase <- 5190 
All_Data$Incarcerations_basecase <- 5440 

All_Data$Deaths_Fewer <- ifelse(All_Data$Deaths<=All_Data$Deaths_basecase, 1,0)

sum(All_Data$Deaths_Fewer)

All_Data$Incarcerations_Fewer <- ifelse(All_Data$Incarcerations<=All_Data$Incarcerations_basecase, 1,0)

sum(All_Data$Incarcerations_Fewer)


All_Data2 <- All_Data[which(All_Data$Deaths_Fewer==1),]

All_Data_Scenario2 <- All_Data
