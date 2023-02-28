library(markovchain)
library(diagram)
library(ggplot2)
library(tidyverse)
library(kableExtra)
library(priceR)
    
    
# Alexandra Savinkina
# 1/25/23

# Code to run PSA for fentanyl project, ranging key inputs through their ranges.
# Code runs as a function, input years, fentanyl felony policy, intervention presence,
#   and number of PSA runs.
    
# Key Inputs:
    
#  The number of PSA runs. Input any number, baseline 1,000.

#n= 1000

#   Time frame of analysis

#years= 5

#  How much fentanyl is a felony? These are the three major scenarios that need to be selected for analysis.

    #"Greater than 4g" = "1", 
    #"Greater than 1g" = "2",
    #"Any amount" = "3"

#atRisk <- "1"

#     Likelihood of diversion program instead of incarceration given arrest. 
#       Baseline either 0 if no diversion program or 10% if diversion program exists. Range 0-20%
#diversionProgram <- 0

#     Likelihood of being offered MOUD in jails. Baseline either 0 if no MOUD
#       Offered in prisons, or 100% if MOUD offered in prisons. Range 0-1.
#MOUD_Jail1 <- 0

#  PSA inputs than move through a range

fentanyl_deaths_PSA <- function(years, atRisk, diversionProgram, MOUD_Jail1, n=1000) {
  
# if diversion program exists, set likelihood to 0.1 upon arrest:
  
recidProgram <- ifelse(diversionProgram==1, 0.1, 0)

#   Duration of MOUD, community and post-incarceration (if available), in months. 
#     The base analysis uses an estimate of 6 months, PSA ranges from 3 to 9.
Duration_MOUD <- runif(n, min = 3, max = 9)

#   Rate of arrest for drug posession. We start at a baseline of 13% from literature. 
#     We then range from 6.5% to 18% annually (converted to monthly).
Arrests <- 1-exp(-(-log(1-0.13))/12)
Jail_Rate <- runif(n, min = Arrests*.5, max = Arrests*1.5) #Arrests/12

#   Multiplier for negative outcomes post-incarceration. 

#     Multiplier for subsequent arrest given prior incarceration. Baseline 2.43x
Arrest_Mult_Arrest <- runif(n, min = 1.22, max = 3.65) #2.43

#     Multiplier for death given incarceration in last month. Baseline 40x   
Arrest_Mult_Death1 <- runif(n, min = 20, max = 51) #40

#     Multiplier for death given incarceration in last year. Baseline 10x 
Arrest_Mult_Death2 <- runif(n, min = 9.5, max = 11.7) #10
    
#     Likelihood of diversion program instead of incarceration given arrest. 
#       Baseline either 0 if no diversion program or 10% if diversion program exists. Range 0-20%
#recidProgram <- runif(n, min = 0, max = 0.2) 

#     Cost of diversion program. Baseline $500/yr/person, range of $250-$750/year/person.
recid_Cost <- (runif(n, min = 500*.5, max = 500*1.5))/12 #500/12

#   Likelihood of not completing recidivism program and going to prison, annual.
#     Max from 2017 data from Colorado, min from 2021 data.

recidJailRate_low <- 1-exp(-(-log(1-0.11))/12)
recidJailRate_high <- 1-exp(-(-log(1-0.21))/12)
recidJail <- runif(n, min = recidJailRate_low, max = recidJailRate_high) #11% baseline

#     Likelihood of being offered MOUD in jails. Baseline either 0 if no MOUD
#       Offered in prisons, or 100% if MOUD offered in prisons. Range 0-1.
#MOUD_Jail1 <- runif(n, min = 0, max = 1) 

#     Likelihood of continuing on MOUD (linking to care) upon exit from prison if started
#       Treatment in prison. Baseline 48%, range 25%-75%.
MOUD_Jail2 <- runif(n, min = 0.25, max = 0.75)

#     Population size for those with opioid use disorder/opioid posession in Colorado.
#       Baseline estimate 139,000. Range 37,000-166,000
N_pop1 <- runif(n, min = 37000, max = 166130)

#   Population on MOUD in the community annually. Baseline estimate from Colorado around 9,000.
MOUD_pop1 <- runif(n, min = 4553, max = 13700) #MOUD_pop[which(MOUD_pop$States=="Colorado"),9]

#   Proportion of population with OUD/opioid possession in possession of fentanyl. 
#     Baseline estimate 50%, range 0-1.
PropFentanyl <- runif(n, min = 0, max = 1)

#   Death rate, monthly, from opioid overdose. Number of opioid deaths in 2021 according to CDC.
#     Divided by population with opioid possession. Divided by 12 for monthly data.
#     80,000 is the number of opioid overdoses seen in the US in 2021. Krawczyk paper estimates
#     0.02 of US drug use in Colorado. 0.02* 80,000 ~ 1,600 in 2021. CDC estimates ~ 1,500 drug deaths in Colorado in 2020.
#     We will use 1500 here as drug deaths for opioids have gone up.
Death_Rate<- 1-exp(-(-log(1-(1500/N_pop1)))/12) #0.000703391


# Non-PSA inputs (single value)
    
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
    
for (i in 1:n) {


    
    N_pop <- N_pop1[i] * PropFentanyl[i]
    
    # Rate of first felony:
    
    Felony_1 <-1-exp(-(-log(1-Jail_Rate[i])*Prop_AtRisk))
    
    # Rate of second felony
    
    Felony_2 <- 1-exp(-(-log(1-Jail_Rate[i])*Prop_AtRisk*Arrest_Mult_Arrest[i]))
      
    
    # Rate of death, not directly after incarceration
    
    Death_1 <- Death_Rate[i]
    
    # Probability of death directly following incarceration (converting to and from rates)
    
    Death_2 <- 1-exp(-(-log(1-Death_Rate[i])*Arrest_Mult_Death1[i]))
    
    Death_3 <- 1-exp(-(-log(1-Death_Rate[i])*Arrest_Mult_Death2[i]))
    
    # Model
    
    trans_mat <- matrix(c(1-Felony_1-Death_1-MOUD_Community[i],MOUD_Community[i],Felony_1*recidProgram,0,Felony_1*(1-recidProgram),0,0,0,0,Death_1, # Not Jailed
                          1/Duration_MOUD[i],1-1/Duration_MOUD[i],0,0,0,0,0,0,0,0, #Community MOUD
                          0,MOUD_Community[i],1-1/recidDuration-recidJail[i]-Death_1-MOUD_Community[i], 1/recidDuration,recidJail[i],0,0,0,0,Death_1, #in recidivism program
                          1/12,MOUD_Community[i],0,1-(1/12)-MOUD_Community[i]-Felony_1*recidIncarcMulti-Death_1,Felony_1*recidIncarcMulti,0,0,0,0,Death_1, # Year post-recidivism program: less likely to be inarcerated
                          0,0,0,0,(1-Death_Rate_Jail-1/6),(1/6)*MOUD_Jail[i],1/6*(1-MOUD_Jail[i]),0,0,Death_Rate_Jail, # Jail
                          0,0,0,0,0,1-1/Duration_MOUD[i],0,1/Duration_MOUD[i],0,0, #MOUD from jail
                          0,0 ,0,0,Felony_2, MOUD_Community[i], 0, 1-Death_3-Felony_2-MOUD_Community[i],0, Death_3,# Within one month of release
                          0,0,0,0,Felony_2, MOUD_Community[i], 0, 1-Felony_2-Death_2-1/11-MOUD_Community[i], 1/11, Death_2,# First year within release
                          0,0,0,0,Felony_2, MOUD_Community[i], 0, 0,1-Felony_2-Death_1-MOUD_Community[i], Death_1,# More than 1 post-release
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
    Data_People$RecidCost <- (Data_People$`Recidivism Program`)* recid_Cost[i]
    
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

    Data_Full[nrow(Data_Full) + 1,] <- tail(Data_People,1)
    colnames(Data_Full) <- colnames(Data_People)
  }    
    
    
    
Data_Full[n+1,] <- colMeans(Data_Full[1:n,])
Data_Full[n+2,] <- apply(Data_Full[1:n,],2,median)
Data_Full[n+3,] <- apply(Data_Full[1:n,],2,quantile , probs = 0.05)
Data_Full[n+4,] <- apply(Data_Full[1:n,],2,quantile , probs = 0.95)

a= n+1
b= n+4

Data_PSA_Summary <- Data_Full[a:b,]

Data_PSA_Summary$SummaryStat <- c("Mean","Median","5%","95%")

Data_PSA_Summary$Category <- paste("Years=",years,"atRisk=",atRisk,"diversionProgam=",diversionProgram,"MOUD_Jail=",MOUD_Jail1)

return(Data_PSA_Summary)

}


# 5yr

Results_5yr_4g_nointerv_PSA <- fentanyl_deaths_PSA(years=1, atRisk="1", diversionProgram=0, MOUD_Jail1=0)
Results_5yr_1g_nointerv_PSA <- fentanyl_deaths_PSA(years=1, atRisk="2", diversionProgram=0, MOUD_Jail1=0)
Results_5yr_Any_nointerv_PSA <- fentanyl_deaths_PSA(years=1, atRisk="3", diversionProgram=0, MOUD_Jail1=0)

Results_5yr_4g_MOUD_PSA <- fentanyl_deaths_PSA(years=1, atRisk="1", diversionProgram=0, MOUD_Jail1=1)
Results_5yr_1g_MOUD_PSA <- fentanyl_deaths_PSA(years=1, atRisk="2", diversionProgram=0, MOUD_Jail1=1)
Results_5yr_Any_MOUD_PSA <- fentanyl_deaths_PSA(years=1, atRisk="3", diversionProgram=0, MOUD_Jail1=1)

Results_5yr_4g_Diversion_PSA <- fentanyl_deaths_PSA(years=1, atRisk="1", diversionProgram=1, MOUD_Jail1=0)
Results_5yr_1g_Diversion_PSA <- fentanyl_deaths_PSA(years=1, atRisk="2", diversionProgram=1, MOUD_Jail1=0)
Results_5yr_Any_Diversion_PSA <- fentanyl_deaths_PSA(years=1, atRisk="3", diversionProgram=1, MOUD_Jail1=0)

Results_5yr_4g_MOUD_Diversion_PSA <- fentanyl_deaths_PSA(years=1, atRisk="1", diversionProgram=1, MOUD_Jail1=1)
Results_5yr_1g_MOUD_Diversion_PSA <- fentanyl_deaths_PSA(years=1, atRisk="2", diversionProgram=1, MOUD_Jail1=1)
Results_5yr_Any_MOUD_Diversion_PSA <- fentanyl_deaths_PSA(years=1, atRisk="3", diversionProgram=1, MOUD_Jail1=1)

Results_PSA <- rbind(Results_5yr_4g_nointerv_PSA,Results_5yr_1g_nointerv_PSA,Results_5yr_Any_nointerv_PSA,
                 Results_5yr_4g_MOUD_PSA,Results_5yr_1g_MOUD_PSA,Results_5yr_Any_MOUD_PSA,
                 Results_5yr_4g_Diversion_PSA,Results_5yr_1g_Diversion_PSA,Results_5yr_Any_Diversion_PSA,
                 Results_5yr_4g_MOUD_Diversion_PSA,Results_5yr_1g_MOUD_Diversion_PSA,Results_5yr_Any_MOUD_Diversion_PSA)

Results_PSA$Incarcerated <- Results_PSA$Arrested+ Results_PSA$Released_immediate + Results_PSA$Released_post1yr+ Results_PSA$Released_1yr

Results_PSA2 <- Results_PSA[,c(11,13,16:21)]

write.csv(Results_PSA2,file="Results_PSA_1yr_22423.csv")


   