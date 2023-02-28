library(formattable)
library(priceR)
library(scales)

#Code to put together Tables 2: Make tables from general and PSA data
#   Combines data from Code_forTables and FentanylPolicyPSA

General_Data <- read.csv("Results_1yr.csv")
PSA_Data <- read.csv("Results_PSA_1yr_22423.csv")

# Format cost and data for PSA data: 
#   1. Deaths and incarcerations to nearest hundred
#   2. Costs to millions (for deaths, incarcerations, MOUD) with 2 decimal points, 
#       Diversion program costs to thousands (2 decimal points)

PSA_Data$TotalCosts <- PSA_Data$ChargeCostCum + PSA_Data$DeathCost + PSA_Data$MOUDCostCum + PSA_Data$RecidCostCum

PSA_Data$Dead <- round(PSA_Data$Dead, digits=-1)
PSA_Data$Dead_comma <- comma(PSA_Data$Dead)
PSA_Data$Incarcerated <- round(PSA_Data$Incarcerated, digits=-1)
PSA_Data$Incarcerated_comma <- comma(PSA_Data$Incarcerated)

PSA_Data$DeathCost_mill <- currency(PSA_Data$DeathCost, digits=2)
PSA_Data$DeathCost_mill <- round(PSA_Data$DeathCost_mill/1000000, digits=2)

PSA_Data$ChargeCostCum_mill <- currency(PSA_Data$ChargeCostCum, digits=2)
PSA_Data$ChargeCostCum_mill <- round(PSA_Data$ChargeCostCum_mill/1000000, digits=2)

PSA_Data$RecidCostCum_mill <- currency(PSA_Data$RecidCostCum, digits=2)
PSA_Data$RecidCostCum_mill <- round(PSA_Data$RecidCostCum_mill/1000, digits=2)

PSA_Data$MOUDCostCum_mill <- currency(PSA_Data$MOUDCostCum, digits=2)
PSA_Data$MOUDCostCum_mill <- round(PSA_Data$MOUDCostCum_mill/1000000, digits=2)

PSA_Data$TotalCosts_mill <- currency(PSA_Data$TotalCosts, digits=2)
PSA_Data$TotalCosts_mill <- round(PSA_Data$TotalCosts_mill/1000000, digits=2)

#   Split PSA data into high and low ranges

PSA_Data_low <- PSA_Data[which(PSA_Data$SummaryStat=="5%"),]
PSA_Data_high <- PSA_Data[which(PSA_Data$SummaryStat=="95%"),]

# Format cost and data for PSA data: 
#   1. Deaths and incarcerations to nearest hundred
#   2. Costs to millions (for deaths, incarcerations, MOUD) with 2 decimal points, 
#       Diversion program costs to thousands (2 decimal points)

General_Data$TotalCosts <- General_Data$ChargeCostCum + General_Data$DeathCost + General_Data$MOUDCostCum + General_Data$RecidCostCum

General_Data$Dead <- round(General_Data$Dead, digits=-1)
General_Data$Dead_comma <- comma(General_Data$Dead)
General_Data$Incarcerated <- round(General_Data$Incarcerated, digits=-1)
General_Data$Incarcerated_comma <- comma(General_Data$Incarcerated)

General_Data$DeathCost_mill <- currency(General_Data$DeathCost, digits=2)
General_Data$DeathCost_mill <- round(General_Data$DeathCost_mill/1000000, digits=2)

General_Data$ChargeCostCum_mill <- currency(General_Data$ChargeCostCum, digits=2)
General_Data$ChargeCostCum_mill <- round(General_Data$ChargeCostCum_mill/1000000, digits=2)

General_Data$RecidCostCum_mill <- currency(General_Data$RecidCostCum, digits=2)
General_Data$RecidCostCum_mill <- round(General_Data$RecidCostCum_mill/1000, digits=2)

General_Data$MOUDCostCum_mill <- currency(General_Data$MOUDCostCum, digits=2)
General_Data$MOUDCostCum_mill <- round(General_Data$MOUDCostCum_mill/1000000, digits=2)

General_Data$TotalCosts_mill <- currency(General_Data$TotalCosts, digits=2)
General_Data$TotalCosts_mill <- round(General_Data$TotalCosts_mill/1000000, digits=2)


# Create empty shell for Table 2

Table2 <- data.frame(matrix(nrow=12, ncol=8))

#     Colnames for Table2

colnames(Table2) <- c("Category", "Total opioid overdose deaths",	"Total incarcerations",	"Total cost of incarcerations, millions", 
                      "Total costs of death (healthcare costs only), millions", "Total cost of MOUD, millions",	"Total cost of diversion program, thousands", "Total Cost" )

Table2$Category <- General_Data$Category
Table2$`Total opioid overdose deaths` <- paste0(General_Data$Dead_comma," (",PSA_Data_low$Dead_comma , "-",PSA_Data_high$Dead_comma,")")
Table2$`Total incarcerations` <- paste0(General_Data$Incarcerated_comma," (",PSA_Data_low$Incarcerated_comma , "-",PSA_Data_high$Incarcerated_comma,")")
Table2$`Total cost of incarcerations, millions` <- paste0(General_Data$ChargeCostCum_mill," (",PSA_Data_low$ChargeCostCum_mill , "-",PSA_Data_high$ChargeCostCum_mill,")")
Table2$`Total costs of death (healthcare costs only), millions` <- paste0(General_Data$DeathCost_mill," (",PSA_Data_low$DeathCost_mill , "-",PSA_Data_high$DeathCost_mill,")")
Table2$`Total cost of MOUD, millions` <-paste0(General_Data$MOUDCostCum_mill," (",PSA_Data_low$MOUDCostCum_mill , "-",PSA_Data_high$MOUDCostCum_mill,")")
Table2$`Total cost of diversion program, thousands` <- paste0(General_Data$RecidCostCum_mill," (",PSA_Data_low$RecidCostCum_mill , "-",PSA_Data_high$RecidCostCum_mill,")")
Table2$`Total Cost` <- paste0(General_Data$TotalCosts_mill," (",PSA_Data_low$TotalCosts_mill , "-",PSA_Data_high$TotalCosts_mill,")")

write.csv(Table2, "1yr_Table2_22423.csv")
