library(readxl,quietly=TRUE)
library(memoise)

suppressMessages({forcings_excel = read_xlsx("CCE - Assignment 1 - Forcings.xlsx",skip = 1)})
forcings = data.frame(year = as.integer(forcings_excel[1,][-1]),co2 = as.numeric(forcings_excel[4,][-1]),
                      rfOther = as.numeric(forcings_excel[5,][-1]),emissions = as.numeric(forcings_excel[8,][-1]))


C_pre = 275 # Pre-industrial atmospheric CO2 level
lambda = 1.2 # Climate sensitivity
mu = 1/66 # Warming delay parameter
T_2010 = 0.8 

RadiativeForcingCO2 = function(t,CO2) {
  return(5.35*log(CO2[t]/C_pre))}  # Note that R uses the natural logarithm by default

RadiativeForcingTotal = function(t,CO2) {
  RadiativeForcingCO2(t,CO2) + forcings$rfOther[t]
}

GlobalAvgTempIncrease_ConstantRF = function(t,CO2) {
  lambda*RadiativeForcingTotal(t,CO2)
}


GlobalAvgTempIncrease = memoise(function(t,CO2) {
  if (t == 1) return(T_2010)
  else        return(GlobalAvgTempIncrease(t-1,CO2) + 
                       mu*(GlobalAvgTempIncrease_ConstantRF(t,CO2) - GlobalAvgTempIncrease(t-1,CO2)))
  
})

alpha = c(1,exp(-1/363),exp(-1/74),exp(-1/17),exp(-1/2))
gamma = c(0.13,0.2,0.32,0.25,0.1)
Box2010 = c(301.099,30.098,34.878,12.357,0.897)
beta = 0.00047

Box = memoise(function(i,t,emissions) {
  if (t == 1) return(Box2010[i])
  else return(alpha[i]*Box(i,t-1,emissions) + gamma[i]*beta*emissions[t])
})


AtmosphericCO2 = function(t,emissions) {
  return(Box(1,t,emissions) + Box(2,t,emissions) + Box(3,t,emissions) + Box(4,t,emissions) + Box(5,t,emissions))
}


CarbonCycleModelCO2 = function(emissions) {
  sapply(1:nrow(forcings),AtmosphericCO2,emissions=emissions)
}

GlobalAvgTempIncrease_FromEmissions = function(t,emissions) {
  GlobalAvgTempIncrease(t,CarbonCycleModelCO2(emissions))
}


ReducedEmissions = memoise(function(t,percent_reduction) {
  if (t == 1) return(forcings$emissions[1])
  else return(ReducedEmissions(t-1,percent_reduction)*(1-percent_reduction))
})

ReducedEmissionsProfile = function(percent_reduction) {
  sapply(1:nrow(forcings),ReducedEmissions,percent_reduction=percent_reduction)
}

TempIncrease_ReducedEmissions = function(t,percent_reduction) {
  return(GlobalAvgTempIncrease_FromEmissions(t,ReducedEmissionsProfile(percent_reduction)))
}