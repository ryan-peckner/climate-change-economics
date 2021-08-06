
alpha1 = 0.3

GrossOutput = function(t,total_factor_productivity,capital,population) {
  return(total_factor_productivity[t]*(capital[t]^alpha1)*(population[t]^(1-alpha1)))
}

#NetOutput = function(t,abatement_cost,gross_output) {
#  return((1-abatement_cost[t])*gross_output[t])
#}

NetOutput = function(t,abatement_cost,damages,gross_output) {
  return((1-abatement_cost[t]-damages[t])*gross_output[t])
}

library(memoise,quietly=TRUE)

K_0 = 139.65
delta = 0.1
s = 0.22

CapitalStock = memoise(function(t,abatement_cost,total_factor_productivity,population) {
  if (t==1) { 
    K_0
  } else {
    K_t_minus_1 = CapitalStock(t-1,abatement_cost,total_factor_productivity,population)
    return((1-delta)*K_t_minus_1 + 
             s*(1-abatement_cost[t-1])*total_factor_productivity[t-1]*(K_t_minus_1^alpha1)*(population[t-1]^(1-alpha1)))}
})

forcings2 = read.csv("CCE_Lab2_Outputs.csv",stringsAsFactors = FALSE)

beta_2 = 2.8

KayaEmissions = function(t,population,per_capita_output,energy_intensity,emission_intensity) {
  return(population[t]*per_capita_output[t]*energy_intensity[t]*emission_intensity[t])
}

BAUEmissions = function(t,kaya_emissions,land_use_emissions) {
  return(kaya_emissions[t] + land_use_emissions[t])
}


ControlledEmissions = function(t,bau_emissions,control_rate) {
  return((1-control_rate[t])*bau_emissions[t])
}

AbatementCost = function(t,abatement_coefficient,control_rate) {
  return(abatement_coefficient[t]*((control_rate[t])^beta_2))
}


KayaEmissionsFromGDP = function(t,population,output,energy_intensity,emission_intensity) {
  per_capita_output = (10^12)*(output/population)
  return(KayaEmissions(t,population,per_capita_output,energy_intensity,emission_intensity))
}

TotalEmissions = function(t,kaya_emissions,land_use_emissions) {
  return(kaya_emissions[t] + land_use_emissions[t])
}


