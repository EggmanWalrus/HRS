source("Functions.R", local = knitr::knit_global())

# Section A: Identifiers
Identifier <- c('HHIDPN')
Response <- c(crosswave('INCAMSC%d', 13, 5),
              crosswave('H%dC10REP', 13, 5),
              crosswave('H%dCHRSCOREF', 13, 5))

# Section B: Consumption
Consumption <- c(crosswave('H%dCTOTC', 12, 5))

VariablesC <- c(Identifier, Response, Consumption)