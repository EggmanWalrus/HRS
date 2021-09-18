source("Functions.R", local = knitr::knit_global())

# Section A: Demographics, Identifiers, and Weights

## Identifier (Person, Household, Spouse)
PSI <- c('HHIDPN', 'HHID', 'PN')
HI <- crosswave('H%dHHID')
SI <- c(crosswave('S%dHHIDPN'),
        'RASPCT',
        crosswave('RASPID%d', 4),
        'RESHHIDPN')

## Wave Status: Response Indicator; Interview Status
WSRI <- crosswave('INW%d')
WSIS <- crosswave('R%dIWSTAT')

## Sample Cohort
SC <- c('HACOHORT')

## Birth/Death Year; Age at interview; Gender; Race; Education
BY <- c('RABYEAR',
        crosswave('S%dBYEAR'))
DY <- c('RADYEAR',
        crosswave('S%dDYEAR'))
Age <- c(crosswave('R%dAGEY_B'),
         crosswave('S%dAGEY_B'),
         'RESPAGEY_B')
Gender <- c('RAGENDER',
            crosswave('S%dGENDER'))
Race <- c('RARACEM',
          crosswave('S%dRACEM'))
Edu <- c('RAEDYRS',
         crosswave('S%dEDYRS'))

## Current Marital Status: Without Partnership
Marriage <- c(crosswave('R%dMSTATH'),
              crosswave('S%dMSTATH'),
              crosswave('R%dMSTATF'),
              crosswave('S%dMSTATF'))
Marriage_length <- c(crosswave('R%dMCURLN'),
                     crosswave('S%dMCURLN'))

## Religion; Veteran; Place of Birth
Religion <- c('RARELIG',
              crosswave('S%dRELIG'))
Veteran <- c('RAVETRN',
             crosswave('S%dVETRN'))
BP <- c('RABPLACE',
        crosswave('S%dBPLACE'),
        crosswave('S%dBPLACF'),
        'RABPLACF')

Demographics <- c(PSI, HI, SI,
                  WSRI, WSIS, SC,
                  BY, DY, Age, Gender, Race, Edu,
                  Marriage, Marriage_length,
                  Religion, Veteran, BP)

# Section B: Health

## Self-reported Health; Mental Health (CESD Score)
SHealth <- c(crosswave('R%dSHLT'),
            crosswave('S%dSHLT'))
MHealth <- c(crosswave('R%dCESD', 14, 2),
             crosswave('S%dCESD', 14, 2))

## Medical Expenditure: Out of Pocket and Total
OOP <- c('H2OOPMD',
         crosswave('R%dOOPMD', 14, 2),
         crosswave('S%dOOPMD', 14, 2),
         'REOOPMD')

## Doctor Diagnosed Health Problems: Ever Have Condition
DDHP <- c(crosswave('R%dHIBPE'), #High blood pressure
          crosswave('S%dHIBPE'),
          crosswave('R%dDIABE'), #Diabetes
          crosswave('S%dDIABE'),
          crosswave('R%dCANCRE'), #Cancer
          crosswave('S%dCANCRE'),
          crosswave('R%dLUNGE'), #Lung disease
          crosswave('S%dLUNGE'),
          crosswave('R%dHEARTE'), #Heart problem
          crosswave('S%dHEARTE'),
          crosswave('R%dSTROKE'), #Stroke
          crosswave('S%dSTROKE'),
          crosswave('R%dPSYCHE'), #Psych problem
          crosswave('S%dPSYCHE'),
          crosswave('R%dARTHRE'), #Arthritis
          crosswave('S%dARTHRE'),
          crosswave('R%dCONDE'), #Sum of conditions ever had
          crosswave('S%dCONDE')
          )

## BMI; Black Problems
BMI <- c(crosswave('R%dBMI'),
         crosswave('S%dBMI'))
Back <- c(crosswave('R%dBACK'),
          crosswave('S%dBACK'))

## Healthy Behaviors: Smoking
Smoke <- c(crosswave('R%dSMOKEN'),
           crosswave('S%dSMOKEN'))

Health <- c(SHealth, MHealth,
            OOP, DDHP,
            BMI, Back,
            Smoke)

# Section D: Financial and Housing Wealth
TWealth <- c(crosswave('H%dATOTA')) #Excluding secondary residence
TNHWealth <- c(crosswave('H%dATOTN'))

Wealth <- c(TWealth, TNHWealth)

# Section E: Income

## Individual Earnings; Total Household Income (R+S only)
IEarn <- c(crosswave('R%dIEARN'),
           crosswave('S%dIEARN'))
IPENA <- c(crosswave('R%dIPENA'),
           crosswave('S%dIPENA'))
ISSDI <- c(crosswave('R%dISSDI'),
           crosswave('S%dISSDI'))
ISRET <- c(crosswave('R%dISRET'),
           crosswave('S%dISRET'))
IUNWC <- c(crosswave('R%dIUNWC'),
           crosswave('S%dIUNWC'))
IGXFR <- c(crosswave('R%dIGXFR'),
           crosswave('S%dIGXFR'))
HIncome <- c(crosswave('H%dITOT'))

Income <- c(IEarn, HIncome, IPENA, ISSDI, ISRET, IUNWC, IGXFR)

# Section H: Health Insurance

## Gov Plan; Medicare; Medicaid; Champus/VA
HGov <- c(crosswave('R%dHIGOV'),
          crosswave('S%dHIGOV'),
          'REHIGOV')
HMedicare <- c(crosswave('R%dGOVMR'),
               crosswave('S%dGOVMR'),
               'REGOVMR')
HMedicaid <- c(crosswave('R%dGOVMD'),
               crosswave('S%dGOVMD'),
               'REGOVMDS')
HVA <- c(crosswave('R%dGOVVA'),
         crosswave('S%dGOVVA'),
         'REGOVVA')

## Number of Employer-provided/Private Health Insurance Plans
HEP <- c(crosswave('R%dHENUM'),
         crosswave('S%dHENUM'))
HOther <- c(crosswave('R%dHIOTHP'),
            crosswave('S%dHIOTHP'))
HLTC <- c(crosswave('R%dHILTC'),
          crosswave('S%dHILTC'),
          'REHILTC')
HLife <- c(crosswave('R%dLIFEIN'),
           crosswave('S%dLIFEIN'))

Insurance <- c(HGov, HMedicaid, HMedicare, HVA,
            HEP, HOther, HLTC, HLife)

# Section I: Family Structure

## Number of people/siblings living in the household
HPeople <- c(crosswave('H%dHHRES'))
HSibling <- c(crosswave('R%dLIVSIB'),
              crosswave('S%dLIVSIB'))

Family <- c(HPeople, HSibling)

# Section J: Retirement Plans, Expectations

Retire <- c(crosswave('R%dSAYRET'),
            crosswave('S%dSAYRET'))

# Section K: Employment History

## Labor Force Status, Unemployed Status, Hourly Wage, Total years worked
Labor <- c(crosswave('R%dLBRF'),
          crosswave('S%dLBRF'))
Unemployed <- c(crosswave('R%dUNEMP'),
                crosswave('S%dUNEMP'))
Wage <- c(crosswave('R%dWGIHR'),
          crosswave('S%dWGIHR'))
WorkYears <- c(crosswave('R%dJYEARS'),
               crosswave('S%dJYEARS'))

Employment <- c(Labor, Unemployed, Wage, WorkYears)

# All Variables
RA <- c('RABYEAR', 'RADYEAR', 'RAGENDER', 'RARACEM', 'RAEDYRS', 'RARELIG', 'RAVETRN', 'RABPLACE')
Exit <- c('REOOPMD', 'REHIGOV', 'REGOVMR', 'REGOVMDS', 'REGOVVA', 'REHILTC' )
Variables <- c(Demographics, Health, Family, Retire, Income, Wealth, Insurance, Employment)
Variables_sorted <- c("HHIDPN", "RASPID1",
                      "FirstWave", "LastWave", "FirstWaveM",
                      "WidowWave", "WidowAge", "MarryLength", "MarryAge",
                      "HACOHORT",
                      "RDAge", "SDAge",
                      "RABYEAR", "SABYEAR",
                      "RADYEAR", "SADYEAR",
                      "RAGENDER", "SAGENDER",
                      "RARACEM", "SARACEM",
                      "RAEDYRS", "SAEDYRS",
                      "RARELIG", "SARELIG",
                      "RAVETRN", "SAVETRN",
                      "RABPLACE", "SABPLACE",
                      "REOOPMD", "SEOOPMD",
                      "REHIGOV", "SEHIGOV",
                      "REGOVMR", "SEGOVMR",
                      "REGOVMDS","SEGOVMDS",
                      "REGOVVA", "SEGOVVA",
                      "REHILTC", "SEHILTC",
                      crosswave('R%dOOPMD', 14, 2), crosswave('S%dOOPMD', 14, 2),
                      crosswave('R%dAGEY_B'), crosswave('S%dAGEY_B'),
                      crosswave('R%dMSTATH'), crosswave('S%dMSTATH'),
                      SHealth, MHealth,
                      DDHP, BMI, Back, Smoke,
                      Wealth,
                      Income,
                      crosswave('R%dHIGOV'), crosswave('S%dHIGOV'),
                      crosswave('R%dGOVMR'), crosswave('S%dGOVMR'),
                      crosswave('R%dGOVMD'), crosswave('S%dGOVMD'),
                      crosswave('R%dGOVVA'), crosswave('S%dGOVVA'),
                      HEP, HOther, HLife,
                      crosswave('R%dHILTC'), crosswave('S%dHILTC'),
                      Family, Retire,
                      Employment
                      )
Variables_inflation <- c("REOOPMD", "SEOOPMD", "ROOPMD", "SOOPMD",
                         "HATOTA", "HATOTN",
                         "RIEARN", "SIEARN", "HITOT",
                         "RIPENA", "SIPENA", "RISSDI", "SISSDI",
                         "RISRET", "SISRET", "RIUNWC", "SIUNWC",
                         "RIGXFR", "SIGXFR",
                         "RWGIHR", "SWGIHR")
