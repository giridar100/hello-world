
#### 01 Load packages ####
library("dampack") # for CEA

#### 02 Define parameter input values ####

# Probabilities
p_flu                    <- 0.0898 # Incidence of influenza positive
p_flu_care               <- 0.38   # Proportion of influenza positive who seek care
p_flu_nocare_recovery    <- 0.997  # Proportion of influenza positive who did not seek care and recovered
p_flu_opd                <- 0.61   # Proportion of influenza positive who seek care and go to OPD
p_flu_opd_recovery       <- 0.996  # Proportion of influenza positive who recovered from OPD care
p_flu_ipd_noicu          <- 0.96   # Proportion of influenza positive who get admitted but not in ICU
p_flu_ipd_noicu_recovery <- 0.993  # Proportion of influenza positive who recovered without ICU care
p_flu_ipd_icu_recovery   <- 0.89   # Proportion of influenza positive who recovered after ICU care
  

# Costs
Cost_OPD_recovery <- 33.9
Cost_IPD_recovery <-	49.7
Cost_NMA_recovery <- 22.3
Cost_ICU_recovery <- 75.0

Cost_OPD_death <- 35.9
Cost_IPD_death <-	51.7
Cost_NMA_death <- 24.3
Cost_ICU_death <- 77.0

# DALYs
d_recovery         <- 5    # DALY lost after recovering without seeking any care
d_opd_recovery     <- 6    # DALY lost after recovering from OPD care
d_ipd_recovery     <- 8    # DALY lost after recovering from IPD care
d_icu_recovery     <- 10   # DALY lost after recovering from ICU care

d_death            <- 20   # DALY lost after death
# number of strategies
v_strategy_names <- c("No Tx", "Active")
n_strategies     <- length(v_strategy_names)

#### 03 Evaluate decision tree for each strategy ####

# Setup vector to store expected outcomes (costs and QALYs) for each strategy
v_ex_cost <- v_ex_daly  <- NA*numeric(length=n_strategies)
names(v_ex_cost)  <- names(v_ex_daly) <- v_strategy_names

##--- No Treatment strategy ---##

# expected costs
v_ex_cost["No Tx"] <- 
  p_flu*(1 - p_flu_care)*p_flu_nocare_recovery*Cost_NMA_recovery +                                       # Flu, no care, recovery
  p_flu*(1 - p_flu_care)*(1 - p_flu_nocare_recovery)*Cost_NMA_death +                                    # Flu, no care, death
  p_flu*(p_flu_care)*p_flu_opd*p_flu_opd_recovery*Cost_OPD_recovery +                                    # Flu, care, OPD, recovery
  p_flu*(p_flu_care)*p_flu_opd*(1 - p_flu_opd_recovery)*Cost_OPD_death +                                 # Flu, care, OPD, death
  p_flu*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_noicu*p_flu_ipd_noicu_recovery*Cost_IPD_recovery +        # Flu,care,IPD,recovery
  p_flu*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_noicu*(1 - p_flu_ipd_noicu_recovery)*Cost_IPD_death +     # Flu,care,IPD,death
  p_flu*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_noicu)*p_flu_ipd_icu_recovery*Cost_ICU_recovery +    # Flu,care,ICU,recovery
  p_flu*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_noicu)*(1 - p_flu_ipd_icu_recovery)*Cost_ICU_death + # Flu,care,ICU,recovery
  
  (1 - p_flu)*(1 - p_flu_care)*p_flu_nocare_recovery*Cost_NMA_recovery +                                       # No Flu, no care, recovery
  (1 - p_flu)*(1 - p_flu_care)*(1 - p_flu_nocare_recovery)*Cost_NMA_death +                                    # No Flu, no care, death
  (1 - p_flu)*(p_flu_care)*p_flu_opd*p_flu_opd_recovery*Cost_OPD_recovery +                                    # No Flu, care, OPD, recovery
  (1 - p_flu)*(p_flu_care)*p_flu_opd*(1 - p_flu_opd_recovery)*Cost_OPD_death +                                 # No Flu, care, OPD, death
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_noicu*p_flu_ipd_noicu_recovery*Cost_IPD_recovery +        # No Flu,care,IPD,recovery
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_noicu*(1 - p_flu_ipd_noicu_recovery)*Cost_IPD_death +     # No Flu,care,IPD,death
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_noicu)*p_flu_ipd_icu_recovery*Cost_ICU_recovery +    # No Flu,care,ICU,recovery
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_noicu)*(1 - p_flu_ipd_icu_recovery)*Cost_ICU_death + # No Flu,care,ICU,recovery
  

# expected DALYs
v_ex_daly["No Tx"] <- 
  p_flu*(1 - p_flu_care)*p_flu_nocare_recovery*Cost_NMA_recovery +                                       # Flu, no care, recovery
  p_flu*(1 - p_flu_care)*(1 - p_flu_nocare_recovery)*Cost_NMA_death +                                    # Flu, no care, death
  p_flu*(p_flu_care)*p_flu_opd*p_flu_opd_recovery*Cost_OPD_recovery +                                    # Flu, care, OPD, recovery
  p_flu*(p_flu_care)*p_flu_opd*(1 - p_flu_opd_recovery)*Cost_OPD_death +                                 # Flu, care, OPD, death
  p_flu*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_noicu*p_flu_ipd_noicu_recovery*Cost_IPD_recovery +        # Flu,care,IPD,recovery
  p_flu*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_noicu*(1 - p_flu_ipd_noicu_recovery)*Cost_IPD_death +     # Flu,care,IPD,death
  p_flu*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_noicu)*p_flu_ipd_icu_recovery*Cost_ICU_recovery +    # Flu,care,ICU,recovery
  p_flu*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_noicu)*(1 - p_flu_ipd_icu_recovery)*Cost_ICU_death + # Flu,care,ICU,recovery
  
  (1 - p_flu)*(1 - p_flu_care)*p_flu_nocare_recovery*Cost_NMA_recovery +                                       # No Flu, no care, recovery
  (1 - p_flu)*(1 - p_flu_care)*(1 - p_flu_nocare_recovery)*Cost_NMA_death +                                    # No Flu, no care, death
  (1 - p_flu)*(p_flu_care)*p_flu_opd*p_flu_opd_recovery*Cost_OPD_recovery +                                    # No Flu, care, OPD, recovery
  (1 - p_flu)*(p_flu_care)*p_flu_opd*(1 - p_flu_opd_recovery)*Cost_OPD_death +                                 # No Flu, care, OPD, death
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_noicu*p_flu_ipd_noicu_recovery*Cost_IPD_recovery +        # No Flu,care,IPD,recovery
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_noicu*(1 - p_flu_ipd_noicu_recovery)*Cost_IPD_death +     # No Flu,care,IPD,death
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_noicu)*p_flu_ipd_icu_recovery*Cost_ICU_recovery +    # No Flu,care,ICU,recovery
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_noicu)*(1 - p_flu_ipd_icu_recovery)*Cost_ICU_death + # No Flu,care,ICU,recovery
#-------------------------------#


#----- Treat All strategy ------#

# expected costs
v_ex_cost["Tx All"] <- 
  c_tx + # cost of treatment
  p_HVE*p_HVE_comp_tx*c_VE_comp +     # HVE w/tx, complications
  p_HVE*(1-p_HVE_comp_tx)*c_VE +      # HVE w/tx, no complications
  (1-p_HVE)*p_OVE_comp_tx*c_VE_comp + # OVE w/tx, complications
  (1-p_HVE)*(1-p_OVE_comp_tx)*c_VE    # OVE w/tx, no complications

# expected QALYs
v_ex_qaly["Tx All"] <- 
  p_HVE*p_HVE_comp_tx*q_VE_comp +     # HVE w/tx, complications
  p_HVE*(1-p_HVE_comp_tx)*q_VE +      # HVE w/tx, no complications
  (1-p_HVE)*p_OVE_comp_tx*q_VE_comp + # OVE w/tx, complications
  (1-p_HVE)*(1-p_OVE_comp_tx)*q_VE    # OVE w/tx, no complications

#-------------------------------#