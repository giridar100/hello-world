
#### 01 Load packages ####
library("dampack") # for CEA
library("ggplot2") # for graphs
#### 02 Define parameter input values ####

# Probabilities
p_flu                    <- 0.0898 # Incidence of influenza positive
p_flu_care               <- 0.261  # Proportion of influenza positive who seek care
p_flu_nocare_death       <- 0.0152 # Proportion of influenza positive who did not seek care and died
p_flu_opd                <- 0.545  # Proportion of influenza positive who sought care in OPD
p_flu_opd_death          <- 0.0304 # Proportion of influenza positive who died after OPD care
p_flu_ipd_icu            <- 0.372  # Proportion of influenza positive who get admitted in ICU
p_flu_ipd_noicu_death    <- 0.185  # Proportion of influenza positive who recovered without ICU care
p_flu_ipd_icu_death      <- 0.370  # Proportion of influenza positive who recovered after ICU care
  
p_vaccine_effectivenss   <- 0.44   # Proportion of influenza protection offered by the vaccine
# Costs in dollars
Cost_OPD_recovery <-  31.6
Cost_IPD_recovery <-	89.4
Cost_NMA_recovery <-  24.2
Cost_ICU_recovery <-  763.5

Cost_OPD_death <- 51.6
Cost_IPD_death <-	109.4
Cost_NMA_death <- 44.2
Cost_ICU_death <- 783.5

Cost_of_vaccination <- 38.7
# DALYs
d_recovery         <- 6    # DALY lost after recovering without seeking any care
d_opd_recovery     <- 51    # DALY lost after recovering from OPD care
d_ipd_recovery     <- 51    # DALY lost after recovering from IPD care
d_icu_recovery     <- 133    # DALY lost after recovering from ICU care

d_death            <- 0        # DALY lost after death
# number of strategies
v_strategy_names <- c("No Tx", "Active")
n_strategies     <- length(v_strategy_names)

#### 03 Evaluate decision tree for each strategy ####

# Setup vector to store expected outcomes (costs and DALYs) for each strategy
v_ex_cost <- v_ex_daly  <- NA*numeric(length=n_strategies)
names(v_ex_cost)  <- names(v_ex_daly) <- v_strategy_names

##--- No Treatment strategy ---##

# expected costs
v_ex_cost["No Tx"] <- 
  p_flu*(1 - p_flu_care)*(1 - p_flu_nocare_death)*Cost_NMA_recovery +                                    # Flu, no care, recovery
  p_flu*(1 - p_flu_care)*(p_flu_nocare_death)*Cost_NMA_death +                                           # Flu, no care, death
  p_flu*(p_flu_care)*p_flu_opd*(1 - p_flu_opd_death)*Cost_OPD_recovery +                                 # Flu, care, OPD, recovery
  p_flu*(p_flu_care)*p_flu_opd*p_flu_opd_death*Cost_OPD_death +                                          # Flu, care, OPD, death
  p_flu*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_icu)*(1 - p_flu_ipd_noicu_death)*Cost_IPD_recovery + # Flu,care,IPD,recovery
  p_flu*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_icu)*p_flu_ipd_noicu_death*Cost_IPD_death +          # Flu,care,IPD,death
  p_flu*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_icu*(1 - p_flu_ipd_icu_death)*Cost_ICU_recovery +         # Flu,care,ICU,recovery
  p_flu*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_icu*p_flu_ipd_icu_death*Cost_ICU_death +                  # Flu,care,ICU,death

  (1 - p_flu)*(1 - p_flu_care)*(1 - p_flu_nocare_death)*Cost_NMA_recovery +                                    #No Flu, no care, recovery
  (1 - p_flu)*(1 - p_flu_care)*(p_flu_nocare_death)*Cost_NMA_death +                                           #No Flu, no care, death
  (1 - p_flu)*(p_flu_care)*p_flu_opd*(1 - p_flu_opd_death)*Cost_OPD_recovery +                                 #No Flu, care, OPD, recovery
  (1 - p_flu)*(p_flu_care)*p_flu_opd*p_flu_opd_death*Cost_OPD_death +                                          #No Flu, care, OPD, death
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_icu)*(1 - p_flu_ipd_noicu_death)*Cost_IPD_recovery + #No Flu,care,IPD,recovery
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_icu)*p_flu_ipd_noicu_death*Cost_IPD_death +          #No Flu,care,IPD,death
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_icu*(1 - p_flu_ipd_icu_death)*Cost_ICU_recovery +         #No Flu,care,ICU,recovery
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_icu*p_flu_ipd_icu_death*Cost_ICU_death                    #No Flu,care,ICU,death
##  
# expected DALYs
v_ex_daly["No Tx"] <- 
  p_flu*(1 - p_flu_care)*(1 - p_flu_nocare_death)*d_recovery +                                           # Flu, no care, recovery
  p_flu*(1 - p_flu_care)*(p_flu_nocare_death)*d_death +                                                  # Flu, no care, death
  p_flu*(p_flu_care)*p_flu_opd*(1 - p_flu_opd_death)*d_opd_recovery +                                    # Flu, care, OPD, recovery
  p_flu*(p_flu_care)*p_flu_opd*p_flu_opd_death*d_death +                                                 # Flu, care, OPD, death
  p_flu*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_icu)*(1 - p_flu_ipd_noicu_death)*d_ipd_recovery +    # Flu,care,IPD,recovery
  p_flu*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_icu)*p_flu_ipd_noicu_death*d_death +                 # Flu,care,IPD,death
  p_flu*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_icu*(1 - p_flu_ipd_icu_death)*d_icu_recovery +            # Flu,care,ICU,recovery
  p_flu*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_icu*p_flu_ipd_icu_death*d_death +                         # Flu,care,ICU,death
  
  (1 - p_flu)*(1 - p_flu_care)*(1 - p_flu_nocare_death)*d_recovery +                                        #No Flu, no care, recovery
  (1 - p_flu)*(1 - p_flu_care)*(p_flu_nocare_death)*d_death +                                               #No Flu, no care, death
  (1 - p_flu)*(p_flu_care)*p_flu_opd*(1 - p_flu_opd_death)*d_opd_recovery +                                 #No Flu, care, OPD, recovery
  (1 - p_flu)*(p_flu_care)*p_flu_opd*p_flu_opd_death*d_death +                                              #No Flu, care, OPD, death
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_icu)*(1 - p_flu_ipd_noicu_death)*d_ipd_recovery + #No Flu,care,IPD,recovery
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_icu)*p_flu_ipd_noicu_death*d_death +              #No Flu,care,IPD,death
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_icu*(1 - p_flu_ipd_icu_death)*d_icu_recovery +         #No Flu,care,ICU,recovery
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_icu*p_flu_ipd_icu_death*d_death                        #No Flu,care,ICU,death

#-------------------------------#


#----- Active vaccination strategy ------#

# expected costs
v_ex_cost["Active"] <- 
  Cost_of_vaccination +
  (1- p_vaccine_effectivenss)*p_flu*(1 - p_flu_care)*(1 - p_flu_nocare_death)*Cost_NMA_recovery +        # Flu, no care, recovery
  (1- p_vaccine_effectivenss)*p_flu*(1 - p_flu_care)*(p_flu_nocare_death)*Cost_NMA_death +                                           # Flu, no care, death
  (1- p_vaccine_effectivenss)*p_flu*(p_flu_care)*p_flu_opd*(1 - p_flu_opd_death)*Cost_OPD_recovery +                                 # Flu, care, OPD, recovery
  (1- p_vaccine_effectivenss)*p_flu*(p_flu_care)*p_flu_opd*p_flu_opd_death*Cost_OPD_death +                                          # Flu, care, OPD, death
  (1- p_vaccine_effectivenss)*p_flu*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_icu)*(1 - p_flu_ipd_noicu_death)*Cost_IPD_recovery + # Flu,care,IPD,recovery
  (1- p_vaccine_effectivenss)*p_flu*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_icu)*p_flu_ipd_noicu_death*Cost_IPD_death +          # Flu,care,IPD,death
  (1- p_vaccine_effectivenss)*p_flu*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_icu*(1 - p_flu_ipd_icu_death)*Cost_ICU_recovery +         # Flu,care,ICU,recovery
  (1- p_vaccine_effectivenss)*p_flu*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_icu*p_flu_ipd_icu_death*Cost_ICU_death +                  # Flu,care,ICU,death
  
  (1 - p_flu)*(1 - p_flu_care)*(1 - p_flu_nocare_death)*Cost_NMA_recovery +                                    #No Flu, no care, recovery
  (1 - p_flu)*(1 - p_flu_care)*(p_flu_nocare_death)*Cost_NMA_death +                                           #No Flu, no care, death
  (1 - p_flu)*(p_flu_care)*p_flu_opd*(1 - p_flu_opd_death)*Cost_OPD_recovery +                                 #No Flu, care, OPD, recovery
  (1 - p_flu)*(p_flu_care)*p_flu_opd*p_flu_opd_death*Cost_OPD_death +                                          #No Flu, care, OPD, death
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_icu)*(1 - p_flu_ipd_noicu_death)*Cost_IPD_recovery + #No Flu,care,IPD,recovery
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_icu)*p_flu_ipd_noicu_death*Cost_IPD_death +          #No Flu,care,IPD,death
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_icu*(1 - p_flu_ipd_icu_death)*Cost_ICU_recovery +         #No Flu,care,ICU,recovery
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_icu*p_flu_ipd_icu_death*Cost_ICU_death                    #No Flu,care,ICU,death

# expected DALYs
v_ex_daly["Active"] <- 
  (1- p_vaccine_effectivenss)*p_flu*(1 - p_flu_care)*(1 - p_flu_nocare_death)*d_recovery +                                           # Flu, no care, recovery
  (1- p_vaccine_effectivenss)*p_flu*(1 - p_flu_care)*(p_flu_nocare_death)*d_death +                                                  # Flu, no care, death
  (1- p_vaccine_effectivenss)*p_flu*(p_flu_care)*p_flu_opd*(1 - p_flu_opd_death)*d_opd_recovery +                                    # Flu, care, OPD, recovery
  (1- p_vaccine_effectivenss)*p_flu*(p_flu_care)*p_flu_opd*p_flu_opd_death*d_death +                                                 # Flu, care, OPD, death
  (1- p_vaccine_effectivenss)*p_flu*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_icu)*(1 - p_flu_ipd_noicu_death)*d_ipd_recovery +    # Flu,care,IPD,recovery
  (1- p_vaccine_effectivenss)*p_flu*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_icu)*p_flu_ipd_noicu_death*d_death +                 # Flu,care,IPD,death
  (1- p_vaccine_effectivenss)*p_flu*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_icu*(1 - p_flu_ipd_icu_death)*d_icu_recovery +            # Flu,care,ICU,recovery
  (1- p_vaccine_effectivenss)*p_flu*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_icu*p_flu_ipd_icu_death*d_death +                         # Flu,care,ICU,death
  
  (1 - p_flu)*(1 - p_flu_care)*(1 - p_flu_nocare_death)*d_recovery +                                        #No Flu, no care, recovery
  (1 - p_flu)*(1 - p_flu_care)*(p_flu_nocare_death)*d_death +                                               #No Flu, no care, death
  (1 - p_flu)*(p_flu_care)*p_flu_opd*(1 - p_flu_opd_death)*d_opd_recovery +                                 #No Flu, care, OPD, recovery
  (1 - p_flu)*(p_flu_care)*p_flu_opd*p_flu_opd_death*d_death +                                              #No Flu, care, OPD, death
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_icu)*(1 - p_flu_ipd_noicu_death)*d_ipd_recovery + #No Flu,care,IPD,recovery
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*(1 - p_flu_ipd_icu)*p_flu_ipd_noicu_death*d_death +              #No Flu,care,IPD,death
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_icu*(1 - p_flu_ipd_icu_death)*d_icu_recovery +         #No Flu,care,ICU,recovery
  (1 - p_flu)*(p_flu_care)*(1 - p_flu_opd)*p_flu_ipd_icu*p_flu_ipd_icu_death*d_death                        #No Flu,care,ICU,death


#### 04 Cost-Effectiveness Analysis ####

# use dampack function "calculate_icers()" to compare strategies
res_cea <- calculate_icers(cost = v_ex_cost, 
                           effect = v_ex_daly, 
                           strategies = v_strategy_names)#,
#ref_strat = "No Tx")

# Table of results
res_cea

# Plot of cost-effectiveness frontier
plot(res_cea,inherit.aes = TRUE)
geom_path(mapping = NULL, data = NULL, stat = "identity",
          position = "identity", res_cea, lineend = "butt", linejoin = "round",
          linemitre = 10, arrow = NULL, na.rm = FALSE, show.legend = NA,
          inherit.aes = TRUE)
View(calculate_icers)
