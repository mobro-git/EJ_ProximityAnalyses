rm(list = ls())
gc()

# Load libraries
source("packages.R")

### Allocation Rule

# Allocation ACS and NATA Analysis
source("scripts/allocation_rule/AIM_Allocation.R")

# Allocation TRI Proximity Analysis
source("scripts/allocation_rule/AIM_Allocation_TRI_Facilities.R")

### Allocation Rule

# Allocation ACS and NATA Analysis
source("scripts/transitions_rule/AIM_Transitions.R")

# Allocation TRI Proximity Analysis
source("scripts/transitions_rule/AIM_Transitions_TRI_Facilities.R")

