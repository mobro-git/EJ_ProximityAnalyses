rm(list = ls())
gc()

# Load libraries
source("scripts/packages.R")

### Allocation Rule

# Allocation ACS and NATA Analysis
source("AIM_Allocation.R")

# Allocation TRI Proximity Analysis
source("AIM_Allocation_TRI_Facilities.R")

### Allocation Rule

# Allocation ACS and NATA Analysis
source("AIM_Transitions.R")

# Allocation TRI Proximity Analysis
source("AIM_Transitions_TRI_Facilities.R")

