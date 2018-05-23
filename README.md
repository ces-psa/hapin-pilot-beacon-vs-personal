# hapin-pilot-beacon-vs-personal

####
## Description of script:

pilot-phase2b_exp_analysis_v3.R: master R scripts include data read-in, check, Beacon walk-through and direct and indirect exposure analysis
analyze by each phase, can export hourly level indirect and direct PM2.5 exposure file

Beacon_data_analysis.R: R scripts analyze hourly level and daily level indirect/direct PM2.5 exposure exported by pilot-phase2b_exp_analysis_v3

pilot-phase2b_beacon_readin_v2.R: ECM, Beacon, CRF (H41), Lascar, Beacon inventory data read-in, called by master R scripts

walk_through_c_Beacon1_v2.R: walk-through analysis for first micro-environment by field workers, called by master R script
walk_through_c_Beacon2_v2.R: walk-through analysis for second micro-environment by field workers, called by master R script
walk_through_c_Beacon3_v2.R: walk-through analysis for third micro-environment by field workers, called by master R script

PEO_ECM_comparison_v2.R: R script for mother direct and indirect PM2.5 analysis using ECM nephelometric data, by minute and hourly level,
called by master R scripts.

###
## Data used:
HAPIN Guatemala folder:
Box Sync\Exposure Data--GUATEMALA (Shirin Jabbarzadeh)\PILOT IIB\DATA

