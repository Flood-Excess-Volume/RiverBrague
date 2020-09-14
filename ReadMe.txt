#R Scripts to perform a full Flood Excess Volume (FEV) analysis with current situation
# project situation with another stage - discharge relationship (e.g., after giving room
# to the river) and the square lake plot.
#
# Step one is the FEV computation. Run script FEVstep1_FEVComputation.R
# Step two, is the square lake plotting. Run script FEVstep2_SquareLake_V0.R
#
# see definition in Bokhove, O., Kelmanson, M. A., Kent, T., Piton, G. and Tacnet, JM.: 
# Communicating (nature-based) flood-mitigation schemes using flood-excess volume, 
# River Research and Applications, 35(9), 1402–1414, doi:10.1002/rra.3507, 2019.
#
# Script based on FEV_V8.R and SquareLake_V0.R 
# Coded by Guillaume PITON guillaume.piton@inrae.fr 
# Funded by the NAIAD H2020 project (Grand Number: 730497)
# Version 0 - prepared and tested on Sept. 14, 2020
#
# Subrepositories /Data/ and /Sources/ must be pasted with their files in 
# the working directory. In /Data/, one can find an example of the data required, namely:
# Discharge time series (Time, Discharge)
# Rating curves of current and project situations (Depth, Discharge)
# Panel of measures (Name, Comment, FEV capacity, Cost). 
#
# The data used as example reflect the final analysis of the Brague River 
# study, perform within NAIAD, for the Oct. 2015 event at Biot municipality. 
# The protection scenario presented in Bokhove et al. (2018) was refined in
# Piton, G., Dupire, S., Arnaud, P., Mas, A., Marchal, R., Moncoulon, D., 
# Curt. T. and Tacnet, JM.: # DELIVERABLE 6.2 From hazards to risk: models 
# for the DEMOs - Part 3: France: Brague catchment DEMO, NAIAD H2020 project
# (Grant Agreement nº 730497). 2018.
#
