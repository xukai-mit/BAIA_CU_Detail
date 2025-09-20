library( readxl )
library( abind )

source( paste( pwd, "/AssignStateInfo.R", sep = "" ) )


## load stock volume and surface area

load( file = paste( pwd, "/Carbon Uptake/stock_C.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/stock_PC.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/stock_RM.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/stock_URM.Rda",sep = "" ) )

sa <- ( stock_C[ , grep( "sa", colnames( stock_C ) ) ] + stock_PC[ , grep( "sa", colnames( stock_C ) ) ] ) / 10.764 # ft2 to m2
sa_cmu <- ( stock_RM[ , grep( "sa", colnames( stock_C ) ) ] + stock_URM[ , grep( "sa", colnames( stock_C ) ) ] ) / 10.764 # ft2 to m2

vol <- ( stock_C[ , grep( "vol", colnames( stock_C ) ) ] + stock_PC[ , grep( "vol", colnames( stock_C ) ) ] ) / 35.315 # ft3 to m3
vol_cmu <- ( stock_RM[ , grep( "vol", colnames( stock_C ) ) ] + stock_URM[ , grep( "vol", colnames( stock_C ) ) ] ) / 35.315 # ft3 to m3

# Note: assuming all RES12 & RES3AB lc and RES3CDEF & COM hc

# lcsa_RES_low <- rep( TRUE, ncol( sa ) )
# lcsa_RES_hi <- rep( FALSE, ncol( sa ) )
# lcsa_COM <- rep( FALSE, ncol( sa ) )
# 
# lcsa_cmuRES_low <- rep( TRUE, ncol( sa ) )
# lcsa_cmuRES_hi <- rep( FALSE, ncol( sa ) )
# lcsa_cmuCOM <- rep( FALSE, ncol( sa ) )
# 
# lcvol_RES_low <- rep( TRUE, ncol( vol ) )
# lcvol_RES_hi <- rep( FALSE, ncol( vol ) )
# lcvol_COM <- rep( FALSE, ncol( vol ) )
# 
# lcvol_cmuRES_low <- rep( TRUE, ncol( vol ) )
# lcvol_cmuRES_hi <- rep( FALSE, ncol( vol ) )
# lcvol_cmuCOM <- rep( FALSE, ncol( vol ) )

# Note: assuming all lc

# lcsa_RES_low <- rep( TRUE, ncol( sa ) )
# lcsa_RES_hi <- rep( TRUE, ncol( sa ) )
# lcsa_COM <- rep( TRUE, ncol( sa ) )
# 
# lcsa_cmuRES_low <- rep( TRUE, ncol( sa ) )
# lcsa_cmuRES_hi <- rep( TRUE, ncol( sa ) )
# lcsa_cmuCOM <- rep( TRUE, ncol( sa ) )
# 
# lcvol_RES_low <- rep( TRUE, ncol( vol ) )
# lcvol_RES_hi <- rep( TRUE, ncol( vol ) )
# lcvol_COM <- rep( TRUE, ncol( vol ) )
# 
# lcvol_cmuRES_low <- rep( TRUE, ncol( vol ) )
# lcvol_cmuRES_hi <- rep( TRUE, ncol( vol ) )
# lcvol_cmuCOM <- rep( TRUE, ncol( vol ) )

# Note: assuming all RES12 & RES3AB lc, including some elements in RES3CDEF & COM

lcsa_el <- grepl( "s_", colnames( sa ) ) | # floor slab or crawlspace
  grepl( "sf_", colnames( sa ) ) | # slab foundation
  grepl( "pm_", colnames( sa ) ) | # masonry pad
  grepl( "pc_", colnames( sa ) ) | # concrete pad
  grepl( "bw_", colnames( sa ) ) | # basement walls
  grepl( "ew_", colnames( sa ) ) | # exterior walls
  grepl( "iw_", colnames( sa ) ) # infill walls

lcvol_el <- grepl( "s_", colnames( vol ) ) | # floor slab or crawlspace
  grepl( "sf_", colnames( vol ) ) | # slab foundation
  grepl( "pm_", colnames( vol ) ) | # masonry pad
  grepl( "pc_", colnames( vol ) ) | # concrete pad
  grepl( "bw_", colnames( vol ) ) | # basement walls
  grepl( "ew_", colnames( vol ) ) | # exterior walls
  grepl( "iw_", colnames( vol ) ) # infill walls

lcsa_RES_low <- rep( TRUE, ncol( sa ) ) | lcsa_el
lcsa_RES_hi <- rep( FALSE, ncol( sa ) ) | lcsa_el
lcsa_COM <- rep( FALSE, ncol( sa ) ) | lcsa_el

lcsa_cmuRES_low <- rep( TRUE, ncol( sa ) ) | lcsa_el
lcsa_cmuRES_hi <- rep( FALSE, ncol( sa ) ) | lcsa_el
lcsa_cmuCOM <- rep( FALSE, ncol( sa ) ) | lcsa_el

lcvol_RES_low <- rep( TRUE, ncol( vol ) ) | lcvol_el
lcvol_RES_hi <- rep( FALSE, ncol( vol ) ) | lcvol_el
lcvol_COM <- rep( FALSE, ncol( vol ) ) | lcvol_el

lcvol_cmuRES_low <- rep( TRUE, ncol( vol ) ) | lcvol_el
lcvol_cmuRES_hi <- rep( FALSE, ncol( vol ) ) | lcvol_el
lcvol_cmuCOM <- rep( FALSE, ncol( vol ) ) | lcvol_el


## disaggregate uptake per m2 per year for each state (row), element (col)

# Note: replace "Input" with uptake model; "Uptake" could be used for validation

# u_m2_lc <- read_excel( paste( pwd, "/Carbon Uptake/Input.xlsx", sep = "" ), sheet = "lc-uptake" )
# u_m2_hc <- read_excel( paste( pwd, "/Carbon Uptake/Input.xlsx", sep = "" ), sheet = "hc-uptake" )
u_m2_cmulc <- read_excel( paste( pwd, "/Carbon Uptake/Input.xlsx", sep = "" ), sheet = "cmu-uptake" )
u_m2_cmuhc <- read_excel( paste( pwd, "/Carbon Uptake/Input.xlsx", sep = "" ), sheet = "cmu-uptake" )

# u_m2_lc <- u_m2_lc[ 1:51, 2:ncol( u_m2_lc ) ] / sqrt( 50 )
# u_m2_hc <- u_m2_hc[ 1:51, 2:ncol( u_m2_hc ) ] / sqrt( 50 )
u_m2_cmulc <- u_m2_cmulc[ 1:51, 2:ncol( u_m2_cmulc ) ] / sqrt( 15 )
u_m2_cmuhc <- u_m2_cmuhc[ 1:51, 2:ncol( u_m2_cmuhc ) ] / sqrt( 15 )

# u_m2_lc <- array( unlist( u_m2_lc ), dim = c( length( states ), ncol( sa ) ) )
# u_m2_hc <- array( unlist( u_m2_hc ), dim = c( length( states ), ncol( sa ) ) )
u_m2_cmulc <- array( unlist( u_m2_cmulc ), dim = c( length( states ), ncol( sa ) ) )
u_m2_cmuhc <- array( unlist( u_m2_cmuhc ), dim = c( length( states ), ncol( sa ) ) )


## infill year built

# OTs_lc <- c( "RES1", "RES2", "RES3A", "RES3C" )

ap_start <- 1940

ap_end <- 2017

ap <- ap_end - ap_start + 1

cc_p <- 0.5 #2

cw_p <- 3

sr <- c( 1, 1 - pweibull( seq( 1, 1000 - 1 ), shape = 2.8, scale = 73.5 ) )

# <- Note: only need to run once

# sa_yb_RES12 <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
# sa_yb_RES3AB <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
# sa_yb_RES3CDEF <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
# sa_yb_COM <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
# sa_yb_cmuRES12 <- array( rep( 0, length( states ) * ncol( sa_cmu ) * ap ), dim = c( length( states ), ncol( sa_cmu ), ap ) )
# sa_yb_cmuRES3AB <- array( rep( 0, length( states ) * ncol( sa_cmu ) * ap ), dim = c( length( states ), ncol( sa_cmu ), ap ) )
# sa_yb_cmuRES3CDEF <- array( rep( 0, length( states ) * ncol( sa_cmu ) * ap ), dim = c( length( states ), ncol( sa_cmu ), ap ) )
# sa_yb_cmuCOM <- array( rep( 0, length( states ) * ncol( sa_cmu ) * ap ), dim = c( length( states ), ncol( sa_cmu ), ap ) )
# 
# vol_yb_RES12 <- array( rep( 0, length( states ) * ncol( vol ) * ap ), dim = c( length( states ), ncol( vol ), ap ) )
# vol_yb_RES3AB <- array( rep( 0, length( states ) * ncol( vol ) * ap ), dim = c( length( states ), ncol( vol ), ap ) )
# vol_yb_RES3CDEF <- array( rep( 0, length( states ) * ncol( vol ) * ap ), dim = c( length( states ), ncol( vol ), ap ) )
# vol_yb_COM <- array( rep( 0, length( states ) * ncol( vol ) * ap ), dim = c( length( states ), ncol( vol ), ap ) )
# vol_yb_cmuRES12 <- array( rep( 0, length( states ) * ncol( vol_cmu ) * ap ), dim = c( length( states ), ncol( vol_cmu ), ap ) )
# vol_yb_cmuRES3AB <- array( rep( 0, length( states ) * ncol( vol_cmu ) * ap ), dim = c( length( states ), ncol( vol_cmu ), ap ) )
# vol_yb_cmuRES3CDEF <- array( rep( 0, length( states ) * ncol( vol_cmu ) * ap ), dim = c( length( states ), ncol( vol_cmu ), ap ) )
# vol_yb_cmuCOM <- array( rep( 0, length( states ) * ncol( vol_cmu ) * ap ), dim = c( length( states ), ncol( vol_cmu ), ap ) )
# 
# # # Note: remove, not necessary
# #
# # bp_perc <- array( rep( 0, length( states ) * ap ), dim = c( length( states ), ap ) )
# 
# for ( i in seq( 1, length( states ) ) ){
# 
#   StateInfo <- AssignStateInfo( states[ i ] )
# 
#   print( StateInfo$StateAbbrev )
# 
# 
#   # residential bldg permits for each year; assumed 1 yr for construction (https://www.census.gov/construction/nrc/pdf/avg_authtostart.pdf https://www.census.gov/construction/nrc/pdf/avg_starttocomp.pdf)
# 
#   bp_ <- read_excel( paste( pwd, "/Carbon Uptake/annualhistorybystate.xls", sep = "" ), sheet = StateInfo$StateAbbrev )
# 
#   bp_ <- array( as.numeric( array( unlist( bp_[ 9:nrow( bp_ ), 2:ncol( bp_ ) ] ) ) ), dim = c( nrow( bp_ ) - 8, ncol( bp_ ) - 1 ) )
# 
#   colnames( bp_ ) <- c( "YB", "RES_bldg", "RES_unit", "RES_val", "RES1_bldg", "RES1_unit", "RES1_val", "RES3A_bldg", "RES3A_unit", "RES3A_val", "RES3x_bldg", "RES3x_unit", "RES3x_val", "RES3B_bldg", "RES3B_unit", "RES3B_val", "RES3CDEF_bldg", "RES3CDEF_unit", "RES3CDEF_val" )
# 
#   bp_[ , "YB" ] <- seq( 1961, 2022 )
# 
# 
#   # correction for years missing bldg data
# 
#   RES3B_corr <- mean( bp_[ ,"RES3B_bldg" ] / bp_[ ,"RES3B_unit" ], na.rm = TRUE )
# 
#   RES3CDEF_corr <- mean( bp_[ ,"RES3CDEF_bldg" ] / bp_[ ,"RES3CDEF_unit" ], na.rm = TRUE )
# 
#   bp_[ is.na( bp_[ , "RES1_bldg" ] ), "RES1_bldg" ] <- bp_[ is.na( bp_[ , "RES1_bldg" ] ), "RES1_unit" ]
# 
#   bp_[ is.na( bp_[ , "RES3A_bldg" ] ), "RES3A_bldg" ] <- rowSums( cbind( bp_[ ,"RES3A_unit" ], bp_[ ,"RES3x_unit" ] / 2 ), na.rm = TRUE )[ is.na( bp_[ , "RES3A_bldg" ] ) ] * 0.5
# 
#   bp_[ is.na( bp_[ , "RES3B_bldg" ] ), "RES3B_bldg" ] <- rowSums( cbind( bp_[ ,"RES3B_unit" ], bp_[ ,"RES3x_unit" ] / 2 ), na.rm = TRUE )[ is.na( bp_[ , "RES3B_bldg" ] ) ] * RES3B_corr
# 
#   bp_[ is.na( bp_[ , "RES3CDEF_bldg" ] ), "RES3CDEF_bldg" ] <- bp_[ is.na( bp_[ , "RES3CDEF_bldg" ] ), "RES3CDEF_unit" ] * RES3CDEF_corr
# 
#   bp_[ , "RES_bldg" ] <- bp_[ , "RES1_bldg" ] + bp_[ , "RES3A_bldg" ] + bp_[ , "RES3B_bldg" ] + bp_[ , "RES3CDEF_bldg" ]
# 
#   bp_ <- bp_[ , grep( "RES3x", colnames( bp_ ), invert = TRUE ) ]
# 
#   bp_ <- bp_[ , grep( "unit", colnames( bp_ ), invert = TRUE ) ]
# 
#   bp_ <- bp_[ , grep( "val", colnames( bp_ ), invert = TRUE ) ]
# 
# 
#   # correction for bldgs that never started or that never completed; assumed proportional to units (https://www.census.gov/construction/nrc/nrcdatarelationships.html)
# 
#   bp_[ , "RES1_bldg" ] <- bp_[ , "RES1_bldg" ] * ( 1 + 0.025 ) #* ( 1 - 0.035 ) # Note: this includes bldgs outside of permit areas
# 
#   # bp_[ , "RES3A_bldg" ] <- bp_[ , "RES3A_bldg" ] * ( 1 - 0.225 ) * ( 1 - 0.075 )
#   #
#   # bp_[ , "RES3B_bldg" ] <- bp_[ , "RES3B_bldg" ] * ( 1 - 0.225 ) * ( 1 - 0.075 )
#   #
#   # bp_[ , "RES3CDEF_bldg" ] <- bp_[ , "RES3CDEF_bldg" ] * ( 1 - 0.225 ) * ( 1 - 0.075 )
# 
#   bp_[ , "RES_bldg" ] <- bp_[ , "RES1_bldg" ] + bp_[ , "RES3A_bldg" ] + bp_[ , "RES3B_bldg" ] + bp_[ , "RES3CDEF_bldg" ]
# 
# 
#   # correction for years missing both bldg and unit data
# 
#   cem_ <- c(
#     0.003921298,
#     0.005106162,
#     0.005586045,
#     0.003819275,
#     0.002788434,
#     0.003126890,
#     0.003507450,
#     0.005871960,
#     0.006451707,
#     0.006525300,
#     0.007357674,
#     0.007682274,
#     0.007977366,
#     0.008285412,
#     0.008798224,
#     0.009642473,
#     0.009936845,
#     0.009291422,
#     0.009813950,
#     0.010777856,
#     0.009991005,
#     0.010224919,
#     0.010613036,
#     0.011452428,
#     0.011902442,
#     0.012161547,
#     0.012323667,
#     0.012138155,
#     0.012814886,
#     0.012793474,
#     0.012141214,
#     0.013106739,
#     0.013813159,
#     0.014743957,
#     0.013466967,
#     0.011445230,
#     0.012147152,
#     0.013212000,
#     0.014266592,
#     0.014208294,
#     0.012626496,
#     0.011892186,
#     0.010719017,
#     0.011846483,
#     0.013708438,
#     0.014185262,
#     0.014905178,
#     0.015151147,
#     0.015087630,
#     0.014829066,
#     0.014568162,
#     0.012919248,
#     0.013705379,
#     0.014340905,
#     0.015559957,
#     0.015474848,
#     0.016257920,
#     0.017276885,
#     0.018615413,
#     0.019587955,
#     0.019877289,
#     0.020296535,
#     0.019796319,
#     0.020528649,
#     0.021948327,
#     0.023076512,
#     0.022970351,
#     0.020971286,
#     0.017410396,
#     0.012872465,
#     0.012807689,
#     0.012991222,
#     0.014013246,
#     0.014709590,
#     0.016053695,
#     0.016580901,
#     0.017120703,
#     0.017482370
#   )
# 
#   cem_missing <- as.numeric( cem_[ 1:21 ] )
# 
#   cem_present <- as.numeric( cem_[ 22:ap ] )
# 
#   bp_ <- rbind( cbind(
#     seq( 1940, 1960 ),
#     cem_missing * mean( bp_[ 1:57, "RES_bldg" ] / cem_present, na.rm = TRUE ),
#     cem_missing * mean( bp_[ 1:57, "RES1_bldg" ] / cem_present, na.rm = TRUE ),
#     cem_missing * mean( bp_[ 1:57, "RES3A_bldg" ] / cem_present, na.rm = TRUE ),
#     cem_missing * mean( bp_[ 1:57, "RES3B_bldg" ] / cem_present, na.rm = TRUE ),
#     cem_missing * mean( bp_[ 1:57, "RES3CDEF_bldg" ] / cem_present, na.rm = TRUE )
#   ), bp_ )
# 
# 
#   # correction of age distribution
# 
#   for ( yb in seq( ap_start, ap_end ) ){
# 
#     yrs_ <- c( "Built40to49", "Built20to45", "Built46to59", "Built50to59", "Built60to69", "Built70to79", "Built80to89", "Built90to98", "Built90to99", "BuiltAfter98", "BuiltAfter99" )
# 
#     if ( yb %in% seq( 1940, 1945 ) ){
# 
#       yrs <- c( "Built40to49", "Built20to45" )
# 
#       ns <- c( 6, 26 )
# 
#       range <- seq( 1940, 1945 )
# 
#     }
# 
#     if ( yb %in% seq( 1946, 1949 ) ){
# 
#       yrs <- c( "Built40to49", "Built46to59" )
# 
#       ns <- c( 6, 13 )
# 
#       range <- seq( 1946, 1949 )
# 
#     }
# 
#     if ( yb %in% seq( 1950, 1959 ) ){
# 
#       yrs <- c( "Built50to59", "Built46to59" )
# 
#       ns <- c( 10, 13 )
# 
#       range <- seq( 1950, 1959 )
# 
#     }
# 
#     if ( yb %in% seq( 1960, 1969 ) ){
# 
#       yrs <- c( "Built60to69" )
# 
#       ns <- c( 10 )
# 
#       range <- seq( 1960, 1969 )
# 
#     }
# 
#     if ( yb %in% seq( 1970, 1979 ) ){
# 
#       yrs <- c( "Built70to79" )
# 
#       ns <- c( 10 )
# 
#       range <- seq( 1970, 1979 )
# 
#     }
# 
#     if ( yb %in% seq( 1980, 1989 ) ){
# 
#       yrs <- c( "Built80to89" )
# 
#       ns <- c( 10 )
# 
#       range <- seq( 1980, 1989 )
# 
#     }
# 
#     if ( yb %in% seq( 1990, 1998 ) ){
# 
#       yrs <- c( "Built90to98", "Built90to99" )
# 
#       ns <- c( 9, 10 )
# 
#       range <- seq( 1990, 1998 )
# 
#     }
# 
#     if ( yb == 1999 ){
# 
#       yrs <- c( "BuiltAfter98", "Built90to99" )
# 
#       ns <- c( 20, 10 )
# 
#       range <- c( 1999 )
# 
#     }
# 
#     if ( yb > 1999 ){
# 
#       yrs <- c( "BuiltAfter98", "BuiltAfter99" )
# 
#       ns <- c( 20, 19 )
# 
#       range <- seq( 2000, ap )
# 
#     }
# 
# 
#     # including correction for retirement
# 
#     # Note:
#     # approach 0 - take all buildings and distr based on bps (might overest older bldgs)
#     # approach 1 - ^ for otherhc, for res bldgs in a period distr based on bps (might overest older otherhc bldgs)
#     # approach 2 - ^ for otherhc and otherlc, for res bldgs in a period correct age based on bps
# 
#     # approach 0 - correct distr, then correct numbers
#     # approach 1 - ^ for otherhc, for res bldgs correct numbers
#     # approach 2 - ^ for otherhc and otherlc
# 
# 
#     # # <- approach 1: use building permits for relative age only
#     #
#     # RES1_corr <- ( 1 / sr[ ap - ( yb - 1939 ) + 1 ] ) * bp_[ yb - 1939, "RES1_bldg" ] / sum( bp_[ bp_[ , "YB"] %in% range, "RES1_bldg" ] )
#     #
#     # RES3A_corr <- ( 1 / sr[ ap - ( yb - 1939 ) + 1 ] ) * bp_[ yb - 1939, "RES3A_bldg" ] / sum( bp_[ bp_[ , "YB"] %in% range, "RES3A_bldg" ] )
#     #
#     # RES3B_corr <- ( 1 / sr[ ap - ( yb - 1939 ) + 1 ] ) * bp_[ yb - 1939, "RES3B_bldg" ] / sum( bp_[ bp_[ , "YB"] %in% range, "RES3B_bldg" ] )
#     #
#     # RES3CDEF_corr <- ( 1 / sr[ ap - ( yb - 1939 ) + 1 ] ) * bp_[ yb - 1939, "RES3CDEF_bldg" ] / sum( bp_[ bp_[ , "YB"] %in% range, "RES3CDEF_bldg" ] )
#     #
#     # otherlc_corr <- ( 1 / sr[ ap - ( yb - 1939 ) + 1 ] ) * bp_[ yb - 1939, "RES_bldg" ] / sum( bp_[ bp_[ , "YB"] %in% range, "RES_bldg" ] )
#     #
#     # otherhc_corr <- ( 1 / sr[ ap - ( yb - 1939 ) + 1 ] ) * ( bp_[ yb - 1939, "RES_bldg" ] * sr[ ap - ( yb - 1939 ) + 1 ] ) / sum( bp_[ 1:ap, "RES_bldg" ] * rev( sr ) ) # based on total
#     #
#     # # ->
# 
# 
#     # <- approach 2: scale residential bldgs by number, scale nonresidential bldgs by relative age
# 
#     RES1_est <- sum( stock_C$n[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES1" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ) ] ) #/ sum( ns )
# 
#     RES3A_est <- sum( stock_C$n[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES3A" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ) ] ) #/ sum( ns )
# 
#     RES3B_est <- sum( stock_C$n[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES3B" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ) ] ) #/ sum( ns )
# 
#     RES3CDEF_est <- sum( stock_C$n[ stock_C$StateFIPS == states[ i ] & stock_C$OT %in% c( "RES3C", "RES3D", "RES3E", "RES3F" ) & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ) ] ) #/ sum( ns )
# 
# 
#     RES1_corr <- bp_[ yb - 1939, "RES1_bldg" ] / RES1_est
# 
#     RES3A_corr <- bp_[ yb - 1939, "RES3A_bldg" ] / RES3A_est
# 
#     RES3B_corr <- bp_[ yb - 1939, "RES3B_bldg" ] / RES3B_est
# 
#     RES3CDEF_corr <- bp_[ yb - 1939, "RES3CDEF_bldg" ] / RES3CDEF_est
# 
#     otherlc_corr <- ( 1 / sr[ ap - ( yb - 1939 ) + 1 ] ) * bp_[ yb - 1939, "RES_bldg" ] / sum( bp_[ bp_[ , "YB"] %in% range, "RES_bldg" ] )
# 
#     otherhc_corr <- ( 1 / sr[ ap - ( yb - 1939 ) + 1 ] ) * ( bp_[ yb - 1939, "RES_bldg" ] * sr[ ap - ( yb - 1939 ) + 1 ] ) / sum( bp_[ 1:ap, "RES_bldg" ] * rev( sr ) ) # based on total
# 
#     # ->
# 
# 
#     sa_yb_RES1 <- colSums( sa[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES1" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * RES1_corr
#     sa_yb_RES3A <- colSums( sa[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES3A" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * RES3A_corr
#     sa_yb_RES3B <- colSums( sa[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES3B" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * RES3B_corr
#     sa_yb_RES3CDEF_ <- colSums( sa[ stock_C$StateFIPS == states[ i ] & stock_C$OT %in% c( "RES3C", "RES3D", "RES3E", "RES3F" ) & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * RES3CDEF_corr
#     sa_yb_otherlc <- colSums( sa[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES2" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * otherlc_corr
#     sa_yb_otherhc <- colSums( sa[ stock_C$StateFIPS == states[ i ] & !( stock_C$OT %in% OTs_lc ) & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * otherhc_corr
# 
#     sa_yb_cmuRES1 <- colSums( sa_cmu[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES1" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * RES1_corr
#     sa_yb_cmuRES3A <- colSums( sa_cmu[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES3A" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * RES3A_corr
#     sa_yb_cmuRES3B <- colSums( sa_cmu[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES3B" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * RES3B_corr
#     sa_yb_cmuRES3CDEF_ <- colSums( sa_cmu[ stock_C$StateFIPS == states[ i ] & stock_C$OT %in% c( "RES3C", "RES3D", "RES3E", "RES3F" ) & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * RES3CDEF_corr
#     sa_yb_cmuotherlc <- colSums( sa_cmu[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES2" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * otherlc_corr
#     sa_yb_cmuotherhc <- colSums( sa_cmu[ stock_C$StateFIPS == states[ i ] & !( stock_C$OT %in% OTs_lc ) & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * otherhc_corr
# 
#     vol_yb_RES1 <- colSums( vol[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES1" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * RES1_corr
#     vol_yb_RES3A <- colSums( vol[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES3A" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * RES3A_corr
#     vol_yb_RES3B <- colSums( vol[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES3B" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * RES3B_corr
#     vol_yb_RES3CDEF_ <- colSums( vol[ stock_C$StateFIPS == states[ i ] & stock_C$OT %in% c( "RES3C", "RES3D", "RES3E", "RES3F" ) & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * RES3CDEF_corr
#     vol_yb_otherlc <- colSums( vol[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES2" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * otherlc_corr
#     vol_yb_otherhc <- colSums( vol[ stock_C$StateFIPS == states[ i ] & !( stock_C$OT %in% OTs_lc ) & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * otherhc_corr
# 
#     vol_yb_cmuRES1 <- colSums( vol_cmu[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES1" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * RES1_corr
#     vol_yb_cmuRES3A <- colSums( vol_cmu[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES3A" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * RES3A_corr
#     vol_yb_cmuRES3B <- colSums( vol_cmu[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES3B" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * RES3B_corr
#     vol_yb_cmuRES3CDEF_ <- colSums( vol_cmu[ stock_C$StateFIPS == states[ i ] & stock_C$OT %in% c( "RES3C", "RES3D", "RES3E", "RES3F" ) & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * RES3CDEF_corr
#     vol_yb_cmuotherlc <- colSums( vol_cmu[ stock_C$StateFIPS == states[ i ] & stock_C$OT == "RES2" & stock_C$YB %in% yrs & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * otherlc_corr
#     vol_yb_cmuotherhc <- colSums( vol_cmu[ stock_C$StateFIPS == states[ i ] & !( stock_C$OT %in% OTs_lc ) & is.finite( stock_C$n ) & !is.na( stock_C$n ), ] ) * otherhc_corr
# 
# 
#     # aggregation by lc or hc
# 
#     sa_yb_RES12[ i, , yb - 1939 ] <- sa_yb_RES1 + sa_yb_otherlc
#     sa_yb_RES3AB[ i, , yb - 1939 ] <- sa_yb_RES3A + sa_yb_RES3B
#     sa_yb_RES3CDEF[ i, , yb - 1939 ] <- sa_yb_RES3CDEF_
#     sa_yb_COM[ i, , yb - 1939 ] <- sa_yb_otherhc
#     sa_yb_cmuRES12[ i, , yb - 1939 ] <- sa_yb_cmuRES1 + sa_yb_cmuotherlc
#     sa_yb_cmuRES3AB[ i, , yb - 1939 ] <- sa_yb_cmuRES3A + sa_yb_cmuRES3B
#     sa_yb_cmuRES3CDEF[ i, , yb - 1939 ] <- sa_yb_cmuRES3CDEF_
#     sa_yb_cmuCOM[ i, , yb - 1939 ] <- sa_yb_cmuotherhc
# 
#     vol_yb_RES12[ i, , yb - 1939 ] <- vol_yb_RES1 + vol_yb_otherlc
#     vol_yb_RES3AB[ i, , yb - 1939 ] <- vol_yb_RES3A + vol_yb_RES3B
#     vol_yb_RES3CDEF[ i, , yb - 1939 ] <- vol_yb_RES3CDEF_
#     vol_yb_COM[ i, , yb - 1939 ] <- vol_yb_otherhc
#     vol_yb_cmuRES12[ i, , yb - 1939 ] <- vol_yb_cmuRES1 + vol_yb_cmuotherlc
#     vol_yb_cmuRES3AB[ i, , yb - 1939 ] <- vol_yb_cmuRES3A + vol_yb_cmuRES3B
#     vol_yb_cmuRES3CDEF[ i, , yb - 1939 ] <- vol_yb_cmuRES3CDEF_
#     vol_yb_cmuCOM[ i, , yb - 1939 ] <- vol_yb_cmuotherhc
# 
#   }
# 
#   # # Note: remove, not necessary
#   #
#   # bp_perc[ i, ] <- ( 1 / sr ) * ( bp_[ 1:ap, "RES_bldg" ] * rev( sr ) ) / sum( bp_[ 1:ap, "RES_bldg" ] * rev( sr ) )
# 
# }
# 
# save( sa_yb_RES12, file = paste( pwd, "/Carbon Uptake/sa_yb_RES12.Rda", sep = "" ) )
# save( sa_yb_RES3AB, file = paste( pwd, "/Carbon Uptake/sa_yb_RES3AB.Rda", sep = "" ) )
# save( sa_yb_RES3CDEF, file = paste( pwd, "/Carbon Uptake/sa_yb_RES3CDEF.Rda", sep = "" ) )
# save( sa_yb_COM, file = paste( pwd, "/Carbon Uptake/sa_yb_COM.Rda", sep = "" ) )
# save( sa_yb_cmuRES12, file = paste( pwd, "/Carbon Uptake/sa_yb_cmuRES12.Rda", sep = "" ) )
# save( sa_yb_cmuRES3AB, file = paste( pwd, "/Carbon Uptake/sa_yb_cmuRES3AB.Rda", sep = "" ) )
# save( sa_yb_cmuRES3CDEF, file = paste( pwd, "/Carbon Uptake/sa_yb_cmuRES3CDEF.Rda", sep = "" ) )
# save( sa_yb_cmuCOM, file = paste( pwd, "/Carbon Uptake/sa_yb_cmuCOM.Rda", sep = "" ) )
# 
# save( vol_yb_RES12, file = paste( pwd, "/Carbon Uptake/vol_yb_RES12.Rda", sep = "" ) )
# save( vol_yb_RES3AB, file = paste( pwd, "/Carbon Uptake/vol_yb_RES3AB.Rda", sep = "" ) )
# save( vol_yb_RES3CDEF, file = paste( pwd, "/Carbon Uptake/vol_yb_RES3CDEF.Rda", sep = "" ) )
# save( vol_yb_COM, file = paste( pwd, "/Carbon Uptake/vol_yb_COM.Rda", sep = "" ) )
# save( vol_yb_cmuRES12, file = paste( pwd, "/Carbon Uptake/vol_yb_cmuRES12.Rda", sep = "" ) )
# save( vol_yb_cmuRES3AB, file = paste( pwd, "/Carbon Uptake/vol_yb_cmuRES3AB.Rda", sep = "" ) )
# save( vol_yb_cmuRES3CDEF, file = paste( pwd, "/Carbon Uptake/vol_yb_cmuRES3CDEF.Rda", sep = "" ) )
# save( vol_yb_cmuCOM, file = paste( pwd, "/Carbon Uptake/vol_yb_cmuCOM.Rda", sep = "" ) )

# ->


## define parameters and functions for uptake

apps <- c( "mortar", "cmu", "lc", "hc" )

exp_cnds <- c( "Exposed to rain", "Sheltered from rain", "In-ground", "Indoor (with cover)", "Indoor (without cover)", "Underwater" )

cmp_strs <- c( "LessThan15MPa", "15to20MPa", "25to35MPa", "MoreThan35MPa" )

app2cat <- function( app ){
  
  if ( app %in% cmp_strs ){
    
    cmp_str <- app
    
  }else{
    
    if ( !is.numeric( app ) ){
      
      if( app == "mortar" ){ cmp_str <- "LessThan15MPa" }
      
      if( app == "cmu" ){ cmp_str <- "15to20MPa" }
      
      # if( app == "lc" ){ cmp_str <- "25to35MPa" } # Note: updated
      
      # if( app == "hc" ){ cmp_str <- "MoreThan35MPa" } # Note: updated
      
      if( app == "lc" ){ cmp_str <- "15to20MPa" }
      
      if( app == "hc" ){ cmp_str <- "25to35MPa" }
      
    }else{
      
      if( app <= 15 ){ cmp_str <- "LessThan15MPa" }
      
      if( app > 15 & app <= 20 ){ cmp_str <- "15to20MPa" }
      
      # Note: what about cmp_str > 20 & cmp_str < 25
      
      if( app >= 25 & app < 35 ){ cmp_str <- "25to35MPa" }
      
      if( app >= 35 ){ cmp_str <- "MoreThan35MPa" }
      
    }
    
  }
  
  return( cmp_str )
  
}

MixDesign <- function( app, RegionNRMCA = "National Average" ){
  
  # app to cat
  
  # Note: this doesn't match excel
  
  cmp_str <- app2cat( app )
  
  # cmp_strs <- c( 17.2, 20.7, 27.6, 34.5, 41.4, 55.2 )
  #
  # if( is.numeric( app ) ){
  #
  #   cmp_str <- which( abs( cmp_strs - app ) == min( abs( cmp_strs - app ) ) )
  #
  # }
  #
  # # Note: is this a fair assumption for mortar?
  #
  # if ( app == "mortar" ){ cmp_str <- 1 } # <= 15 MPa
  #
  # # Note: is this a fair assumption for cmu?
  #
  # if ( app == "cmu" ){ cmp_str <- 2 } # 15-20 MPa
  #
  # if ( app == "lc" ){ cmp_str <- 3 } # 25-35 MPa
  #
  # if ( app == "hc" ){ cmp_str <- 4 } # >= 35 MPa
  
  
  # kg per m3 concrete
  
  # Note: this doesn't match excel sheet
  
  mix <- list(
    "LessThan15MPa" = data.frame(
      "Eastern" = c( 175.5, 30.4, 17.7 ),
      "GreatLakes" = c( 173.3, 12.2, 20 ),
      "NorthCentral" = c( 177.8, 2.3, 33.1 ),
      "PacificNorthwest" = c( 184.2, 5.9, 47.6 ),
      "PacificSouthwest" = c( 190.1, 0, 27.7 ),
      "RockyMountains" = c( 182.3, 0, 34.9 ),
      "SouthCentral" = c( 165.1, 0.5, 30.8 ),
      "Southeastern" = c( 177.4, 10.9, 42.2 ),
      row.names = c( "cement", "slag", "flyash" ) ),
    "15to20MPa" = data.frame(
      "Eastern" = c( 215.5, 37.2, 21.3 ),
      "GreatLakes" = c( 212.3, 15.0, 24.5 ),
      "NorthCentral" = c( 215.9, 2.3, 39.9 ),
      "PacificNorthwest" = c( 226.8, 7.3, 58.5 ),
      "PacificSouthwest" = c( 225.4, 0, 33.1 ),
      "RockyMountains" = c( 220.4, 0, 42.2 ),
      "SouthCentral" = c( 198.7, 0.5, 37.2 ),
      "Southeastern" = c( 210.9, 13.2, 50.3 ),
      row.names = c( "cement", "slag", "flyash" ) ),
    "25to35MPa" = data.frame(
      "Eastern" = c( 265.4, 45.8, 26.8 ),
      "GreatLakes" = c( 261.3, 18.6, 30.4 ),
      "NorthCentral" = c( 262.6, 3.2, 48.5 ),
      "PacificNorthwest" = c( 280.8, 8.6, 72.1 ),
      "PacificSouthwest" = c( 269.4, 0, 39.5 ),
      "RockyMountains" = c( 266.7, 0, 50.8 ),
      "SouthCentral" = c( 239.5, 0.5, 44.9 ),
      "Southeastern" = c( 251.7, 15.4, 60.3 ),
      row.names = c( "cement", "slag", "flyash" ) ),
    "MoreThan35MPa" = data.frame(
      "Eastern" = c( 281.2, 48.5, 28.1 ),
      "GreatLakes" = c( 277.1, 20.0, 32.2 ),
      "NorthCentral" = c( 279.0, 3.2, 51.7 ),
      "PacificNorthwest" = c( 297.1, 9.5, 76.7 ),
      "PacificSouthwest" = c( 286.7, 0, 41.7 ),
      "RockyMountains" = c( 283.5, 0.5, 54.0 ),
      "SouthCentral" = c( 254.5, 0.5, 47.6 ),
      "Southeastern" = c( 268.1, 16.3, 64.0 ),
      row.names = c( "cement", "slag", "flyash" ) )
  )
  
  mix[[ "LessThan15MPa" ]]$NationalAverage <- rowMeans( mix[[ "LessThan15MPa" ]] )
  mix[[ "15to20MPa" ]]$NationalAverage <- rowMeans( mix[[ "15to20MPa" ]] )
  mix[[ "25to35MPa" ]]$NationalAverage <- rowMeans( mix[[ "25to35MPa" ]] )
  mix[[ "MoreThan35MPa" ]]$NationalAverage <- rowMeans( mix[[ "MoreThan35MPa" ]] )
  
  return( data.frame(
    cmp_str = cmp_str,
    cement = mix[[ cmp_str ]][ "cement", gsub( " ", "", RegionNRMCA ) ],
    flyash = mix[[ cmp_str ]][ "flyash", gsub( " ", "", RegionNRMCA ) ],
    slag = mix[[ cmp_str ]][ "slag", gsub( " ", "", RegionNRMCA ) ]
  ) )
  
  # cement <- data.frame(
  #   "Eastern" = c( 204.7, 229.6, 281.8, 347.0, 367.8, 442.5 ),
  #   "GreatLakes" = c( 202.3, 226.6, 277.6, 341.7, 362.4, 434.8 ),
  #   "NorthCentral" = c( 208.8, 232.5, 282.3, 343.4, 364.8, 431.8 ),
  #   "PacificNorthwest" = c( 214.7, 240.8, 296.6, 367.1, 388.5, 469.2 ),
  #   "PacificSouthwest" = c( 224.8, 248.5, 294.8, 352.3, 374.9, 433.6 ),
  #   "RockyMountains" = c( 214.7, 238.4, 288.3, 348.8, 370.7, 436.5 ),
  #   "SouthCentral" = c( 194.5, 215.9, 259.8, 313.2, 332.7, 390.3 ),
  #   "Southeastern" = c( 210.0, 231.9, 275.8, 329.2, 350.5, 405.7 )
  # )
  #
  # cement[ "NationalAverage" ] <- rowMeans( cement )
  #
  # flyash <- data.frame(
  #   "Eastern" = c( 20.8, 23.1, 27.9, 35.0, 36.8, 44.5 ),
  #   "GreatLakes" = c( 23.1, 26.1, 32.0, 39.7, 42.1, 50.4 ),
  #   "NorthCentral" = c( 38.6, 43.3, 52.2, 63.5, 67.6, 80.1 ),
  #   "PacificNorthwest" = c( 55.2, 62.3, 76.5, 94.3, 100.2, 121.0 ),
  #   "PacificSouthwest" = c( 32.6, 36.2, 43.3, 51.6, 54.6, 63.5 ),
  #   "RockyMountains" = c( 40.9, 45.7, 55.2, 66.4, 70.6, 83.6 ),
  #   "SouthCentral" = c( 36.2, 40.3, 48.6, 58.7, 62.3, 73.0 ),
  #   "Southeastern" = c( 50.4, 55.2, 65.8, 78.9, 83.6, 96.7 )
  # )
  #
  # flyash[ "NationalAverage" ] <- rowMeans( flyash )
  #
  # slag <- data.frame(
  #   "Eastern" = c( 35.6, 39.7, 48.6, 59.9, 63.5, 76.5 ),
  #   "GreatLakes" = c( 14.2, 16.0, 19.6, 24.3, 26.1, 30.8 ),
  #   "NorthCentral" = c( 2.4, 3.0, 3.0, 4.2, 4.2, 4.7 ),
  #   "PacificNorthwest" = c( 6.5, 7.7, 9.5, 11.3, 12.5, 14.8 ),
  #   "PacificSouthwest" = c( 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ),
  #   "RockyMountains" = c( 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ),
  #   "SouthCentral" = c( 0.6, 0.6, 0.6, 0.6, 0.6, 1.2 ),
  #   "Southeastern" = c( 13.0, 14.2, 17.2, 20.2, 21.4, 24.9 )
  # )
  #
  # slag[ "NationalAverage" ] <- rowMeans( slag )
  #
  # return( data.frame(
  #   cmp_str = cmp_str[ cmp_str ],
  #   cement = cement[ cmp_str, gsub( " ", "", RegionNRMCA ) ],
  #   flyash = flyash[ cmp_str, gsub( " ", "", RegionNRMCA ) ],
  #   slag = slag[ cmp_str, gsub( " ", "", RegionNRMCA ) ]
  # ) )
  
}

k_ <- function( exp_cnd, cmp_str ){
  
  # mm / sqrt( yr )
  
  k <- t( array( c( c( 5.5, 2.7, 1.6, 1.1 ),
                    c( 11.0, 6.6, 4.4, 2.7 ),
                    c( 5.5, 1.1, 0.8, 0.5 ),
                    c( 11.6, 6.9, 4.6, 2.7 ),
                    c( 16.5, 9.9, 6.6, 3.8 ),
                    c( 0.2, 0.2, 0.2, 0.2 ) ), dim = c( 4, 6 ) ) )
  
  return(
    k[ which( exp_cnds == exp_cnd ), which( cmp_strs == cmp_str ) ]
  )
  
}

k_corr_ <- function( k, cement, flyash, slag, limestone = 0 ){
  
  # flyash to corr
  
  flyash_ <- flyash / ( cement + flyash + slag + limestone )
  
  if ( flyash_ <= 0.10 ){ flyash_corr <- 1.00 }
  
  if( flyash_ > 0.10 & flyash_ <= 0.30 ){ flyash_corr <- 1.05 }
  
  if( flyash_ > 0.30 ){ flyash_corr <- 1.10 }
  
  
  # slag to corr
  
  slag_ <- slag / ( cement + flyash + slag + limestone )
  
  if ( slag_ <= 0.10 ){ slag_corr <- 1.05 }
  
  if ( slag_ > 0.10 & slag_ <= 0.20 ){ slag_corr <- 1.10 }
  
  if ( slag_ > 0.20 & slag_ <= 0.30 ){ slag_corr <- 1.15 }
  
  if ( slag_ > 0.30 & slag_ <= 0.40 ){ slag_corr <- 1.20 }
  
  if ( slag_ > 0.40 & slag_ <= 0.60 ){ slag_corr <- 1.25 }
  
  if ( slag_ > 0.60 ){ slag_corr <- 1.30 }
  
  
  # limestone to corr
  
  limestone_ <- limestone / ( cement + flyash + slag + limestone )
  
  if ( limestone_ <= 0.10 ){ limestone_corr <- 1.00 }
  
  if ( limestone_ > 0.10 & limestone_ <= 0.20 ){ limestone_corr <- 1.05 }
  
  if ( limestone_ > 0.20 ){ limestone_corr <- 1.10 }
  
  # Note: this doesn't match excel sheet
  
  # return(
  #   k * flyash_corr * slag_corr * limestone_corr
  # )
  
  return(
    k * ( flyash_ * flyash_corr + slag_ * slag_corr + limestone_ * limestone_corr ) / ( flyash_ + slag_ + limestone_ )
  )
  
}

k_doc_ <- function( k_corr, exp_cnd ){
  
  # %
  
  doc <- c( 0.85, 0.75, 0.85, 0.40, 0.40, 0.85 )
  
  return(
    k_corr * doc[ which( exp_cnds == exp_cnd ) ]
  )
  
}

Utcc_ <- function( app, cement, flyash, slag, limestone = 0 ){
  
  if ( app == "mortar" | app == "cmu" ){
    
    cement_Utcc <- ( 44 / 56 ) * 0.95 * 0.636
    
  }else{
    
    cement_Utcc <- ( 44 / 56 ) * 0.95 * 0.65
    
  }
  
  # Note: this doesn't match excel sheet
  
  # flyash_Utcc <- 0.7 * 0.25
  # 
  # slag_Utcc <- 0.7 * 0.40
  
  flyash_Utcc <- ( 44 / 56 ) * 0.25
  
  slag_Utcc <- ( 44 / 56 ) * 0.40
  
  return(
    cement * cement_Utcc + flyash * flyash_Utcc + slag * slag_Utcc
  )
  
}

c_up <- function( app, exp_cnd, ap = 1, sa = 1, vol = FALSE, RegionNRMCA = "National Average", cement = FALSE, flyash = FALSE, slag = FALSE, limestone = FALSE ){
  
  mix <- MixDesign( app, RegionNRMCA )
  
  if( cement != FALSE ){ mix$cement <- cement }
  
  if( flyash != FALSE ){ mix$flyash <- flyash }
  
  if( slag != FALSE ){ mix$slag <- slag }
  
  k <- k_( exp_cnd, mix$cmp_str )
  
  k_corr <- k_corr_( k, mix$cement, mix$flyash, mix$slag )
  
  k_doc <- k_doc_( k_corr, exp_cnd )
  
  Utcc <- Utcc_( app, mix$cement, mix$flyash, mix$slag )
  
  
  # cc
  
  # Note: check formulas; this was modeled for one surface of an element, what does this mean for volume?
  
  if ( vol != FALSE ){
    
    # perc_carb <- ( k_corr / 1000 ) * sqrt( ap ) * sa / ( sa * t )
    
    d <- 2350 # kg per m3 # Note: assumed
    
    rem <- 0.90 # % # Note: assumed
    
    h_sp <- 0.5 # m # Note: assumed; could be input
    
    slope_sp <- 15  * 0.0175 # degree to rad # Note: assumed; could be input
    
    sp <- TRUE # Note: assumed; could be input
    
    if( sp == TRUE ){
      
      doc_cc <- 0.75
      
      doc_cc_ <- 1
      
    }else{
      
      doc_cc <- 0.85
      
      doc_cc_ <- 0.85 / 0.75
      
    }
    
    grade <- "coarse" # Note: assumed; could be input
    
    if( grade == "fine" ){
      
      rate <- 22.7 #34.6 # kg co2 per m3 per year^0.5
      
      d_sp <- 1330.0 # kg per m3
      
    }
    
    if( grade == "medium" ){
      
      rate <- 17.1 #21.6
      
      d_sp <- 1415.0
      
    }
    
    if( grade == "coarse" ){
      
      rate <- 12.8 #13.5
      
      d_sp <- 1475.0
      
    }
    
    l_sp <- h_sp / tan( slope_sp )
    
    sh_sp <- sqrt( h_sp^2 + l_sp^2 )
    
    vol_carb <- ( k_corr / 1000 ) * sqrt( ap ) * sa
    
    vol_sp <- ( ( vol - vol_carb ) * rem * d ) / d_sp #( vol * rem * d ) / d_sp
    
    r_top <- max(
      ( -6 * l_sp - sqrt( ( 36 * l_sp^2 ) - 12 * ( 4 * l_sp^2 - ( 3 * vol_sp / ( pi * h_sp ) ) ) ) ) / 6,
      ( -6 * l_sp + sqrt( ( 36 * l_sp^2 ) - 12 * ( 4 * l_sp^2 - ( 3 * vol_sp / ( pi * h_sp ) ) ) ) ) / 6
    )
    
    r_bot <- r_top + l_sp
    
    
    # top
    
    if ( h_sp > 0.3 ){
      
      h_tc <- 0.3
      
    }else{
      
      h_tc <- h_sp
      
    }
    
    r_tc <- r_top + ( r_bot - r_top ) * h_tc / h_sp
    
    sh_tc <- sqrt( ( r_tc - r_top )^2 + h_tc^2 )
    
    vol_tc <- pi * h_tc * ( r_top^2 + ( r_top * r_tc ) + r_tc^2 ) / 3
    
    
    # lateral
    
    sa_lc <- pi * ( r_tc + r_bot ) * ( sh_sp - sh_tc )
    
    if ( ( h_sp - h_tc ) / ( sh_sp - sh_tc ) * r_tc < 0.3 ){
      
      t_lc <- ( h_sp - h_tc ) / ( sh_sp - sh_tc ) * r_tc
      
    }else{
      
      t_lc <- 0.3
      
    }
    
    vol_lc <- sa_lc * t_lc
    
    
    # total
    
    if ( ( vol_tc + vol_lc ) / vol_sp > 1 ){
      
      coeff <- 1
      
    }else{
      
      coeff <- ( vol_tc + vol_lc ) / vol_sp
      
    }
    
    cc_max <- coeff * doc_cc * Utcc * ( vol - vol_carb ) * rem
    
    cc <- min(
      cc_max,
      rate * coeff * doc_cc_ * sqrt( 3 ) * ( vol - vol_carb ) * rem
    )
    
    if ( ap < 3 ){

      cc <- cc * sqrt( ap ) / sqrt( 3 )

    }
    
    m_dust <- d * ( vol - vol_carb ) * ( 1 - rem ) # ton
    
    m_paste <- 0.25 * m_dust
    
    m_binder <- ( 1 / 1.43 ) * m_paste
    
    doc_dust <- 0.85
    
    dust <- doc_dust * Utcc * m_binder / ( mix$cement + mix$flyash + mix$slag )
    
    cc <- cc + dust
    
  }else{
    
    cc <- 0
    
  }
  
  
  return( data.frame(
    vol_carb = ( k_corr / 1000 ) * sqrt( ap ) * sa, # m3 # Note: used k_corr instead of k_doc
    u = ( k_doc / 1000 ) * Utcc * sqrt( ap ) * sa, # kg CO2
    cc = cc # kg CO2
  ) )
  
}


## calculate uptake per m2 per year for each state (row), element (col)

# u_m2_mortar <- array( rep( 0, length( states ) * ncol( sa ) ), dim = c( length( states ), ncol( sa ) ) ) # Note: not sure
# u_m2_cmu <- array( rep( 0, length( states ) * ncol( sa ) ), dim = c( length( states ), ncol( sa ) ) ) # Note: not sure
u_m2_lc <- array( rep( 0, length( states ) * ncol( sa ) ), dim = c( length( states ), ncol( sa ) ) )
u_m2_hc <- array( rep( 0, length( states ) * ncol( sa ) ), dim = c( length( states ), ncol( sa ) ) )

## calculate uptake per m3 per year for each state (row), element (col)

cc_m3_lc <- array( rep( 0, length( states ) * ncol( vol ) ), dim = c( length( states ), ncol( vol ) ) )
cc_m3_hc <- array( rep( 0, length( states ) * ncol( vol ) ), dim = c( length( states ), ncol( vol ) ) )

## calculate uptake per kg per year for each state (row), element (col)

cw_kg_lc <- array( rep( 0, length( states ) * 1 ), dim = c( length( states ), 1 ) )
cw_kg_hc <- array( rep( 0, length( states ) * 1 ), dim = c( length( states ), 1 ) )

sa_ <- 15000

vol_ <- sa_ * 0.20 # Note: thickness of typical wall

for ( i in seq( 1, length( states ) ) ){
  
  StateInfo <- AssignStateInfo( states[ i ] )
  
  for ( app in apps ){
    
    rainy <- c_up( app, exp_cnd = "Exposed to rain", sa = sa_, vol = vol_, RegionNRMCA = StateInfo$RegionNRMCA )
    
    nonrainy <- c_up( app, exp_cnd = "Sheltered from rain", sa = sa_, vol = vol_, RegionNRMCA = StateInfo$RegionNRMCA )
    
    finished <- c_up( app, exp_cnd = "Indoor (with cover)", sa = sa_, vol = vol_, RegionNRMCA = StateInfo$RegionNRMCA )
    
    unfinished <- c_up( app, exp_cnd = "Indoor (without cover)", sa = sa_, vol = vol_, RegionNRMCA = StateInfo$RegionNRMCA )
    
    inground <- c_up( app, exp_cnd = "In-ground", sa = sa_, vol = vol_, RegionNRMCA = StateInfo$RegionNRMCA )
    
    # if( app == "mortar" ){} # Note: not sure
    # 
    # if ( app == "cmu" ){} # Note: not sure
    
    if ( app == "lc" ){
      
      u_m2_lc[ i, grep( "roof", colnames( sa ) ) ] <- ( ( StateInfo$RainyDays / 365 ) * rainy$u + ( 1 - StateInfo$RainyDays / 365 ) * nonrainy$u ) / sa_
      
      u_m2_lc[ i, grep( "outdoor", colnames( sa ) ) ] <- ( ( StateInfo$RainyDays / 365 ) * rainy$u + ( 1 - StateInfo$RainyDays / 365 ) * nonrainy$u ) / sa_
      
      u_m2_lc[ i, grep( "cs_sa_outdoor", colnames( sa ) ) ] <- nonrainy$u / sa_
      
      u_m2_lc[ i, grep( "finished", colnames( sa ) ) ] <- finished$u / sa_
      
      u_m2_lc[ i, grep( "unfinished", colnames( sa ) ) ] <- unfinished$u / sa_
      
      u_m2_lc[ i, grep( "inground", colnames( sa ) ) ] <- inground$u / sa_
      
      cc_m3_lc[ i, ] <- finished$cc / vol_ # Note: had to pick one
      
      # cc_m3_lc[ i, ] <- finished$cc / sa_
      
      cw_kg_lc[ i, ] <- Utcc_( "lc", 1, 0, 0 ) * 0.8 * ( 1 / 3 )
      
    }
    
    if ( app == "hc" ){
      
      u_m2_hc[ i, grep( "roof", colnames( sa ) ) ] <- ( ( StateInfo$RainyDays / 365 ) * rainy$u + ( 1 - StateInfo$RainyDays / 365 ) * nonrainy$u ) / sa_
      
      u_m2_hc[ i, grep( "outdoor", colnames( sa ) ) ] <- ( ( StateInfo$RainyDays / 365 ) * rainy$u + ( 1 - StateInfo$RainyDays / 365 ) * nonrainy$u ) / sa_
      
      u_m2_hc[ i, grep( "cs_sa_outdoor", colnames( sa ) ) ] <- nonrainy$u / sa_
      
      u_m2_hc[ i, grep( "finished", colnames( sa ) ) ] <- finished$u / sa_
      
      u_m2_hc[ i, grep( "unfinished", colnames( sa ) ) ] <- unfinished$u / sa_
      
      u_m2_hc[ i, grep( "inground", colnames( sa ) ) ] <- inground$u / sa_
      
      cc_m3_hc[ i, ] <- finished$cc / vol_ # Note: had to pick one
      
      # cc_m3_hc[ i, ] <- finished$cc / sa_
      
      cw_kg_hc[ i, ] <- Utcc_( "hc", 1, 0, 0 ) * 0.8 * ( 1 / 3 )
      
    }
    
  }
  
}


## load pca data

# kg cement consumption per year

sheets <- excel_sheets( paste( pwd, "/Carbon Uptake/PCAStateAPUTimeSeries 46 Market - 20 years._REKxlsx.xlsx", sep = "" ) )

vp_start <- 1995

vp_end <- 2017

vp <- vp_end - vp_start + 1

valid_RES12 <- array( rep( 0, length( states ) * vp ), dim = c( length( states ), vp ) )
valid_RES3AB <- array( rep( 0, length( states ) * vp ), dim = c( length( states ), vp ) )
valid_RES3CDEF <- array( rep( 0, length( states ) * vp ), dim = c( length( states ), vp ) )
valid_COM <- array( rep( 0, length( states ) * vp ), dim = c( length( states ), vp ) )

# Note: updated to include additions

valid_RES_add <- array( rep( 0, length( states ) * vp ), dim = c( length( states ), vp ) )

# Note: updated to include M&R

valid_RES_rep <- array( rep( 0, length( states ) * vp ), dim = c( length( states ), vp ) )
valid_COM_rep <- array( rep( 0, length( states ) * vp ), dim = c( length( states ), vp ) )

for ( i in seq( 1, length( states ) ) ){
  
  StateInfo <- AssignStateInfo( states[ i ] )
  
  print( StateInfo$StateAbbrev )
  
  
  # kg per year
  
  # Note: this is cement production, not consumption
  
  if ( StateInfo$StateName %in% sheets ){
    
    cem_ <- read_excel( paste( pwd, "/Carbon Uptake/PCAStateAPUTimeSeries 46 Market - 20 years._REKxlsx.xlsx", sep = "" ), sheet = StateInfo$StateName )
    
    cem_RES12 <- 1000000 * colSums( cem_[ c( 11, 12 ) - 1, 3:25 ] )
    cem_RES3AB <- 1000000 * colSums( cem_[ c( 13 ) - 1, 3:25 ] )
    cem_RES3CDEF <- 1000000 * colSums( cem_[ c( 15 ) - 1, 3:25 ] )
    cem_COM <- 1000000 * colSums( cem_[ c( 16, 17, 18, 22, 23, 24, 25, 26, 27, 28, 32, 33, 34, 35, 36, 37, 40 ) - 1, 3:25 ] )
    
    # Note: updated to include additions
    
    cem_RES_add <- 1000000 * colSums( cem_[ c( 14 ) - 1, 3:25 ] )
    
    # Note: updated to include M&R
    
    cem_RES_rep <- 1000000 * colSums( cem_[ c( 19 ) - 1, 3:25 ] )
    cem_COM_rep <- 1000000 * colSums( cem_[ c( 29, 41 ) - 1, 3:25 ] )
    
    if ( StateInfo$StateName == "Illinois"){
      
      cem_ <- read_excel( paste( pwd, "/Carbon Uptake/PCAStateAPUTimeSeries 46 Market - 20 years._REKxlsx.xlsx", sep = "" ), sheet = sheets[ grep( "Chicago", sheets ) ] )
      
      cem_RES12 <- 1000000 * colSums( cem_[ c( 11, 12 ) - 1, 3:25 ] )
      cem_RES3AB <- 1000000 * colSums( cem_[ c( 13 ) - 1, 3:25 ] )
      cem_RES3CDEF <- 1000000 * colSums( cem_[ c( 15 ) - 1, 3:25 ] )
      cem_COM <- 1000000 * colSums( cem_[ c( 16, 17, 18, 22, 23, 24, 25, 26, 27, 28, 32, 33, 34, 35, 36, 37, 40 ) - 1, 3:25 ] )
      
      # Note: updated to include additions
      
      cem_RES_add <- 1000000 * colSums( cem_[ c( 14 ) - 1, 3:25 ] )
      
      # Note: updated to include M&R
      
      cem_RES_rep <- 1000000 * colSums( cem_[ c( 19 ) - 1, 3:25 ] )
      cem_COM_rep <- 1000000 * colSums( cem_[ c( 29, 41 ) - 1, 3:25 ] )
      
    }
    
    valid_RES12[ i, ] <- cem_RES12
    valid_RES3AB[ i, ] <- cem_RES3AB
    valid_RES3CDEF[ i, ] <- cem_RES3CDEF
    valid_COM[ i, ] <- cem_COM
    
    # Note: updated to include additions
    
    valid_RES_add[ i, ] <- cem_RES_add
    
    # Note: updated to include M&R
    
    valid_RES_rep[ i, ] <- cem_RES_rep
    valid_COM_rep[ i, ] <- cem_COM_rep
    
  }
  
  if ( StateInfo$StateName == "California" | StateInfo$StateName == "New York" | StateInfo$StateName == "Pennsylvania" | StateInfo$StateName == "Texas" ){
    
    for ( sht in seq( 1, length( grep( StateInfo$StateName, sheets ) ) ) ){
      
      if ( sht == 1 ){
        
        cem_ <- read_excel( paste( pwd, "/Carbon Uptake/PCAStateAPUTimeSeries 46 Market - 20 years._REKxlsx.xlsx", sep = "" ), sheet = sheets[ grep( StateInfo$StateName, sheets ) ][ sht ] )
        
        cem_RES12 <- 1000000 * colSums( cem_[ c( 11, 12 ) - 1, 3:25 ] )
        cem_RES3AB <- 1000000 * colSums( cem_[ c( 13 ) - 1, 3:25 ] )
        cem_RES3CDEF <- 1000000 * colSums( cem_[ c( 15 ) - 1, 3:25 ] )
        cem_COM <- 1000000 * colSums( cem_[ c( 16, 17, 18, 22, 23, 24, 25, 26, 27, 28, 32, 33, 34, 35, 36, 37, 40 ) - 1, 3:25 ] )
        
        # Note: updated to include additions
        
        cem_RES_add <- 1000000 * colSums( cem_[ c( 14 ) - 1, 3:25 ] )
        
        # Note: updated to include M&R
        
        cem_RES_rep <- 1000000 * colSums( cem_[ c( 19 ) - 1, 3:25 ] )
        cem_COM_rep <- 1000000 * colSums( cem_[ c( 29, 41 ) - 1, 3:25 ] )
        
      }else{
        
        cem_ <- read_excel( paste( pwd, "/Carbon Uptake/PCAStateAPUTimeSeries 46 Market - 20 years._REKxlsx.xlsx", sep = "" ), sheet = sheets[ grep( StateInfo$StateName, sheets ) ][ sht ] )
        
        cem_RES12 <- 1000000 * colSums( cem_[ c( 11, 12 ) - 1, 3:25 ] )
        cem_RES3AB <- 1000000 * colSums( cem_[ c( 13 ) - 1, 3:25 ] )
        cem_RES3CDEF <- 1000000 * colSums( cem_[ c( 15 ) - 1, 3:25 ] )
        cem_COM <- 1000000 * colSums( cem_[ c( 16, 17, 18, 22, 23, 24, 25, 26, 27, 28, 32, 33, 34, 35, 36, 37, 40 ) - 1, 3:25 ] )
        
        # Note: updated to include additions
        
        cem_RES_add <- 1000000 * colSums( cem_[ c( 14 ) - 1, 3:25 ] )
        
        # Note: updated to include M&R
        
        cem_RES_rep <- 1000000 * colSums( cem_[ c( 19 ) - 1, 3:25 ] )
        cem_COM_rep <- 1000000 * colSums( cem_[ c( 29, 41 ) - 1, 3:25 ] )
        
      }
      
    }
    
    valid_RES12[ i, ] <- cem_RES12
    valid_RES3AB[ i, ] <- cem_RES3AB
    valid_RES3CDEF[ i, ] <- cem_RES3CDEF
    valid_COM[ i, ] <- cem_COM
    
    # Note: updated to include additions
    
    valid_RES_add[ i, ] <- cem_RES_add
    
    # Note: updated to include M&R
    
    valid_RES_rep[ i, ] <- cem_RES_rep
    valid_COM_rep[ i, ] <- cem_COM_rep
    
  }
  
}


## load stock volume for approaches 0-2

# m3 per year

# approaches <- c( "previous", "approach0", "approach1", "approach2" )

load( file = paste( pwd, "/Carbon Uptake/vol_yb_RES12.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/vol_yb_RES3AB.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/vol_yb_RES3CDEF.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/vol_yb_COM.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/vol_yb_cmuRES12.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/vol_yb_cmuRES3AB.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/vol_yb_cmuRES3CDEF.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/vol_yb_cmuCOM.Rda",sep = "" ) )

vol_yb_RES12[ is.na( vol_yb_RES12 ) | !is.finite( vol_yb_RES12 ) ] <- 0
vol_yb_RES3AB[ is.na( vol_yb_RES3AB ) | !is.finite( vol_yb_RES3AB ) ] <- 0
vol_yb_RES3CDEF[ is.na( vol_yb_RES3CDEF ) | !is.finite( vol_yb_RES3CDEF ) ] <- 0
vol_yb_COM[ is.na( vol_yb_COM ) | !is.finite( vol_yb_COM ) ] <- 0
vol_yb_cmuRES12[ is.na( vol_yb_cmuRES12 ) | !is.finite( vol_yb_cmuRES12 ) ] <- 0
vol_yb_cmuRES3AB[ is.na( vol_yb_cmuRES3AB ) | !is.finite( vol_yb_cmuRES3AB ) ] <- 0
vol_yb_cmuRES3CDEF[ is.na( vol_yb_cmuRES3CDEF ) | !is.finite( vol_yb_cmuRES3CDEF ) ] <- 0
vol_yb_cmuCOM[ is.na( vol_yb_cmuCOM ) | !is.finite( vol_yb_cmuCOM ) ] <- 0

vol_yb_RES12_lc <- vol_yb_RES12
vol_yb_RES3AB_lc <- vol_yb_RES3AB
vol_yb_RES3CDEF_lc <- vol_yb_RES3CDEF
vol_yb_COM_lc <- vol_yb_COM
vol_yb_cmuRES12_lc <- vol_yb_cmuRES12
vol_yb_cmuRES3AB_lc <- vol_yb_cmuRES3AB
vol_yb_cmuRES3CDEF_lc <- vol_yb_cmuRES3CDEF
vol_yb_cmuCOM_lc <- vol_yb_cmuCOM

vol_yb_RES12_lc[ , !( lcvol_RES_low ), ] <- 0
vol_yb_RES3AB_lc[ , !( lcvol_RES_low ), ] <- 0
vol_yb_RES3CDEF_lc[ , !( lcvol_RES_hi ), ] <- 0
vol_yb_COM_lc[ , !( lcvol_COM ), ] <- 0
vol_yb_cmuRES12_lc[ , !( lcvol_cmuRES_low ), ] <- 0
vol_yb_cmuRES3AB_lc[ , !( lcvol_cmuRES_low ), ] <- 0
vol_yb_cmuRES3CDEF_lc[ , !( lcvol_cmuRES_hi ), ] <- 0
vol_yb_cmuCOM_lc[ , !( lcvol_cmuCOM ), ] <- 0

vol_yb_RES12_hc <- vol_yb_RES12
vol_yb_RES3AB_hc <- vol_yb_RES3AB
vol_yb_RES3CDEF_hc <- vol_yb_RES3CDEF
vol_yb_COM_hc <- vol_yb_COM
vol_yb_cmuRES12_hc <- vol_yb_cmuRES12
vol_yb_cmuRES3AB_hc <- vol_yb_cmuRES3AB
vol_yb_cmuRES3CDEF_hc <- vol_yb_cmuRES3CDEF
vol_yb_cmuCOM_hc <- vol_yb_cmuCOM

vol_yb_RES12_hc[ , lcvol_RES_low, ] <- 0
vol_yb_RES3AB_hc[ , lcvol_RES_low, ] <- 0
vol_yb_RES3CDEF_hc[ , lcvol_RES_hi, ] <- 0
vol_yb_COM_hc[ , lcvol_COM, ] <- 0
vol_yb_cmuRES12_hc[ , lcvol_cmuRES_low, ] <- 0
vol_yb_cmuRES3AB_hc[ , lcvol_cmuRES_low, ] <- 0
vol_yb_cmuRES3CDEF_hc[ , lcvol_cmuRES_hi, ] <- 0
vol_yb_cmuCOM_hc[ , lcvol_cmuCOM, ] <- 0

vol_RES12_lc <- list()
vol_RES3AB_lc <- list()
vol_RES3CDEF_lc <- list()
vol_COM_lc <- list()
vol_cmuRES12_lc <- list()
vol_cmuRES3AB_lc <- list()
vol_cmuRES3CDEF_lc <- list()
vol_cmuCOM_lc <- list()

vol_RES12_hc <- list()
vol_RES3AB_hc <- list()
vol_RES3CDEF_hc <- list()
vol_COM_hc <- list()
vol_cmuRES12_hc <- list()
vol_cmuRES3AB_hc <- list()
vol_cmuRES3CDEF_hc <- list()
vol_cmuCOM_hc <- list()

for ( ay in seq( 1, ap ) ){
  
  if ( ay == 1 ){
    
    tot_RES12_lc <- rowSums( vol_yb_RES12_lc[ , , ay ] )
    tot_RES3AB_lc <- rowSums( vol_yb_RES3AB_lc[ , , ay ] )
    tot_RES3CDEF_lc <- rowSums( vol_yb_RES3CDEF_lc[ , , ay ] )
    tot_COM_lc <- rowSums( vol_yb_COM_lc[ , , ay ] )
    tot_cmuRES12_lc <- rowSums( vol_yb_cmuRES12_lc[ , , ay ] )
    tot_cmuRES3AB_lc <- rowSums( vol_yb_cmuRES3AB_lc[ , , ay ] )
    tot_cmuRES3CDEF_lc <- rowSums( vol_yb_cmuRES3CDEF_lc[ , , ay ] )
    tot_cmuCOM_lc <- rowSums( vol_yb_cmuCOM_lc[ , , ay ] )
    
    tot_RES12_hc <- rowSums( vol_yb_RES12_hc[ , , ay ] )
    tot_RES3AB_hc <- rowSums( vol_yb_RES3AB_hc[ , , ay ] )
    tot_RES3CDEF_hc <- rowSums( vol_yb_RES3CDEF_hc[ , , ay ] )
    tot_COM_hc <- rowSums( vol_yb_COM_hc[ , , ay ] )
    tot_cmuRES12_hc <- rowSums( vol_yb_cmuRES12_hc[ , , ay ] )
    tot_cmuRES3AB_hc <- rowSums( vol_yb_cmuRES3AB_hc[ , , ay ] )
    tot_cmuRES3CDEF_hc <- rowSums( vol_yb_cmuRES3CDEF_hc[ , , ay ] )
    tot_cmuCOM_hc <- rowSums( vol_yb_cmuCOM_hc[ , , ay ] )
    
  }else{
    
    tot_RES12_lc <- cbind( tot_RES12_lc, rowSums( vol_yb_RES12_lc[ , , ay ] ) )
    tot_RES3AB_lc <- cbind( tot_RES3AB_lc, rowSums( vol_yb_RES3AB_lc[ , , ay ] ) )
    tot_RES3CDEF_lc <- cbind( tot_RES3CDEF_lc, rowSums( vol_yb_RES3CDEF_lc[ , , ay ] ) )
    tot_COM_lc <- cbind( tot_COM_lc, rowSums( vol_yb_COM_lc[ , , ay ] ) )
    tot_cmuRES12_lc <- cbind( tot_cmuRES12_lc, rowSums( vol_yb_cmuRES12_lc[ , , ay ] ) )
    tot_cmuRES3AB_lc <- cbind( tot_cmuRES3AB_lc, rowSums( vol_yb_cmuRES3AB_lc[ , , ay ] ) )
    tot_cmuRES3CDEF_lc <- cbind( tot_cmuRES3CDEF_lc, rowSums( vol_yb_cmuRES3CDEF_lc[ , , ay ] ) )
    tot_cmuCOM_lc <- cbind( tot_cmuCOM_lc, rowSums( vol_yb_cmuCOM_lc[ , , ay ] ) )
    
    tot_RES12_hc <- cbind( tot_RES12_hc, rowSums( vol_yb_RES12_hc[ , , ay ] ) )
    tot_RES3AB_hc <- cbind( tot_RES3AB_hc, rowSums( vol_yb_RES3AB_hc[ , , ay ] ) )
    tot_RES3CDEF_hc <- cbind( tot_RES3CDEF_hc, rowSums( vol_yb_RES3CDEF_hc[ , , ay ] ) )
    tot_COM_hc <- cbind( tot_COM_hc, rowSums( vol_yb_COM_hc[ , , ay ] ) )
    tot_cmuRES12_hc <- cbind( tot_cmuRES12_hc, rowSums( vol_yb_cmuRES12_hc[ , , ay ] ) )
    tot_cmuRES3AB_hc <- cbind( tot_cmuRES3AB_hc, rowSums( vol_yb_cmuRES3AB_hc[ , , ay ] ) )
    tot_cmuRES3CDEF_hc <- cbind( tot_cmuRES3CDEF_hc, rowSums( vol_yb_cmuRES3CDEF_hc[ , , ay ] ) )
    tot_cmuCOM_hc <- cbind( tot_cmuCOM_hc, rowSums( vol_yb_cmuCOM_hc[ , , ay ] ) )
    
  }
  
}

vol_RES12_lc <- tot_RES12_lc
vol_RES3AB_lc <- tot_RES3AB_lc
vol_RES3CDEF_lc <- tot_RES3CDEF_lc
vol_COM_lc <- tot_COM_lc
vol_cmuRES12_lc <- tot_cmuRES12_lc
vol_cmuRES3AB_lc <- tot_cmuRES3AB_lc
vol_cmuRES3CDEF_lc <- tot_cmuRES3CDEF_lc
vol_cmuCOM_lc <- tot_cmuCOM_lc

vol_RES12_hc <- tot_RES12_hc
vol_RES3AB_hc <- tot_RES3AB_hc
vol_RES3CDEF_hc <- tot_RES3CDEF_hc
vol_COM_hc <- tot_COM_hc
vol_cmuRES12_hc <- tot_cmuRES12_hc
vol_cmuRES3AB_hc <- tot_cmuRES3AB_hc
vol_cmuRES3CDEF_hc <- tot_cmuRES3CDEF_hc
vol_cmuCOM_hc <- tot_cmuCOM_hc

# Note: updated to include additions

vol_yb_RES_add_lc <- vol_yb_RES12_lc + vol_yb_RES3AB_lc
vol_yb_cmuRES_add_lc <- vol_yb_cmuRES12_lc + vol_yb_cmuRES3AB_lc
vol_yb_RES_add_hc <- vol_yb_RES12_hc + vol_yb_RES3AB_hc
vol_yb_cmuRES_add_hc <- vol_yb_cmuRES12_hc + vol_yb_cmuRES3AB_hc

vol_RES_add_lc <- tot_RES12_lc + tot_RES3AB_lc
vol_cmuRES_add_lc <- tot_cmuRES12_lc + tot_cmuRES3AB_lc
vol_RES_add_hc <- tot_RES12_hc + tot_RES3AB_hc
vol_cmuRES_add_hc <- tot_cmuRES12_hc + tot_cmuRES3AB_hc


## load stock surface area for approaches 0-2

# m2 per year

load( file = paste( pwd, "/Carbon Uptake/sa_yb_RES12.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/sa_yb_RES3AB.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/sa_yb_RES3CDEF.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/sa_yb_COM.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/sa_yb_cmuRES12.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/sa_yb_cmuRES3AB.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/sa_yb_cmuRES3CDEF.Rda",sep = "" ) )
load( file = paste( pwd, "/Carbon Uptake/sa_yb_cmuCOM.Rda",sep = "" ) )

sa_yb_RES12[ is.na( sa_yb_RES12 ) | !is.finite( sa_yb_RES12 ) ] <- 0
sa_yb_RES3AB[ is.na( sa_yb_RES3AB ) | !is.finite( sa_yb_RES3AB ) ] <- 0
sa_yb_RES3CDEF[ is.na( sa_yb_RES3CDEF ) | !is.finite( sa_yb_RES3CDEF ) ] <- 0
sa_yb_COM[ is.na( sa_yb_COM ) | !is.finite( sa_yb_COM ) ] <- 0
sa_yb_cmuRES12[ is.na( sa_yb_cmuRES12 ) | !is.finite( sa_yb_cmuRES12 ) ] <- 0
sa_yb_cmuRES3AB[ is.na( sa_yb_cmuRES3AB ) | !is.finite( sa_yb_cmuRES3AB ) ] <- 0
sa_yb_cmuRES3CDEF[ is.na( sa_yb_cmuRES3CDEF ) | !is.finite( sa_yb_cmuRES3CDEF ) ] <- 0
sa_yb_cmuCOM[ is.na( sa_yb_cmuCOM ) | !is.finite( sa_yb_cmuCOM ) ] <- 0

sa_yb_RES12_lc <- sa_yb_RES12
sa_yb_RES3AB_lc <- sa_yb_RES3AB
sa_yb_RES3CDEF_lc <- sa_yb_RES3CDEF
sa_yb_COM_lc <- sa_yb_COM
sa_yb_cmuRES12_lc <- sa_yb_cmuRES12
sa_yb_cmuRES3AB_lc <- sa_yb_cmuRES3AB
sa_yb_cmuRES3CDEF_lc <- sa_yb_cmuRES3CDEF
sa_yb_cmuCOM_lc <- sa_yb_cmuCOM

sa_yb_RES12_lc[ , !( lcsa_RES_low ), ] <- 0
sa_yb_RES3AB_lc[ , !( lcsa_RES_low ), ] <- 0
sa_yb_RES3CDEF_lc[ , !( lcsa_RES_hi ), ] <- 0
sa_yb_COM_lc[ , !( lcsa_COM ), ] <- 0
sa_yb_cmuRES12_lc[ , !( lcsa_cmuRES_low ), ] <- 0
sa_yb_cmuRES3AB_lc[ , !( lcsa_cmuRES_low ), ] <- 0
sa_yb_cmuRES3CDEF_lc[ , !( lcsa_cmuRES_hi ), ] <- 0
sa_yb_cmuCOM_lc[ , !( lcsa_cmuCOM ), ] <- 0

sa_yb_RES12_hc <- sa_yb_RES12
sa_yb_RES3AB_hc <- sa_yb_RES3AB
sa_yb_RES3CDEF_hc <- sa_yb_RES3CDEF
sa_yb_COM_hc <- sa_yb_COM
sa_yb_cmuRES12_hc <- sa_yb_cmuRES12
sa_yb_cmuRES3AB_hc <- sa_yb_cmuRES3AB
sa_yb_cmuRES3CDEF_hc <- sa_yb_cmuRES3CDEF
sa_yb_cmuCOM_hc <- sa_yb_cmuCOM

sa_yb_RES12_hc[ , lcsa_RES_low, ] <- 0
sa_yb_RES3AB_hc[ , lcsa_RES_low, ] <- 0
sa_yb_RES3CDEF_hc[ , lcsa_RES_hi, ] <- 0
sa_yb_COM_hc[ , lcsa_COM, ] <- 0
sa_yb_cmuRES12_hc[ , lcsa_cmuRES_low, ] <- 0
sa_yb_cmuRES3AB_hc[ , lcsa_cmuRES_low, ] <- 0
sa_yb_cmuRES3CDEF_hc[ , lcsa_cmuRES_hi, ] <- 0
sa_yb_cmuCOM_hc[ , lcsa_cmuCOM, ] <- 0

sa_RES12_lc <- list()
sa_RES3AB_lc <- list()
sa_RES3CDEF_lc <- list()
sa_COM_lc <- list()
sa_cmuRES12_lc <- list()
sa_cmuRES3AB_lc <- list()
sa_cmuRES3CDEF_lc <- list()
sa_cmuCOM_lc <- list()

sa_RES12_hc <- list()
sa_RES3AB_hc <- list()
sa_RES3CDEF_hc <- list()
sa_COM_hc <- list()
sa_cmuRES12_hc <- list()
sa_cmuRES3AB_hc <- list()
sa_cmuRES3CDEF_hc <- list()
sa_cmuCOM_hc <- list()

# Note: updated to include M&R

temp_sa_RES_rep_lc <- list()
temp_sa_COM_rep_lc <- list()

temp_sa_RES_rep_hc <- list()
temp_sa_COM_rep_hc <- list()

for ( ay in seq( 1, ap ) ){
  
  if ( ay == 1 ){
    
    tot_RES12_lc <- rowSums( sa_yb_RES12_lc[ , , ay ] )
    tot_RES3AB_lc <- rowSums( sa_yb_RES3AB_lc[ , , ay ] )
    tot_RES3CDEF_lc <- rowSums( sa_yb_RES3CDEF_lc[ , , ay ] )
    tot_COM_lc <- rowSums( sa_yb_COM_lc[ , , ay ] )
    tot_cmuRES12_lc <- rowSums( sa_yb_cmuRES12_lc[ , , ay ] )
    tot_cmuRES3AB_lc <- rowSums( sa_yb_cmuRES3AB_lc[ , , ay ] )
    tot_cmuRES3CDEF_lc <- rowSums( sa_yb_cmuRES3CDEF_lc[ , , ay ] )
    tot_cmuCOM_lc <- rowSums( sa_yb_cmuCOM_lc[ , , ay ] )
    
    tot_RES12_hc <- rowSums( sa_yb_RES12_hc[ , , ay ] )
    tot_RES3AB_hc <- rowSums( sa_yb_RES3AB_hc[ , , ay ] )
    tot_RES3CDEF_hc <- rowSums( sa_yb_RES3CDEF_hc[ , , ay ] )
    tot_COM_hc <- rowSums( sa_yb_COM_hc[ , , ay ] )
    tot_cmuRES12_hc <- rowSums( sa_yb_cmuRES12_hc[ , , ay ] )
    tot_cmuRES3AB_hc <- rowSums( sa_yb_cmuRES3AB_hc[ , , ay ] )
    tot_cmuRES3CDEF_hc <- rowSums( sa_yb_cmuRES3CDEF_hc[ , , ay ] )
    tot_cmuCOM_hc <- rowSums( sa_yb_cmuCOM_hc[ , , ay ] )
    
    # Note: updated to include M&R
    
    temp_sa_RES_rep_lc <- sa_yb_RES12_lc[ , , ay ] + sa_yb_RES3AB_lc[ , , ay ] + sa_yb_RES3CDEF_lc[ , , ay ]
    temp_sa_COM_rep_lc <- sa_yb_COM_lc[ , , ay ]
    
    temp_sa_RES_rep_hc <- sa_yb_RES12_hc[ , , ay ] + sa_yb_RES3AB_hc[ , , ay ] + sa_yb_RES3CDEF_hc[ , , ay ]
    temp_sa_COM_rep_hc <- sa_yb_COM_hc[ , , ay ]
    
  }else{
    
    tot_RES12_lc <- cbind( tot_RES12_lc, rowSums( sa_yb_RES12_lc[ , , ay ] ) )
    tot_RES3AB_lc <- cbind( tot_RES3AB_lc, rowSums( sa_yb_RES3AB_lc[ , , ay ] ) )
    tot_RES3CDEF_lc <- cbind( tot_RES3CDEF_lc, rowSums( sa_yb_RES3CDEF_lc[ , , ay ] ) )
    tot_COM_lc <- cbind( tot_COM_lc, rowSums( sa_yb_COM_lc[ , , ay ] ) )
    tot_cmuRES12_lc <- cbind( tot_cmuRES12_lc, rowSums( sa_yb_cmuRES12_lc[ , , ay ] ) )
    tot_cmuRES3AB_lc <- cbind( tot_cmuRES3AB_lc, rowSums( sa_yb_cmuRES3AB_lc[ , , ay ] ) )
    tot_cmuRES3CDEF_lc <- cbind( tot_cmuRES3CDEF_lc, rowSums( sa_yb_cmuRES3CDEF_lc[ , , ay ] ) )
    tot_cmuCOM_lc <- cbind( tot_cmuCOM_lc, rowSums( sa_yb_cmuCOM_lc[ , , ay ] ) )
    
    tot_RES12_hc <- cbind( tot_RES12_hc, rowSums( sa_yb_RES12_hc[ , , ay ] ) )
    tot_RES3AB_hc <- cbind( tot_RES3AB_hc, rowSums( sa_yb_RES3AB_hc[ , , ay ] ) )
    tot_RES3CDEF_hc <- cbind( tot_RES3CDEF_hc, rowSums( sa_yb_RES3CDEF_hc[ , , ay ] ) )
    tot_COM_hc <- cbind( tot_COM_hc, rowSums( sa_yb_COM_hc[ , , ay ] ) )
    tot_cmuRES12_hc <- cbind( tot_cmuRES12_hc, rowSums( sa_yb_cmuRES12_hc[ , , ay ] ) )
    tot_cmuRES3AB_hc <- cbind( tot_cmuRES3AB_hc, rowSums( sa_yb_cmuRES3AB_hc[ , , ay ] ) )
    tot_cmuRES3CDEF_hc <- cbind( tot_cmuRES3CDEF_hc, rowSums( sa_yb_cmuRES3CDEF_hc[ , , ay ] ) )
    tot_cmuCOM_hc <- cbind( tot_cmuCOM_hc, rowSums( sa_yb_cmuCOM_hc[ , , ay ] ) )
    
    # Note: updated to include M&R
    
    temp_sa_RES_rep_lc <- temp_sa_RES_rep_lc + sa_yb_RES12_lc[ , , ay ] + sa_yb_RES3AB_lc[ , , ay ] + sa_yb_RES3CDEF_lc[ , , ay ]
    temp_sa_COM_rep_lc <- temp_sa_COM_rep_lc + sa_yb_COM_lc[ , , ay ]
    
    temp_sa_RES_rep_hc <- temp_sa_RES_rep_hc + sa_yb_RES12_hc[ , , ay ] + sa_yb_RES3AB_hc[ , , ay ] + sa_yb_RES3CDEF_hc[ , , ay ]
    temp_sa_COM_rep_hc <- temp_sa_COM_rep_hc + sa_yb_COM_hc[ , , ay ]
    
  }
  
}

sa_RES12_lc <- tot_RES12_lc
sa_RES3AB_lc <- tot_RES3AB_lc
sa_RES3CDEF_lc <- tot_RES3CDEF_lc
sa_COM_lc <- tot_COM_lc
sa_cmuRES12_lc <- tot_cmuRES12_lc
sa_cmuRES3AB_lc <- tot_cmuRES3AB_lc
sa_cmuRES3CDEF_lc <- tot_cmuRES3CDEF_lc
sa_cmuCOM_lc <- tot_cmuCOM_lc

sa_RES12_hc <- tot_RES12_hc
sa_RES3AB_hc <- tot_RES3AB_hc
sa_RES3CDEF_hc <- tot_RES3CDEF_hc
sa_COM_hc <- tot_COM_hc
sa_cmuRES12_hc <- tot_cmuRES12_hc
sa_cmuRES3AB_hc <- tot_cmuRES3AB_hc
sa_cmuRES3CDEF_hc <- tot_cmuRES3CDEF_hc
sa_cmuCOM_hc <- tot_cmuCOM_hc

# Note: updated to include additions

sa_yb_RES_add_lc <- sa_yb_RES12_lc + sa_yb_RES3AB_lc
sa_yb_cmuRES_add_lc <- sa_yb_cmuRES12_lc + sa_yb_cmuRES3AB_lc
sa_yb_RES_add_hc <- sa_yb_RES12_hc + sa_yb_RES3AB_hc
sa_yb_cmuRES_add_hc <- sa_yb_cmuRES12_hc + sa_yb_cmuRES3AB_hc

sa_RES_add_lc <- tot_RES12_lc + tot_RES3AB_lc
sa_cmuRES_add_lc <- tot_cmuRES12_lc + tot_cmuRES3AB_lc
sa_RES_add_hc <- tot_RES12_hc + tot_RES3AB_hc
sa_cmuRES_add_hc <- tot_cmuRES12_hc + tot_cmuRES3AB_hc

# Note: updated to include M&R

temp_sa_RES_rep_lc[ , grepl( "inground", colnames( sa ) ) ] <- 0 # Note: assumed no inground surfaces
temp_sa_COM_rep_lc[ , grepl( "inground", colnames( sa ) ) ] <- 0 # Note: assumed no inground surfaces

temp_sa_RES_rep_hc[ , grepl( "inground", colnames( sa ) ) ] <- 0 # Note: assumed no inground surfaces
temp_sa_COM_rep_hc[ , grepl( "inground", colnames( sa ) ) ] <- 0 # Note: assumed no inground surfaces

el_ <- c( "c_", "b_", "s_", "sf_", "f_", "cs_", "pm_", "pc_", "bw_", "ew_", "iw_", "sw_" )

temp_vol_RES_rep_lc <- cbind(
  rowSums( temp_sa_RES_rep_lc[ , grepl( el_[ 1 ], colnames( sa ) ) & !grepl( "pc_", colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_lc[ , grepl( el_[ 2 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_lc[ , grepl( el_[ 3 ], colnames( sa ) ) & !grepl( "cs_", colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_lc[ , grepl( el_[ 4 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_lc[ , grepl( el_[ 5 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_lc[ , grepl( el_[ 6 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_lc[ , grepl( el_[ 7 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_lc[ , grepl( el_[ 8 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_lc[ , grepl( el_[ 9 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_lc[ , grepl( el_[ 10 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_lc[ , grepl( el_[ 11 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_lc[ , grepl( el_[ 12 ], colnames( sa ) ) ] ) * 2 * 0.0254 # in to m # Note: assumed only concrete cover
)

temp_vol_COM_rep_lc <- cbind(
  rowSums( temp_sa_COM_rep_lc[ , grepl( el_[ 1 ], colnames( sa ) ) & !grepl( "pc_", colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_lc[ , grepl( el_[ 2 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_lc[ , grepl( el_[ 3 ], colnames( sa ) ) & !grepl( "cs_", colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_lc[ , grepl( el_[ 4 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_lc[ , grepl( el_[ 5 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_lc[ , grepl( el_[ 6 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_lc[ , grepl( el_[ 7 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_lc[ , grepl( el_[ 8 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_lc[ , grepl( el_[ 9 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_lc[ , grepl( el_[ 10 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_lc[ , grepl( el_[ 11 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_lc[ , grepl( el_[ 12 ], colnames( sa ) ) ] ) * 2 * 0.0254 # in to m # Note: assumed only concrete cover
)

temp_vol_RES_rep_hc <- cbind(
  rowSums( temp_sa_RES_rep_hc[ , grepl( el_[ 1 ], colnames( sa ) ) & !grepl( "pc_", colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_hc[ , grepl( el_[ 2 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_hc[ , grepl( el_[ 3 ], colnames( sa ) ) & !grepl( "cs_", colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_hc[ , grepl( el_[ 4 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_hc[ , grepl( el_[ 5 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_hc[ , grepl( el_[ 6 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_hc[ , grepl( el_[ 7 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_hc[ , grepl( el_[ 8 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_hc[ , grepl( el_[ 9 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_hc[ , grepl( el_[ 10 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_hc[ , grepl( el_[ 11 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_RES_rep_hc[ , grepl( el_[ 12 ], colnames( sa ) ) ] ) * 2 * 0.0254 # in to m # Note: assumed only concrete cover
)

temp_vol_COM_rep_hc <- cbind(
  rowSums( temp_sa_COM_rep_hc[ , grepl( el_[ 1 ], colnames( sa ) ) & !grepl( "pc_", colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_hc[ , grepl( el_[ 2 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_hc[ , grepl( el_[ 3 ], colnames( sa ) ) & !grepl( "cs_", colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_hc[ , grepl( el_[ 4 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_hc[ , grepl( el_[ 5 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_hc[ , grepl( el_[ 6 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_hc[ , grepl( el_[ 7 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_hc[ , grepl( el_[ 8 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_hc[ , grepl( el_[ 9 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_hc[ , grepl( el_[ 10 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_hc[ , grepl( el_[ 11 ], colnames( sa ) ) ] ) * 2 * 0.0254, # in to m # Note: assumed only concrete cover
  rowSums( temp_sa_COM_rep_hc[ , grepl( el_[ 12 ], colnames( sa ) ) ] ) * 2 * 0.0254 # in to m # Note: assumed only concrete cover
)

sa_yb_RES_rep_lc <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
sa_yb_COM_rep_lc <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )

sa_yb_RES_rep_hc <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
sa_yb_COM_rep_hc <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )

vol_yb_RES_rep_lc <- array( rep( 0, length( states ) * ncol( vol ) * ap ), dim = c( length( states ), ncol( vol ), ap ) )
vol_yb_COM_rep_lc <- array( rep( 0, length( states ) * ncol( vol ) * ap ), dim = c( length( states ), ncol( vol ), ap ) )

vol_yb_RES_rep_hc <- array( rep( 0, length( states ) * ncol( vol ) * ap ), dim = c( length( states ), ncol( vol ), ap ) )
vol_yb_COM_rep_hc <- array( rep( 0, length( states ) * ncol( vol ) * ap ), dim = c( length( states ), ncol( vol ), ap ) )

sa_RES_rep_lc <- list()
sa_COM_rep_lc <- list()

sa_RES_rep_hc <- list()
sa_COM_rep_hc <- list()

vol_RES_rep_lc <- list()
vol_COM_rep_lc <- list()

vol_RES_rep_hc <- list()
vol_COM_rep_hc <- list()

for ( ay in seq( 1, ap ) ){
  
  sa_yb_RES_rep_lc[ , , ay ] <- temp_sa_RES_rep_lc
  sa_yb_COM_rep_lc[ , , ay ] <- temp_sa_COM_rep_lc
  
  sa_yb_RES_rep_hc[ , , ay ] <- temp_sa_RES_rep_hc
  sa_yb_COM_rep_hc[ , , ay ] <- temp_sa_COM_rep_hc
  
  vol_yb_RES_rep_lc[ , , ay ] <- temp_vol_RES_rep_lc
  vol_yb_COM_rep_lc[ , , ay ] <- temp_vol_COM_rep_lc
  
  vol_yb_RES_rep_hc[ , , ay ] <- temp_vol_RES_rep_hc
  vol_yb_COM_rep_hc[ , , ay ] <- temp_vol_COM_rep_hc
  
  if ( ay == 1 ){
    
    tot_sa_RES_rep_lc <- rowSums( temp_sa_RES_rep_lc )
    tot_sa_COM_rep_lc <- rowSums( temp_sa_COM_rep_lc )
    
    tot_sa_RES_rep_hc <- rowSums( temp_sa_RES_rep_hc )
    tot_sa_COM_rep_hc <- rowSums( temp_sa_COM_rep_hc )
    
    tot_vol_RES_rep_lc <- rowSums( temp_vol_RES_rep_lc )
    tot_vol_COM_rep_lc <- rowSums( temp_vol_COM_rep_lc )
    
    tot_vol_RES_rep_hc <- rowSums( temp_vol_RES_rep_hc )
    tot_vol_COM_rep_hc <- rowSums( temp_vol_COM_rep_hc )
    
  }else{
    
    tot_sa_RES_rep_lc <- cbind( tot_sa_RES_rep_lc, rowSums( temp_sa_RES_rep_lc ) )
    tot_sa_COM_rep_lc <- cbind( tot_sa_COM_rep_lc, rowSums( temp_sa_COM_rep_lc ) )
    
    tot_sa_RES_rep_hc <- cbind( tot_sa_RES_rep_hc, rowSums( temp_sa_RES_rep_hc ) )
    tot_sa_COM_rep_hc <- cbind( tot_sa_COM_rep_hc, rowSums( temp_sa_COM_rep_hc ) )
    
    tot_vol_RES_rep_lc <- cbind( tot_vol_RES_rep_lc, rowSums( temp_vol_RES_rep_lc ) )
    tot_vol_COM_rep_lc <- cbind( tot_vol_COM_rep_lc, rowSums( temp_vol_COM_rep_lc ) )
    
    tot_vol_RES_rep_hc <- cbind( tot_vol_RES_rep_hc, rowSums( temp_vol_RES_rep_hc ) )
    tot_vol_COM_rep_hc <- cbind( tot_vol_COM_rep_hc, rowSums( temp_vol_COM_rep_hc ) )
    
  }
  
}

sa_RES_rep_lc <- tot_sa_RES_rep_lc
sa_COM_rep_lc <- tot_sa_COM_rep_lc

sa_RES_rep_hc <- tot_sa_RES_rep_hc
sa_COM_rep_hc <- tot_sa_COM_rep_hc

vol_RES_rep_lc <- tot_vol_RES_rep_lc
vol_COM_rep_lc <- tot_vol_COM_rep_lc

vol_RES_rep_hc <- tot_vol_RES_rep_hc
vol_COM_rep_hc <- tot_vol_COM_rep_hc

sa_yb_RES_rep <- sa_yb_RES_rep_lc + sa_yb_RES_rep_hc # Note: assumed all concrete surfaces (lc and hc)
sa_yb_COM_rep <- sa_yb_COM_rep_lc + sa_yb_COM_rep_hc # Note: assumed all concrete surfaces (lc and hc)

sa_RES_rep <- sa_RES_rep_lc + sa_RES_rep_hc # Note: assumed all concrete surfaces (lc and hc)
sa_COM_rep <- sa_COM_rep_lc + sa_COM_rep_hc # Note: assumed all concrete surfaces (lc and hc)

vol_yb_RES_rep <- vol_yb_RES_rep_lc + vol_yb_RES_rep_hc # Note: assumed all concrete surfaces (lc and hc)
vol_yb_COM_rep <- vol_yb_COM_rep_lc + vol_yb_COM_rep_hc # Note: assumed all concrete surfaces (lc and hc)

vol_RES_rep <- vol_RES_rep_lc + vol_RES_rep_hc # Note: assumed all concrete surfaces (lc and hc)
vol_COM_rep <- vol_COM_rep_lc + vol_COM_rep_hc # Note: assumed all concrete surfaces (lc and hc)


## correct stock volume and surface area

cem_perc <- read_excel( paste( pwd, "/Carbon Uptake/Uptake.xlsx", sep = "" ), sheet = "Sheet1" )
cem_perc <- array( unlist( cem_perc[ 2, ] ) )
cem_perc_ <- cem_perc[ ( vp_start - ap_start + 1 ):( vp_end - ap_start + 1 ) ]

cem_RES12_ <- colSums( valid_RES12 ) / cem_perc_
cem_RES12_corr <- mean( cem_RES12_[ 1:5 ] )
cem_RES12 <- cem_RES12_corr * cem_perc[ 1:( vp_start - ap_start ) ]

cem_RES3AB_ <- colSums( valid_RES3AB ) / cem_perc_
cem_RES3AB_corr <- mean( cem_RES3AB_[ 1:5 ] )
cem_RES3AB <- cem_RES3AB_corr * cem_perc[ 1:( vp_start - ap_start ) ]

cem_RES3CDEF_ <- colSums( valid_RES3CDEF ) / cem_perc_
cem_RES3CDEF_corr <- mean( cem_RES3CDEF_[ 1:5 ] )
cem_RES3CDEF <- cem_RES3CDEF_corr * cem_perc[ 1:( vp_start - ap_start ) ]

cem_COM_ <- colSums( valid_COM ) / cem_perc_
cem_COM_corr <- mean( cem_COM_[ 1:5 ] )
cem_COM <- cem_COM_corr * cem_perc[ 1:( vp_start - ap_start ) ]

valid_RES12_ <- cbind( array( rep( rowSums( valid_RES12[ , 1:5 ] ) / sum( valid_RES12[ , 1:5 ] ), vp_start - ap_start ), dim = c( length( states ), vp_start - ap_start ) ) * t( array( rep( cem_RES12, length( states ) ), dim = c( vp_start - ap_start, length( states ) ) ) ), valid_RES12 )
valid_RES3AB_ <- cbind( array( rep( rowSums( valid_RES3AB[ , 1:5 ] ) / sum( valid_RES3AB[ , 1:5 ] ), vp_start - ap_start ), dim = c( length( states ), vp_start - ap_start ) ) * t( array( rep( cem_RES3AB, length( states ) ), dim = c( vp_start - ap_start, length( states ) ) ) ), valid_RES3AB )
valid_RES3CDEF_ <- cbind( array( rep( rowSums( valid_RES3CDEF[ , 1:5 ] ) / sum( valid_RES3CDEF[ , 1:5 ] ), vp_start - ap_start ), dim = c( length( states ), vp_start - ap_start ) ) * t( array( rep( cem_RES3CDEF, length( states ) ), dim = c( vp_start - ap_start, length( states ) ) ) ), valid_RES3CDEF )
valid_COM_ <- cbind( array( rep( rowSums( valid_COM[ , 1:5 ] ) / sum( valid_COM[ , 1:5 ] ), vp_start - ap_start ), dim = c( length( states ), vp_start - ap_start ) ) * t( array( rep( cem_COM, length( states ) ), dim = c( vp_start - ap_start, length( states ) ) ) ), valid_COM )

corr_RES12 <- ( 0.98 * 0.97 + 0.85 * 0.03 ) * valid_RES12_ / ( MixDesign( app = "lc" )$cement * vol_RES12_lc + MixDesign( app = "hc" )$cement * vol_RES12_hc + ( ( 180 + 200 ) / 2 ) * ( vol_cmuRES12_lc + vol_cmuRES12_hc ) )
corr_RES3AB <- ( 0.98 * 0.97 + 0.85 * 0.03 ) * valid_RES3AB_ / ( MixDesign( app = "lc" )$cement * vol_RES3AB_lc + MixDesign( app = "hc" )$cement * vol_RES3AB_hc + ( ( 180 + 200 ) / 2 ) * ( vol_cmuRES3AB_lc + vol_cmuRES3AB_hc ) )
corr_RES3CDEF <- ( 0.98 * 0.97 + 0.85 * 0.03 ) * valid_RES3CDEF_ / ( MixDesign( app = "lc" )$cement * vol_RES3CDEF_lc + MixDesign( app = "hc" )$cement * vol_RES3CDEF_hc + ( ( 180 + 200 ) / 2 ) * ( vol_cmuRES3CDEF_lc + vol_cmuRES3CDEF_hc ) )
corr_COM <- ( 0.98 * 0.97 + 0.85 * 0.03 ) * valid_COM_ / ( MixDesign( app = "lc" )$cement * vol_COM_lc + MixDesign( app = "hc" )$cement * vol_COM_hc + ( ( 180 + 200 ) / 2 ) * ( vol_cmuCOM_lc + vol_cmuCOM_hc ) )

corr_RES12[ is.na( corr_RES12 ) | !is.finite( corr_RES12 ) ] <- 1
corr_RES3AB[ is.na( corr_RES3AB ) | !is.finite( corr_RES3AB ) ] <- 1
corr_RES3CDEF[ is.na( corr_RES3CDEF ) | !is.finite( corr_RES3CDEF ) ] <- 1
corr_COM[ is.na( corr_COM ) | !is.finite( corr_COM ) ] <- 1

# Note: updated to include additions

cem_RES_add_ <- colSums( valid_RES_add ) / cem_perc_
cem_RES_add_corr <- mean( cem_RES_add_[ 1:5 ] )
cem_RES_add <- cem_RES_add_corr * cem_perc[ 1:( vp_start - ap_start ) ]

valid_RES_add_ <- cbind( array( rep( rowSums( valid_RES_add[ , 1:5 ] ) / sum( valid_RES_add[ , 1:5 ] ), vp_start - ap_start ), dim = c( length( states ), vp_start - ap_start ) ) * t( array( rep( cem_RES_add, length( states ) ), dim = c( vp_start - ap_start, length( states ) ) ) ), valid_RES_add )

corr_RES_add <- ( 0.98 * 0.97 + 0.85 * 0.03 ) * valid_RES_add_ / ( MixDesign( app = "lc" )$cement * vol_RES_add_lc + MixDesign( app = "hc" )$cement * vol_RES_add_hc + ( ( 180 + 200 ) / 2 ) * ( vol_cmuRES_add_lc + vol_cmuRES_add_hc ) )

corr_RES_add[ is.na( corr_RES_add ) | !is.finite( corr_RES_add ) ] <- 1

# Note: updated to include M&R

cem_RES_rep_ <- colSums( valid_RES_rep ) / cem_perc_
cem_RES_rep_corr <- mean( cem_RES_rep_[ 1:5 ] )
cem_RES_rep <- cem_RES_rep_corr * cem_perc[ 1:( vp_start - ap_start ) ]

cem_COM_rep_ <- colSums( valid_COM_rep ) / cem_perc_
cem_COM_rep_corr <- mean( cem_COM_rep_[ 1:5 ] )
cem_COM_rep <- cem_COM_rep_corr * cem_perc[ 1:( vp_start - ap_start ) ]

valid_RES_rep_ <- cbind( array( rep( rowSums( valid_RES_rep[ , 1:5 ] ) / sum( valid_RES_rep[ , 1:5 ] ), vp_start - ap_start ), dim = c( length( states ), vp_start - ap_start ) ) * t( array( rep( cem_RES_rep, length( states ) ), dim = c( vp_start - ap_start, length( states ) ) ) ), valid_RES_rep )
valid_COM_rep_ <- cbind( array( rep( rowSums( valid_COM_rep[ , 1:5 ] ) / sum( valid_COM_rep[ , 1:5 ] ), vp_start - ap_start ), dim = c( length( states ), vp_start - ap_start ) ) * t( array( rep( cem_COM_rep, length( states ) ), dim = c( vp_start - ap_start, length( states ) ) ) ), valid_COM_rep )

corr_RES_rep <- ( 0.98 * 0.97 + 0.85 * 0.03 ) * valid_RES_rep_ / ( MixDesign( app = "hc" )$cement * vol_RES_rep ) # Note: assumed only hc
corr_COM_rep <- ( 0.98 * 0.97 + 0.85 * 0.03 ) * valid_COM_rep_ / ( MixDesign( app = "hc" )$cement * vol_COM_rep ) # Note: assumed only hc

corr_RES_rep[ is.na( corr_RES_rep ) | !is.finite( corr_RES_rep ) ] <- 1
corr_COM_rep[ is.na( corr_COM_rep ) | !is.finite( corr_COM_rep ) ] <- 1

for ( yb in seq( 1, ap ) ){
  
  sa_yb_RES12_lc[ , , yb ] <- sa_yb_RES12_lc[ , , yb ] * corr_RES12[ , yb ]
  sa_yb_RES3AB_lc[ , , yb ] <- sa_yb_RES3AB_lc[ , , yb ] * corr_RES3AB[ , yb ]
  sa_yb_RES3CDEF_lc[ , , yb ] <- sa_yb_RES3CDEF_lc[ , , yb ] * corr_RES3CDEF[ , yb ]
  sa_yb_COM_lc[ , , yb ] <- sa_yb_COM_lc[ , , yb ] * corr_COM[ , yb ]
  sa_yb_cmuRES12_lc[ , , yb ] <- sa_yb_cmuRES12_lc[ , , yb ] * corr_RES12[ , yb ]
  sa_yb_cmuRES3AB_lc[ , , yb ] <- sa_yb_cmuRES3AB_lc[ , , yb ] * corr_RES3AB[ , yb ]
  sa_yb_cmuRES3CDEF_lc[ , , yb ] <- sa_yb_cmuRES3CDEF_lc[ , , yb ] * corr_RES3CDEF[ , yb ]
  sa_yb_cmuCOM_lc[ , , yb ] <- sa_yb_cmuCOM_lc[ , , yb ] * corr_COM[ , yb ]
  
  sa_yb_RES12_hc[ , , yb ] <- sa_yb_RES12_hc[ , , yb ] * corr_RES12[ , yb ]
  sa_yb_RES3AB_hc[ , , yb ] <- sa_yb_RES3AB_hc[ , , yb ] * corr_RES3AB[ , yb ]
  sa_yb_RES3CDEF_hc[ , , yb ] <- sa_yb_RES3CDEF_hc[ , , yb ] * corr_RES3CDEF[ , yb ]
  sa_yb_COM_hc[ , , yb ] <- sa_yb_COM_hc[ , , yb ] * corr_COM[ , yb ]
  sa_yb_cmuRES12_hc[ , , yb ] <- sa_yb_cmuRES12_hc[ , , yb ] * corr_RES12[ , yb ]
  sa_yb_cmuRES3AB_hc[ , , yb ] <- sa_yb_cmuRES3AB_hc[ , , yb ] * corr_RES3AB[ , yb ]
  sa_yb_cmuRES3CDEF_hc[ , , yb ] <- sa_yb_cmuRES3CDEF_hc[ , , yb ] * corr_RES3CDEF[ , yb ]
  sa_yb_cmuCOM_hc[ , , yb ] <- sa_yb_cmuCOM_hc[ , , yb ] * corr_COM[ , yb ]
  
  vol_yb_RES12_lc[ , , yb ] <- vol_yb_RES12_lc[ , , yb ] * corr_RES12[ , yb ]
  vol_yb_RES3AB_lc[ , , yb ] <- vol_yb_RES3AB_lc[ , , yb ] * corr_RES3AB[ , yb ]
  vol_yb_RES3CDEF_lc[ , , yb ] <- vol_yb_RES3CDEF_lc[ , , yb ] * corr_RES3CDEF[ , yb ]
  vol_yb_COM_lc[ , , yb ] <- vol_yb_COM_lc[ , , yb ] * corr_COM[ , yb ]
  vol_yb_cmuRES12_lc[ , , yb ] <- vol_yb_cmuRES12_lc[ , , yb ] * corr_RES12[ , yb ]
  vol_yb_cmuRES3AB_lc[ , , yb ] <- vol_yb_cmuRES3AB_lc[ , , yb ] * corr_RES3AB[ , yb ]
  vol_yb_cmuRES3CDEF_lc[ , , yb ] <- vol_yb_cmuRES3CDEF_lc[ , , yb ] * corr_RES3CDEF[ , yb ]
  vol_yb_cmuCOM_lc[ , , yb ] <- vol_yb_cmuCOM_lc[ , , yb ] * corr_COM[ , yb ]
  
  vol_yb_RES12_hc[ , , yb ] <- vol_yb_RES12_hc[ , , yb ] * corr_RES12[ , yb ]
  vol_yb_RES3AB_hc[ , , yb ] <- vol_yb_RES3AB_hc[ , , yb ] * corr_RES3AB[ , yb ]
  vol_yb_RES3CDEF_hc[ , , yb ] <- vol_yb_RES3CDEF_hc[ , , yb ] * corr_RES3CDEF[ , yb ]
  vol_yb_COM_hc[ , , yb ] <- vol_yb_COM_hc[ , , yb ] * corr_COM[ , yb ]
  vol_yb_cmuRES12_hc[ , , yb ] <- vol_yb_cmuRES12_hc[ , , yb ] * corr_RES12[ , yb ]
  vol_yb_cmuRES3AB_hc[ , , yb ] <- vol_yb_cmuRES3AB_hc[ , , yb ] * corr_RES3AB[ , yb ]
  vol_yb_cmuRES3CDEF_hc[ , , yb ] <- vol_yb_cmuRES3CDEF_hc[ , , yb ] * corr_RES3CDEF[ , yb ]
  vol_yb_cmuCOM_hc[ , , yb ] <- vol_yb_cmuCOM_hc[ , , yb ] * corr_COM[ , yb ]
  
  # Note: updated to include additions
  
  sa_yb_RES_add_lc[ , , yb ] <- sa_yb_RES_add_lc[ , , yb ] * corr_RES_add[ , yb ]
  sa_yb_cmuRES_add_lc[ , , yb ] <- sa_yb_cmuRES_add_lc[ , , yb ] * corr_RES_add[ , yb ]
  sa_yb_RES_add_hc[ , , yb ] <- sa_yb_RES_add_hc[ , , yb ] * corr_RES_add[ , yb ]
  sa_yb_cmuRES_add_hc[ , , yb ] <- sa_yb_cmuRES_add_hc[ , , yb ] * corr_RES_add[ , yb ]
  
  vol_yb_RES_add_lc[ , , yb ] <- vol_yb_RES_add_lc[ , , yb ] * corr_RES_add[ , yb ]
  vol_yb_cmuRES_add_lc[ , , yb ] <- vol_yb_cmuRES_add_lc[ , , yb ] * corr_RES_add[ , yb ]
  vol_yb_RES_add_hc[ , , yb ] <- vol_yb_RES_add_hc[ , , yb ] * corr_RES_add[ , yb ]
  vol_yb_cmuRES_add_hc[ , , yb ] <- vol_yb_cmuRES_add_hc[ , , yb ] * corr_RES_add[ , yb ]
  
  # Note: updated to include M&R
  
  sa_yb_RES_rep[ , , yb ] <- sa_yb_RES_rep[ , , yb ] * corr_RES_rep[ , yb ]
  sa_yb_COM_rep[ , , yb ] <- sa_yb_COM_rep[ , , yb ] * corr_COM_rep[ , yb ]
  
  vol_yb_RES_rep[ , , yb ] <- vol_yb_RES_rep[ , , yb ] * corr_RES_rep[ , yb ]
  vol_yb_COM_rep[ , , yb ] <- vol_yb_COM_rep[ , , yb ] * corr_COM_rep[ , yb ]
  
}


## add later years

cem_2017 <- MixDesign( app = "lc" )$cement * ( vol_yb_RES12_lc[ , , ap ] + vol_yb_RES3AB_lc[ , , ap ] + vol_yb_RES3CDEF_lc[ , , ap ] + vol_yb_COM_lc[ , , ap ] + vol_yb_RES_add_lc[ , , ap ] ) +
  MixDesign( app = "hc" )$cement * ( vol_yb_RES12_hc[ , , ap ] + vol_yb_RES3AB_hc[ , , ap ] + vol_yb_RES3CDEF_hc[ , , ap ] + vol_yb_COM_hc[ , , ap ] + vol_yb_RES_add_hc[ , , ap ] + vol_yb_RES_rep[ , , ap ] + vol_yb_COM_rep[ , , ap ] ) +  # Note: assumed only hc
  ( ( 180 + 200 ) / 2 ) * ( ( vol_yb_cmuRES12_lc[ , , ap ] + vol_yb_cmuRES3AB_lc[ , , ap ] + vol_yb_cmuRES3CDEF_lc[ , , ap ] + vol_yb_cmuCOM_lc[ , , ap ] + vol_yb_cmuRES_add_lc[ , , ap ] ) + ( vol_yb_cmuRES12_hc[ , , ap ] + vol_yb_cmuRES3AB_hc[ , , ap ] + vol_yb_cmuRES3CDEF_hc[ , , ap ] + vol_yb_cmuCOM_hc[ , , ap ] + vol_yb_cmuRES_add_hc[ , , ap ] ) )

# Note: cement consumption too high

# valid_later <- c( 99300,	101584,	103311,	107650,	107973 ) * 1000000

valid_later <- sum( cem_2017 ) * c( 99300,	101584,	103311,	107650,	107973 ) / 94161

lp <- length( valid_later )

for ( ly in seq( 1, lp ) ){
  
  if ( ly == 1 ){
    
    sa_temp_RES12_lc <- sa_yb_RES12_lc[ , , ap - ly + 1 ] / lp
    sa_temp_RES3AB_lc <- sa_yb_RES3AB_lc[ , , ap - ly + 1 ] / lp
    sa_temp_RES3CDEF_lc <- sa_yb_RES3CDEF_lc[ , , ap - ly + 1 ] / lp
    sa_temp_COM_lc <- sa_yb_COM_lc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuRES12_lc <- sa_yb_cmuRES12_lc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuRES3AB_lc <- sa_yb_cmuRES3AB_lc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuRES3CDEF_lc <- sa_yb_cmuRES3CDEF_lc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuCOM_lc <- sa_yb_cmuCOM_lc[ , , ap - ly + 1 ] / lp
    
    sa_temp_RES12_hc <- sa_yb_RES12_hc[ , , ap - ly + 1 ] / lp
    sa_temp_RES3AB_hc <- sa_yb_RES3AB_hc[ , , ap - ly + 1 ] / lp
    sa_temp_RES3CDEF_hc <- sa_yb_RES3CDEF_hc[ , , ap - ly + 1 ] / lp
    sa_temp_COM_hc <- sa_yb_COM_hc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuRES12_hc <- sa_yb_cmuRES12_hc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuRES3AB_hc <- sa_yb_cmuRES3AB_hc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuRES3CDEF_hc <- sa_yb_cmuRES3CDEF_hc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuCOM_hc <- sa_yb_cmuCOM_hc[ , , ap - ly + 1 ] / lp
    
    vol_temp_RES12_lc <- vol_yb_RES12_lc[ , , ap - ly + 1 ] / lp
    vol_temp_RES3AB_lc <- vol_yb_RES3AB_lc[ , , ap - ly + 1 ] / lp
    vol_temp_RES3CDEF_lc <- vol_yb_RES3CDEF_lc[ , , ap - ly + 1 ] / lp
    vol_temp_COM_lc <- vol_yb_COM_lc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuRES12_lc <- vol_yb_cmuRES12_lc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuRES3AB_lc <- vol_yb_cmuRES3AB_lc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuRES3CDEF_lc <- vol_yb_cmuRES3CDEF_lc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuCOM_lc <- vol_yb_cmuCOM_lc[ , , ap - ly + 1 ] / lp
    
    vol_temp_RES12_hc <- vol_yb_RES12_hc[ , , ap - ly + 1 ] / lp
    vol_temp_RES3AB_hc <- vol_yb_RES3AB_hc[ , , ap - ly + 1 ] / lp
    vol_temp_RES3CDEF_hc <- vol_yb_RES3CDEF_hc[ , , ap - ly + 1 ] / lp
    vol_temp_COM_hc <- vol_yb_COM_hc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuRES12_hc <- vol_yb_cmuRES12_hc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuRES3AB_hc <- vol_yb_cmuRES3AB_hc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuRES3CDEF_hc <- vol_yb_cmuRES3CDEF_hc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuCOM_hc <- vol_yb_cmuCOM_hc[ , , ap - ly + 1 ] / lp
    
    # Note: updated to include additions
    
    sa_temp_RES_add_lc <- sa_yb_RES_add_lc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuRES_add_lc <- sa_yb_cmuRES_add_lc[ , , ap - ly + 1 ] / lp
    sa_temp_RES_add_hc <- sa_yb_RES_add_hc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuRES_add_hc <- sa_yb_cmuRES_add_hc[ , , ap - ly + 1 ] / lp
    
    vol_temp_RES_add_lc <- vol_yb_RES_add_lc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuRES_add_lc <- vol_yb_cmuRES_add_lc[ , , ap - ly + 1 ] / lp
    vol_temp_RES_add_hc <- vol_yb_RES_add_hc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuRES_add_hc <- vol_yb_cmuRES_add_hc[ , , ap - ly + 1 ] / lp
    
    # Note: updated to include M&R
    
    sa_temp_RES_rep <- sa_yb_RES_rep[ , , ap - ly + 1 ] / lp
    sa_temp_COM_rep <- sa_yb_COM_rep[ , , ap - ly + 1 ] / lp
    
    vol_temp_RES_rep <- vol_yb_RES_rep[ , , ap - ly + 1 ] / lp
    vol_temp_COM_rep <- vol_yb_COM_rep[ , , ap - ly + 1 ] / lp
    
  }else{
    
    sa_temp_RES12_lc <- sa_temp_RES12_lc + sa_yb_RES12_lc[ , , ap - ly + 1 ] / lp
    sa_temp_RES3AB_lc <- sa_temp_RES3AB_lc + sa_yb_RES3AB_lc[ , , ap - ly + 1 ] / lp
    sa_temp_RES3CDEF_lc <- sa_temp_RES3CDEF_lc + sa_yb_RES3CDEF_lc[ , , ap - ly + 1 ] / lp
    sa_temp_COM_lc <- sa_temp_COM_lc + sa_yb_COM_lc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuRES12_lc <- sa_temp_cmuRES12_lc + sa_yb_cmuRES12_lc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuRES3AB_lc <- sa_temp_cmuRES3AB_lc + sa_yb_cmuRES3AB_lc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuRES3CDEF_lc <- sa_temp_cmuRES3CDEF_lc + sa_yb_cmuRES3CDEF_lc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuCOM_lc <- sa_temp_cmuCOM_lc + sa_yb_cmuCOM_lc[ , , ap - ly + 1 ] / lp
    
    sa_temp_RES12_hc <- sa_temp_RES12_hc + sa_yb_RES12_hc[ , , ap - ly + 1 ] / lp
    sa_temp_RES3AB_hc <- sa_temp_RES3AB_hc + sa_yb_RES3AB_hc[ , , ap - ly + 1 ] / lp
    sa_temp_RES3CDEF_hc <- sa_temp_RES3CDEF_hc + sa_yb_RES3CDEF_hc[ , , ap - ly + 1 ] / lp
    sa_temp_COM_hc <- sa_temp_COM_hc + sa_yb_COM_hc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuRES12_hc <- sa_temp_cmuRES12_hc + sa_yb_cmuRES12_hc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuRES3AB_hc <- sa_temp_cmuRES3AB_hc + sa_yb_cmuRES3AB_hc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuRES3CDEF_hc <- sa_temp_cmuRES3CDEF_hc + sa_yb_cmuRES3CDEF_hc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuCOM_hc <- sa_temp_cmuCOM_hc + sa_yb_cmuCOM_hc[ , , ap - ly + 1 ] / lp
    
    vol_temp_RES12_lc <- vol_temp_RES12_lc + vol_yb_RES12_lc[ , , ap - ly + 1 ] / lp
    vol_temp_RES3AB_lc <- vol_temp_RES3AB_lc + vol_yb_RES3AB_lc[ , , ap - ly + 1 ] / lp
    vol_temp_RES3CDEF_lc <- vol_temp_RES3CDEF_lc + vol_yb_RES3CDEF_lc[ , , ap - ly + 1 ] / lp
    vol_temp_COM_lc <- vol_temp_COM_lc + vol_yb_COM_lc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuRES12_lc <- vol_temp_cmuRES12_lc + vol_yb_cmuRES12_lc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuRES3AB_lc <- vol_temp_cmuRES3AB_lc + vol_yb_cmuRES3AB_lc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuRES3CDEF_lc <- vol_temp_cmuRES3CDEF_lc + vol_yb_cmuRES3CDEF_lc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuCOM_lc <- vol_temp_cmuCOM_lc + vol_yb_cmuCOM_lc[ , , ap - ly + 1 ] / lp
    
    vol_temp_RES12_hc <- vol_temp_RES12_hc + vol_yb_RES12_hc[ , , ap - ly + 1 ] / lp
    vol_temp_RES3AB_hc <- vol_temp_RES3AB_hc + vol_yb_RES3AB_hc[ , , ap - ly + 1 ] / lp
    vol_temp_RES3CDEF_hc <- vol_temp_RES3CDEF_hc + vol_yb_RES3CDEF_hc[ , , ap - ly + 1 ] / lp
    vol_temp_COM_hc <- vol_temp_COM_hc + vol_yb_COM_hc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuRES12_hc <- vol_temp_cmuRES12_hc + vol_yb_cmuRES12_hc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuRES3AB_hc <- vol_temp_cmuRES3AB_hc + vol_yb_cmuRES3AB_hc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuRES3CDEF_hc <- vol_temp_cmuRES3CDEF_hc + vol_yb_cmuRES3CDEF_hc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuCOM_hc <- vol_temp_cmuCOM_hc + vol_yb_cmuCOM_hc[ , , ap - ly + 1 ] / lp
    
    # Note: updated to include additions
    
    sa_temp_RES_add_lc <- sa_temp_RES_add_lc + sa_yb_RES_add_lc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuRES_add_lc <- sa_temp_cmuRES_add_lc + sa_yb_cmuRES_add_lc[ , , ap - ly + 1 ] / lp
    sa_temp_RES_add_hc <- sa_temp_RES_add_hc + sa_yb_RES_add_hc[ , , ap - ly + 1 ] / lp
    sa_temp_cmuRES_add_hc <- sa_temp_cmuRES_add_hc + sa_yb_cmuRES_add_hc[ , , ap - ly + 1 ] / lp
    
    vol_temp_RES_add_lc <- vol_temp_RES_add_lc + vol_yb_RES_add_lc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuRES_add_lc <- vol_temp_cmuRES_add_lc + vol_yb_cmuRES_add_lc[ , , ap - ly + 1 ] / lp
    vol_temp_RES_add_hc <- vol_temp_RES_add_hc + vol_yb_RES_add_hc[ , , ap - ly + 1 ] / lp
    vol_temp_cmuRES_add_hc <- vol_temp_cmuRES_add_hc + vol_yb_cmuRES_add_hc[ , , ap - ly + 1 ] / lp
    
    # Note: updated to include M&R
    
    sa_temp_RES_rep <- sa_temp_RES_rep + sa_yb_RES_rep[ , , ap - ly + 1 ] / lp
    sa_temp_COM_rep <- sa_temp_COM_rep + sa_yb_COM_rep[ , , ap - ly + 1 ] / lp
    
    vol_temp_RES_rep <- vol_temp_RES_rep + vol_yb_RES_rep[ , , ap - ly + 1 ] / lp
    vol_temp_COM_rep <- vol_temp_COM_rep + vol_yb_COM_rep[ , , ap - ly + 1 ] / lp
    
  }
  
}

cem_temp <- MixDesign( app = "lc" )$cement * ( vol_temp_RES12_lc + vol_temp_RES3AB_lc + vol_temp_RES3CDEF_lc + vol_temp_COM_lc + vol_temp_RES_add_lc ) +
  MixDesign( app = "hc" )$cement * ( vol_temp_RES12_hc + vol_temp_RES3AB_hc + vol_temp_RES3CDEF_hc + vol_temp_COM_hc + vol_temp_RES_add_hc + vol_temp_RES_rep + vol_temp_COM_rep ) +  # Note: assumed only hc
  ( ( 180 + 200 ) / 2 ) * ( ( vol_temp_cmuRES12_lc + vol_temp_cmuRES3AB_lc + vol_temp_cmuRES3CDEF_lc + vol_temp_cmuCOM_lc + vol_temp_cmuRES_add_lc ) + ( vol_temp_cmuRES12_hc + vol_temp_cmuRES3AB_hc + vol_temp_cmuRES3CDEF_hc + vol_temp_cmuCOM_hc + vol_temp_cmuRES_add_hc ) )

corr_later <- ( 0.98 * 0.97 + 0.85 * 0.03 ) * valid_later / sum( cem_temp )

for ( ly in seq( 1, lp ) ){
  
  sa_yb_RES12_lc <- abind( sa_yb_RES12_lc, sa_temp_RES12_lc * corr_later[ ly ], along = 3 )
  sa_yb_RES3AB_lc <- abind( sa_yb_RES3AB_lc, sa_temp_RES3AB_lc * corr_later[ ly ], along = 3 )
  sa_yb_RES3CDEF_lc <- abind( sa_yb_RES3CDEF_lc, sa_temp_RES3CDEF_lc * corr_later[ ly ], along = 3 )
  sa_yb_COM_lc <- abind( sa_yb_COM_lc, sa_temp_COM_lc * corr_later[ ly ], along = 3 )
  sa_yb_cmuRES12_lc <- abind( sa_yb_cmuRES12_lc, sa_temp_cmuRES12_lc * corr_later[ ly ], along = 3 )
  sa_yb_cmuRES3AB_lc <- abind( sa_yb_cmuRES3AB_lc, sa_temp_cmuRES3AB_lc * corr_later[ ly ], along = 3 )
  sa_yb_cmuRES3CDEF_lc <- abind( sa_yb_cmuRES3CDEF_lc, sa_temp_cmuRES3CDEF_lc * corr_later[ ly ], along = 3 )
  sa_yb_cmuCOM_lc <- abind( sa_yb_cmuCOM_lc, sa_temp_cmuCOM_lc * corr_later[ ly ], along = 3 )
  
  sa_yb_RES12_hc <- abind( sa_yb_RES12_hc, sa_temp_RES12_hc * corr_later[ ly ], along = 3 )
  sa_yb_RES3AB_hc <- abind( sa_yb_RES3AB_hc, sa_temp_RES3AB_hc * corr_later[ ly ], along = 3 )
  sa_yb_RES3CDEF_hc <- abind( sa_yb_RES3CDEF_hc, sa_temp_RES3CDEF_hc * corr_later[ ly ], along = 3 )
  sa_yb_COM_hc <- abind( sa_yb_COM_hc, sa_temp_COM_hc * corr_later[ ly ], along = 3 )
  sa_yb_cmuRES12_hc <- abind( sa_yb_cmuRES12_hc, sa_temp_cmuRES12_hc * corr_later[ ly ], along = 3 )
  sa_yb_cmuRES3AB_hc <- abind( sa_yb_cmuRES3AB_hc, sa_temp_cmuRES3AB_hc * corr_later[ ly ], along = 3 )
  sa_yb_cmuRES3CDEF_hc <- abind( sa_yb_cmuRES3CDEF_hc, sa_temp_cmuRES3CDEF_hc * corr_later[ ly ], along = 3 )
  sa_yb_cmuCOM_hc <- abind( sa_yb_cmuCOM_hc, sa_temp_cmuCOM_hc * corr_later[ ly ], along = 3 )
  
  vol_yb_RES12_lc <- abind( vol_yb_RES12_lc, vol_temp_RES12_lc * corr_later[ ly ], along = 3 )
  vol_yb_RES3AB_lc <- abind( vol_yb_RES3AB_lc, vol_temp_RES3AB_lc * corr_later[ ly ], along = 3 )
  vol_yb_RES3CDEF_lc <- abind( vol_yb_RES3CDEF_lc, vol_temp_RES3CDEF_lc * corr_later[ ly ], along = 3 )
  vol_yb_COM_lc <- abind( vol_yb_COM_lc, vol_temp_COM_lc * corr_later[ ly ], along = 3 )
  vol_yb_cmuRES12_lc <- abind( vol_yb_cmuRES12_lc, vol_temp_cmuRES12_lc * corr_later[ ly ], along = 3 )
  vol_yb_cmuRES3AB_lc <- abind( vol_yb_cmuRES3AB_lc, vol_temp_cmuRES3AB_lc * corr_later[ ly ], along = 3 )
  vol_yb_cmuRES3CDEF_lc <- abind( vol_yb_cmuRES3CDEF_lc, vol_temp_cmuRES3CDEF_lc * corr_later[ ly ], along = 3 )
  vol_yb_cmuCOM_lc <- abind( vol_yb_cmuCOM_lc, vol_temp_cmuCOM_lc * corr_later[ ly ], along = 3 )
  
  vol_yb_RES12_hc <- abind( vol_yb_RES12_hc, vol_temp_RES12_hc * corr_later[ ly ], along = 3 )
  vol_yb_RES3AB_hc <- abind( vol_yb_RES3AB_hc, vol_temp_RES3AB_hc * corr_later[ ly ], along = 3 )
  vol_yb_RES3CDEF_hc <- abind( vol_yb_RES3CDEF_hc, vol_temp_RES3CDEF_hc * corr_later[ ly ], along = 3 )
  vol_yb_COM_hc <- abind( vol_yb_COM_hc, vol_temp_COM_hc * corr_later[ ly ], along = 3 )
  vol_yb_cmuRES12_hc <- abind( vol_yb_cmuRES12_hc, vol_temp_cmuRES12_hc * corr_later[ ly ], along = 3 )
  vol_yb_cmuRES3AB_hc <- abind( vol_yb_cmuRES3AB_hc, vol_temp_cmuRES3AB_hc * corr_later[ ly ], along = 3 )
  vol_yb_cmuRES3CDEF_hc <- abind( vol_yb_cmuRES3CDEF_hc, vol_temp_cmuRES3CDEF_hc * corr_later[ ly ], along = 3 )
  vol_yb_cmuCOM_hc <- abind( vol_yb_cmuCOM_hc, vol_temp_cmuCOM_hc * corr_later[ ly ], along = 3 )
  
  # Note: updated to include additions
  
  sa_yb_RES_add_lc <- abind( sa_yb_RES_add_lc, sa_temp_RES_add_lc * corr_later[ ly ], along = 3 )
  sa_yb_cmuRES_add_lc <- abind( sa_yb_cmuRES_add_lc, sa_temp_cmuRES_add_lc * corr_later[ ly ], along = 3 )
  sa_yb_RES_add_hc <- abind( sa_yb_RES_add_hc, sa_temp_RES_add_hc * corr_later[ ly ], along = 3 )
  sa_yb_cmuRES_add_hc <- abind( sa_yb_cmuRES_add_hc, sa_temp_cmuRES_add_hc * corr_later[ ly ], along = 3 )
  
  vol_yb_RES_add_lc <- abind( vol_yb_RES_add_lc, vol_temp_RES_add_lc * corr_later[ ly ], along = 3 )
  vol_yb_cmuRES_add_lc <- abind( vol_yb_cmuRES_add_lc, vol_temp_cmuRES_add_lc * corr_later[ ly ], along = 3 )
  vol_yb_RES_add_hc <- abind( vol_yb_RES_add_hc, vol_temp_RES_add_hc * corr_later[ ly ], along = 3 )
  vol_yb_cmuRES_add_hc <- abind( vol_yb_cmuRES_add_hc, vol_temp_cmuRES_add_hc * corr_later[ ly ], along = 3 )
  
  # Note: updated to include M&R
  
  sa_yb_RES_rep <- abind( sa_yb_RES_rep, sa_temp_RES_rep * corr_later[ ly ], along = 3 )
  sa_yb_COM_rep <- abind( sa_yb_COM_rep, sa_temp_COM_rep * corr_later[ ly ], along = 3 )
  
  vol_yb_RES_rep <- abind( vol_yb_RES_rep, vol_temp_RES_rep * corr_later[ ly ], along = 3 )
  vol_yb_COM_rep <- abind( vol_yb_COM_rep, vol_temp_COM_rep * corr_later[ ly ], along = 3 )
  
}

ap <- ap + lp


## calculate uptake per m2 for each state (row), element (col), age (3rd dim)

# age from 0 to 50

u_m2_a_lc <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
u_m2_a_hc <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
u_m2_a_cmulc <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
u_m2_a_cmuhc <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )

cc_m3_a_lc <- array( rep( 0, length( states ) * ncol( vol ) * ceiling( cc_p ) ), dim = c( length( states ), ncol( vol ), ceiling( cc_p ) ) )
cc_m3_a_hc <- array( rep( 0, length( states ) * ncol( vol ) * ceiling( cc_p ) ), dim = c( length( states ), ncol( vol ), ceiling( cc_p ) ) )

cw_kg_a_lc <- array( rep( 0, length( states ) * 1 * ceiling( cw_p ) ), dim = c( length( states ), 1, ceiling( cw_p ) ) )
cw_kg_a_hc <- array( rep( 0, length( states ) * 1 * ceiling( cw_p ) ), dim = c( length( states ), 1, ceiling( cw_p ) ) )

for ( a in seq( 1, ap ) ){
  
  if ( a > 1 ){
    
    u_m2_a_lc[ , , a ] <- u_m2_lc * ( sqrt( a - 1 ) - sqrt( a - 2 ) )
    u_m2_a_hc[ , , a ] <- u_m2_hc * ( sqrt( a - 1 ) - sqrt( a - 2 ) )
    
    if ( a <= 15 ){
      
      # Note: now cutting off cmu uptake at 15 years
      
      u_m2_a_cmulc[ , , a ] <- u_m2_cmulc * ( sqrt( a - 1 ) - sqrt( a - 2 ) )
      u_m2_a_cmuhc[ , , a ] <- u_m2_cmuhc * ( sqrt( a - 1 ) - sqrt( a - 2 ) )
      
    }
    
  }
  
  if ( a <= ceiling( cc_p ) ){
    
    if ( cc_p < 1 ){
      
      cc_m3_a_lc[ , , a ] <- cc_m3_lc * sqrt( cc_p )
      cc_m3_a_hc[ , , a ] <- cc_m3_hc * sqrt( cc_p )
      
    }else{
      
      if ( a == 1 ){
        
        cc_m3_a_lc[ , , a ] <- cc_m3_lc * sqrt( a )
        cc_m3_a_hc[ , , a ] <- cc_m3_hc * sqrt( a )
        
      }else{
        
        cc_m3_a_lc[ , , a ] <- cc_m3_lc * ( sqrt( a - 1 ) - sqrt( a - 2 ) )
        cc_m3_a_hc[ , , a ] <- cc_m3_hc * ( sqrt( a - 1 ) - sqrt( a - 2 ) )
        
      }
      
    }
    
  }
  
  if ( a <= ceiling( cw_p ) ){
    
    cw_kg_a_lc[ , , a ] <- cw_kg_lc
    cw_kg_a_hc[ , , a ] <- cw_kg_hc
    
  }
  
}


## calculate uptake for each state (row), element (col), year built (3rd dim), age (4th dim)

# year built from 1940 to 2017, age from 0 to 50

u_yb_a_RES12 <- array( rep( 0, length( states ) * ncol( sa ) * ap * ap ), dim = c( length( states ), ncol( sa ), ap, ap ) )
u_yb_a_RES3AB <- array( rep( 0, length( states ) * ncol( sa ) * ap * ap ), dim = c( length( states ), ncol( sa ), ap, ap ) )
u_yb_a_RES3CDEF <- array( rep( 0, length( states ) * ncol( sa ) * ap * ap ), dim = c( length( states ), ncol( sa ), ap, ap ) )
u_yb_a_COM <- array( rep( 0, length( states ) * ncol( sa ) * ap * ap ), dim = c( length( states ), ncol( sa ), ap, ap ) )
u_yb_a_cmuRES12 <- array( rep( 0, length( states ) * ncol( sa ) * ap * ap ), dim = c( length( states ), ncol( sa ), ap, ap ) )
u_yb_a_cmuRES3AB <- array( rep( 0, length( states ) * ncol( sa ) * ap * ap ), dim = c( length( states ), ncol( sa ), ap, ap ) )
u_yb_a_cmuRES3CDEF <- array( rep( 0, length( states ) * ncol( sa ) * ap * ap ), dim = c( length( states ), ncol( sa ), ap, ap ) )
u_yb_a_cmuCOM <- array( rep( 0, length( states ) * ncol( sa ) * ap * ap ), dim = c( length( states ), ncol( sa ), ap, ap ) )

cc_yb_yd_a_RES12 <- array( rep( 0, length( states ) * ncol( vol ) * ap * ap * ceiling( cc_p ) ), dim = c( length( states ), ncol( vol ), ap, ap, ceiling( cc_p ) ) )
cc_yb_yd_a_RES3AB <- array( rep( 0, length( states ) * ncol( vol ) * ap * ap * ceiling( cc_p ) ), dim = c( length( states ), ncol( vol ), ap, ap, ceiling( cc_p ) ) )
cc_yb_yd_a_RES3CDEF <- array( rep( 0, length( states ) * ncol( vol ) * ap * ap * ceiling( cc_p ) ), dim = c( length( states ), ncol( vol ), ap, ap, ceiling( cc_p ) ) )
cc_yb_yd_a_COM <- array( rep( 0, length( states ) * ncol( vol ) * ap * ap * ceiling( cc_p ) ), dim = c( length( states ), ncol( vol ), ap, ap, ceiling( cc_p ) ) )

cw_yb_a_RES12 <- array( rep( 0, length( states ) * 1 * ap * ceiling( cw_p ) ), dim = c( length( states ), 1, ap, ceiling( cw_p ) ) )
cw_yb_a_RES3AB <- array( rep( 0, length( states ) * 1 * ap * ceiling( cw_p ) ), dim = c( length( states ), 1, ap, ceiling( cw_p ) ) )
cw_yb_a_RES3CDEF <- array( rep( 0, length( states ) * 1 * ap * ceiling( cw_p ) ), dim = c( length( states ), 1, ap, ceiling( cw_p ) ) )
cw_yb_a_COM <- array( rep( 0, length( states ) * 1 * ap * ceiling( cw_p ) ), dim = c( length( states ), 1, ap, ceiling( cw_p ) ) )

# Note: updated to include additions

u_yb_a_RES_add <- array( rep( 0, length( states ) * ncol( sa ) * ap * ap ), dim = c( length( states ), ncol( sa ), ap, ap ) )
u_yb_a_cmuRES_add <- array( rep( 0, length( states ) * ncol( sa ) * ap * ap ), dim = c( length( states ), ncol( sa ), ap, ap ) )

cc_yb_yd_a_RES_add <- array( rep( 0, length( states ) * ncol( vol ) * ap * ap * ceiling( cc_p ) ), dim = c( length( states ), ncol( vol ), ap, ap, ceiling( cc_p ) ) )

cw_yb_a_RES_add <- array( rep( 0, length( states ) * 1 * ap * ceiling( cw_p ) ), dim = c( length( states ), 1, ap, ceiling( cw_p ) ) )

# Note: updated to include M&R

u_yb_a_RES_rep <- array( rep( 0, length( states ) * ncol( sa ) * ap * ap ), dim = c( length( states ), ncol( sa ), ap, ap ) )
u_yb_a_COM_rep <- array( rep( 0, length( states ) * ncol( sa ) * ap * ap ), dim = c( length( states ), ncol( sa ), ap, ap ) )

cw_yb_a_RES_rep <- array( rep( 0, length( states ) * 1 * ap * ceiling( cw_p ) ), dim = c( length( states ), 1, ap, ceiling( cw_p ) ) )
cw_yb_a_COM_rep <- array( rep( 0, length( states ) * 1 * ap * ceiling( cw_p ) ), dim = c( length( states ), 1, ap, ceiling( cw_p ) ) )

for ( yb in seq( 1, ap ) ){
  
  for ( a in seq( 1, ap ) ){
    
    u_yb_a_RES12[ , , yb, a ] <- sr[ a ] * ( sa_yb_RES12_lc[ , , yb ] * u_m2_a_lc[ , , a ] + sa_yb_RES12_hc[ , , yb ] * u_m2_a_hc[ , , a ] )
    u_yb_a_RES3AB[ , , yb, a ] <- sr[ a ] * ( sa_yb_RES3AB_lc[ , , yb ] * u_m2_a_lc[ , , a ] + sa_yb_RES3AB_hc[ , , yb ] * u_m2_a_hc[ , , a ] )
    u_yb_a_RES3CDEF[ , , yb, a ] <- sr[ a ] * ( sa_yb_RES3CDEF_lc[ , , yb ] * u_m2_a_lc[ , , a ] + sa_yb_RES3CDEF_hc[ , , yb ] * u_m2_a_hc[ , , a ] )
    u_yb_a_COM[ , , yb, a ] <- sr[ a ] * ( sa_yb_COM_lc[ , , yb ] * u_m2_a_lc[ , , a ] + sa_yb_COM_hc[ , , yb ] * u_m2_a_hc[ , , a ] )
    u_yb_a_cmuRES12[ , , yb, a ] <- sr[ a ] * ( sa_yb_cmuRES12_lc[ , , yb ] * u_m2_a_cmulc[ , , a ] + sa_yb_cmuRES12_hc[ , , yb ] * u_m2_a_cmuhc[ , , a ] )
    u_yb_a_cmuRES3AB[ , , yb, a ] <- sr[ a ] * ( sa_yb_cmuRES3AB_lc[ , , yb ] * u_m2_a_cmulc[ , , a ] + sa_yb_cmuRES3AB_hc[ , , yb ] * u_m2_a_cmuhc[ , , a ] )
    u_yb_a_cmuRES3CDEF[ , , yb, a ] <- sr[ a ] * ( sa_yb_cmuRES3CDEF_lc[ , , yb ] * u_m2_a_cmulc[ , , a ] + sa_yb_cmuRES3CDEF_hc[ , , yb ] * u_m2_a_cmuhc[ , , a ] )
    u_yb_a_cmuCOM[ , , yb, a ] <- sr[ a ] * ( sa_yb_cmuCOM_lc[ , , yb ] * u_m2_a_cmulc[ , , a ] + sa_yb_cmuCOM_hc[ , , yb ] * u_m2_a_cmuhc[ , , a ] )
    
    # Note: updated to include additions
    
    u_yb_a_RES_add[ , , yb, a ] <- sr[ a ] * ( sa_yb_RES_add_lc[ , , yb ] * u_m2_a_lc[ , , a ] + sa_yb_RES_add_hc[ , , yb ] * u_m2_a_hc[ , , a ] )
    u_yb_a_cmuRES_add[ , , yb, a ] <- sr[ a ] * ( sa_yb_cmuRES_add_lc[ , , yb ] * u_m2_a_cmulc[ , , a ] + sa_yb_cmuRES_add_hc[ , , yb ] * u_m2_a_cmuhc[ , , a ] )
    
    # Note: updated to include M&R
    
    u_yb_a_RES_rep[ , , yb, a ] <- sr[ a ] * ( sa_yb_RES_rep[ , , yb ] * u_m2_a_hc[ , , a ] ) # Note: assumed only hc
    u_yb_a_COM_rep[ , , yb, a ] <- sr[ a ] * ( sa_yb_COM_rep[ , , yb ] * u_m2_a_hc[ , , a ] ) # Note: assumed only hc
    
  }
  
  
  # cc
  
  if( yb < ap ){
    
    for ( yd in seq( yb + 1, ap ) ){
      
      for ( a in seq( 1, ceiling( cc_p ) ) ){
        
        if ( yd - yb - 1 == 0 ){
          
          cc_yb_yd_a_RES12[ , , yb, yd, a ] <- ( 1 - sr[ yd - yb ] ) * ( vol_yb_RES12_lc[ , , yb ] * cc_m3_a_lc[ , , a ] + vol_yb_RES12_hc[ , , yb ] * cc_m3_a_hc[ , , a ] )
          cc_yb_yd_a_RES3AB[ , , yb, yd, a ] <- ( 1 - sr[ yd - yb ] ) * ( vol_yb_RES3AB_lc[ , , yb ] * cc_m3_a_lc[ , , a ] + vol_yb_RES3AB_hc[ , , yb ] * cc_m3_a_hc[ , , a ] )
          cc_yb_yd_a_RES3CDEF[ , , yb, yd, a ] <- ( 1 - sr[ yd - yb ] ) * ( vol_yb_RES3CDEF_lc[ , , yb ] * cc_m3_a_lc[ , , a ] + vol_yb_RES3CDEF_hc[ , , yb ] * cc_m3_a_hc[ , , a ] )
          cc_yb_yd_a_COM[ , , yb, yd, a ] <- ( 1 - sr[ yd - yb ] ) * ( vol_yb_COM_lc[ , , yb ] * cc_m3_a_lc[ , , a ] + vol_yb_COM_hc[ , , yb ] * cc_m3_a_hc[ , , a ] )
          
          # Note: updated to include additions
          
          cc_yb_yd_a_RES_add[ , , yb, yd, a ] <- ( 1 - sr[ yd - yb ] ) * ( vol_yb_RES_add_lc[ , , yb ] * cc_m3_a_lc[ , , a ] + vol_yb_RES_add_hc[ , , yb ] * cc_m3_a_hc[ , , a ] )
          
        }else{
          
          cc_yb_yd_a_RES12[ , , yb, yd, a ] <- ( sr[ yd - yb - 1 ] - sr[ yd - yb ] ) * ( vol_yb_RES12_lc[ , , yb ] * cc_m3_a_lc[ , , a ] + vol_yb_RES12_hc[ , , yb ] * cc_m3_a_hc[ , , a ] )
          cc_yb_yd_a_RES3AB[ , , yb, yd, a ] <- ( sr[ yd - yb - 1 ] - sr[ yd - yb ] ) * ( vol_yb_RES3AB_lc[ , , yb ] * cc_m3_a_lc[ , , a ] + vol_yb_RES3AB_hc[ , , yb ] * cc_m3_a_hc[ , , a ] )
          cc_yb_yd_a_RES3CDEF[ , , yb, yd, a ] <- ( sr[ yd - yb - 1 ] - sr[ yd - yb ] ) * ( vol_yb_RES3CDEF_lc[ , , yb ] * cc_m3_a_lc[ , , a ] + vol_yb_RES3CDEF_hc[ , , yb ] * cc_m3_a_hc[ , , a ] )
          cc_yb_yd_a_COM[ , , yb, yd, a ] <- ( sr[ yd - yb - 1 ] - sr[ yd - yb ] ) * ( vol_yb_COM_lc[ , , yb ] * cc_m3_a_lc[ , , a ] + vol_yb_COM_hc[ , , yb ] * cc_m3_a_hc[ , , a ] )
          
          # Note: updated to include additions
          
          cc_yb_yd_a_RES_add[ , , yb, yd, a ] <- ( sr[ yd - yb - 1 ] - sr[ yd - yb ] ) * ( vol_yb_RES_add_lc[ , , yb ] * cc_m3_a_lc[ , , a ] + vol_yb_RES_add_hc[ , , yb ] * cc_m3_a_hc[ , , a ] )
          
        }
        
      }
      
    }
    
  }
  
  
  # cw
  
  for ( a in seq( 1, ceiling( cw_p ) ) ){
    
    cw_yb_a_RES12[ , , yb, a ] <- ( 0.02 * 0.97 + 0.15 * 0.03 ) * ( rowSums( MixDesign( app = "lc" )$cement * vol_yb_RES12_lc[ , , yb ] + ( ( 180 + 200 ) / 2 ) * vol_yb_cmuRES12_lc[ , , yb ] ) * cw_kg_a_lc[ , , a ] + rowSums( MixDesign( app = "hc" )$cement * vol_yb_RES12_hc[ , , yb ] + ( ( 180 + 200 ) / 2 ) * vol_yb_cmuRES12_hc[ , , yb ] ) * cw_kg_a_hc[ , , a ] ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
    cw_yb_a_RES3AB[ , , yb, a ] <- ( 0.02 * 0.97 + 0.15 * 0.03 ) * ( rowSums( MixDesign( app = "lc" )$cement * vol_yb_RES3AB_lc[ , , yb ] + ( ( 180 + 200 ) / 2 ) * vol_yb_cmuRES3AB_lc[ , , yb ] ) * cw_kg_a_lc[ , , a ] + rowSums( MixDesign( app = "hc" )$cement * vol_yb_RES3AB_hc[ , , yb ] + ( ( 180 + 200 ) / 2 ) * vol_yb_cmuRES3AB_hc[ , , yb ] ) * cw_kg_a_hc[ , , a ] ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
    cw_yb_a_RES3CDEF[ , , yb, a ] <- ( 0.02 * 0.97 + 0.15 * 0.03 ) * ( rowSums( MixDesign( app = "lc" )$cement * vol_yb_RES3CDEF_lc[ , , yb ] + ( ( 180 + 200 ) / 2 ) * vol_yb_cmuRES3CDEF_lc[ , , yb ] ) * cw_kg_a_lc[ , , a ] + rowSums( MixDesign( app = "hc" )$cement * vol_yb_RES3CDEF_hc[ , , yb ] + ( ( 180 + 200 ) / 2 ) * vol_yb_cmuRES3CDEF_hc[ , , yb ] ) * cw_kg_a_hc[ , , a ] ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
    cw_yb_a_COM[ , , yb, a ] <- ( 0.02 * 0.97 + 0.15 * 0.03 ) * ( rowSums( MixDesign( app = "lc" )$cement * vol_yb_COM_lc[ , , yb ] + ( ( 180 + 200 ) / 2 ) * vol_yb_cmuCOM_lc[ , , yb ] ) * cw_kg_a_lc[ , , a ] + rowSums( MixDesign( app = "hc" )$cement * vol_yb_COM_hc[ , , yb ] + ( ( 180 + 200 ) / 2 ) * vol_yb_cmuCOM_hc[ , , yb ] ) * cw_kg_a_hc[ , , a ] ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
    
    # Note: updated to include additions
    
    cw_yb_a_RES_add[ , , yb, a ] <- ( 0.02 * 0.97 + 0.15 * 0.03 ) * ( rowSums( MixDesign( app = "lc" )$cement * vol_yb_RES_add_lc[ , , yb ] + ( ( 180 + 200 ) / 2 ) * vol_yb_cmuRES_add_lc[ , , yb ] ) * cw_kg_a_lc[ , , a ] + rowSums( MixDesign( app = "hc" )$cement * vol_yb_RES_add_hc[ , , yb ] + ( ( 180 + 200 ) / 2 ) * vol_yb_cmuRES_add_hc[ , , yb ] ) * cw_kg_a_hc[ , , a ] ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
    
    # Note: updated to include M&R
    
    cw_yb_a_RES_rep[ , , yb, a ] <- ( 0.02 * 0.97 + 0.15 * 0.03 ) * ( rowSums( MixDesign( app = "hc" )$cement * vol_yb_RES_rep[ , , yb ] ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) # Note: assumed only hc
    cw_yb_a_COM_rep[ , , yb, a ] <- ( 0.02 * 0.97 + 0.15 * 0.03 ) * ( rowSums( MixDesign( app = "hc" )$cement * vol_yb_COM_rep[ , , yb ] ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) # Note: assumed only hc
    
  }
  
}


## calculate uptake for each state (row), element (col), analysis year (3rd dim)

# analysis year 1940 to 2017

u_ay_RES12 <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
u_ay_RES3AB <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
u_ay_RES3CDEF <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
u_ay_COM <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
u_ay_cmuRES12 <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
u_ay_cmuRES3AB <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
u_ay_cmuRES3CDEF <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
u_ay_cmuCOM <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )

cc_ay_RES12 <- array( rep( 0, length( states ) * ncol( vol ) * ap ), dim = c( length( states ), ncol( vol ), ap ) )
cc_ay_RES3AB <- array( rep( 0, length( states ) * ncol( vol ) * ap ), dim = c( length( states ), ncol( vol ), ap ) )
cc_ay_RES3CDEF <- array( rep( 0, length( states ) * ncol( vol ) * ap ), dim = c( length( states ), ncol( vol ), ap ) )
cc_ay_COM <- array( rep( 0, length( states ) * ncol( vol ) * ap ), dim = c( length( states ), ncol( vol ), ap ) )

cw_ay_RES12 <- array( rep( 0, length( states ) * 1 * ap ), dim = c( length( states ), 1, ap ) )
cw_ay_RES3AB <- array( rep( 0, length( states ) * 1 * ap ), dim = c( length( states ), 1, ap ) )
cw_ay_RES3CDEF <- array( rep( 0, length( states ) * 1 * ap ), dim = c( length( states ), 1, ap ) )
cw_ay_COM <- array( rep( 0, length( states ) * 1 * ap ), dim = c( length( states ), 1, ap ) )

# Note: updated to include additions

u_ay_RES_add <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
u_ay_cmuRES_add <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )

cc_ay_RES_add <- array( rep( 0, length( states ) * ncol( vol ) * ap ), dim = c( length( states ), ncol( vol ), ap ) )

cw_ay_RES_add <- array( rep( 0, length( states ) * 1 * ap ), dim = c( length( states ), 1, ap ) )

# Note: updated to include M&R

u_ay_RES_rep <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )
u_ay_COM_rep <- array( rep( 0, length( states ) * ncol( sa ) * ap ), dim = c( length( states ), ncol( sa ), ap ) )

cw_ay_RES_rep <- array( rep( 0, length( states ) * 1 * ap ), dim = c( length( states ), 1, ap ) )
cw_ay_COM_rep <- array( rep( 0, length( states ) * 1 * ap ), dim = c( length( states ), 1, ap ) )

for ( ay in seq( 1, ap ) ){
  
  u_RES12 <- 0
  u_RES3AB <- 0
  u_RES3CDEF <- 0
  u_COM <- 0
  u_cmuRES12 <- 0
  u_cmuRES3AB <- 0
  u_cmuRES3CDEF <- 0
  u_cmuCOM <- 0
  
  cc_RES12 <- 0
  cc_RES3AB <- 0
  cc_RES3CDEF <- 0
  cc_COM <- 0
  
  cw_RES12 <- 0
  cw_RES3AB <- 0
  cw_RES3CDEF <- 0
  cw_COM <- 0
  
  # Note: updated to include additions
  
  u_RES_add <- 0
  u_cmuRES_add <- 0
  
  cc_RES_add <- 0
  
  cw_RES_add <- 0
  
  # Note: updated to include M&R
  
  u_RES_rep <- 0
  u_COM_rep <- 0
  
  cw_RES_rep <- 0
  cw_COM_rep <- 0
  
  for ( yb in seq( 1, ay ) ){
    
    u_RES12 <- u_RES12 + u_yb_a_RES12[ , , yb, ay - yb + 1 ]
    u_RES3AB <- u_RES3AB + u_yb_a_RES3AB[ , , yb, ay - yb + 1 ]
    u_RES3CDEF <- u_RES3CDEF + u_yb_a_RES3CDEF[ , , yb, ay - yb + 1 ]
    u_COM <- u_COM + u_yb_a_COM[ , , yb, ay - yb + 1 ]
    u_cmuRES12 <- u_cmuRES12 + u_yb_a_cmuRES12[ , , yb, ay - yb + 1 ]
    u_cmuRES3AB <- u_cmuRES3AB + u_yb_a_cmuRES3AB[ , , yb, ay - yb + 1 ]
    u_cmuRES3CDEF <- u_cmuRES3CDEF + u_yb_a_cmuRES3CDEF[ , , yb, ay - yb + 1 ]
    u_cmuCOM <- u_cmuCOM + u_yb_a_cmuCOM[ , , yb, ay - yb + 1 ]
    
    # Note: updated to include additions
    
    u_RES_add <- u_RES_add + u_yb_a_RES_add[ , , yb, ay - yb + 1 ]
    u_cmuRES_add <- u_cmuRES_add + u_yb_a_cmuRES_add[ , , yb, ay - yb + 1 ]
    
    # Note: updated to include M&R
    
    u_RES_rep <- u_RES_rep + u_yb_a_RES_rep[ , , yb, ay - yb + 1 ]
    u_COM_rep <- u_COM_rep + u_yb_a_COM_rep[ , , yb, ay - yb + 1 ]
    
    
    # cc
    
    if( yb < ap & yb + 1 <= ay ){
      
      for ( yd in seq( yb + 1, ay ) ){
        
        if ( ay - yd + 1 <= ceiling( cc_p ) ){
          
          cc_RES12 <- cc_RES12 + cc_yb_yd_a_RES12[ , , yb, yd, ay - yd + 1 ]
          cc_RES3AB <- cc_RES3AB + cc_yb_yd_a_RES3AB[ , , yb, yd, ay - yd + 1 ]
          cc_RES3CDEF <- cc_RES3CDEF + cc_yb_yd_a_RES3CDEF[ , , yb, yd, ay - yd + 1 ]
          cc_COM <- cc_COM + cc_yb_yd_a_COM[ , , yb, yd, ay - yd + 1 ]
          
          # Note: updated to include additions
          
          cc_RES_add <- cc_RES_add + cc_yb_yd_a_RES_add[ , , yb, yd, ay - yd + 1 ]
          
        }
        
      }
      
    }
    
    
    # cw
    
    if ( ay - yb + 1 <= ceiling( cw_p ) ){
      
      cw_RES12 <- cw_RES12 + cw_yb_a_RES12[ , , yb, ay - yb + 1 ]
      cw_RES3AB <- cw_RES3AB + cw_yb_a_RES3AB[ , , yb, ay - yb + 1 ]
      cw_RES3CDEF <- cw_RES3CDEF + cw_yb_a_RES3CDEF[ , , yb, ay - yb + 1 ]
      cw_COM <- cw_COM + cw_yb_a_COM[ , , yb, ay - yb + 1 ]
      
      # Note: updated to include additions
      
      cw_RES_add <- cw_RES_add + cw_yb_a_RES_add[ , , yb, ay - yb + 1 ]
      
      # Note: updated to include M&R
      
      cw_RES_rep <- cw_RES_rep + cw_yb_a_RES_rep[ , , yb, ay - yb + 1 ]
      cw_COM_rep <- cw_COM_rep + cw_yb_a_COM_rep[ , , yb, ay - yb + 1 ]
      
    }
    
  }
  
  u_ay_RES12[ , , ay ] <- u_RES12
  u_ay_RES3AB[ , , ay ] <- u_RES3AB
  u_ay_RES3CDEF[ , , ay ] <- u_RES3CDEF
  u_ay_COM[ , , ay ] <- u_COM
  u_ay_cmuRES12[ , , ay ] <- u_cmuRES12
  u_ay_cmuRES3AB[ , , ay ] <- u_cmuRES3AB
  u_ay_cmuRES3CDEF[ , , ay ] <- u_cmuRES3CDEF
  u_ay_cmuCOM[ , , ay ] <- u_cmuCOM
  
  cc_ay_RES12[ , , ay ] <- cc_RES12
  cc_ay_RES3AB[ , , ay ] <- cc_RES3AB
  cc_ay_RES3CDEF[ , , ay ] <- cc_RES3CDEF
  cc_ay_COM[ , , ay ] <- cc_COM
  
  cw_ay_RES12[ , , ay ] <- cw_RES12
  cw_ay_RES3AB[ , , ay ] <- cw_RES3AB
  cw_ay_RES3CDEF[ , , ay ] <- cw_RES3CDEF
  cw_ay_COM[ , , ay ] <- cw_COM
  
  # Note: updated to include additions
  
  u_ay_RES_add[ , , ay ] <- u_RES_add
  u_ay_cmuRES_add[ , , ay ] <- u_cmuRES_add
  
  cc_ay_RES_add[ , , ay ] <- cc_RES_add
  
  cw_ay_RES_add[ , , ay ] <- cw_RES_add
  
  # Note: updated to include M&R
  
  u_ay_RES_rep[ , , ay ] <- u_RES_rep
  u_ay_COM_rep[ , , ay ] <- u_COM_rep
  
  cw_ay_RES_rep[ , , ay ] <- cw_RES_rep
  cw_ay_COM_rep[ , , ay ] <- cw_COM_rep
  
}


# # <- TEMPORARY - calculating avoided emissions
# 
# 
# library( "rjson" )
# 
# loadpath <- paste( pwd, "/Avoided Emissions", sep = "" )
# 
# savepath <- paste( pwd, "/BAIA", sep = "" )
# 
# stageNames <- c( "A1to3", "A4", "A5", "C", "D" )
# 
# # Note: need to update
# 
# case <- "Realistic"
# 
# # case <- "AllWood"
# 
# # case <- "AllConcrete"
# 
# state <- 12
# 
# ap_start <- 1940
# 
# ap_end <- 2017
# 
# ap <- ap_end - ap_start + 1
# 
# 
# ## load data
# 
# MatData_BAIA <- read.csv( paste( loadpath, "/MatData.csv", sep = "" ) )
# 
# MatLevels_BAIA <- MatData_BAIA$Mat_Level # Note: rows represented by MatData_BAIA$ML0, MatData_BAIA$CSI_Spec_1, MatData_BAIA$SCI_Spec_2, MatData_BAIA$Mat_Level
# 
# MainGroups_BAIA <- c(
#   "ExtWalls",
#   "ExtWalls",
#   "ExtWalls",
#   "Windows",
#   "RoofExt",
#   "RoofExt",
#   "RoofExt",
#   "RoofExt",
#   "AtticInt",
#   "IntWalls",
#   "Doors",
#   "Doors",
#   "FloorAndCeil",
#   "FloorAndCeil",
#   "FloorAndCeil",
#   "Foundation",
#   "Foundation",
#   "Foundation",
#   "Foundation"
# )
# 
# Assemblies_BAIA <- c(
#   "ExtWallFinGrp",
#   "ExtWallCoreGrp",
#   "ExtWallIntGrp",
#   "WinGrp",
#   "RoofFinGrp",
#   "RoofDeckGrp",
#   "RoofRadBarGrp",
#   "RoofRaftGrp",
#   "AtticIntInsGrp",
#   "IntWallGrp",
#   "ExtDoorGrp",
#   "IntDoorGrp",
#   "AtticFloorGrp",
#   "LivingCeilingGrp",
#   "LivingFloorGrp",
#   "FoundFooterGrp",
#   "FoundWallGrp",
#   "FoundSlabGrp",
#   "FoundInsGrp"
# )
# 
# # Note: updated to include hazard repairs
# 
# Assemblies_HAZUS <- c(
#   "Structural Framing",
#   "Roof Covering",
#   "Roof Framing",
#   "Exterior Walls",
#   "Interiors"
# )
# 
# # <- Note: only need to run once
# 
# # source( paste( pwd, "/IntegrateBAIA.R" ) )
# 
# # load( paste( pwd, "/Avoided Emissions/huCount_RES.Rda", sep = "" ) )
# 
# load( paste( pwd, "/Avoided Emissions/DamLoss_RES1.Rda", sep = "" ) )
# 
# # ->
# 
# 
# ## create functional unit
# 
# funUnit_A_A1to3 <- 0
# funUnit_A_A4 <- 0
# funUnit_A_A5 <- 0
# funUnit_A_C <- 0
# funUnit_A_D <- 0
# 
# funUnit_B4_A1to3 <- 0
# funUnit_B4_A4 <- 0
# funUnit_B4_A5 <- 0
# funUnit_B4_C <- 0
# funUnit_B4_D <- 0
# 
# funUnit_B6 <- 0
# 
# # Note: updated to define "excess"
# 
# funUnit_excB6_AllOrNothing <- 0
# funUnit_excB6_ApprByMass <- 0
# funUnit_excB6_ApprByPerformance <- 0
# 
# # Note: updated to include hazard repairs
# 
# funUnit_B3_A1to3 <- 0
# funUnit_B3_A4 <- 0
# funUnit_B3_A5 <- 0
# funUnit_B3_C <- 0
# funUnit_B3_D <- 0
# 
# CreateFileName <- function( GBT, j, k, l, m, case = "Realistic" ){
# 
#   if( GBT == 1 ){
# 
#     # Note: updated to simulate cases
# 
#     if( case == "Realistic"){
# 
#       filename <- "Wood"
# 
#     }else{
# 
#       if( case == "AllWood" ){ filename <- "Wood" }
# 
#       if( case == "AllConcrete" ){ filename <- "Concrete" }
# 
#     }
# 
#   }
# 
#   if( GBT == 2 ){
# 
#     # Note: updated to simulate cases
# 
#     if( case == "Realistic"){
# 
#       filename <- "Masonry"
# 
#     }else{
# 
#       if( case == "AllWood" ){ filename <- "Wood" }
# 
#       if( case == "AllConcrete" ){ filename <- "Concrete" }
# 
#     }
# 
#   }
# 
#   if( GBT == 3 ){
# 
#     # Note: updated to simulate cases
# 
#     if( case == "Realistic"){
# 
#       filename <- "Concrete"
# 
#     }else{
# 
#       if( case == "AllWood" ){ filename <- "Wood" }
# 
#       if( case == "AllConcrete" ){ filename <- "Concrete" }
# 
#     }
# 
#   }
# 
# 
#   if( j == 1 ){ filename <- paste( filename, "_One", sep = "" ) }
# 
#   if( j == 2 ){ filename <- paste( filename, "_Two", sep = "" ) }
# 
#   if( j == 3 ){ filename <- paste( filename, "_More", sep = "" ) }
# 
# 
#   if( k == 1 ){ filename <- paste( filename, "_rsgab", sep = "" ) }
# 
#   if( k == 2 ){ filename <- paste( filename, "_rship", sep = "" ) }
# 
# 
#   if( l == 1 ){ filename <- paste( filename, "_rcshg", sep = "" ) }
# 
#   if( l == 2 ){ filename <- paste( filename, "_rccnt", sep = "" ) }
# 
#   if( l == 3 ){ filename <- paste( filename, "_rcmet", sep = "" ) }
# 
# 
#   if( m == 1 ){ filename <- paste( filename, "_walow", sep = "" ) }
# 
#   if( m == 2 ){ filename <- paste( filename, "_wamed", sep = "" ) }
# 
#   if( m == 3 ){ filename <- paste( filename, "_wahig", sep = "" ) }
# 
# 
#   return( filename )
# 
# }
# 
# # Note: updated to define "excess"
# 
# funUnit_excB6_AllOrNothing <- 0
# funUnit_excB6_ApprByMass <- 0
# funUnit_excB6_ApprByPerformance <- 0
# 
# energyFrac <- function( GBT, j, k, l, m, savepath, method = "AllOrNothing" ){
# 
#   energyFrac <- rep( 1, 5 )
# 
#   energy <- fromJSON( file = paste( savepath, "/energy.json", sep = "" ) )
# 
#   energyFrac[ grepl( "Other", energy ) ] <- 0
# 
#   if ( method == "AllOrNothing" ){
# 
#     if( GBT == 1 ){
# 
#       energyFrac <- rep( 0, 5 )
# 
#     }
# 
#   }else{
# 
#     MainGroups <- fromJSON( file = paste( savepath, "/MainGroups.json", sep = "" ) )
# 
#     ExtWallCoreElements <- fromJSON( file = paste( savepath, "/ExtWallCoreElements.json", sep = "" ) )
# 
#     InUse_ExtWallCoreElements <- fromJSON( file = paste( savepath, "/", CreateFileName( GBT, j, k, l, m ), "_ExtWallCoreElements_InUse.json", sep = "" ) )
# 
#     InUse_ExtWallCoreElements <- array( unlist( InUse_ExtWallCoreElements ), dim = c( 47, 1000 ) )
# 
#     AAMEinputs <- fromJSON( file = paste( savepath, "/AAMEinputs.json", sep = "" ) )
# 
#     InUse_AAMEinputs <- fromJSON( file = paste( savepath, "/", CreateFileName( GBT, j, k, l, m ), "_AAMEinputs.json", sep = "" ) )
# 
#     InUse_AAMEinputs <- array( unlist( InUse_AAMEinputs ), dim = c( 30, 1000 ) )
# 
#     if ( method == "ApprByMass" ){
# 
#       mass_MainGroups <- fromJSON( file = paste( savepath, "/", CreateFileName( GBT, j, k, l, m ), "_MainGroups_MassVect.json", sep = "" ) )
# 
#       mass_MainGroups <- array( unlist( mass_MainGroups ), dim = c( 19, 1000 ) )
# 
#       massFrac_MainGroups <- rowMeans( mass_MainGroups[ grepl( "Exterior wall", MainGroups ) | grepl( "Window", MainGroups ) | grepl( "Exterior door", MainGroups ), ] ) / sum( rowMeans( mass_MainGroups[ grepl( "Exterior wall", MainGroups ) | grepl( "Window", MainGroups ) | grepl( "Exterior door", MainGroups ), ] ) )
# 
#       energyFrac <- massFrac_MainGroups[ MainGroups[ grepl( "Exterior wall", MainGroups ) | grepl( "Window", MainGroups ) | grepl( "Exterior door", MainGroups ) ] == "Exterior wall core" ] * energyFrac
# 
#       mass_ExtWallCoreElements <- fromJSON( file = paste( savepath, "/", CreateFileName( GBT, j, k, l, m ), "_ExtWallCoreElements_MassVect.json", sep = "" ) )
# 
#       mass_ExtWallCoreElements <- array( unlist( mass_ExtWallCoreElements ), dim = c( 47, 1000 ) )
# 
#       mass_ExtWallCoreElements <- InUse_ExtWallCoreElements * mass_ExtWallCoreElements
# 
#       if ( GBT == 1 ){
# 
#         ExtWallCoreElements_ <- ExtWallCoreElements[[ 2 ]][ 30:35 ]
# 
#         mass_ExtWallCoreElements_ <- mass_ExtWallCoreElements[ 30:35, ] + mass_ExtWallCoreElements[ 36:41, ] + mass_ExtWallCoreElements[ 42:47, ]
# 
#       }
# 
#       if ( GBT == 2 ){
# 
#         ExtWallCoreElements_ <- ExtWallCoreElements[[ 2 ]][ 1:5 ]
# 
#         mass_ExtWallCoreElements_ <- mass_ExtWallCoreElements[ 1:5, ] + mass_ExtWallCoreElements[ 6:10, ] + mass_ExtWallCoreElements[ 11:15, ]
# 
#       }
# 
#       if ( GBT == 3 ){
# 
#         ExtWallCoreElements_ <- ExtWallCoreElements[[ 2 ]][ 16:19 ]
# 
#         mass_ExtWallCoreElements_ <- mass_ExtWallCoreElements[ 16:19, ]
# 
#         mass_ExtWallCoreElements_[ 1, ] <- mass_ExtWallCoreElements_[ 1, ] - mass_ExtWallCoreElements_[ 2, ] # Note: problem with mass of rigid insulation for icf and precast
# 
#       }
# 
#       massFrac_ExtWallCoreElements <- rowMeans( mass_ExtWallCoreElements_ ) / sum( rowMeans( mass_ExtWallCoreElements_ ) )
# 
#       energyFrac <- sum( massFrac_ExtWallCoreElements[ grepl( "Concrete", ExtWallCoreElements_ ) | grepl( "concrete", ExtWallCoreElements_ ) | grepl( "Mortar", ExtWallCoreElements_ ) | grepl( "mortar", ExtWallCoreElements_ ) ] ) * energyFrac
# 
#     }
# 
#     if ( method == "ApprByPerformance" ){
# 
#       R_MainGroups <- fromJSON( file = paste( savepath, "/", CreateFileName( GBT, j, k, l, m ), "_MainGroups_Rvals.json", sep = "" ) )
# 
#       R_MainGroups <- array( unlist( R_MainGroups ), dim = c( 19, 1000 ) )
# 
#       R_MainGroups[ grepl( "Window", MainGroups ), ] <- 1 / InUse_AAMEinputs[ AAMEinputs[[ 1 ]] == "WinU", ]
# 
#       U_MainGroups <- 1 / R_MainGroups
# 
#       U_MainGroups[ is.na( U_MainGroups ) | !is.finite( U_MainGroups ) ] <- 0
# 
#       # UFrac_MainGroups <- rowMeans( U_MainGroups[ grepl( "Exterior wall", MainGroups ) | grepl( "Window", MainGroups ) | grepl( "Exterior door", MainGroups ), ] ) / sum( rowMeans( U_MainGroups[ grepl( "Exterior wall", MainGroups ) | grepl( "Window", MainGroups ) | grepl( "Exterior door", MainGroups ), ] ) )
#       #
#       # energyFrac <- UFrac_MainGroups[ MainGroups[ grepl( "Exterior wall", MainGroups ) | grepl( "Window", MainGroups ) | grepl( "Exterior door", MainGroups ) ] == "Exterior wall core" ] * energyFrac
# 
#       UA_MainGroups <- rbind( U_MainGroups[ grepl( "Exterior wall core", MainGroups ), ] * InUse_AAMEinputs[ AAMEinputs[[ 1 ]] == "ExtWallArea", ], U_MainGroups[ grepl( "Window", MainGroups ), ] * InUse_AAMEinputs[ AAMEinputs[[ 1 ]] == "WindowArea", ], U_MainGroups[ grepl( "Exterior door", MainGroups ), ] * InUse_AAMEinputs[ AAMEinputs[[ 1 ]] == "ExtDoorArea", ] )
# 
#       UAFrac_MainGroups <- rowMeans( UA_MainGroups ) / sum( rowMeans( UA_MainGroups ) )
# 
#       energyFrac <- UAFrac_MainGroups[ 1 ] * energyFrac
# 
#       R_ExtWallCoreElements <- fromJSON( file = paste( savepath, "/", CreateFileName( GBT, j, k, l, m ), "_ExtWallCoreElements_Rvals.json", sep = "" ) )
# 
#       R_ExtWallCoreElements <- array( unlist( R_ExtWallCoreElements ), dim = c( 47, 1000 ) )
# 
#       R_ExtWallCoreElements <- InUse_ExtWallCoreElements * R_ExtWallCoreElements
# 
#       q_ExtWallCoreElements <- fromJSON( file = paste( savepath, "/", CreateFileName( GBT, j, k, l, m ), "_ExtWallCoreElements_CondTrials.json", sep = "" ) )
# 
#       q_ExtWallCoreElements <- array( unlist( q_ExtWallCoreElements ), dim = c( 47, 1000 ) )
# 
#       q_ExtWallCoreElements <- InUse_ExtWallCoreElements * q_ExtWallCoreElements
# 
#       L_ExtWallCoreElements <- fromJSON( file = paste( savepath, "/", CreateFileName( GBT, j, k, l, m ), "_ExtWallCoreElements_Thickness.json", sep = "" ) )
# 
#       L_ExtWallCoreElements <- array( unlist( L_ExtWallCoreElements ), dim = c( 47, 1000 ) )
# 
#       # L_ExtWallCoreElements <- InUse_ExtWallCoreElements * L_ExtWallCoreElements
# 
#       A_ExtWallCoreElements <- fromJSON( file = paste( savepath, "/", CreateFileName( GBT, j, k, l, m ), "_ExtWallCoreElements_FFArea.json", sep = "" ) )
# 
#       A_ExtWallCoreElements <- array( unlist( A_ExtWallCoreElements ), dim = c( 47, 1000 ) )
# 
#       # A_ExtWallCoreElements <- InUse_ExtWallCoreElements * A_ExtWallCoreElements
# 
#       U_ExtWallCoreElements <- q_ExtWallCoreElements / L_ExtWallCoreElements #( q_ExtWallCoreElements * A_ExtWallCoreElements ) / L_ExtWallCoreElements
# 
#       U_ExtWallCoreElements[ R_ExtWallCoreElements > 0 ] <- 1 / R_ExtWallCoreElements[ R_ExtWallCoreElements > 0 ]
# 
#       U_ExtWallCoreElements[ is.na( U_ExtWallCoreElements ) | !is.finite( U_ExtWallCoreElements ) ] <- 0
# 
#       R_ExtWallCoreElements <- 1 / U_ExtWallCoreElements
# 
#       R_ExtWallCoreElements[ is.na( R_ExtWallCoreElements ) | !is.finite( R_ExtWallCoreElements ) ] <- 0
# 
#       if ( GBT == 1 ){
# 
#         ExtWallCoreElements_ <- ExtWallCoreElements[[ 2 ]][ 30:35 ]
# 
#         U_ExtWallCoreElements_ <- U_ExtWallCoreElements[ 30:35, ] + U_ExtWallCoreElements[ 36:41, ] + U_ExtWallCoreElements[ 42:47, ]
# 
#         R_ExtWallCoreElements_ <- R_ExtWallCoreElements[ 30:35, ] + R_ExtWallCoreElements[ 36:41, ] + R_ExtWallCoreElements[ 42:47, ]
# 
#       }
# 
#       if ( GBT == 2 ){
# 
#         ExtWallCoreElements_ <- ExtWallCoreElements[[ 2 ]][ 1:5 ]
# 
#         U_ExtWallCoreElements_ <- U_ExtWallCoreElements[ 1:5, ] + U_ExtWallCoreElements[ 6:10, ] + U_ExtWallCoreElements[ 11:15, ]
# 
#         R_ExtWallCoreElements_ <- R_ExtWallCoreElements[ 1:5, ] + R_ExtWallCoreElements[ 6:10, ] + R_ExtWallCoreElements[ 11:15, ]
# 
#       }
# 
#       if ( GBT == 3 ){
# 
#         ExtWallCoreElements_ <- ExtWallCoreElements[[ 2 ]][ 16:19 ]
# 
#         U_ExtWallCoreElements_ <- U_ExtWallCoreElements[ 16:19, ]
# 
#         R_ExtWallCoreElements_ <- R_ExtWallCoreElements[ 16:19, ]
# 
#       }
# 
#       # Note: need to update so that U_EFIS = 1 / ( R_EWF + R_EWC + R_EWI ) and U_EWC = 1 / (R_INS + R_OTHER )
# 
#       # U_ExtWallCore <- 1 / colSums( R_ExtWallCoreElements_ )
#       #
#       # R_ExtWallCore <- 1 / U_ExtWallCore
# 
#       UFrac_ExtWallCoreElements <- rowMeans( U_ExtWallCoreElements_ ) / sum( rowMeans( U_ExtWallCoreElements_ ) )
# 
#       if( sum( grepl( "Concrete", ExtWallCoreElements_ ) | grepl( "concrete", ExtWallCoreElements_ ) | grepl( "Mortar", ExtWallCoreElements_ ) | grepl( "mortar", ExtWallCoreElements_ ) ) > 0 ){
# 
#         energyFrac <- sum( UFrac_ExtWallCoreElements[ !grepl( "insulation", ExtWallCoreElements_ ) ] ) * energyFrac
# 
#       }else{
# 
#         energyFrac <- 0 * energyFrac
# 
#       }
# 
#     }
# 
#   }
# 
#   return( energyFrac )
# 
# }
# 
# RunThis <- function( activity, stage, GBT, j, k, l, m, savepath, case = "Realistic" ){
# 
#   filename <- CreateFileName( GBT, j, k, l, m, case )
# 
#   if( GBT == 1 ){
# 
#     # Note: need to update
# 
#     pGBT <- 25 / 100
# 
#   }
# 
#   if( GBT == 2 ){
# 
#     # Note: need to update
# 
#     pGBT <- 70 / 100
# 
#   }
# 
#   if( GBT == 3 ){
# 
#     # Note: need to update
# 
#     pGBT <- 5 / 100
# 
#   }
# 
# 
#   if( j == 1 ){
# 
#     # Note: need to update
# 
#     # if ( GBT == 1 ){ pJ <- 70 / 100 }
#     # 
#     # if ( GBT == 2 ){ pJ <- 83 / 100 }
#     # 
#     # if ( GBT == 3 ){ pJ <- 100 / 100 }
#     
#     pJ <- 44 / 100
#     
#     if ( n == 1 ){ pN <- 0.69 }
#     
#     if ( n == 2 ){ pN <- 0.18 }
#     
#     if ( n == 3 ){ pN <- 0.13 }
# 
#   }
# 
#   if( j == 2 ){
# 
#     # Note: need to update
# 
#     # if ( GBT == 1 ){ pJ <- 30 / 100 }
#     # 
#     # if ( GBT == 2 ){ pJ <- 17 / 100 }
#     # 
#     # if ( GBT == 3 ){ pJ <- 0 / 100 }
#     
#     pJ <- 32 / 100
#     
#     if ( n == 1 ){ pN <- 0.44 }
#     
#     if ( n == 2 ){ pN <- 0.31 }
#     
#     if ( n == 3 ){ pN <- 0.25 }
# 
#   }
# 
#   if( j == 3 ){
# 
#     # Note: need to update
# 
#     # if ( GBT == 1 ){ pJ <- 0 / 100 }
#     # 
#     # if ( GBT == 2 ){ pJ <- 0 / 100 }
#     # 
#     # if ( GBT == 3 ){ pJ <- 0 / 100 }
#     
#     pJ <- 23 / 100
#     
#     if ( n == 1 ){ pN <- 0.28 }
#     
#     if ( n == 2 ){ pN <- 0.30 }
#     
#     if ( n == 3 ){ pN <- 0.42 }
# 
#   }
# 
# 
#   if( k == 1 ){
# 
#     # Note: need to update
# 
#     if ( GBT == 1 ){ pK <- 62 / 100 }
# 
#     if ( GBT == 2 ){ pK <- 62 / 100 }
# 
#     if ( GBT == 3 ){ pK <- 62 / 100 } # Note: assumed same as masonry
# 
#   }
# 
#   if( k == 2 ){
# 
#     # Note: need to update
# 
#     if ( GBT == 1 ){ pK <- 38 / 100 }
# 
#     if ( GBT == 2 ){ pK <- 38 / 100 }
# 
#     if ( GBT == 3 ){ pK <- 38 / 100 }
# 
#   }
# 
# 
#   if( l == 1 ){
# 
#     # Note: need to update
# 
#     if ( GBT == 1 ){ pL <- 100 / 100 }
# 
#     if ( GBT == 2 ){ pL <- 100 / 100 }
# 
#     if ( GBT == 3 ){ pL <- 100 / 100 } # Note: assumed same as masonry
# 
#   }
# 
#   if( l == 2 ){
# 
#     # Note: need to update
# 
#     if ( GBT == 1 ){ pL <- 0 / 100 }
# 
#     if ( GBT == 2 ){ pL <- 0 / 100 }
# 
#     if ( GBT == 3 ){ pL <- 0 / 100 }
# 
#   }
# 
#   if( l == 3 ){
# 
#     # Note: need to update
# 
#     if ( GBT == 1 ){ pL <- 0 / 100 }
# 
#     if ( GBT == 2 ){ pL <- 0 / 100 }
# 
#     if ( GBT == 3 ){ pL <- 0 / 100 }
# 
#   }
# 
# 
#   if( m == 1 ){
# 
#     # Note: need to update
# 
#     if ( GBT == 1 ){ pM <- 100 / 100 }
# 
#     if ( GBT == 2 ){ pM <- 100 / 100 }
# 
#     if ( GBT == 3 ){ pM <- 100 / 100 } # Note: assumed same as masonry
# 
#   }
# 
#   if( m == 2 ){
# 
#     # Note: need to update
# 
#     if ( GBT == 1 ){ pM <- 0 / 100 }
# 
#     if ( GBT == 2 ){ pM <- 0 / 100 }
# 
#     if ( GBT == 3 ){ pM <- 0 / 100 }
# 
#   }
# 
#   if( m == 3 ){
# 
#     # Note: need to update
# 
#     if ( GBT == 1 ){ pM <- 0 / 100 }
# 
#     if ( GBT == 2 ){ pM <- 0 / 100 }
# 
#     if ( GBT == 3 ){ pM <- 0 / 100 }
# 
#   }
# 
# 
#   # Note: no column or row names in csvs
# 
#   if( activity == "A" ){
# 
#     # avg_CSV <- read.csv( paste( savepath, "/", filename, "_GlobWarm_", stageNames[ stage ], "_mean.csv", sep = "" ) )
# 
#     res_JSON <- fromJSON( file = paste( savepath, "/", filename, "_GlobWarm_", stageNames[ stage ], ".json", sep = "" ) )
# 
#     res_JSON <- array( unlist( res_JSON ), dim = c( 1000, 19, 347 ) )
# 
#     avg_JSON <- t( colMeans( res_JSON ) )
# 
#   }
# 
#   if( activity == "B4" ){
# 
#     # avg_CSV <- read.csv( paste( savepath, "/", filename, "_repGlobWarm_", stageNames[ stage ], "_mean.csv", sep = "" ) )
# 
#     res_JSON <- fromJSON( file = paste( savepath, "/", filename, "_repGlobWarm_", stageNames[ stage ], ".json", sep = "" ) )
# 
#     res_JSON <- array( unlist( res_JSON ), dim = c( 1000, 19, 347 ) )
# 
#     avg_JSON <- t( colMeans( res_JSON ) )
# 
#   }
# 
#   if( activity == "B6" ){
# 
#     # avg_CSV <- read.csv( paste( savepath, "/", filename, "_energyGlobWarm_mean.csv", sep = "" ) )
# 
#     res_JSON <- fromJSON( file = paste( savepath, "/", filename, "_energyGlobWarm.json", sep = "" ) )
# 
#     res_JSON <- array( unlist( res_JSON ), dim = c( 5, 1000 ) )
# 
#     avg_JSON <- rowMeans( res_JSON )
# 
#   }
# 
#   # Note: updated to include hazard repairs
# 
#   if( activity == "B3" ){
# 
#     # avg_CSV <- read.csv( paste( savepath, "/", filename, "_GlobWarm_", stageNames[ stage ], "_mean.csv", sep = "" ) )
#     #
#     # # Note: replacement costs 25% higher than initial construction costs; assumed similar for impacts
#     #
#     # corr_B3 <- 1.25
#     #
#     # # Note: assumed material items that are never replaced are never repaired either
#     #
#     # filt <- read.csv( paste( savepath, "/", filename, "_repGlobWarm_", stageNames[ stage ], "_mean.csv", sep = "" ) ) > 0
#     #
#     # avg_CSV <- avg_CSV * filt
#     #
#     # # huCount_RES[ "RES1", , ] # SBT per year
#     #
#     # avg_CSV <- corr_B3 * cbind(
#     #   avg_CSV[ , Assemblies_BAIA == "ExtWallCoreGrp" ] * sum( DamLoss_RES1[[ filename ]]$EAL$sub$SF * DamLoss_RES1$n / sum( DamLoss_RES1$n ) ),
#     #   rowSums( avg_CSV[ , Assemblies_BAIA %in% c( "RoofFinGrp", "RoofRadBarGrp", "AtticIntInsGrp" ) ] ) * sum( DamLoss_RES1[[ filename ]]$EAL$sub$RC * DamLoss_RES1$n / sum( DamLoss_RES1$n ) ),
#     #   rowSums( avg_CSV[ , Assemblies_BAIA %in% c( "RoofDeckGrp", "RoofRaftGrp" ) ] ) * sum( DamLoss_RES1[[ filename ]]$EAL$sub$RF * DamLoss_RES1$n / sum( DamLoss_RES1$n ) ),
#     #   rowSums( avg_CSV[ , Assemblies_BAIA %in% c( "ExtWallFinGrp", "WinGrp", "ExtDoorGrp" ) ] ) * sum( DamLoss_RES1[[ filename ]]$EAL$sub$EW * DamLoss_RES1$n / sum( DamLoss_RES1$n ) ),
#     #   rowSums( avg_CSV[ , Assemblies_BAIA %in% c( "ExtWallIntGrp", "IntWallGrp", "IntDoorGrp", "AtticFloorGrp", "LivingCeilingGrp", "LivingFloorGrp" ) ] ) * sum( DamLoss_RES1[[ filename ]]$EAL$sub$Is * DamLoss_RES1$n / sum( DamLoss_RES1$n ) )
#     # )
# 
#     res_JSON <- fromJSON( file = paste( savepath, "/", filename, "_GlobWarm_", stageNames[ stage ], ".json", sep = "" ) )
# 
#     res_JSON <- array( unlist( res_JSON ), dim = c( 1000, 19, 347 ) )
# 
#     avg_JSON <- t( colMeans( res_JSON ) )
# 
#     # Note: replacement costs 25% higher than initial construction costs; assumed similar for impacts
# 
#     corr_B3 <- 1.25
# 
#     # Note: assumed material items that are never replaced are never repaired either
# 
#     # filt <- read.csv( paste( savepath, "/", filename, "_repGlobWarm_", stageNames[ stage ], "_mean.csv", sep = "" ) ) > 0
# 
#     res_repJSON <- fromJSON( file = paste( savepath, "/", filename, "_repGlobWarm_", stageNames[ stage ], ".json", sep = "" ) )
# 
#     res_repJSON <- array( unlist( res_JSON ), dim = c( 1000, 19, 347 ) )
# 
#     avg_repJSON <- t( colMeans( res_JSON ) )
# 
#     filt <- avg_repJSON > 0
# 
#     avg_JSON <- avg_JSON * filt
# 
#     # huCount_RES[ "RES1", , ] # SBT per year
# 
#     avg_JSON <- corr_B3 * cbind(
#       avg_JSON[ , Assemblies_BAIA == "ExtWallCoreGrp" ] * sum( DamLoss_RES1[[ filename ]]$EAL$sub$SF * DamLoss_RES1$n / sum( DamLoss_RES1$n ) ),
#       rowSums( avg_JSON[ , Assemblies_BAIA %in% c( "RoofFinGrp", "RoofRadBarGrp", "AtticIntInsGrp" ) ] ) * sum( DamLoss_RES1[[ filename ]]$EAL$sub$RC * DamLoss_RES1$n / sum( DamLoss_RES1$n ) ),
#       rowSums( avg_JSON[ , Assemblies_BAIA %in% c( "RoofDeckGrp", "RoofRaftGrp" ) ] ) * sum( DamLoss_RES1[[ filename ]]$EAL$sub$RF * DamLoss_RES1$n / sum( DamLoss_RES1$n ) ),
#       rowSums( avg_JSON[ , Assemblies_BAIA %in% c( "ExtWallFinGrp", "WinGrp", "ExtDoorGrp" ) ] ) * sum( DamLoss_RES1[[ filename ]]$EAL$sub$EW * DamLoss_RES1$n / sum( DamLoss_RES1$n ) ),
#       rowSums( avg_JSON[ , Assemblies_BAIA %in% c( "ExtWallIntGrp", "IntWallGrp", "IntDoorGrp", "AtticFloorGrp", "LivingCeilingGrp", "LivingFloorGrp" ) ] ) * sum( DamLoss_RES1[[ filename ]]$EAL$sub$Is * DamLoss_RES1$n / sum( DamLoss_RES1$n ) )
#     )
# 
#   }
# 
#   p <- pGBT * pJ * pK * pL *pM
# 
#   # return( p * avg_CSV )
# 
#   return( p * avg_JSON )
# 
# }
# 
# for ( GBT in seq( 1, 3 ) ){
# 
#   for ( j in seq( 1, 3 ) ){
# 
#     for ( k in seq( 1, 2 ) ){
# 
#       for ( l in seq( 1, 3 ) ){
# 
#         # Note: need to update
# 
#         m <- 1
# 
#         # for ( m in seq( 1, 3 ) ){
# 
#         if ( case == "Realistic" ){
# 
#           funUnit_A_A1to3 <- funUnit_A_A1to3 + RunThis( activity = "A", stage = 1, GBT, j, k, l, m, savepath )
#           funUnit_A_A4 <- funUnit_A_A4 + RunThis( activity = "A", stage = 2, GBT, j, k, l, m, savepath )
#           funUnit_A_A5 <- funUnit_A_A5 + RunThis( activity = "A", stage = 3, GBT, j, k, l, m, savepath )
#           funUnit_A_C <- funUnit_A_C + RunThis( activity = "A", stage = 4, GBT, j, k, l, m, savepath )
#           funUnit_A_D <- funUnit_A_D + RunThis( activity = "A", stage = 5, GBT, j, k, l, m, savepath )
# 
#           funUnit_B4_A1to3 <- funUnit_B4_A1to3 + RunThis( activity = "B4", stage = 1, GBT, j, k, l, m, savepath ) / 50
#           funUnit_B4_A4 <- funUnit_B4_A4 + RunThis( activity = "B4", stage = 2, GBT, j, k, l, m, savepath ) / 50
#           funUnit_B4_A5 <- funUnit_B4_A5 + RunThis( activity = "B4", stage = 3, GBT, j, k, l, m, savepath ) / 50
#           funUnit_B4_C <- funUnit_B4_C + RunThis( activity = "B4", stage = 4, GBT, j, k, l, m, savepath ) / 50
#           funUnit_B4_D <- funUnit_B4_D + RunThis( activity = "B4", stage = 5, GBT, j, k, l, m, savepath ) / 50
# 
#           funUnit_B6 <- funUnit_B6 + RunThis( activity = "B6", stage = NA, GBT, j, k, l, m, savepath )
# 
#           # Note: updated to define "excess"
# 
#           # if( GBT == 2 | GBT == 3 ){
#           #
#           #   funUnit_excB6_AllOrNothing <- funUnit_excB6_AllOrNothing + RunThis( activity = "B6", stage = NA, GBT, j, k, l, m, savepath ) # Note: only masonry and concrete
#           #
#           # }
# 
#           funUnit_excB6_AllOrNothing <- funUnit_excB6_AllOrNothing + energyFrac( GBT, j, k, l, m, savepath, method = "AllOrNothing" ) * RunThis( activity = "B6", stage = NA, GBT, j, k, l, m, savepath )
#           funUnit_excB6_ApprByMass <- funUnit_excB6_ApprByMass + energyFrac( GBT, j, k, l, m, savepath, method = "ApprByMass" ) * RunThis( activity = "B6", stage = NA, GBT, j, k, l, m, savepath )
#           funUnit_excB6_ApprByPerformance <- funUnit_excB6_ApprByPerformance + energyFrac( GBT, j, k, l, m, savepath, method = "ApprByPerformance" ) * RunThis( activity = "B6", stage = NA, GBT, j, k, l, m, savepath )
# 
#           # Note: updated to include hazard repairs
# 
#           funUnit_B3_A1to3 <- funUnit_B3_A1to3 + RunThis( activity = "B3", stage = 1, GBT, j, k, l, m, savepath )
#           funUnit_B3_A4 <- funUnit_B3_A4 + RunThis( activity = "B3", stage = 2, GBT, j, k, l, m, savepath )
#           funUnit_B3_A5 <- funUnit_B3_A5 + RunThis( activity = "B3", stage = 3, GBT, j, k, l, m, savepath )
#           funUnit_B3_C <- funUnit_B3_C + RunThis( activity = "B3", stage = 4, GBT, j, k, l, m, savepath )
#           funUnit_B3_D <- funUnit_B3_D + RunThis( activity = "B3", stage = 5, GBT, j, k, l, m, savepath )
# 
#         }else{
# 
#           funUnit_A_A1to3 <- funUnit_A_A1to3 + RunThis( activity = "A", stage = 1, GBT, j, k, l, m, savepath, case = case )
#           funUnit_A_A4 <- funUnit_A_A4 + RunThis( activity = "A", stage = 2, GBT, j, k, l, m, savepath, case = case )
#           funUnit_A_A5 <- funUnit_A_A5 + RunThis( activity = "A", stage = 3, GBT, j, k, l, m, savepath, case = case )
#           funUnit_A_C <- funUnit_A_C + RunThis( activity = "A", stage = 4, GBT, j, k, l, m, savepath, case = case )
#           funUnit_A_D <- funUnit_A_D + RunThis( activity = "A", stage = 5, GBT, j, k, l, m, savepath, case = case )
# 
#           funUnit_B4_A1to3 <- funUnit_B4_A1to3 + RunThis( activity = "B4", stage = 1, GBT, j, k, l, m, savepath, case = case ) / 50
#           funUnit_B4_A4 <- funUnit_B4_A4 + RunThis( activity = "B4", stage = 2, GBT, j, k, l, m, savepath, case = case ) / 50
#           funUnit_B4_A5 <- funUnit_B4_A5 + RunThis( activity = "B4", stage = 3, GBT, j, k, l, m, savepath, case = case ) / 50
#           funUnit_B4_C <- funUnit_B4_C + RunThis( activity = "B4", stage = 4, GBT, j, k, l, m, savepath, case = case ) / 50
#           funUnit_B4_D <- funUnit_B4_D + RunThis( activity = "B4", stage = 5, GBT, j, k, l, m, savepath, case = case ) / 50
# 
#           funUnit_B6 <- funUnit_B6 + RunThis( activity = "B6", stage = NA, GBT, j, k, l, m, savepath, case = case )
# 
#           # Note: updated to define "excess"
# 
#           # if( GBT == 2 | GBT == 3 ){
#           #
#           #   funUnit_excB6_AllOrNothing <- funUnit_excB6_AllOrNothing + RunThis( activity = "B6", stage = NA, GBT, j, k, l, m, savepath, case = case ) # Note: only masonry and concrete
#           #
#           # }
# 
#           funUnit_excB6_AllOrNothing <- funUnit_excB6_AllOrNothing + energyFrac( GBT, j, k, l, m, savepath, method = "AllOrNothing" ) * RunThis( activity = "B6", stage = NA, GBT, j, k, l, m, savepath, case = case )
#           funUnit_excB6_ApprByMass <- funUnit_excB6_ApprByMass + energyFrac( GBT, j, k, l, m, savepath, method = "ApprByMass" ) * RunThis( activity = "B6", stage = NA, GBT, j, k, l, m, savepath, case = case )
#           funUnit_excB6_ApprByPerformance <- funUnit_excB6_ApprByPerformance + energyFrac( GBT, j, k, l, m, savepath, method = "ApprByPerformance" ) * RunThis( activity = "B6", stage = NA, GBT, j, k, l, m, savepath, case = case )
# 
#           # Note: updated to include hazard repairs
# 
#           funUnit_B3_A1to3 <- funUnit_B3_A1to3 + RunThis( activity = "B3", stage = 1, GBT, j, k, l, m, savepath, case = "AllWood" )
#           funUnit_B3_A4 <- funUnit_B3_A4 + RunThis( activity = "B3", stage = 2, GBT, j, k, l, m, savepath, case = "AllWood" )
#           funUnit_B3_A5 <- funUnit_B3_A5 + RunThis( activity = "B3", stage = 3, GBT, j, k, l, m, savepath, case = "AllWood" )
#           funUnit_B3_C <- funUnit_B3_C + RunThis( activity = "B3", stage = 4, GBT, j, k, l, m, savepath, case = "AllWood" )
#           funUnit_B3_D <- funUnit_B3_D + RunThis( activity = "B3", stage = 5, GBT, j, k, l, m, savepath, case = "AllWood" )
# 
#         }
# 
#         # }
# 
#       }
# 
#     }
# 
#   }
# 
# }
# 
# 
# ## define "streams"
# 
# # Note: need to update
# 
# if ( case == "Realistic" ){
# 
#   IF_cement <- 0.883 # kg CO2e / kg cement
# 
#   # Note: updated to include additions
# 
#   frac_RES_add <- valid_RES12_[ states == state, ] / ( valid_RES12_[ states == state, ] + valid_RES3AB_[ states == state, ] )
# 
#   streams_A_A1to3 <- IF_cement * ( 0.98 * 0.97 + 0.85 * 0.03 ) * ( valid_RES12_[ states == state, ] + frac_RES_add * valid_RES_add_[ states == state, ] ) / sum( rowSums( funUnit_A_A1to3 ) * as.numeric( gsub( "%", "", MatData_BAIA$X.CO2e.cement ) ) / 100 )
# 
#   streams_A_A4 <- streams_A_A1to3
# 
#   streams_A_A5 <- streams_A_A1to3
# 
#   for ( i in 1:ap ){
# 
#     if ( i == 1 ){
# 
#       streams_A_C <- c( rep( 0, i - 1 ), ( 1 - sr[ 1:( ap - i + 1 ) ] ) ) * streams_A_A1to3[ i ]
# 
#       streams_B4_A1to3 <- c( rep( 0, i ), ( sr[ 2:( ap - i + 1 ) ] ) ) * streams_A_A1to3[ i ]
# 
#       streams_B6 <- c( rep( 0, i ), ( sr[ 2:( ap - i + 1 ) ] ) ) * streams_A_A1to3[ i ]
# 
#       # Note: updated to include hazard repairs
# 
#       streams_B3_A1to3 <- c( rep( 0, i ), ( sr[ 2:( ap - i + 1 ) ] ) ) * streams_A_A1to3[ i ]
# 
#     }else{
# 
#       streams_A_C <- rbind( streams_A_C, c( rep( 0, i - 1 ), ( 1 - sr[ 1:( ap - i + 1 ) ] ) ) * streams_A_A1to3[ i ] )
# 
#       if ( i == ap ){
# 
#         streams_B4_A1to3 <- rbind( streams_B4_A1to3, rep( 0, i ) * streams_A_A1to3[ i ] )
# 
#         streams_B6 <- rbind( streams_B6, rep( 0, i ) * streams_A_A1to3[ i ] )
# 
#         # Note: updated to include hazard repairs
# 
#         streams_B3_A1to3 <- rbind( streams_B3_A1to3, rep( 0, i ) * streams_A_A1to3[ i ] )
# 
#       }else{
# 
#         streams_B4_A1to3 <- rbind( streams_B4_A1to3, c( rep( 0, i ), ( sr[ 2:( ap - i + 1 ) ] ) ) * streams_A_A1to3[ i ] )
# 
#         streams_B6 <- rbind( streams_B6, c( rep( 0, i ), ( sr[ 2:( ap - i + 1 ) ] ) ) * streams_A_A1to3[ i ] )
# 
#         # Note: updated to include hazard repairs
# 
#         streams_B3_A1to3 <- rbind( streams_B3_A1to3, c( rep( 0, i ), ( sr[ 2:( ap - i + 1 ) ] ) ) * streams_A_A1to3[ i ] )
# 
#       }
# 
#     }
# 
#   }
# 
#   streams_B4_A4 <- streams_B4_A1to3
# 
#   streams_B4_A5 <- streams_B4_A1to3
# 
#   # Note: updated to include hazard repairs
# 
#   streams_B3_A4 <- streams_B3_A1to3
# 
#   streams_B3_A5 <- streams_B3_A1to3
# 
#   for ( i in 1:ap ){
# 
#     if ( i == 1 ){
# 
#       streams_B4_C <- c( rep( 0, i ), ( 1 - sr[ 2:( ap - i + 1 ) ] ) ) * sum( streams_B4_A1to3[ , i ] )
# 
#       # Note: updated to include hazard repairs
# 
#       streams_B3_C <- c( rep( 0, i ), ( 1 - sr[ 2:( ap - i + 1 ) ] ) ) * sum( streams_B3_A1to3[ , i ] )
# 
#     }else{
# 
#       if ( i == ap ){
# 
#         streams_B4_C <- rbind( streams_B4_C, rep( 0, i ) * sum( streams_B4_A1to3[ , i ] ) )
# 
#         # Note: updated to include hazard repairs
# 
#         streams_B3_C <- rbind( streams_B3_C, rep( 0, i ) * sum( streams_B3_A1to3[ , i ] ) )
# 
#       }else{
# 
#         streams_B4_C <- rbind( streams_B4_C, c( rep( 0, i ), ( 1 - sr[ 2:( ap - i + 1 ) ] ) ) * sum( streams_B4_A1to3[ , i ] ) )
# 
#         # Note: updated to include hazard repairs
# 
#         streams_B3_C <- rbind( streams_B3_C, c( rep( 0, i ), ( 1 - sr[ 2:( ap - i + 1 ) ] ) ) * sum( streams_B3_A1to3[ , i ] ) )
# 
#       }
# 
#     }
# 
#   }
# 
# }
# 
# 
# ## calculate "flows"
# 
# # streams_A_A1to3 * sum( rowSums( funUnit_A_A1to3 ) * ( MatData_BAIA$Cement. >= 1 ) )
# # streams_A_A4 * sum( rowSums( funUnit_A_A4 ) * ( MatData_BAIA$Cement. >= 1 ) )
# # streams_A_A5 * sum( rowSums( funUnit_A_A5 ) * ( MatData_BAIA$Cement. >= 1 ) )
# # streams_A_C * sum( rowSums( funUnit_A_C ) * ( MatData_BAIA$Cement. >= 1 ) )
# #
# # streams_B4_A1to3 * sum( rowSums( funUnit_B4_A1to3 ) * ( MatData_BAIA$Cement. >= 1 ) )
# # streams_B4_A4 * sum( rowSums( funUnit_B4_A4 ) * ( MatData_BAIA$Cement. >= 1 ) )
# # streams_B4_A5 * sum( rowSums( funUnit_B4_A5 ) * ( MatData_BAIA$Cement. >= 1 ) )
# # streams_B4_C * sum( rowSums( funUnit_B4_C ) * ( MatData_BAIA$Cement. >= 1 ) )
# #
# # streams_B6 * sum( funUnit_B6[ c( 1, 2, 4 ), 1 ] )
# #
# # # Note: updated to define "excess"
# #
# # streams_B6 * sum( funUnit_excB6_AllOrNothing[ c( 1, 2, 4 ), 1 ] ) # Note: only cooling/heating for masonry and concrete
# 
# 
# ## map "flows" onto scopes
# 
# scope_nos <- c( rep( 1, 3 ), rep( 2, 1 ), rep( 3, 15 ) )
# 
# cat_nos <- c( rep( NA, 4 ), rep( 1, 2 ), rep( 2, 1 ), rep( 3, 3 ), rep( 4, 2 ), rep( 9, 1 ), rep( 10, 2 ), rep( 11, 2 ), rep( 12, 2 ) )
# 
# cats <- c(
#   "Calcination and combustion of raw materials",
#   "Combustion of kiln fuels",
#   "Combustion of non kiln fuels",
#   "Combustion of fuels for purchased electricity",
#   "Cradle-to-gate production of raw materials",
#   "Cradle-to-gate production of other purchased goods",
#   "Cradle-to-gate production of capital goods (attributed)",
#   "Cradle-to-gate production of purchased fuels",
#   "Cradle-to-gate production of purchased electricity",
#   "T&D losses",
#   "Upstream T&D of raw materials",
#   "Upstream T&D of other purchased goods",
#   "Downstream T&D of CBPs",
#   "Processing of cement into CBPs",
#   "Installation of CBPs in buildings",
#   "Carbon uptake during building use",
#   "Excess operational energy usage",
#   "Carbon uptake at building end-of-life",
#   "End-of-life of CBPs"
# )
# 
# A_ests <- array( rep( 0, ap * ap * length( cats ) ), dim = c( ap, ap, length( cats ) ) )
# 
# B4_ests <- array( rep( 0, ap * ap * length( cats ) ), dim = c( ap, ap, length( cats ) ) )
# 
# frac_scope1 <- c( 54.98, 30.60, 0.14, 4.86, 4.70, 0.50, 0.36, 2.87, 0.77, 0.16, 0.05, 0.01 ) / 100
# 
# # Note: updated to include B3
# 
# B3_ests <- array( rep( 0, ap * ap * length( cats ) ), dim = c( ap, ap, length( cats ) ) )
# 
# # Note: updated to simulate cases
# 
# corr_CU <- streams_A_A1to3 * sum( rowSums( funUnit_A_A1to3 ) * as.numeric( gsub( "%", "", MatData_BAIA$X.CO2e.cement ) ) / 100 ) / ( IF_cement * ( 0.98 * 0.97 + 0.85 * 0.03 ) * ( valid_RES12_[ states == state, ] + frac_RES_add * valid_RES_add_[ states == state, ] ) )
# 
# for ( k in 1:length( frac_scope1 ) ){
# 
#   for ( j in 1:ap ){
# 
#     A_ests[ j, j, k ] <- ( 1.02 * 0.97 + 1.15 * 0.03 ) * frac_scope1[ k ] * corr_CU[ j ] * streams_A_A1to3[ j ] * sum( rowSums( funUnit_A_A1to3 ) * as.numeric( gsub( "%", "", MatData_BAIA$X.CO2e.cement ) ) / 100 ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) # Note: updated to include cw
# 
#   }
# 
#   B4_ests[ , , k ] <- ( 1.02 * 0.97 + 1.15 * 0.03 ) * frac_scope1[ k ] * corr_CU[ j ] * streams_B4_A1to3 * sum( rowSums( funUnit_B4_A1to3 ) * as.numeric( gsub( "%", "", MatData_BAIA$X.CO2e.cement ) ) / 100 ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) # Note: updated to include cw
# 
#   # Note: updated to include B3
# 
#   B3_ests[ , , k ] <- ( 1.02 * 0.97 + 1.15 * 0.03 ) * frac_scope1[ k ] * corr_CU[ j ] * streams_B3_A1to3 * sum( rowSums( funUnit_B3_A1to3 ) * as.numeric( gsub( "%", "", MatData_BAIA$X.CO2e.cement ) ) / 100 ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) # Note: updated to include cw
# 
# }
# 
# for ( j in 1:ap ){
# 
#   A_ests[ j, j, 13 ] <- corr_CU[ j ] * streams_A_A4[ j ] * sum( rowSums( funUnit_A_A4 ) * ( MatData_BAIA$Cement. >= 1 ) )
# 
#   A_ests[ j, j, 14 ] <- corr_CU[ j ] * streams_A_A1to3[ j ] * sum( rowSums( funUnit_A_A1to3 ) * ( MatData_BAIA$Cement. >= 1 ) * ( 1 - as.numeric( gsub( "%", "", MatData_BAIA$X.CO2e.cement ) ) / 100 ) )
# 
#   A_ests[ j, j, 15 ] <- corr_CU[ j ] * streams_A_A5[ j ] * sum( rowSums( funUnit_A_A5 ) * ( MatData_BAIA$Cement. >= 1 ) )
# 
# }
# 
# # A_ests[ , , 17 ] <- corr_CU * streams_B6 * sum( funUnit_excB6_AllOrNothing[ c( 1, 2, 4 ), 1 ] ) # Note: only cooling/heating for masonry and concrete
# 
# A_ests[ , , 19 ] <- corr_CU * streams_A_C * sum( rowSums( funUnit_A_C ) * ( MatData_BAIA$Cement. >= 1 ) )
# 
# B4_ests[ , , 13 ] <- corr_CU * streams_B4_A4 * sum( rowSums( funUnit_B4_A4 ) * ( MatData_BAIA$Cement. >= 1 ) )
# 
# B4_ests[ , , 14 ] <- corr_CU * streams_B4_A1to3 * sum( rowSums( funUnit_B4_A1to3 ) * ( MatData_BAIA$Cement. >= 1 ) * ( 1 - as.numeric( gsub( "%", "", MatData_BAIA$X.CO2e.cement ) ) / 100 ) )
# 
# B4_ests[ , , 15 ] <- corr_CU * streams_B4_A5 * sum( rowSums( funUnit_B4_A5 ) * ( MatData_BAIA$Cement. >= 1 ) )
# 
# B4_ests[ , , 19 ] <- corr_CU * streams_B4_C * sum( rowSums( funUnit_B4_C ) * ( MatData_BAIA$Cement. >= 1 ) )
# 
# # Note: updated to include B3
# 
# B3_ests[ , , 13 ] <- corr_CU * streams_B3_A4 * sum( rowSums( funUnit_B3_A4 ) * ( MatData_BAIA$Cement. >= 1 ) )
# 
# B3_ests[ , , 14 ] <- corr_CU * streams_B3_A1to3 * sum( rowSums( funUnit_B3_A1to3 ) * ( MatData_BAIA$Cement. >= 1 ) * ( 1 - as.numeric( gsub( "%", "", MatData_BAIA$X.CO2e.cement ) ) / 100 ) )
# 
# B3_ests[ , , 15 ] <- corr_CU * streams_B3_A5 * sum( rowSums( funUnit_B3_A5 ) * ( MatData_BAIA$Cement. >= 1 ) )
# 
# B3_ests[ , , 19 ] <- corr_CU * streams_B3_C * sum( rowSums( funUnit_B3_C ) * ( MatData_BAIA$Cement. >= 1 ) )
# 
# 
# A_ests[ , , 17 ] <- corr_CU * streams_B6 * sum( funUnit_excB6_AllOrNothing ) # Note: only cooling/heating for masonry and concrete
# 
# # funUnit_excB6_ApprByMass # Note: add this
# #
# # funUnit_excB6_ApprByPerformance # Note: add this
# 
# 
# ## map carbon uptake onto scopes
# 
# # Note: updated to include repairs
# 
# frac_RES_rep <- colSums( corr_CU * streams_B4_A1to3 * sum( rowSums( funUnit_B4_A1to3 ) * as.numeric( gsub( "%", "", MatData_BAIA$X.CO2e.cement ) ) / 100 ) ) / ( IF_cement * ( 0.98 * 0.97 + 0.85 * 0.03 ) * valid_RES_rep_[ states == state, ] )
# 
# # Note: updated to include B3
# 
# frac_B3 <- colSums( corr_CU * streams_B3_A1to3 * sum( rowSums( funUnit_B3_A1to3 ) * as.numeric( gsub( "%", "", MatData_BAIA$X.CO2e.cement ) ) / 100 ) ) / ( IF_cement * ( 0.98 * 0.97 + 0.85 * 0.03 ) * valid_RES_rep_[ states == state, ] )
# 
# if( case == "Realistic" ){
# 
#   u_yb_a_RES12_ <- u_yb_a_RES12
# 
#   u_yb_a_RES_add_ <- u_yb_a_RES_add
# 
#   u_yb_a_RES_rep_ <- u_yb_a_RES_rep
# 
# }else{
# 
#   if( case == "AllWood" ){
# 
#     # Note: only sf, f, pm, pc, bw
# 
#     sel <- c( grep( "f_", colnames( sa ) ), grep( "pm_", colnames( sa ) ), grep( "pc_", colnames( sa ) ), grep( "bw_", colnames( sa ) ) )
# 
#   }
# 
#   if( case == "AllConcrete" ){
# 
#     # Note: all except ew, iw
# 
#     sel <- c( grep( "ew_", colnames( sa ) ), grep( "iw_", colnames( sa ) ) )
# 
#   }
# 
#   u_yb_a_RES12_ <- u_yb_a_RES12
# 
#   u_yb_a_RES_add_ <- u_yb_a_RES_add
# 
#   u_yb_a_RES_rep_ <- u_yb_a_RES_rep
# 
#   u_yb_a_RES12_[ , sel, , ] <- 0
# 
#   u_yb_a_RES_add_[ , sel, , ] <- 0
# 
#   u_yb_a_RES_rep_[ , sel, , ] <- 0
# 
#   for( yb in 1:dim( u_yb_a_RES12 )[ 3 ] ){
# 
#     for ( a in 1:dim( u_yb_a_RES12 )[ 4 ] ){
# 
#       corr_CU_RES12 <- rowSums( u_yb_a_RES12[ , , yb, a ], na.rm = TRUE ) / rowSums( u_yb_a_RES12_[ , , yb, a ], na.rm = TRUE )
# 
#       corr_CU_RES_add <- rowSums( u_yb_a_RES_add[ , , yb, a ], na.rm = TRUE ) / rowSums( u_yb_a_RES_add_[ , , yb, a ], na.rm = TRUE )
# 
#       corr_CU_RES_rep <- rowSums( u_yb_a_RES_rep[ , , yb, a ], na.rm = TRUE ) / rowSums( u_yb_a_RES_rep_[ , , yb, a ], na.rm = TRUE )
# 
#       corr_CU_RES12[ is.na( corr_CU_RES12 ) | !is.finite( corr_CU_RES12 ) ] <- 0
# 
#       corr_CU_RES_add[ is.na( corr_CU_RES_add ) | !is.finite( corr_CU_RES_add ) ] <- 0
# 
#       corr_CU_RES_rep[ is.na( corr_CU_RES_rep ) | !is.finite( corr_CU_RES_rep ) ] <- 0
# 
#       u_yb_a_RES12_[ , , yb, a ] <- corr_CU_RES12 * u_yb_a_RES12_[ , , yb, a ]
# 
#       u_yb_a_RES_add_[ , , yb, a ] <- corr_CU_RES_add * u_yb_a_RES_add_[ , , yb, a ]
# 
#       u_yb_a_RES_rep_[ , , yb, a ] <- corr_CU_RES_rep * u_yb_a_RES_rep_[ , , yb, a ]
# 
#     }
# 
#   }
# 
# }
# 
# for ( i in 1:ap ){
# 
#   # Note: updated to include additions; updated to include repairs
# 
#   if ( i < ap ){
# 
#     A_ests[ i, , 16 ] <- c( rep( 0, i - 1 ), corr_CU[ i ] * colSums( u_yb_a_RES12_[ states == state, , i, 1:( ap - i + 1 ) ] ) + frac_RES_add[ i ] * corr_CU[ i ] * colSums( u_yb_a_RES_add_[ states == state, , i, 1:( ap - i + 1 ) ] ) )
# 
#     B4_ests[ i, , 16 ] <- c( rep( 0, i - 1 ), frac_RES_rep[ i ] * colSums( u_yb_a_RES_rep_[ states == state, , i, 1:( ap - i + 1 ) ] ) )
# 
#     # Note: updated to include B3
# 
#     B3_ests[ i, , 16 ] <- c( rep( 0, i - 1 ), frac_B3[ i ] * colSums( u_yb_a_RES_rep_[ states == state, , i, 1:( ap - i + 1 ) ] ) )
# 
#   }else{
# 
#     A_ests[ i, , 16 ] <- c( rep( 0, i - 1 ), corr_CU[ i ] * sum( u_yb_a_RES12_[ states == state, , i, 1:( ap - i + 1 ) ] ) + frac_RES_add[ i ] * corr_CU[ i ] * sum( u_yb_a_RES_add_[ states == state, , i, 1:( ap - i + 1 ) ] ) )
# 
#     B4_ests[ i, , 16 ] <- c( rep( 0, i - 1 ), frac_RES_rep[ i ] * sum( u_yb_a_RES_rep_[ states == state, , i, 1:( ap - i + 1 ) ] ) )
# 
#     # Note: updated to include B3
# 
#     B3_ests[ i, , 16 ] <- c( rep( 0, i - 1 ), frac_B3[ i ] * sum( u_yb_a_RES_rep_[ states == state, , i, 1:( ap - i + 1 ) ] ) )
# 
#   }
# 
#   # Note: updated to include cw; updated to include additions; updated to include repairs
# 
#   if ( i + 2 <= ap ){
# 
#     A_ests[ i, , 16 ] <- A_ests[ i, , 16 ] + c( rep( 0, i - 1 ), corr_CU[ i ] * cw_yb_a_RES12[ states == state, , i, ] + frac_RES_add[ i ] * corr_CU[ i ] * cw_yb_a_RES_add[ states == state, , i, ], rep( 0, ap - i - 2 ) )
# 
#     B4_ests[ i, , 16 ] <- B4_ests[ i, , 16 ] + c( rep( 0, i - 1 ), frac_RES_rep[ i ] * cw_yb_a_RES_rep[ states == state, , i, ], rep( 0, ap - i - 2 ) )
# 
#     # Note: updated to include B3
# 
#     B3_ests[ i, , 16 ] <- B3_ests[ i, , 16 ] + c( rep( 0, i - 1 ), frac_B3[ i ] * cw_yb_a_RES_rep[ states == state, , i, ], rep( 0, ap - i - 2 ) )
# 
#   }else{
# 
#     if ( i + 1 <= ap ){
# 
#       A_ests[ i, , 16 ] <- A_ests[ i, , 16 ] + c( rep( 0, i - 1 ), corr_CU[ i ] * cw_yb_a_RES12[ states == state, , i, 1:2 ] + frac_RES_add[ i ] * corr_CU[ i ] * cw_yb_a_RES_add[ states == state, , i, 1:2 ] )
# 
#       B4_ests[ i, , 16 ] <- B4_ests[ i, , 16 ] + c( rep( 0, i - 1 ), frac_RES_rep[ i ] * cw_yb_a_RES_rep[ states == state, , i, 1:2 ] )
# 
#       # Note: updated to include B3
# 
#       B3_ests[ i, , 16 ] <- B3_ests[ i, , 16 ] + c( rep( 0, i - 1 ), frac_B3[ i ] * cw_yb_a_RES_rep[ states == state, , i, 1:2 ] )
# 
#     }else{
# 
#       A_ests[ i, , 16 ] <- A_ests[ i, , 16 ] + c( rep( 0, i - 1 ), corr_CU[ i ] * cw_yb_a_RES12[ states == state, , i, 1 ] + frac_RES_add[ i ] * corr_CU[ i ] * cw_yb_a_RES_add[ states == state, , i, 1 ] )
# 
#       B4_ests[ i, , 16 ] <- B4_ests[ i, , 16 ] + c( rep( 0, i - 1 ), frac_RES_rep[ i ] * cw_yb_a_RES_rep[ states == state, , i, 1 ] )
# 
#       # Note: updated to include B3
# 
#       B3_ests[ i, , 16 ] <- B3_ests[ i, , 16 ] + c( rep( 0, i - 1 ), frac_B3[ i ] * cw_yb_a_RES_rep[ states == state, , i, 1 ] )
# 
#     }
# 
#   }
# 
# }
# 
# # Note: updated to include cc; updated to include additions
# 
# A_ests[ , , 18 ] <- colSums( corr_CU * cc_yb_yd_a_RES12[ states == state, , 1:ap, 1:ap, ] + frac_RES_add * corr_CU * cc_yb_yd_a_RES_add[ states == state, , 1:ap, 1:ap, ] )
# 
# # B4_ests[ , , 18 ]
# 
# A_ests[ , , 16 ] <- -A_ests[ , , 16 ]
# 
# A_ests[ , , 18 ] <- -A_ests[ , , 18 ]
# 
# B4_ests[ , , 16 ] <- -B4_ests[ , , 16 ]
# 
# B4_ests[ , , 18 ] <- -B4_ests[ , , 18 ]
# 
# # Note: updated to include B3
# 
# B3_ests[ , , 16 ] <- -B3_ests[ , , 16 ]
# 
# B3_ests[ , , 18 ] <- -B3_ests[ , , 18 ]
# 
# 
# # # <- Note: realistic case
# #
# # write.csv( colSums( A_ests ), file = paste( pwd, "/Avoided Emissions/prelim_A.csv", sep = "" ) )
# #
# # write.csv( colSums( B4_ests ), file = paste( pwd, "/Avoided Emissions/prelim_B4.csv", sep = "" ) )
# #
# # # Note: updated to include B3
# #
# # write.csv( colSums( B3_ests ), file = paste( pwd, "/Avoided Emissions/prelim_B3.csv", sep = "" ) )
# #
# #
# # write.csv( A_ests[ 1967 - ap_start + 1, , ], file = paste( pwd, "/Avoided Emissions/prelim_A_1967.csv", sep = "" ) )
# #
# # write.csv( B4_ests[ 1967 - ap_start + 1, , ], file = paste( pwd, "/Avoided Emissions/prelim_B4_1967.csv", sep = "" ) )
# #
# # # Note: updated to include B3
# #
# # write.csv( B3_ests[ 1967 - ap_start + 1, , ], file = paste( pwd, "/Avoided Emissions/prelim_B3_1967.csv", sep = "" ) )
# #
# # # ->
# 
# j <- 1967 - ap_start + 1
# 
# 
# # # product stage
# #
# # streams_A_A1to3[ j ] * sum( rowSums( funUnit_A_A1to3 ) * ( MatData_BAIA$Cement. >= 1 ) )
# #
# # streams_A_A1to3[ j ] * sum( rowSums( funUnit_A_A1to3 ) * ( MatData_BAIA$Cement. < 1 ) )
# #
# #
# # # construction stage
# #
# # streams_A_A4[ j ] * sum( rowSums( funUnit_A_A4 ) * ( MatData_BAIA$Cement. >= 1 ) ) +
# #   streams_A_A5[ j ] * sum( rowSums( funUnit_A_A5 ) * ( MatData_BAIA$Cement. >= 1 ) )
# #
# # streams_A_A4[ j ] * sum( rowSums( funUnit_A_A4 ) * ( MatData_BAIA$Cement. < 1 ) ) +
# #   streams_A_A5[ j ] * sum( rowSums( funUnit_A_A5 ) * ( MatData_BAIA$Cement. < 1 ) )
# #
# #
# # # use stage
# #
# # sum( streams_B4_A1to3[ j, ] * sum( rowSums( funUnit_B4_A1to3 ) * ( MatData_BAIA$Cement. >= 1 ) ) ) +
# #   sum( streams_B4_A4[ j, ] * sum( rowSums( funUnit_B4_A4 ) * ( MatData_BAIA$Cement. >= 1 ) ) ) +
# #   sum( streams_B4_A5[ j, ] * sum( rowSums( funUnit_B4_A5 ) * ( MatData_BAIA$Cement. >= 1 ) ) )
# #
# # sum( streams_B4_A1to3[ j, ] * sum( rowSums( funUnit_B4_A1to3 ) * ( MatData_BAIA$Cement. < 1 ) ) ) +
# #   sum( streams_B4_A4[ j, ] * sum( rowSums( funUnit_B4_A4 ) * ( MatData_BAIA$Cement. < 1 ) ) ) +
# #   sum( streams_B4_A5[ j, ] * sum( rowSums( funUnit_B4_A5 ) * ( MatData_BAIA$Cement. < 1 ) ) )
# #
# # sum( streams_B3_A1to3[ j, ] * sum( rowSums( funUnit_B3_A1to3 ) * ( MatData_BAIA$Cement. >= 1 ) ) ) +
# #   sum( streams_B3_A4[ j, ] * sum( rowSums( funUnit_B3_A4 ) * ( MatData_BAIA$Cement. >= 1 ) ) ) +
# #   sum( streams_B3_A5[ j, ] * sum( rowSums( funUnit_B3_A5 ) * ( MatData_BAIA$Cement. >= 1 ) ) )
# #
# # sum( streams_B3_A1to3[ j, ] * sum( rowSums( funUnit_B3_A1to3 ) * ( MatData_BAIA$Cement. < 1 ) ) ) +
# #   sum( streams_B3_A4[ j, ] * sum( rowSums( funUnit_B3_A4 ) * ( MatData_BAIA$Cement. < 1 ) ) ) +
# #   sum( streams_B3_A5[ j, ] * sum( rowSums( funUnit_B3_A5 ) * ( MatData_BAIA$Cement. < 1 ) ) )
# #
# # sum( streams_B6[ j, ] * sum( funUnit_excB6_AllOrNothing[ c( 1, 2, 4 ), 1 ] ) )
# #
# # sum( streams_B6[ j, ] * sum( funUnit_excB6_AllOrNothing[ -c( 1, 2, 4 ), 1 ] ) )
# #
# #
# # # end of life stage
# #
# # sum( streams_A_C[ j, ] * sum( rowSums( funUnit_A_C ) * ( MatData_BAIA$Cement. >= 1 ) ) ) +
# #   sum( streams_B4_C[ j, ] * sum( rowSums( funUnit_B4_C ) * ( MatData_BAIA$Cement. >= 1 ) ) ) +
# #   sum( streams_B3_C[ j, ] * sum( rowSums( funUnit_B3_C ) * ( MatData_BAIA$Cement. >= 1 ) ) )
# #
# # sum( streams_A_C[ j, ] * sum( rowSums( funUnit_A_C ) * ( MatData_BAIA$Cement. < 1 ) ) ) +
# #   sum( streams_B4_C[ j, ] * sum( rowSums( funUnit_B4_C ) * ( MatData_BAIA$Cement. < 1 ) ) ) +
# #   sum( streams_B3_C[ j, ] * sum( rowSums( funUnit_B3_C ) * ( MatData_BAIA$Cement. < 1 ) ) )
# #
# #
# # # carbon uptake
# #
# # sum( A_ests[ j, , c( 16, 18 ) ] ) +
# #   sum( B3_ests[ j, , c( 16, 18 ) ] ) +
# #   sum( B4_ests[ j, , c( 16, 18 ) ] )
# 
# # ->
