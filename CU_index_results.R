# <- TEMPORARY - assessing impact year

# <- Note: only need to run once

lab_StateAbbrev <- c()

lab_StateName <- c()

for ( state in states ){

  StateInfo <- AssignStateInfo( state )

  lab_StateAbbrev <- c( lab_StateAbbrev, StateInfo$StateAbbrev )

  lab_StateName <- c( lab_StateName, StateInfo$StateName )

}

# ->

lab_ElementAbbrev <- gsub( "_vol", "", colnames( vol ) )

lab_ElementName <- c( "Columns", "Beams", "Slabs", "Slab Foundations", "Footings", "Crawlspaces", "Masonry Pads", "Concrete Pads", "Basement Walls", "Exterior Walls", "Infill Walls", "Shear Walls" )

byElement <- function( fig ){
  
  fig_ <- t( data.frame(
    c = colSums( fig[ grep( "c_", colnames( sa ) )[ !grep( "c_", colnames( sa ) ) %in% grep( "pc_", colnames( sa ) ) ], ] ),
    b = colSums( fig[ grep( "b_", colnames( sa ) ), ] ),
    s = colSums( fig[ grep( "s_", colnames( sa ) )[ !grep( "s_", colnames( sa ) ) %in% grep( "cs_", colnames( sa ) ) ], ] ),
    sf = colSums( fig[ grep( "sf_", colnames( sa ) ), ] ),
    f = colSums( fig[ grep( "f_", colnames( sa ) )[ !grep( "f_", colnames( sa ) ) %in% grep( "sf_", colnames( sa ) ) ], ] ),
    cs = colSums( fig[ grep( "cs_", colnames( sa ) ), ] ),
    pm = colSums( fig[ grep( "pm_", colnames( sa ) ), ] ),
    pc = colSums( fig[ grep( "pc_", colnames( sa ) ), ] ),
    bw = colSums( fig[ grep( "bw_", colnames( sa ) ), ] ),
    ew = colSums( fig[ grep( "ew_", colnames( sa ) ), ] ),
    iw = colSums( fig[ grep( "iw_", colnames( sa ) ), ] ),
    sw = colSums( fig[ grep( "sw_", colnames( sa ) ), ] )
  ) )
  
  rownames( fig_ ) <- lab_ElementName
  
  colnames( fig_ ) <- seq( 1940, 1940 + ap - 1 )
  
  return( fig_ )
  
}

# cem <- t( array( c( 1972, 1.0579,
#                     1973, 1.0647,
#                     1974, 1.0606,
#                     1975, 1.0620,
#                     1976, 1.0620,
#                     1977, 1.0769,
#                     1978, 1.0769,
#                     1979, 1.0701,
#                     1980, 1.0688,
#                     1981, 1.0606,
#                     1982, 1.0321,
#                     1983, 1.0226,
#                     1984, 1.0172,
#                     1985, 1.0049,
#                     1985, 1.0008,
#                     1987, 0.9804,
#                     1988, 0.9791,
#                     1989, 0.9832,
#                     1990, 0.9723,
#                     1991, 0.9777,
#                     1992, 0.9655,
#                     1993, 0.9709,
#                     1994, 0.9668,
#                     1995, 0.9641,
#                     1996, 0.9641,
#                     1997, 0.9641,
#                     1998, 0.9546,
#                     1999, 0.9505,
#                     2000, 0.9478,
#                     2001, 0.9465,
#                     2002, 0.9410,
#                     2003, 0.9342,
#                     2004, 0.9315,
#                     2005, 0.9274,
#                     2006, 0.9207,
#                     2007, 0.9274,
#                     2008, 0.9207,
#                     2009, 0.9030,
#                     2010, 0.8989,
#                     2011, 0.8989,
#                     2012, 0.8758,
#                     2013, 0.8880,
#                     2014, 0.8880,
#                     2015, 0.8636,
#                     2016, 0.8663,
#                     2017, 0.8609 ), dim = c( 2, 2017 - 1972 + 1 ) ) )
# 
# cem_ <- rbind( cbind( seq( 1940, 1971 ), rep( mean( cem[ 1:5, 2 ] ), 1971 - 1940 + 1 ) ), cem )

# write.csv( cem_, paste( pwd, "/Carbon Uptake/cem_conv.csv", sep = "" ) )


# national level

# process emissions

proc_RES12 <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * colSums( colSums( vol_yb_RES12_lc, na.rm = TRUE ) ) + MixDesign( app = "hc" )$cement * colSums( colSums( vol_yb_RES12_hc, na.rm = TRUE ) ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
proc_RES3AB <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * colSums( colSums( vol_yb_RES3AB_lc, na.rm = TRUE ) ) + MixDesign( app = "hc" )$cement * colSums( colSums( vol_yb_RES3AB_hc, na.rm = TRUE ) ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
proc_RES3CDEF <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * colSums( colSums( vol_yb_RES3CDEF_lc, na.rm = TRUE ) ) + MixDesign( app = "hc" )$cement * colSums( colSums( vol_yb_RES3CDEF_hc, na.rm = TRUE ) ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
proc_COM <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * colSums( colSums( vol_yb_COM_lc, na.rm = TRUE ) ) + MixDesign( app = "hc" )$cement * colSums( colSums( vol_yb_COM_hc, na.rm = TRUE ) ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
proc_RES3 <- proc_RES3AB + proc_RES3CDEF

proc_cmuRES12 <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * colSums( colSums( vol_yb_cmuRES12_lc, na.rm = TRUE ) ) + ( ( 180 + 200 ) / 2 ) * colSums( colSums( vol_yb_cmuRES12_hc, na.rm = TRUE ) ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
proc_cmuRES3AB <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * colSums( colSums( vol_yb_cmuRES3AB_lc, na.rm = TRUE ) ) + ( ( 180 + 200 ) / 2 ) * colSums( colSums( vol_yb_cmuRES3AB_hc, na.rm = TRUE ) ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
proc_cmuRES3CDEF <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * colSums( colSums( vol_yb_cmuRES3CDEF_lc, na.rm = TRUE ) ) + ( ( 180 + 200 ) / 2 ) * colSums( colSums( vol_yb_cmuRES3CDEF_hc, na.rm = TRUE ) ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
proc_cmuCOM <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * colSums( colSums( vol_yb_cmuCOM_lc, na.rm = TRUE ) ) + ( ( 180 + 200 ) / 2 ) * colSums( colSums( vol_yb_cmuCOM_hc, na.rm = TRUE ) ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
proc_cmuRES3 <- proc_cmuRES3AB + proc_cmuRES3CDEF

# Note: updated to include additions

proc_RES_add <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * colSums( colSums( vol_yb_RES_add_lc, na.rm = TRUE ) ) + MixDesign( app = "hc" )$cement * colSums( colSums( vol_yb_RES_add_hc, na.rm = TRUE ) ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
proc_cmuRES_add <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * colSums( colSums( vol_yb_cmuRES_add_lc, na.rm = TRUE ) ) + ( ( 180 + 200 ) / 2 ) * colSums( colSums( vol_yb_cmuRES_add_hc, na.rm = TRUE ) ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )

# Note: updated to include M&R

proc_RES_rep <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "hc" )$cement * colSums( colSums( vol_yb_RES_rep, na.rm = TRUE ) ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) # Note: assumed only hc
proc_COM_rep <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "hc" )$cement * colSums( colSums( vol_yb_COM_rep, na.rm = TRUE ) ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) # Note: assumed only hc

proc_data <- list(
  concrete = data.frame(
    "Single-family" = proc_RES12,
    "Multi-family" = proc_RES3,
    "Commercial" = proc_COM,
    "Residential Additions" = proc_RES_add,
    "Residential M&R" = proc_RES_rep,
    "Commercial M&R" = proc_COM_rep,
    row.names = seq( 1940, 1940 + ap - 1 )
  ),
  mortar = data.frame(
    "Single-family" = proc_cmuRES12,
    "Multi-family" = proc_cmuRES3,
    "Commercial" = proc_cmuCOM,
    "Residential Additions" = proc_cmuRES_add,
    "Residential M&R" = 0 * proc_RES_rep,
    "Commercial M&R" = 0 * proc_COM_rep,
    row.names = seq( 1940, 1940 + ap - 1 )
  )
)

proc_abs <- rbind( t( proc_data$concrete ), t( proc_data$mortar ) )
rownames( proc_abs ) <- c( "Single-family", "Multi-family", "Commercial", "Residential Additions", "Residential M&R", "Commercial M&R", "Single-family (CMU)", "Multi-family (CMU)", "Commercial (CMU)", "Residential Additions (CMU)", "Residential M&R (CMU)", "Commercial M&R (CMU)" )

proc_perc <- proc_abs / t( matrix( array( rep( colSums( proc_abs ), nrow( proc_abs ) ) ), nrow = ap ) )

barplot( proc_abs / 1000000 , legend = rownames( proc_abs ), xlab = "Analysis Year", ylab = "Process Emissions (1000 ton CO2)" )

barplot( proc_perc * 100 , legend = lab_ElementName, xlab = "Analysis Year", ylab = "Percentage of Process Emissions" )

pie( rowSums( proc_abs ) )


# total uptake

u_RES12 <- colSums( colSums( u_ay_RES12, na.rm = TRUE ) )
u_RES3AB <- colSums( colSums( u_ay_RES3AB, na.rm = TRUE ) )
u_RES3CDEF <- colSums( colSums( u_ay_RES3CDEF, na.rm = TRUE ) )
u_COM <- colSums( colSums( u_ay_COM, na.rm = TRUE ) )
u_RES3 <- u_RES3AB + u_RES3CDEF

u_cmuRES12 <- colSums( colSums( u_ay_cmuRES12, na.rm = TRUE ) )
u_cmuRES3AB <- colSums( colSums( u_ay_cmuRES3AB, na.rm = TRUE ) )
u_cmuRES3CDEF <- colSums( colSums( u_ay_cmuRES3CDEF, na.rm = TRUE ) )
u_cmuCOM <- colSums( colSums( u_ay_COM, na.rm = TRUE ) )
u_cmuRES3 <- u_cmuRES3AB + u_cmuRES3CDEF

u_ccRES12 <- colSums( colSums( cc_ay_RES12, na.rm = TRUE ) )
u_ccRES3AB <- colSums( colSums( cc_ay_RES3AB, na.rm = TRUE ) )
u_ccRES3CDEF <- colSums( colSums( cc_ay_RES3CDEF, na.rm = TRUE ) )
u_ccCOM <- colSums( colSums( cc_ay_COM, na.rm = TRUE ) )
u_ccRES3 <- u_ccRES3AB + u_ccRES3CDEF

u_cwRES12 <- colSums( colSums( cw_ay_RES12, na.rm = TRUE ) )
u_cwRES3AB <- colSums( colSums( cw_ay_RES3AB, na.rm = TRUE ) )
u_cwRES3CDEF <- colSums( colSums( cw_ay_RES3CDEF, na.rm = TRUE ) )
u_cwCOM <- colSums( colSums( cw_ay_COM, na.rm = TRUE ) )
u_cwRES3 <- u_cwRES3AB + u_cwRES3CDEF

# Note: updated to include additions

u_RES_add <- colSums( colSums( u_ay_RES_add, na.rm = TRUE ) )
u_cmuRES_add <- colSums( colSums( u_ay_cmuRES_add, na.rm = TRUE ) )

u_ccRES_add <- colSums( colSums( cc_ay_RES_add, na.rm = TRUE ) )

u_cwRES_add <- colSums( colSums( cw_ay_RES_add, na.rm = TRUE ) )

# Note: updated to include M&R

u_RES_rep <- colSums( colSums( u_ay_RES_rep, na.rm = TRUE ) )
u_COM_rep <- colSums( colSums( u_ay_COM_rep, na.rm = TRUE ) )

u_cwRES_rep <- colSums( colSums( cw_ay_RES_rep, na.rm = TRUE ) )
u_cwCOM_rep <- colSums( colSums( cw_ay_COM_rep, na.rm = TRUE ) )

u_data <- list(
  concrete = data.frame(
    "Single-family" = u_RES12,
    "Multi-family" = u_RES3,
    "Commercial" = u_COM,
    "Residential Additions" = u_RES_add,
    "Residential M&R" = u_RES_rep,
    "Commercial M&R" = u_COM_rep,
    row.names = seq( 1940, 1940 + ap - 1 )
  ),
  mortar = data.frame(
    "Single-family" = u_cmuRES12,
    "Multi-family" = u_cmuRES3,
    "Commercial" = u_cmuCOM,
    "Residential Additions" = u_cmuRES_add,
    "Residential M&R" = 0 * u_RES_rep,
    "Commercial M&R" = 0 * u_COM_rep,
    row.names = seq( 1940, 1940 + ap - 1 )
  ),
  EoL = data.frame(
    "Single-family" = u_ccRES12,
    "Multi-family" = u_ccRES3,
    "Commercial" = u_ccCOM,
    "Residential Additions" = u_ccRES_add,
    "Residential M&R" = 0 * u_RES_rep,
    "Commercial M&R" = 0 * u_COM_rep,
    row.names = seq( 1940, 1940 + ap - 1 )
  ),
  waste = data.frame(
    "Single-family" = u_cwRES12,
    "Multi-family" = u_cwRES3,
    "Commercial" = u_cwCOM,
    "Residential Additions" = u_cwRES_add,
    "Residential M&R" = u_cwRES_rep,
    "Commercial M&R" = u_cwCOM_rep,
    row.names = seq( 1940, 1940 + ap - 1 )
  )
)

u_abs <- rbind( t( u_data$concrete ), t( u_data$mortar ), t( u_data$EoL ) )
rownames( u_abs ) <- c( "Single-family", "Multi-family", "Commercial", "Residential Additions", "Residential M&R", "Commercial M&R", "Single-family (CMU)", "Multi-family (CMU)", "Commercial (CMU)", "Residential M&R (CMU)", "Commercial M&R (CMU)", "Single-family (EoL)", "Multi-family (EoL)", "Residential Additions (EoL)", "Commercial (EoL)", "Residential Additions (CMU)", "Residential M&R (EoL)", "Commercial M&R (EoL)" )

u_perc <- u_abs / t( matrix( array( rep( colSums( u_abs ), nrow( u_abs ) ) ), nrow = ap ) )

barplot( u_abs / 1000000 , legend = rownames( u_abs ), xlab = "Analysis Year", ylab = "Total Uptake (1000 ton CO2)" )

barplot( u_perc * 100 , legend = rownames( u_abs ), xlab = "Analysis Year", ylab = "Percentage of Total Uptake" )

pie( rowSums( u_abs ) )


# sequestration

seq_data <- data.frame(
  concrete = rowSums( u_data$concrete ),
  mortar = rowSums( u_data$mortar ),
  EoL = rowSums( u_data$EoL ),
  waste = rowSums( u_data$waste )
) / ( rowSums( proc_data$concrete ) + rowSums( proc_data$mortar ) )

max( rowSums( seq_data ) )

barplot( t( seq_data ) * 100, legend = c( "Use-Stage", "Use-Stage (CMU)", "End-of-Life", "Waste" ), xlab = "Analysis Year", ylab = "Percentage of Process Emissions Sequestered" )


# state level

# process emissions

map_proc_RES12 <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * rowSums(vol_yb_RES12_lc, na.rm = TRUE ) + MixDesign( app = "hc" )$cement * rowSums(vol_yb_RES12_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_proc_RES3AB <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * rowSums(vol_yb_RES3AB_lc, na.rm = TRUE ) + MixDesign( app = "hc" )$cement * rowSums(vol_yb_RES3AB_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_proc_RES3CDEF <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * rowSums(vol_yb_RES3CDEF_lc, na.rm = TRUE ) + MixDesign( app = "hc" )$cement * rowSums(vol_yb_RES3CDEF_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_proc_COM <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * rowSums(vol_yb_COM_lc, na.rm = TRUE ) + MixDesign( app = "hc" )$cement * rowSums(vol_yb_COM_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_proc_RES3 <- map_proc_RES3AB + map_proc_RES3CDEF

map_proc_cmuRES12 <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES12_lc, na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES12_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_proc_cmuRES3AB <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES3AB_lc, na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES3AB_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_proc_cmuRES3CDEF <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES3CDEF_lc, na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES3CDEF_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_proc_cmuCOM <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuCOM_lc, na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuCOM_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_proc_cmuRES3 <- map_proc_cmuRES3AB + map_proc_cmuRES3CDEF

# Note: updated to include additions

map_proc_RES_add <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * rowSums(vol_yb_RES_add_lc, na.rm = TRUE ) + MixDesign( app = "hc" )$cement * rowSums(vol_yb_RES_add_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_proc_cmuRES_add <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES_add_lc, na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES_add_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )

# Note: updated to include M&R

map_proc_RES_rep <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "hc" )$cement * rowSums(vol_yb_RES_rep, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) # Note: assumed all hc
map_proc_COM_rep <- 0.475 * ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "hc" )$cement * rowSums(vol_yb_COM_rep, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) # Note: assumed all hc

map_proc_data <- list(
  concrete = data.frame(
    "Single-family" = map_proc_RES12,
    "Multi-family" = map_proc_RES3,
    "Commercial" = map_proc_COM,
    "Residential Additions" = map_proc_RES_add,
    "Residential M&R" = map_proc_RES_rep,
    "Commercial M&R" = map_proc_COM_rep,
    row.names = lab_StateAbbrev
  ),
  mortar = data.frame(
    "Single-family" = map_proc_cmuRES12,
    "Multi-family" = map_proc_cmuRES3,
    "Commercial" = map_proc_cmuCOM,
    "Residential Additions" = map_proc_cmuRES_add,
    "Residential M&R" = 0 * map_proc_RES_rep,
    "Commercial M&R" = 0 * map_proc_COM_rep,
    row.names = lab_StateAbbrev
  )
)

map_proc_abs <- rbind( t( map_proc_data$concrete ), t( map_proc_data$mortar ) )
rownames( map_proc_abs ) <- c( "Single-family", "Multi-family", "Commercial", "Residential Additions", "Residential M&R", "Commercial M&R", "Single-family (CMU)", "Multi-family (CMU)", "Commercial (CMU)", "Residential Additions (CMU)", "Residential M&R (CMU)", "Commercial M&R (CMU)" )

map_proc_perc <- map_proc_abs / t( matrix( array( rep( colSums( map_proc_abs ), nrow( map_proc_abs ) ) ), nrow = length( states ) ) )

barplot( map_proc_abs / 1000000 , legend = rownames( map_proc_abs ), xlab = "Analysis State", ylab = "Process Emissions (1000 ton CO2)" )

barplot( map_proc_perc * 100 , legend = rownames( map_proc_abs ), xlab = "Analysis State", ylab = "Percentage of Process Emissions" )


# total uptake

map_u_RES12 <- rowSums( u_ay_RES12, na.rm = TRUE )
map_u_RES3AB <- rowSums( u_ay_RES3AB, na.rm = TRUE )
map_u_RES3CDEF <- rowSums( u_ay_RES3CDEF, na.rm = TRUE )
map_u_COM <- rowSums( u_ay_COM, na.rm = TRUE )
map_u_RES3 <- map_u_RES3AB + map_u_RES3CDEF

map_u_cmuRES12 <- rowSums( u_ay_cmuRES12, na.rm = TRUE )
map_u_cmuRES3AB <- rowSums( u_ay_cmuRES3AB, na.rm = TRUE )
map_u_cmuRES3CDEF <- rowSums( u_ay_cmuRES3CDEF, na.rm = TRUE )
map_u_cmuCOM <- rowSums( u_ay_COM, na.rm = TRUE )
map_u_cmuRES3 <- map_u_cmuRES3AB + map_u_cmuRES3CDEF

map_u_ccRES12 <- rowSums( cc_ay_RES12, na.rm = TRUE )
map_u_ccRES3AB <- rowSums( cc_ay_RES3AB, na.rm = TRUE )
map_u_ccRES3CDEF <- rowSums( cc_ay_RES3CDEF, na.rm = TRUE )
map_u_ccCOM <- rowSums( cc_ay_COM, na.rm = TRUE )
map_u_ccRES3 <- map_u_ccRES3AB + map_u_ccRES3CDEF

map_u_cwRES12 <- rowSums( cw_ay_RES12, na.rm = TRUE )
map_u_cwRES3AB <- rowSums( cw_ay_RES3AB, na.rm = TRUE )
map_u_cwRES3CDEF <- rowSums( cw_ay_RES3CDEF, na.rm = TRUE )
map_u_cwCOM <- rowSums( cw_ay_COM, na.rm = TRUE )
map_u_cwRES3 <- map_u_cwRES3AB + map_u_cwRES3CDEF

# Note: updated to include additions

map_u_RES_add <- rowSums( u_ay_RES_add, na.rm = TRUE )
map_u_cmuRES_add <- rowSums( u_ay_cmuRES_add, na.rm = TRUE )

map_u_ccRES_add <- rowSums( cc_ay_RES_add, na.rm = TRUE )

map_u_cwRES_add <- rowSums( cw_ay_RES_add, na.rm = TRUE )

# Note: updated to include M&R

map_u_RES_rep <- rowSums( u_ay_RES_rep, na.rm = TRUE )
map_u_COM_rep <- rowSums( u_ay_COM_rep, na.rm = TRUE )

map_u_cwRES_rep <- rowSums( cw_ay_RES_rep, na.rm = TRUE )
map_u_cwCOM_rep <- rowSums( cw_ay_COM_rep, na.rm = TRUE )

map_u_data <- list(
  concrete = data.frame(
    "Single-family" = map_u_RES12,
    "Multi-family" = map_u_RES3,
    "Commercial" = map_u_COM,
    "Residential Additions" = map_u_RES_add,
    "Residential M&R" = map_u_RES_rep,
    "Commercial M&R" = map_u_COM_rep,
    row.names = lab_StateAbbrev
  ),
  mortar = data.frame(
    "Single-family" = map_u_cmuRES12,
    "Multi-family" = map_u_cmuRES3,
    "Commercial" = map_u_cmuCOM,
    "Residential Additions" = map_u_cmuRES_add,
    "Residential M&R" = 0 * map_u_RES_rep,
    "Commercial M&R" = 0 * map_u_COM_rep,
    row.names = lab_StateAbbrev
  ),
  EoL = data.frame(
    "Single-family" = map_u_ccRES12,
    "Multi-family" = map_u_ccRES3,
    "Commercial" = map_u_ccCOM,
    "Residential Additions" = map_u_ccRES_add,
    "Residential M&R" = 0 * map_u_RES_rep,
    "Commercial M&R" = 0 * map_u_COM_rep,
    row.names = lab_StateAbbrev
  ),
  waste = data.frame(
    "Single-family" = map_u_cwRES12,
    "Multi-family" = map_u_cwRES3,
    "Commercial" = map_u_cwCOM,
    "Residential Additions" = map_u_cwRES_add,
    "Residential M&R" = map_u_cwRES_rep,
    "Commercial M&R" = map_u_cwCOM_rep,
    row.names = lab_StateAbbrev
  )
)

map_u_abs <- rbind( t( map_u_data$concrete ), t( map_u_data$mortar ), t( map_u_data$EoL ) )
rownames( map_u_abs ) <- c( "Single-family", "Multi-family", "Commercial", "Residential Additions", "Residential M&R", "Commercial M&R", "Single-family (CMU)", "Multi-family (CMU)", "Commercial (CMU)", "Residential Additions (CMU)", "Residential M&R (CMU)", "Commercial M&R (CMU)", "Single-family (EoL)", "Multi-family (EoL)", "Commercial (EoL)", "Residential Additions (EoL)", "Residential M&R (EoL)", "Commercial M&R (EoL)" )

map_u_perc <- map_u_abs / t( matrix( array( rep( colSums( map_u_abs ), nrow( map_u_abs ) ) ), nrow = length( states ) ) )

barplot( map_u_abs / 1000000 , legend = rownames( map_u_abs ), xlab = "Analysis State", ylab = "Total Uptake (1000 ton CO2)" )

barplot( map_u_perc * 100 , legend = rownames( map_u_abs ), xlab = "Analysis State", ylab = "Percentage of Total Uptake" )


# sequestration

map_seq_data <- data.frame(
  concrete = rowSums( map_u_data$concrete ),
  mortar = rowSums( map_u_data$mortar ),
  EoL = rowSums( map_u_data$EoL ),
  waste = rowSums( map_u_data$waste )
) / ( rowSums( map_proc_data$concrete ) + rowSums( map_proc_data$mortar ) )

map_seq_data[ is.na( map_seq_data ) ] <- 0

# map_seq_data_ <- map_seq_data[ rowSums( map_seq_data ) > 0, ]
# 
# min( rowSums( map_seq_data_ ) ) # min 9.7%
# names( rowSums( map_seq_data_ ) )[ which( rowSums( map_seq_data_ ) == min( rowSums( map_seq_data_ ) ) ) ] # which is TX
# 
# median( rowSums( map_seq_data_ ) ) # median 11.9%
# # names( rowSums( map_seq_data_ ) )[ which( rowSums( map_seq_data_ ) == median( rowSums( map_seq_data_ ) ) ) ] # which is ???
# 
# max( rowSums( map_seq_data_ ) ) # max 13.8%
# names( rowSums( map_seq_data_ ) )[ which( rowSums( map_seq_data_ ) == max( rowSums( map_seq_data_ ) ) ) ] # which is MA

barplot( t( map_seq_data ) * 100, legend = c( "Use-Stage", "Use-Stage (CMU)", "End-of-Life", "Waste" ), xlab = "Analysis State", ylab = "Percentage of Process Emissions Sequestered" )

# ->


# figs - total uptake

u_abs_ <- rbind( t( u_data$concrete ), t( u_data$mortar ), t( u_data$EoL ), t( u_data$waste ) )
rownames( u_abs_ ) <- c( "Single-family", "Multi-family", "Commercial", "Residential Additions", "Residential M&R", "Commercial M&R", "Single-family (CMU)", "Multi-family (CMU)", "Commercial (CMU)", "Residential Additions (CMU)", "Residential M&R (CMU)", "Commercial M&R (CMU)", "Single-family (EoL)", "Multi-family (EoL)", "Commercial (EoL)", "Residential Additions (EoL)", "Residential M&R (EoL)", "Commercial M&R (EoL)", "Single-family (waste)", "Multi-family (waste)", "Commercial (waste)", "Residential Additions (waste)", "Residential M&R (waste)", "Commercial M&R (waste)" )

u_perc_ <- u_abs_ / t( matrix( array( rep( colSums( u_abs_ ), nrow( u_abs_ ) ) ), nrow = ap ) )

# barplot( u_abs_ / 1000000 , legend = rownames( u_abs_ ), xlab = "Analysis Year", ylab = "Total Uptake (1000 ton CO2)" )
# 
# barplot( u_perc_ * 100 , legend = rownames( u_abs_ ), xlab = "Analysis Year", ylab = "Percentage of Total Uptake" )
# 
# pie( rowSums( u_abs_ ) )

u_bySector <- rbind(
  u_abs_[ "Single-family", ] + u_abs_[ "Single-family (CMU)", ] + u_abs_[ "Single-family (EoL)", ] + u_abs_[ "Single-family (waste)", ],
  u_abs_[ "Multi-family", ] + u_abs_[ "Multi-family (CMU)", ] + u_abs_[ "Multi-family (EoL)", ] + u_abs_[ "Multi-family (waste)", ],
  u_abs_[ "Commercial", ] + u_abs_[ "Commercial (CMU)", ] + u_abs_[ "Commercial (EoL)", ] + u_abs_[ "Commercial (waste)", ],
  u_abs_[ "Residential Additions", ] + u_abs_[ "Residential Additions (CMU)", ] + u_abs_[ "Residential Additions (EoL)", ] + u_abs_[ "Residential Additions (waste)", ],
  u_abs_[ "Residential M&R", ] + u_abs_[ "Residential M&R (CMU)", ] + u_abs_[ "Residential M&R (EoL)", ] + u_abs_[ "Residential M&R (waste)", ],
  u_abs_[ "Commercial M&R", ] + u_abs_[ "Commercial M&R (CMU)", ] + u_abs_[ "Commercial M&R (EoL)", ] + u_abs_[ "Commercial M&R (waste)", ]
)
rownames( u_bySector ) <- c( "Single-family", "Multi-family", "Residential Additions", "Commercial", "Residential M&R", "Commercial M&R" )

write.csv( t( u_bySector ), file = paste( pwd, "/Carbon Uptake/figs_u_bySector.csv", sep = "" ) )

u_byElement_RES12 <- rowSums( byElement( colSums(  u_ay_RES12, na.rm = TRUE ) ) )
u_byElement_RES3AB <- rowSums( byElement( colSums(  u_ay_RES3AB, na.rm = TRUE ) ) )
u_byElement_RES3CDEF <- rowSums( byElement( colSums(  u_ay_RES3CDEF, na.rm = TRUE ) ) )
u_byElement_COM <- rowSums( byElement( colSums(  u_ay_COM, na.rm = TRUE ) ) )
u_byElement_RES3 <- u_byElement_RES3AB + u_byElement_RES3CDEF

u_byElement_cmuRES12 <- rowSums( byElement( colSums(  u_ay_cmuRES12, na.rm = TRUE ) ) )
u_byElement_cmuRES3AB <- rowSums( byElement( colSums(  u_ay_cmuRES3AB, na.rm = TRUE ) ) )
u_byElement_cmuRES3CDEF <- rowSums( byElement( colSums(  u_ay_cmuRES3CDEF, na.rm = TRUE ) ) )
u_byElement_cmuCOM <- rowSums( byElement( colSums(  u_ay_COM, na.rm = TRUE ) ) )
u_byElement_cmuRES3 <- u_byElement_cmuRES3AB + u_byElement_cmuRES3CDEF

u_byElement_ccRES12 <- rowSums( colSums(  cc_ay_RES12, na.rm = TRUE ) )
u_byElement_ccRES3AB <- rowSums( colSums(  cc_ay_RES3AB, na.rm = TRUE ) )
u_byElement_ccRES3CDEF <- rowSums( colSums(  cc_ay_RES3CDEF, na.rm = TRUE ) )
u_byElement_ccCOM <- rowSums( colSums(  cc_ay_COM, na.rm = TRUE ) )
u_byElement_ccRES3 <- u_byElement_ccRES3AB + u_byElement_ccRES3CDEF

u_byElement_RES12 <- c( u_byElement_RES12 + u_byElement_ccRES12, sum( u_byElement_cmuRES12 ) )
u_byElement_RES3 <- c( u_byElement_RES3 + u_byElement_ccRES3, sum( u_byElement_cmuRES3 ) )
u_byElement_COM <- c( u_byElement_COM + u_byElement_ccCOM, sum( u_byElement_cmuCOM ) )

write.csv( u_byElement_RES12, file = paste( pwd, "/Carbon Uptake/figs_u_byElement_singlefamily.csv", sep = "" ) )
write.csv( u_byElement_RES3, file = paste( pwd, "/Carbon Uptake/figs_u_byElement_multifamily.csv", sep = "" ) )
write.csv( u_byElement_COM, file = paste( pwd, "/Carbon Uptake/figs_u_byElement_commercial.csv", sep = "" ) )

# Note: updated to include additions

u_byElement_RES_add <- rowSums( byElement( colSums(  u_ay_RES_add, na.rm = TRUE ) ) )
u_byElement_cmuRES_add <- rowSums( byElement( colSums(  u_ay_cmuRES_add, na.rm = TRUE ) ) )
u_byElement_ccRES_add <- rowSums( colSums(  cc_ay_RES_add, na.rm = TRUE ) )
u_byElement_RES_add <- c( u_byElement_RES_add + u_byElement_ccRES_add, sum( u_byElement_cmuRES_add ) )
write.csv( u_byElement_RES_add, file = paste( pwd, "/Carbon Uptake/figs_u_byElement_residential_additions.csv", sep = "" ) )

# Note: updated to include M&R

u_byElement_RES_rep <- rowSums( byElement( colSums(  u_ay_RES_rep, na.rm = TRUE ) ) )
write.csv( u_byElement_RES_rep, file = paste( pwd, "/Carbon Uptake/figs_u_byElement_residential_repairs.csv", sep = "" ) )

u_byElement_COM_rep <- rowSums( byElement( colSums(  u_ay_COM_rep, na.rm = TRUE ) ) )
write.csv( u_byElement_COM_rep, file = paste( pwd, "/Carbon Uptake/figs_u_byElement_commercial_repairs.csv", sep = "" ) )

u_byStage <- data.frame(
  use_RES12 = u_abs_[ "Single-family", ] + u_abs_[ "Single-family (CMU)", ] + u_abs_[ "Single-family (waste)", ],
  eol_RES12 = u_abs_[ "Single-family (EoL)", ],
  use_RES3 = u_abs_[ "Multi-family", ] + u_abs_[ "Multi-family (CMU)", ] + u_abs_[ "Multi-family (waste)", ],
  eol_RES3 = u_abs_[ "Multi-family (EoL)", ],
  use_COM = u_abs_[ "Commercial", ] + u_abs_[ "Commercial (CMU)", ] + u_abs_[ "Commercial (waste)", ],
  eol_COM = u_abs_[ "Commercial (EoL)", ],
  use_RES_add = u_abs_[ "Residential Additions", ] + u_abs_[ "Residential Additions (CMU)", ] + u_abs_[ "Residential Additions (waste)", ],
  eol_RES_add = u_abs_[ "Residential Additions (EoL)", ],
  use_RES_rep = u_abs_[ "Residential M&R", ] + u_abs_[ "Residential M&R (CMU)", ] + u_abs_[ "Residential M&R (waste)", ],
  use_COM_rep = u_abs_[ "Commercial M&R", ] + u_abs_[ "Commercial M&R (CMU)", ] + u_abs_[ "Commercial M&R (waste)", ]
)

write.csv( u_byStage, file = paste( pwd, "/Carbon Uptake/figs_u_byStage.csv", sep = "" ) )


# figs - cement consumption

write.csv( ( proc_data$concrete + proc_data$mortar ) / 0.475, paste( pwd, "/Carbon Uptake/figs_cem_bySector.csv", sep = "" ) )

cem_byElement_RES12 <- rowSums( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * colSums( vol_yb_RES12_lc, na.rm = TRUE ) + MixDesign( app = "hc" )$cement * colSums( vol_yb_RES12_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
cem_byElement_RES3AB <- rowSums( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * colSums( vol_yb_RES3AB_lc, na.rm = TRUE ) + MixDesign( app = "hc" )$cement * colSums( vol_yb_RES3AB_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
cem_byElement_RES3CDEF <- rowSums( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * colSums( vol_yb_RES3CDEF_lc, na.rm = TRUE ) + MixDesign( app = "hc" )$cement * colSums( vol_yb_RES3CDEF_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
cem_byElement_COM <- rowSums( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * colSums( vol_yb_COM_lc, na.rm = TRUE ) + MixDesign( app = "hc" )$cement * colSums( vol_yb_COM_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
cem_byElement_RES3 <- cem_byElement_RES3AB + cem_byElement_RES3CDEF

cem_byElement_cmuRES12 <- rowSums( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * colSums( vol_yb_cmuRES12_lc, na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * colSums( vol_yb_cmuRES12_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
cem_byElement_cmuRES3AB <- rowSums( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * colSums( vol_yb_cmuRES3AB_lc, na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * colSums( vol_yb_cmuRES3AB_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
cem_byElement_cmuRES3CDEF <- rowSums( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * colSums( vol_yb_cmuRES3CDEF_lc, na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * colSums( vol_yb_cmuRES3CDEF_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
cem_byElement_cmuCOM <- rowSums( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * colSums( vol_yb_cmuCOM_lc, na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * colSums( vol_yb_cmuCOM_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
cem_byElement_cmuRES3 <- cem_byElement_cmuRES3AB + cem_byElement_cmuRES3CDEF

cem_byElement_RES12 <- c( cem_byElement_RES12, sum( cem_byElement_cmuRES12 ) )
cem_byElement_RES3 <- c( cem_byElement_RES3, sum( cem_byElement_cmuRES3 ) )
cem_byElement_COM <- c( cem_byElement_COM, sum( cem_byElement_cmuCOM ) )

write.csv( cem_byElement_RES12, file = paste( pwd, "/Carbon Uptake/figs_cem_byElement_singlefamily.csv", sep = "" ) )
write.csv( cem_byElement_RES3, file = paste( pwd, "/Carbon Uptake/figs_cem_byElement_multifamily.csv", sep = "" ) )
write.csv( cem_byElement_COM, file = paste( pwd, "/Carbon Uptake/figs_cem_byElement_commercial.csv", sep = "" ) )

# Note: updated to include additions

cem_byElement_RES_add <- rowSums( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * colSums( vol_yb_RES_add_lc, na.rm = TRUE ) + MixDesign( app = "hc" )$cement * colSums( vol_yb_RES_add_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
cem_byElement_cmuRES_add <- rowSums( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * colSums( vol_yb_cmuRES_add_lc, na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * colSums( vol_yb_cmuRES_add_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
cem_byElement_RES_add <- c( cem_byElement_RES_add, sum( cem_byElement_cmuRES_add ) )
write.csv( cem_byElement_RES_add, file = paste( pwd, "/Carbon Uptake/figs_cem_byElement_residential_additions.csv", sep = "" ) )

# Note: updated to include M&R

cem_byElement_RES_rep <- rowSums( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "hc" )$cement * colSums( vol_yb_RES_rep, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) ) # Note: assumed only hc
write.csv( cem_byElement_RES_rep, file = paste( pwd, "/Carbon Uptake/figs_cem_byElement_residential_repairs.csv", sep = "" ) )

cem_byElement_COM_rep <- rowSums( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "hc" )$cement * colSums( vol_yb_COM_rep, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) ) # Note: assumed only hc
write.csv( cem_byElement_COM_rep, file = paste( pwd, "/Carbon Uptake/figs_cem_byElement_commercial_repairs.csv", sep = "" ) )


# maps

map_u_abs_ <- rbind( t( map_u_data$concrete ), t( map_u_data$mortar ), t( map_u_data$EoL ), t( map_u_data$waste ) )
rownames( map_u_abs_ ) <- c( "Single-family", "Multi-family", "Commercial", "Residential Additions", "Residential M&R", "Commercial M&R", "Single-family (CMU)", "Multi-family (CMU)", "Commercial (CMU)", "Residential Additions (CMU)", "Residential M&R (CMU)", "Commercial M&R (CMU)", "Single-family (EoL)", "Multi-family (EoL)", "Commercial (EoL)", "Residential Additions (EoL)", "Residential M&R (EoL)", "Commercial M&R (EoL)", "Single-family (waste)", "Multi-family (waste)", "Commercial (waste)", "Residential Additions (waste)", "Residential M&R (waste)", "Commercial M&R (waste)" )

map_u_perc_ <- map_u_abs_ / t( matrix( array( rep( colSums( map_u_abs_ ), nrow( map_u_abs_ ) ) ), nrow = length( states ) ) )

# barplot( map_u_abs_ / 1000000 , legend = rownames( map_u_abs_ ), xlab = "Analysis State", ylab = "Total Uptake (1000 ton CO2)" )
#
# barplot( map_u_perc_ * 100 , legend = rownames( map_u_abs_ ), xlab = "Analysis Year", ylab = "Percentage of Total Uptake" )

map_cem_lastYear_RES12 <- ( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * rowSums( vol_yb_RES12_lc[ , , ap ], na.rm = TRUE ) + MixDesign( app = "hc" )$cement * rowSums( vol_yb_RES12_hc[ , , ap ], na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
map_cem_lastYear_RES3AB <- ( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * rowSums( vol_yb_RES3AB_lc[ , , ap ], na.rm = TRUE ) + MixDesign( app = "hc" )$cement * rowSums( vol_yb_RES3AB_hc[ , , ap ], na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
map_cem_lastYear_RES3CDEF <- ( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * rowSums( vol_yb_RES3CDEF_lc[ , , ap ], na.rm = TRUE ) + MixDesign( app = "hc" )$cement * rowSums( vol_yb_RES3CDEF_hc[ , , ap ], na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
map_cem_lastYear_COM <- ( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * rowSums( vol_yb_COM_lc[ , , ap ], na.rm = TRUE ) + MixDesign( app = "hc" )$cement * rowSums( vol_yb_COM_hc[ , , ap ], na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
map_cem_lastYear_RES3 <- map_cem_lastYear_RES3AB + map_cem_lastYear_RES3CDEF

map_cem_lastYear_cmuRES12 <- ( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES12_lc[ , , ap ], na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES12_hc[ , , ap ], na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
map_cem_lastYear_cmuRES3AB <- ( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES3AB_lc[ , , ap ], na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES3AB_hc[ , , ap ], na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
map_cem_lastYear_cmuRES3CDEF <- ( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES3CDEF_lc[ , , ap ], na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES3CDEF_hc[ , , ap ], na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
map_cem_lastYear_cmuCOM <- ( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuCOM_lc[ , , ap ], na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuCOM_hc[ , , ap ], na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
map_cem_lastYear_cmuRES3 <- map_cem_lastYear_cmuRES3AB + map_cem_lastYear_cmuRES3CDEF

map_u_lastYear_RES12 <- rowSums( u_ay_RES12[ , , ap ], na.rm = TRUE )
map_u_lastYear_RES3AB <- rowSums( u_ay_RES3AB[ , , ap ], na.rm = TRUE )
map_u_lastYear_RES3CDEF <- rowSums( u_ay_RES3CDEF[ , , ap ], na.rm = TRUE )
map_u_lastYear_COM <- rowSums( u_ay_COM[ , , ap ], na.rm = TRUE )
map_u_lastYear_RES3 <- map_u_lastYear_RES3AB + map_u_lastYear_RES3CDEF

map_u_lastYear_cmuRES12 <- rowSums( u_ay_cmuRES12[ , , ap ], na.rm = TRUE )
map_u_lastYear_cmuRES3AB <- rowSums( u_ay_cmuRES3AB[ , , ap ], na.rm = TRUE )
map_u_lastYear_cmuRES3CDEF <- rowSums( u_ay_cmuRES3CDEF[ , , ap ], na.rm = TRUE )
map_u_lastYear_cmuCOM <- rowSums( u_ay_COM[ , , ap ], na.rm = TRUE )
map_u_lastYear_cmuRES3 <- map_u_lastYear_cmuRES3AB + map_u_lastYear_cmuRES3CDEF

map_u_lastYear_ccRES12 <- rowSums( cc_ay_RES12[ , , ap ], na.rm = TRUE )
map_u_lastYear_ccRES3AB <- rowSums( cc_ay_RES3AB[ , , ap ], na.rm = TRUE )
map_u_lastYear_ccRES3CDEF <- rowSums( cc_ay_RES3CDEF[ , , ap ], na.rm = TRUE )
map_u_lastYear_ccCOM <- rowSums( cc_ay_COM[ , , ap ], na.rm = TRUE )
map_u_lastYear_ccRES3 <- map_u_lastYear_ccRES3AB + map_u_lastYear_ccRES3CDEF

map_u_lastYear_cwRES12 <- cw_ay_RES12[ , , ap ]
map_u_lastYear_cwRES3AB <- cw_ay_RES3AB[ , , ap ]
map_u_lastYear_cwRES3CDEF <- cw_ay_RES3CDEF[ , , ap ]
map_u_lastYear_cwCOM <- cw_ay_COM[ , , ap ]
map_u_lastYear_cwRES3 <- map_u_lastYear_cwRES3AB + map_u_lastYear_cwRES3CDEF

# Note: updated to include additions

map_cem_lastYear_RES_add <- ( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * rowSums( vol_yb_RES_add_lc[ , , ap ], na.rm = TRUE ) + MixDesign( app = "hc" )$cement * rowSums( vol_yb_RES_add_hc[ , , ap ], na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )
map_cem_lastYear_cmuRES_add <- ( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES_add_lc[ , , ap ], na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES_add_hc[ , , ap ], na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) )

map_u_lastYear_RES_add <- rowSums( u_ay_RES_add[ , , ap ], na.rm = TRUE )
map_u_lastYear_cmuRES_add <- rowSums( u_ay_cmuRES_add[ , , ap ], na.rm = TRUE )

map_u_lastYear_ccRES_add <- rowSums( cc_ay_RES_add[ , , ap ], na.rm = TRUE )

map_u_lastYear_cwRES_add <- cw_ay_RES_add[ , , ap ]

# Note: updated to include M&R

map_cem_lastYear_RES_rep <- ( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "hc" )$cement * rowSums( vol_yb_RES_rep[ , , ap ], na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) ) # Note: assumed only hc
map_cem_lastYear_COM_rep <- ( ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "hc" )$cement * rowSums( vol_yb_COM_rep[ , , ap ], na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) ) # Note: assumed only hc

map_u_lastYear_RES_rep <- rowSums( u_ay_RES_rep[ , , ap ], na.rm = TRUE )
map_u_lastYear_COM_rep <- rowSums( u_ay_COM_rep[ , , ap ], na.rm = TRUE )

map_u_lastYear_cwRES_rep <- cw_ay_RES_rep[ , , ap ]
map_u_lastYear_cwCOM_rep <- cw_ay_COM_rep[ , , ap ]

write.csv(
  data.frame(
    cem_RES12 = map_cem_lastYear_RES12 + map_cem_lastYear_cmuRES12,
    cem_RES3 = map_cem_lastYear_RES3 + map_cem_lastYear_cmuRES3,
    cem_COM = map_cem_lastYear_COM + map_cem_lastYear_cmuCOM,
    cem_RES_add = map_cem_lastYear_RES_add + map_cem_lastYear_cmuRES_add,
    cem_RES_rep = map_cem_lastYear_RES_rep,
    cem_COM_rep = map_cem_lastYear_COM_rep,
    use_RES12 = map_u_lastYear_RES12 + map_u_lastYear_cmuRES12 + map_u_lastYear_cwRES12,
    use_RES3 = map_u_lastYear_RES3 + map_u_lastYear_cmuRES3 + map_u_lastYear_cwRES3,
    use_COM = map_u_lastYear_COM + map_u_lastYear_cmuCOM + map_u_lastYear_cwCOM,
    use_RES_add = map_u_lastYear_RES_add + map_u_lastYear_cmuRES_add + map_u_lastYear_cwRES_add,
    use_RES_rep = map_u_lastYear_RES_rep + map_u_lastYear_cwRES_rep,
    use_COM_rep = map_u_lastYear_COM_rep + map_u_lastYear_cwCOM_rep,
    eol_RES12 = map_u_lastYear_ccRES12,
    eol_RES3 = map_u_lastYear_ccRES3,
    eol_COM = map_u_lastYear_ccCOM,
    eol_RES_add = map_u_lastYear_ccRES_add,
    row.names = rownames( map_u_data$concrete )
  ), file = paste( pwd, "/Carbon Uptake/figs_map.csv", sep = "" )
)

# <- TEMPORARY - measuring effect of scm on uptake

# Note: what if we look at one model bldg (rather than element)?

regions <- c( "Eastern", "GreatLakes", "NorthCentral", "PacificNorthwest", "PacificSouthwest", "RockyMountains", "SouthCentral", "Southeastern" )

sa_ <- 15000

vol_ <- sa_ * 0.20 # Note: thickness of typical wall

bl <- c()

for ( region in regions ){

  for ( cmp_str in cmp_strs ){

    mix <- MixDesign( cmp_str, RegionNRMCA = region )

    binder <- mix$cement + mix$flyash + mix$slag

    scm <- ( mix$flyash + mix$slag ) / binder

    s <- mix$slag / ( mix$flyash + mix$slag )

    for ( exp_cnd in exp_cnds ){

      bl <- rbind(
        bl,
        c( region, mix$cmp_str, exp_cnd, scm, s, c_up( cmp_str, exp_cnd, ap = 60, sa = sa_, vol = vol_, RegionNRMCA = region ) )
      )

    }

  }

}

colnames( bl ) <- c( "region", "cmp_str", "exp_cnd", "scm", "slag", "vol_carb", "u", "cc" )

res <- c()

for ( region in regions ){

  for ( cmp_str in cmp_strs ){

    mix <- MixDesign( cmp_str, RegionNRMCA = region )

    # Note: hypothetical binder = cement + scm

    binder <- mix$cement + mix$flyash + mix$slag

    scm <- 0.5

    f <- 0

    s <- 1

    cement <- ( 1 - scm ) * binder

    for ( exp_cnd in exp_cnds ){

      res <- rbind(
        res,
        c( region, mix$cmp_str, exp_cnd, scm, s, c_up( cmp_str, exp_cnd, ap = 60, sa = sa_, vol = vol_, cement = cement, flyash = f * scm * binder, slag = s * scm * binder ) )
      )

    }

  }

}

colnames( res ) <- c( "region", "cmp_str", "exp_cnd", "scm", "slag", "vol_carb", "u", "cc" )

res_ <- c()

for ( case in c( "Average", "Hypothetical" ) ){

  if( case == "Average" ){

    for ( cmp_str in cmp_strs ){

      mix <- MixDesign( cmp_str )

      binder <- mix$cement + mix$flyash + mix$slag

      scm <- ( mix$flyash + mix$slag ) / binder

      s <- mix$slag / ( mix$flyash + mix$slag )

      for ( exp_cnd in exp_cnds ){

        res_ <- rbind(
          res_,
          c( case, mix$cmp_str, exp_cnd, scm, s, c_up( cmp_str, exp_cnd, ap = 60, sa = sa_, vol = vol_ ) )
        )

      }

    }

  }else{

    for ( cmp_str in cmp_strs ){

      mix <- MixDesign( cmp_str )

      # Note: hypothetical binder = cement + scm

      binder <- mix$cement + mix$flyash + mix$slag

      scm <- 0.5

      f <- 0

      s <- 1

      cement <- ( 1 - scm ) * binder

      for ( exp_cnd in exp_cnds ){

        res_ <- rbind(
          res_,
          c( case, mix$cmp_str, exp_cnd, scm, s, c_up( cmp_str, exp_cnd, ap = 60, sa = sa_, vol = vol_, cement = cement, flyash = f * scm * binder, slag = s * scm * binder ) )
        )

      }

    }

  }

}

colnames( res_ ) <- c( "region", "cmp_str", "exp_cnd", "scm", "slag", "vol_carb", "u", "cc" )

bl <- as.data.frame( bl )

res <- as.data.frame( res )

res_ <- as.data.frame( res_ )

write.csv(
  data.frame(
    "LessThan15MPa" = c( as.numeric( bl[ bl$cmp_str == "LessThan15MPa" & bl$exp_cnd == "Indoor (without cover)", ]$vol_carb ), as.numeric( res[ res$cmp_str == "LessThan15MPa" & res$exp_cnd == "Indoor (without cover)" & res$slag == 1, ]$vol_carb ) ) / sa_,
    "15to20MPa" = c( as.numeric( bl[ bl$cmp_str == "15to20MPa" & bl$exp_cnd == "Indoor (without cover)", ]$vol_carb ), as.numeric( res[ res$cmp_str == "15to20MPa" & res$exp_cnd == "Indoor (without cover)" & res$slag == 1, ]$vol_carb ) ) / sa_,
    "25to35MPa" = c( as.numeric( bl[ bl$cmp_str == "25to35MPa" & bl$exp_cnd == "Indoor (without cover)", ]$vol_carb ), as.numeric( res[ res$cmp_str == "25to35MPa" & res$exp_cnd == "Indoor (without cover)" & res$slag == 1, ]$vol_carb ) ) / sa_,
    "MoreThan35MPa" = c( as.numeric( bl[ bl$cmp_str == "MoreThan35MPa" & bl$exp_cnd == "Indoor (without cover)", ]$vol_carb ), as.numeric( res[ res$cmp_str == "MoreThan35MPa" & res$exp_cnd == "Indoor (without cover)" & res$slag == 1, ]$vol_carb ) ) / sa_,
    row.names = c( regions, paste( regions, "_hyp", sep = "" ) )
  ), file = paste( pwd, "/Carbon Uptake/figs_ex_depth.csv", sep = "" )
)

write.csv(
  data.frame(
    "LessThan15MPa" = c( as.numeric( bl[ bl$cmp_str == "LessThan15MPa" & bl$exp_cnd == "Indoor (without cover)", ]$u ), as.numeric( res[ res$cmp_str == "LessThan15MPa" & res$exp_cnd == "Indoor (without cover)" & res$slag == 1, ]$u ) ),
    "15to20MPa" = c( as.numeric( bl[ bl$cmp_str == "15to20MPa" & bl$exp_cnd == "Indoor (without cover)", ]$u ), as.numeric( res[ res$cmp_str == "15to20MPa" & res$exp_cnd == "Indoor (without cover)" & res$slag == 1, ]$u ) ),
    "25to35MPa" = c( as.numeric( bl[ bl$cmp_str == "25to35MPa" & bl$exp_cnd == "Indoor (without cover)", ]$u ), as.numeric( res[ res$cmp_str == "25to35MPa" & res$exp_cnd == "Indoor (without cover)" & res$slag == 1, ]$u ) ),
    "MoreThan35MPa" = c( as.numeric( bl[ bl$cmp_str == "MoreThan35MPa" & bl$exp_cnd == "Indoor (without cover)", ]$u ), as.numeric( res[ res$cmp_str == "MoreThan35MPa" & res$exp_cnd == "Indoor (without cover)" & res$slag == 1, ]$u ) ),
    row.names = c( regions, paste( regions, "_hyp", sep = "" ) )
  ), file = paste( pwd, "/Carbon Uptake/figs_ex_u.csv", sep = "" )
)

write.csv(
  data.frame(
    "LessThan15MPa" = c( min( as.numeric( res_[ res_$cmp_str == "LessThan15MPa" & res_$region == "Average" & res_$exp_cnd != "Underwater" , ]$vol_carb ) ),
                         max( as.numeric( res_[ res_$cmp_str == "LessThan15MPa" & res_$region == "Average" & res_$exp_cnd != "Underwater" , ]$vol_carb ) ),
                         min( as.numeric( res_[ res_$cmp_str == "LessThan15MPa" & res_$region == "Hypothetical" & res_$exp_cnd != "Underwater" , ]$vol_carb ) ),
                         max( as.numeric( res_[ res_$cmp_str == "LessThan15MPa" & res_$region == "Hypothetical" & res_$exp_cnd != "Underwater" , ]$vol_carb ) ) ) / sa_,
    "15to20MPa" = c( min( as.numeric( res_[ res_$cmp_str == "15to20MPa" & res_$region == "Average" & res_$exp_cnd != "Underwater" , ]$vol_carb ) ),
                     max( as.numeric( res_[ res_$cmp_str == "15to20MPa" & res_$region == "Average" & res_$exp_cnd != "Underwater" , ]$vol_carb ) ),
                     min( as.numeric( res_[ res_$cmp_str == "15to20MPa" & res_$region == "Hypothetical" & res_$exp_cnd != "Underwater" , ]$vol_carb ) ),
                     max( as.numeric( res_[ res_$cmp_str == "15to20MPa" & res_$region == "Hypothetical" & res_$exp_cnd != "Underwater" , ]$vol_carb ) ) ) / sa_,
    "25to35MPa" = c( min( as.numeric( res_[ res_$cmp_str == "25to35MPa" & res_$region == "Average" & res_$exp_cnd != "Underwater" , ]$vol_carb ) ),
                     max( as.numeric( res_[ res_$cmp_str == "25to35MPa" & res_$region == "Average" & res_$exp_cnd != "Underwater" , ]$vol_carb ) ),
                     min( as.numeric( res_[ res_$cmp_str == "25to35MPa" & res_$region == "Hypothetical" & res_$exp_cnd != "Underwater" , ]$vol_carb ) ),
                     max( as.numeric( res_[ res_$cmp_str == "25to35MPa" & res_$region == "Hypothetical" & res_$exp_cnd != "Underwater" , ]$vol_carb ) ) ) / sa_,
    "MoreThan35MPa" = c( min( as.numeric( res_[ res_$cmp_str == "MoreThan35MPa" & res_$region == "Average" & res_$exp_cnd != "Underwater" , ]$vol_carb ) ),
                         max( as.numeric( res_[ res_$cmp_str == "MoreThan35MPa" & res_$region == "Average" & res_$exp_cnd != "Underwater" , ]$vol_carb ) ),
                         min( as.numeric( res_[ res_$cmp_str == "MoreThan35MPa" & res_$region == "Hypothetical" & res_$exp_cnd != "Underwater" , ]$vol_carb ) ),
                         max( as.numeric( res_[ res_$cmp_str == "MoreThan35MPa" & res_$region == "Hypothetical" & res_$exp_cnd != "Underwater" , ]$vol_carb ) ) ) / sa_,
    row.names = c( "Average_min", "Average_max", "Hypothetical_min", "Hypothetical_max" )
  ), file = paste( pwd, "/Carbon Uptake/figs_ex_sensitivity.csv", sep = "" )
)

# ->

# <- TEMPORARY - for comparing to Xi

# Note: continue here

write.csv(
  data.frame(
    lc = ( 1.02 * 0.97 + 1.15 * 0.03 ) * MixDesign( app = "lc" )$cement * ( colSums( colSums( vol_yb_RES12_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_RES3AB_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_RES3CDEF_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_COM_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_RES_add_lc, na.rm = TRUE ) ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ),
    hc = ( 1.02 * 0.97 + 1.15 * 0.03 ) * MixDesign( app = "hc" )$cement * ( colSums( colSums( vol_yb_RES12_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_RES3AB_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_RES3CDEF_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_COM_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_RES_add_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_RES_rep, na.rm = TRUE ) ) + colSums( colSums( vol_yb_COM_rep, na.rm = TRUE ) ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ),
    cmu = ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( 180 + 200 ) / 2 ) * ( colSums( colSums( vol_yb_cmuRES12_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuRES3AB_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuRES3CDEF_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuCOM_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuRES_add_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuRES12_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuRES3AB_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuRES3CDEF_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuCOM_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuRES_add_hc, na.rm = TRUE ) ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ),
    row.names = seq( 1940, 1940 + ap - 1 )
  ),
  file = paste( pwd, "/Carbon Uptake/figsxi_cmp_str_cem.csv", sep = "" )
)

write.csv(
  data.frame(
    lc = ( colSums( colSums( vol_yb_RES12_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_RES3AB_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_RES3CDEF_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_COM_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_RES_add_lc, na.rm = TRUE ) ) ),
    hc = ( colSums( colSums( vol_yb_RES12_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_RES3AB_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_RES3CDEF_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_COM_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_RES_add_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_RES_rep, na.rm = TRUE ) ) + colSums( colSums( vol_yb_COM_rep, na.rm = TRUE ) ) ),
    cmu = ( colSums( colSums( vol_yb_cmuRES12_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuRES3AB_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuRES3CDEF_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuCOM_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuRES_add_lc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuRES12_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuRES3AB_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuRES3CDEF_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuCOM_hc, na.rm = TRUE ) ) + colSums( colSums( vol_yb_cmuRES_add_hc, na.rm = TRUE ) ) ),
    row.names = seq( 1940, 1940 + ap - 1 )
  ),
  file = paste( pwd, "/Carbon Uptake/figsxi_cmp_str_vol.csv", sep = "" )
)

write.csv(
  data.frame(
    lc = ( colSums( colSums( sa_yb_RES12_lc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_RES3AB_lc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_RES3CDEF_lc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_COM_lc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_RES_add_lc, na.rm = TRUE ) ) ),
    hc = ( colSums( colSums( sa_yb_RES12_hc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_RES3AB_hc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_RES3CDEF_hc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_COM_hc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_RES_add_hc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_RES_rep, na.rm = TRUE ) ) + colSums( colSums( sa_yb_COM_rep, na.rm = TRUE ) ) ),
    cmu = ( colSums( colSums( sa_yb_cmuRES12_lc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_cmuRES3AB_lc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_cmuRES3CDEF_lc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_cmuCOM_lc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_cmuRES_add_lc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_cmuRES12_hc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_cmuRES3AB_hc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_cmuRES3CDEF_hc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_cmuCOM_hc, na.rm = TRUE ) ) + colSums( colSums( sa_yb_cmuRES_add_hc, na.rm = TRUE ) ) ),
    row.names = seq( 1940, 1940 + ap - 1 )
  ),
  file = paste( pwd, "/Carbon Uptake/figsxi_cmp_str_sa.csv", sep = "" )
)

exp_cnds_ <- c( "_outdoor", "_inground", "_finished", "_unfinished")

write.csv(
  data.frame(
    outdoor = colSums( colSums( sa_yb_RES12_lc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES3AB_lc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES3CDEF_lc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_COM_lc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES_add_lc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ),
    inground = colSums( colSums( sa_yb_RES12_lc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES3AB_lc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES3CDEF_lc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_COM_lc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES_add_lc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ),
    finished = colSums( colSums( sa_yb_RES12_lc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES3AB_lc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES3CDEF_lc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_COM_lc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES_add_lc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ),
    unfinished = colSums( colSums( sa_yb_RES12_lc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES3AB_lc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES3CDEF_lc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_COM_lc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES_add_lc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ),
    row.names = seq( 1940, 1940 + ap - 1 )
  ),
  file = paste( pwd, "/Carbon Uptake/figsxi_exp_cnd_lc.csv", sep = "" )
)

write.csv(
  data.frame(
    outdoor = colSums( colSums( sa_yb_RES12_hc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES3AB_hc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES3CDEF_hc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_COM_hc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES_add_hc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES_rep[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_COM_rep[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ),
    inground = colSums( colSums( sa_yb_RES12_hc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES3AB_hc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES3CDEF_hc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_COM_hc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES_add_hc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES_rep[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_COM_rep[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ),
    finished = colSums( colSums( sa_yb_RES12_hc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES3AB_hc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES3CDEF_hc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_COM_hc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES_add_hc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES_rep[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_COM_rep[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ),
    unfinished = colSums( colSums( sa_yb_RES12_hc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES3AB_hc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES3CDEF_hc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_COM_hc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES_add_hc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_RES_rep[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_COM_rep[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ),
    row.names = seq( 1940, 1940 + ap - 1 )
  ),
  file = paste( pwd, "/Carbon Uptake/figsxi_exp_cnd_hc.csv", sep = "" )
)

write.csv(
  data.frame(
    outdoor = colSums( colSums( sa_yb_cmuRES12_lc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES3AB_lc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES3CDEF_lc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuCOM_lc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES_add_lc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES12_hc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES3AB_hc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES3CDEF_hc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuCOM_hc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES_add_hc[ , grep( exp_cnds_[ 1 ], colnames( sa ) ), ], na.rm = TRUE ) ),
    inground = colSums( colSums( sa_yb_cmuRES12_lc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES3AB_lc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES3CDEF_lc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuCOM_lc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES_add_lc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES12_hc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES3AB_hc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES3CDEF_hc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuCOM_hc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES_add_hc[ , grep( exp_cnds_[ 2 ], colnames( sa ) ), ], na.rm = TRUE ) ),
    finished = colSums( colSums( sa_yb_cmuRES12_lc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES3AB_lc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES3CDEF_lc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuCOM_lc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES_add_lc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES12_hc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES3AB_hc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES3CDEF_hc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuCOM_hc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES_add_hc[ , grep( exp_cnds_[ 3 ], colnames( sa ) ), ], na.rm = TRUE ) ),
    unfinished = colSums( colSums( sa_yb_cmuRES12_lc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES3AB_lc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES3CDEF_lc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuCOM_lc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES_add_lc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES12_hc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES3AB_hc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES3CDEF_hc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuCOM_hc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ) +
      colSums( colSums( sa_yb_cmuRES_add_hc[ , grep( exp_cnds_[ 4 ], colnames( sa ) ), ], na.rm = TRUE ) ),
    row.names = seq( 1940, 1940 + ap - 1 )
  ),
  file = paste( pwd, "/Carbon Uptake/figsxi_exp_cnd_cmu.csv", sep = "" )
)

# ->


# <- TEMPORARY - comparing product year

map_cem_RES12 <- ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * rowSums( vol_yb_RES12_lc, na.rm = TRUE ) + MixDesign( app = "hc" )$cement * rowSums( vol_yb_RES12_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_cem_RES3AB <- ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * rowSums( vol_yb_RES3AB_lc, na.rm = TRUE ) + MixDesign( app = "hc" )$cement * rowSums( vol_yb_RES3AB_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_cem_RES3CDEF <- ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * rowSums( vol_yb_RES3CDEF_lc, na.rm = TRUE ) + MixDesign( app = "hc" )$cement * rowSums( vol_yb_RES3CDEF_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_cem_COM <- ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * rowSums( vol_yb_COM_lc, na.rm = TRUE ) + MixDesign( app = "hc" )$cement * rowSums( vol_yb_COM_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_cem_RES3 <- map_cem_RES3AB + map_cem_RES3CDEF

map_cem_cmuRES12 <- ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES12_lc, na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES12_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_cem_cmuRES3AB <- ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES3AB_lc, na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES3AB_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_cem_cmuRES3CDEF <- ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES3CDEF_lc, na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES3CDEF_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_cem_cmuCOM <- ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuCOM_lc, na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuCOM_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_cem_cmuRES3 <- map_cem_cmuRES3AB + map_cem_cmuRES3CDEF

# Note: updated to include additions

map_cem_RES_add <- ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "lc" )$cement * rowSums( vol_yb_RES_add_lc, na.rm = TRUE ) + MixDesign( app = "hc" )$cement * rowSums( vol_yb_RES_add_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )
map_cem_cmuRES_add <- ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES_add_lc, na.rm = TRUE ) + ( ( 180 + 200 ) / 2 ) * rowSums( vol_yb_cmuRES_add_hc, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 )

# Note: updated to include M&R

map_cem_RES_rep <- ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "hc" )$cement * rowSums( vol_yb_RES_rep, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) # Note: assumed all hc
map_cem_COM_rep <- ( 1.02 * 0.97 + 1.15 * 0.03 ) * ( MixDesign( app = "hc" )$cement * rowSums( vol_yb_COM_rep, na.rm = TRUE ) ) / ( 0.98 * 0.97 + 0.85 * 0.03 ) # Note: assumed all hc

write.csv(
  data.frame(
    cem_RES12 = map_cem_RES12 + map_cem_cmuRES12,
    cem_RES3 = map_cem_RES3 + map_cem_cmuRES3,
    cem_COM = map_cem_COM + map_cem_cmuCOM,
    cem_RES_add = map_cem_RES_add + map_cem_cmuRES_add,
    cem_RES_rep = map_cem_RES_rep,
    cem_COM_rep = map_cem_COM_rep,
    lca_RES12 = map_u_RES12 + map_u_cmuRES12 + map_u_ccRES12 + map_u_cwRES12,
    lca_RES3 = map_u_RES3 + map_u_cmuRES3 + map_u_ccRES3 + map_u_cwRES3,
    lca_COM = map_u_COM + map_u_cmuCOM + map_u_ccCOM + map_u_cwCOM,
    lca_RES_add = map_u_RES_add + map_u_cmuRES_add + map_u_ccRES_add + map_u_cwRES_add,
    lca_RES_rep = map_u_RES_rep + map_u_cwRES_rep,
    lca_COM_rep = map_u_COM_rep + map_u_cwCOM_rep,
    row.names = lab_StateAbbrev
  ),
  file = paste( pwd, "/Carbon Uptake/figs_lca.csv", sep = "" )
)

# ->

# # <- TEMPORARY - building lca
# 
# areas <- CalcAreas_ow( 1, "URML", "RES3A" )
# areas[ grep( "ew", char ) ] <- 0
# areas[ grep( "sw", char ) ] <- 0
# 
# ex_sa <- areas[ grep( "_sa", colnames( areas ) ) ] / 10.764 # ft2 to m2
# ex_vol <- areas[ grep( "_vol", colnames( areas ) ) ] / 35.315 # ft3 to m3
# names( ex_sa ) <- colnames( areas )[ grep( "_sa", colnames( areas ) ) ]
# names( ex_vol ) <- colnames( areas )[ grep( "_vol", colnames( areas ) ) ]
# 
# ex_sa_C <- ex_sa
# ex_sa_C[ grepl( "bw", names( ex_sa ) ) | grepl( "iw", names( ex_sa ) ) ] <- 0
# ex_sa_URM <- ex_sa - ex_sa_C
# 
# ex_vol_C <- ex_vol
# ex_vol_C[ grepl( "bw", names( ex_vol ) ) | grepl( "iw", names( ex_vol ) ) ] <- 0
# ex_vol_URM <- (ex_vol - ex_vol_C)
# 
# ex_vol_C <- ( 1 - frame_rf ) * ex_vol_C
# ex_vol_URM <- ( 1 - m_hc ) * ex_vol_URM
# 
# sa_ <- 15000
# vol_ <- sa_ * 0.20 # Note: thickness of typical wall
# 
# rainy <- c_up( app, exp_cnd = "Exposed to rain", sa = sa_, vol = vol_, RegionNRMCA = StateInfo$RegionNRMCA )
# nonrainy <- c_up( app, exp_cnd = "Sheltered from rain", sa = sa_, vol = vol_, RegionNRMCA = StateInfo$RegionNRMCA )
# finished <- c_up( app, exp_cnd = "Indoor (with cover)", sa = sa_, vol = vol_, RegionNRMCA = StateInfo$RegionNRMCA )
# unfinished <- c_up( app, exp_cnd = "Indoor (without cover)", sa = sa_, vol = vol_, RegionNRMCA = StateInfo$RegionNRMCA )
# inground <- c_up( app, exp_cnd = "In-ground", sa = sa_, vol = vol_, RegionNRMCA = StateInfo$RegionNRMCA )
# 
# u_m2_ex <- rep( 0, ncol( sa ) )
# u_m2_ex[ grep( "roof", colnames( sa ) ) ] <- ( 0.25 * rainy$u + ( 1 - 0.25 ) * nonrainy$u ) / sa_
# u_m2_ex[ grep( "outdoor", colnames( sa ) ) ] <- ( 0.25 * rainy$u + ( 1 - 0.25 ) * nonrainy$u ) / sa_
# u_m2_ex[ grep( "cs_sa_outdoor", colnames( sa ) ) ] <- nonrainy$u / sa_
# u_m2_ex[ grep( "finished", colnames( sa ) ) ] <- finished$u / sa_
# u_m2_ex[ grep( "unfinished", colnames( sa ) ) ] <- unfinished$u / sa_
# u_m2_ex[ grep( "inground", colnames( sa ) ) ] <- inground$u / sa_
# cc_m3_ex <- finished$cc / vol_ # Note: had to pick one
# 
# t( data.frame(
#   c = sum( ex_sa_URM[ grep( "c_", colnames( sa ) )[ !grep( "c_", colnames( sa ) ) %in% grep( "pc_", colnames( sa ) ) ] ] ),
#   b = sum( ex_sa_URM[ grep( "b_", colnames( sa ) ) ] ),
#   s = sum( ex_sa_URM[ grep( "s_", colnames( sa ) )[ !grep( "s_", colnames( sa ) ) %in% grep( "cs_", colnames( sa ) ) ] ] ),
#   sf = sum( ex_sa_URM[ grep( "sf_", colnames( sa ) ) ] ),
#   f = sum( ex_sa_URM[ grep( "f_", colnames( sa ) )[ !grep( "f_", colnames( sa ) ) %in% grep( "sf_", colnames( sa ) ) ] ] ),
#   cs = sum( ex_sa_URM[ grep( "cs_", colnames( sa ) ) ] ),
#   pm = sum( ex_sa_URM[ grep( "pm_", colnames( sa ) ) ] ),
#   pc = sum( ex_sa_URM[ grep( "pc_", colnames( sa ) ) ] ),
#   bw = sum( ex_sa_URM[ grep( "bw_", colnames( sa ) ) ] ),
#   ew = sum( ex_sa_URM[ grep( "ew_", colnames( sa ) ) ] ),
#   iw = sum( ex_sa_URM[ grep( "iw_", colnames( sa ) ) ] ),
#   sw = sum( ex_sa_URM[ grep( "sw_", colnames( sa ) ) ] )
# ) ) * 1.250886 +
# t( data.frame(
#   c = sum( ( ex_sa_C * u_m2_ex )[ grep( "c_", colnames( sa ) )[ !grep( "c_", colnames( sa ) ) %in% grep( "pc_", colnames( sa ) ) ] ] ),
#   b = sum( ( ex_sa_C * u_m2_ex )[ grep( "b_", colnames( sa ) ) ] ),
#   s = sum( ( ex_sa_C * u_m2_ex )[ grep( "s_", colnames( sa ) )[ !grep( "s_", colnames( sa ) ) %in% grep( "cs_", colnames( sa ) ) ] ] ),
#   sf = sum( ( ex_sa_C * u_m2_ex )[ grep( "sf_", colnames( sa ) ) ] ),
#   f = sum( ( ex_sa_C * u_m2_ex )[ grep( "f_", colnames( sa ) )[ !grep( "f_", colnames( sa ) ) %in% grep( "sf_", colnames( sa ) ) ] ] ),
#   cs = sum( ( ex_sa_C * u_m2_ex )[ grep( "cs_", colnames( sa ) ) ] ),
#   pm = sum( ( ex_sa_C * u_m2_ex )[ grep( "pm_", colnames( sa ) ) ] ),
#   pc = sum( ( ex_sa_C * u_m2_ex )[ grep( "pc_", colnames( sa ) ) ] ),
#   bw = sum( ( ex_sa_C * u_m2_ex )[ grep( "bw_", colnames( sa ) ) ] ),
#   ew = sum( ( ex_sa_C * u_m2_ex )[ grep( "ew_", colnames( sa ) ) ] ),
#   iw = sum( ( ex_sa_C * u_m2_ex )[ grep( "iw_", colnames( sa ) ) ] ),
#   sw = sum( ( ex_sa_C * u_m2_ex )[ grep( "sw_", colnames( sa ) ) ] )
# ) )
# 
# t( data.frame(
#   c = sum( ( ex_vol_C * cc_m3_ex )[ grep( "c_", colnames( vol ) )[ !grep( "c_", colnames( vol ) ) %in% grep( "pc_", colnames( vol ) ) ] ] ),
#   b = sum( ( ex_vol_C * cc_m3_ex )[ grep( "b_", colnames( vol ) ) ] ),
#   s = sum( ( ex_vol_C * cc_m3_ex )[ grep( "s_", colnames( vol ) )[ !grep( "s_", colnames( vol ) ) %in% grep( "cs_", colnames( vol ) ) ] ] ),
#   sf = sum( ( ex_vol_C * cc_m3_ex )[ grep( "sf_", colnames( vol ) ) ] ),
#   f = sum( ( ex_vol_C * cc_m3_ex )[ grep( "f_", colnames( vol ) )[ !grep( "f_", colnames( vol ) ) %in% grep( "sf_", colnames( vol ) ) ] ] ),
#   cs = sum( ( ex_vol_C * cc_m3_ex )[ grep( "cs_", colnames( vol ) ) ] ),
#   pm = sum( ( ex_vol_C * cc_m3_ex )[ grep( "pm_", colnames( vol ) ) ] ),
#   pc = sum( ( ex_vol_C * cc_m3_ex )[ grep( "pc_", colnames( vol ) ) ] ),
#   bw = sum( ( ex_vol_C * cc_m3_ex )[ grep( "bw_", colnames( vol ) ) ] ),
#   ew = sum( ( ex_vol_C * cc_m3_ex )[ grep( "ew_", colnames( vol ) ) ] ),
#   iw = sum( ( ex_vol_C * cc_m3_ex )[ grep( "iw_", colnames( vol ) ) ] ),
#   sw = sum( ( ex_vol_C * cc_m3_ex )[ grep( "sw_", colnames( vol ) ) ] )
# ) )
# 
# 0.475 * ( ( 180 + 200 ) / 2 ) * sum( ex_vol_URM ) + MixDesign( "lc" )$cement * sum( ex_vol_C )
# 
# # ->