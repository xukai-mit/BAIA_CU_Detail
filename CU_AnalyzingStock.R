library( DBI )
library( dplyr )
library( dbplyr )
library( odbc )

setwd( "C:/Users/Ipek Bensu/Desktop/EVERYTHING" )

pwd <- getwd()

year <- "18"

# state <- 12

# states <- c( 9, 23, 25, 33, 44, 50, 34, 36, 42, 10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 28, 22, 48 )

states <- c( 2, 4, 6, 8, 15, 16, 30, 35, 32, 41, 49, 53, 56, 5, 19, 17, 18, 20, 21, 22, 26, 27, 29, 28, 38, 31, 39, 40, 46, 47, 48, 55, 1, 9, 11, 10, 12, 13, 25, 24, 23, 37, 33, 34, 36, 42, 44, 45, 51, 50, 54 )


# ## west coast
# 
# group <- "WEST COAST"
# 
# states <- c( 2, 4, 6, 8, 15, 16, 30, 35, 32, 41, 49, 53, 56 )


# ## midwest
# 
# group <- "MIDWEST"
# 
# states <- c( 5, 19, 17, 18, 20, 21, 22, 26, 27, 29, 28, 38, 31, 39, 40, 46, 47, 48, 55 )


# ## east coast
# 
# group <- "EAST COAST"
# 
# states <- c( 1, 9, 11, 10, 12, 13, 25, 24, 23, 37, 33, 34, 36, 42, 44, 45, 51, 50, 54 )


# ## region 1 ( northeast )
# 
# states <- c( 9, 23, 25, 33, 44, 50 ) # division 1
# 
# states <- c( 34, 36, 42 ) # division 2
# 
# 
# ## region 3 ( south )
# 
# states <- c( 10, 11, 12, 13, 24, 37, 45, 51, 54 ) # division 5
# 
# # states <- c( 1, 21, 28, 47 ) # division 6
# states <- c( 1, 28 ) # 21, 47 no hu model
# 
# # states <- c( 5, 22, 40, 48 ) # division 7
# states <- c( 22, 48 ) # 5, 40 no hu model


## part 1: calculating and saving counts and sqfts

for ( state in states ){
  
  source( paste( pwd, "/AssignStateInfo.R", sep = "" ) )
  
  StateInfo <- AssignStateInfo( state )
  
  print( StateInfo$StateAbbrev )
  
  
  ## querying state database
  
  QueryState_HU <- function( StateInfo ){
    
    con <- dbConnect( odbc(), Driver = "SQL Server", Server = "DESKTOP-QGE4DP3\\SQLEXPRESS", Database = StateInfo$StateAbbrev, Trusted_Connection = "True" )
    
    HU <- list()
    
    HU$block$attr <- dbGetQuery( con, ' select * from "huCensusBlock" ' )
    
    HU$block$terrain <- dbGetQuery( con, ' select * from "huTerrainB" ' )
    
    HU$tract$attr <- dbGetQuery( con, ' select * from "huTract" ' )
    
    HU$tract$terrain <- dbGetQuery( con, ' select * from "huTerrain" ' )
    
    HU$tract$wind <- dbGetQuery( con, ' select * from "huHazardMapWindSpeed" ' )
    
    HU$scheme$list <- dbGetQuery( con, ' select * from "huBldgMappingList" ' )
    
    HU$SBT$list <- dbGetQuery( con, ' select * from "clBldgTypeHu" ' )
    
    return( HU )
    
  }
  
  QueryState_EQ <- function( StateInfo ){
    
    con <- dbConnect( odbc(), Driver = "SQL Server", Server = "DESKTOP-QGE4DP3\\SQLEXPRESS", Database = StateInfo$StateAbbrev, Trusted_Connection = "True" )
    
    EQ <- list()
    
    EQ$tract$attr <- dbGetQuery( con, ' select * from "eqTractAttribs" ' )
    
    EQ$scheme$list <- dbGetQuery( con, ' select * from "eqSpcBldgSchemes" ' )
    
    EQ$SBT$list <- dbGetQuery( con, ' select * from "eqclBldgType" ' )
    
    EQ$SBT$C <- dbGetQuery( con, ' select * from "eqCBldgTypeMp" ' )
    
    EQ$SBT$H <- dbGetQuery( con, ' select * from "eqHBldgTypeMp" ' )
    
    EQ$SBT$M <- dbGetQuery( con, ' select * from "eqMBldgTypeMp" ' )
    
    EQ$SBT$S <- dbGetQuery( con, ' select * from "eqSBldgTypeMp" ' )
    
    EQ$SBT$W <- dbGetQuery( con, ' select * from "eqWBldgTypeMp" ' )
    
    return( EQ )
    
  }
  
  QueryState_HZ <- function( StateInfo ){
    
    con <- dbConnect( odbc(), Driver = "SQL Server", Server = "DESKTOP-QGE4DP3\\SQLEXPRESS", Database = StateInfo$StateAbbrev, Trusted_Connection = "True" )
    
    HZ <- list()
    
    HZ$block$count <- dbGetQuery( con, ' select * from "hzBldgCountOccupB" ' )
    
    HZ$block$sqft <- dbGetQuery( con, ' select * from "hzSqFootageOccupB" ' )
    
    HZ$block$pop <- dbGetQuery( con, ' select * from "hzDemographicsB" ' )
    
    HZ$tract$count <- dbGetQuery( con, ' select * from "hzBldgCountOccupT" ' )
    
    HZ$tract$sqft <- dbGetQuery( con, ' select * from "hzSqFootageOccupT" ' )
    
    HZ$tract$pop <- dbGetQuery( con, ' select * from "hzDemographicsT" ' )
    
    HZ$county$factor <- dbGetQuery( con, ' select * from "hzMeansCountyLocationFactor" ' )
    
    HZ$scheme$list <- dbGetQuery( con, ' select * from "hzGenBldgSchemes" ' )
    
    HZ$GBT$map <- dbGetQuery( con, ' select * from "hzGenBldgScheme" ' )
    
    return( HZ )
    
  }
  
  HU <- QueryState_HU( StateInfo )
  
  EQ <- QueryState_EQ( StateInfo )
  
  HZ <- QueryState_HZ( StateInfo )
  
  
  ## querying model database
  
  QueryModel_HU <- function( HU ){
    
    con <- dbConnect( odbc(), Driver = "SQL Server", Server = "DESKTOP-QGE4DP3\\SQLEXPRESS", Database = "Hazus_model", Trusted_Connection = "True" )
    
    HU$WBC$list <- dbGetQuery( con, ' select * from "huListOfBldgChar" ' )
    
    HU$WBC$map <- dbGetQuery( con, ' select * from "huBldgMapping" ' )
    
    HU$WBT$list <- dbGetQuery( con, ' select * from "huListOfWindBldgTypes" ' )
    
    HU$SBT$list <- dbGetQuery( con, ' select * from "huGbsOccMappingList" ' )
    
    HU$SBT$map <- dbGetQuery( con, ' select * from "huGbsOccMapping" ' )
    
    return( HU )
    
  }
  
  QueryModel_EQ <- function( EQ ){
    
    con <- dbConnect( odbc(), Driver = "SQL Server", Server = "DESKTOP-QGE4DP3\\SQLEXPRESS", Database = "Hazus_model", Trusted_Connection = "True" )
    
    EQ$DL$list <- dbGetQuery( con, ' select * from "eqClDesignLevel" ' )
    
    return( EQ )
    
  }
  
  HU <- QueryModel_HU( HU )
  
  EQ <- QueryModel_EQ( EQ )
  
  
  ## calculating and saving counts and sqfts
  
  source( paste( pwd, "/CreateHUBldgScheme.R", sep = "" ) )

  CreateHUBldgScheme( )
  
  source( paste( pwd, "/CreateEQBldgScheme.R", sep = "" ) )
  
  CreateEQBldgScheme()
  
}


# counts <- c()
# 
# sqfts <- c()
# 
# rows <- c()
# 
# for ( state in states ){
#   
#   source( paste( pwd, "/AssignStateInfo.R", sep = "" ) )
#   
#   StateInfo <- AssignStateInfo( state )
#   
#   # counts_ <- read.csv( file = paste( pwd, "/HAZUS - GBT - SBT/huCount_", StateInfo$StateAbbrev, ".csv", sep = "" ) )
#   # 
#   # sqfts_ <- read.csv( file = paste( pwd, "/HAZUS - GBT - SBT/huSqft_", StateInfo$StateAbbrev, ".csv", sep = "" ) )
#   # 
#   # counts <- rbind( counts, colSums( counts_[ ,2:40 ] ) )
#   # 
#   # sqfts <- rbind( sqfts, colSums( sqfts_[ ,2:40 ] ) )
#   
#   counts_ <- read.csv( file = paste( pwd, "/HAZUS - GBT - SBT/eqCount_", StateInfo$StateAbbrev, ".csv", sep = "" ) )
# 
#   sqfts_ <- read.csv( file = paste( pwd, "/HAZUS - GBT - SBT/eqSqft_", StateInfo$StateAbbrev, ".csv", sep = "" ) )
#   
#   counts <- rbind( counts, colSums( counts_[ ,2:38 ] ) )
#   
#   sqfts <- rbind( sqfts, colSums( sqfts_[ ,2:38 ] ) )
#   
#   rows <- c( rows, StateInfo$StateAbbrev )
#   
# }
# 
# rownames( counts ) <- rows
# 
# rownames( sqfts ) <- rows
# 
# # write.csv( counts, file = paste( pwd, "/HAZUS - GBT - SBT/huCount.csv", sep = "" ) )
# # 
# # write.csv( sqfts, file = paste( pwd, "/HAZUS - GBT - SBT/huSqft.csv", sep = "" ) )
# 
# write.csv( counts, file = paste( pwd, "/HAZUS - GBT - SBT/eqCount.csv", sep = "" ) )
# 
# write.csv( sqfts, file = paste( pwd, "/HAZUS - GBT - SBT/eqSqft.csv", sep = "" ) )


## part 2: creating and saving volumes and surface areas

for ( state in states ){
  
  source( paste( pwd, "/AssignStateInfo.R", sep = "" ) )
  
  StateInfo <- AssignStateInfo( state )
  
  print( StateInfo$StateAbbrev )
  
  
  ## reading in AHS
  
  ReadAHS <- function( year, division ){
    
    # Note: need to add other years
    
    setwd( paste( pwd, "/US Census Bureau - AHS/AHS 2017 National PUF v3.1 CSV", sep = "" ) )
    
    lines <- readLines( "household.csv" )
    
    lines <- gsub( "'", "", lines )
    
    AHS <- read.csv( textConnection( lines ) )
    
    AHS_ <- AHS[ ( AHS$DIVISION == division ), ]
    
    return( AHS_ )
    
  }
  
  AHS <- ReadAHS( year, StateInfo$division )
  
  
  ## reading in RECS
  
  ReadRECS <- function( year, division ){
    
    # Note: need to add other years
    
    setwd( paste( pwd, "/DoE - RECS", sep = "" ) )
    
    lines <- readLines( paste( "recs2015_public_v4.csv", sep = "" ) )
    
    lines <- gsub( "'", "", lines )
    
    RECS <- read.csv( textConnection( lines ) )
    
    RECS_ <- RECS[ ( RECS$DIVISION == division ), ]
    
    return( RECS_ )
    
  }
  
  RECS <- ReadRECS( year, StateInfo$division )
  
  
  ## reading in CBECS
  
  ReadCBECS <- function( year, division ){
    
    # Note: need to add other years
    
    setwd( paste( pwd, "/DoE - CBECS", sep = "" ) )
    
    lines <- readLines( paste( "20", toString( year ), "_public_use_data.csv", sep = "" ) )
    
    lines <- gsub( "'", "", lines )
    
    CBECS <- read.csv( textConnection( lines ) )
    
    CBECS_ <- CBECS[ ( CBECS$CENDIV == division ), ]
    
    return( CBECS_ )
    
  }
  
  CBECS <- ReadCBECS( year, StateInfo$division )
  
  
  ## calculating and saving volumes and surface areas
  
  source( paste( pwd, "/CalculateVolumeSurfaceArea.R", sep = "" ) )
  
  CalculateVolumeSurfaceArea()
  
}


## part 3: parking garages

for ( state in states ){
  
  source( paste( pwd, "/AssignStateInfo.R", sep = "" ) )
  
  StateInfo <- AssignStateInfo( state )
  
  print( StateInfo$StateAbbrev )
  
  
  ## querying state database
  
  QueryState_HU <- function( StateInfo ){
    
    con <- dbConnect( odbc(), Driver = "SQL Server", Server = "DESKTOP-QGE4DP3\\SQLEXPRESS", Database = StateInfo$StateAbbrev, Trusted_Connection = "True" )
    
    HU <- list()
    
    HU$block$attr <- dbGetQuery( con, ' select * from "huCensusBlock" ' )
    
    HU$block$terrain <- dbGetQuery( con, ' select * from "huTerrainB" ' )
    
    HU$tract$attr <- dbGetQuery( con, ' select * from "huTract" ' )
    
    HU$tract$terrain <- dbGetQuery( con, ' select * from "huTerrain" ' )
    
    HU$tract$wind <- dbGetQuery( con, ' select * from "huHazardMapWindSpeed" ' )
    
    HU$scheme$list <- dbGetQuery( con, ' select * from "huBldgMappingList" ' )
    
    HU$SBT$list <- dbGetQuery( con, ' select * from "clBldgTypeHu" ' )
    
    return( HU )
    
  }
  
  QueryState_EQ <- function( StateInfo ){
    
    con <- dbConnect( odbc(), Driver = "SQL Server", Server = "DESKTOP-QGE4DP3\\SQLEXPRESS", Database = StateInfo$StateAbbrev, Trusted_Connection = "True" )
    
    EQ <- list()
    
    EQ$tract$attr <- dbGetQuery( con, ' select * from "eqTractAttribs" ' )
    
    EQ$scheme$list <- dbGetQuery( con, ' select * from "eqSpcBldgSchemes" ' )
    
    EQ$SBT$list <- dbGetQuery( con, ' select * from "eqclBldgType" ' )
    
    EQ$SBT$C <- dbGetQuery( con, ' select * from "eqCBldgTypeMp" ' )
    
    EQ$SBT$H <- dbGetQuery( con, ' select * from "eqHBldgTypeMp" ' )
    
    EQ$SBT$M <- dbGetQuery( con, ' select * from "eqMBldgTypeMp" ' )
    
    EQ$SBT$S <- dbGetQuery( con, ' select * from "eqSBldgTypeMp" ' )
    
    EQ$SBT$W <- dbGetQuery( con, ' select * from "eqWBldgTypeMp" ' )
    
    return( EQ )
    
  }
  
  QueryState_HZ <- function( StateInfo ){
    
    con <- dbConnect( odbc(), Driver = "SQL Server", Server = "DESKTOP-QGE4DP3\\SQLEXPRESS", Database = StateInfo$StateAbbrev, Trusted_Connection = "True" )
    
    HZ <- list()
    
    HZ$block$count <- dbGetQuery( con, ' select * from "hzBldgCountOccupB" ' )
    
    HZ$block$sqft <- dbGetQuery( con, ' select * from "hzSqFootageOccupB" ' )
    
    HZ$block$pop <- dbGetQuery( con, ' select * from "hzDemographicsB" ' )
    
    HZ$tract$count <- dbGetQuery( con, ' select * from "hzBldgCountOccupT" ' )
    
    HZ$tract$sqft <- dbGetQuery( con, ' select * from "hzSqFootageOccupT" ' )
    
    HZ$tract$pop <- dbGetQuery( con, ' select * from "hzDemographicsT" ' )
    
    HZ$county$factor <- dbGetQuery( con, ' select * from "hzMeansCountyLocationFactor" ' )
    
    HZ$scheme$list <- dbGetQuery( con, ' select * from "hzGenBldgSchemes" ' )
    
    HZ$GBT$map <- dbGetQuery( con, ' select * from "hzGenBldgScheme" ' )
    
    return( HZ )
    
  }
  
  # HU <- QueryState_HU( StateInfo )
  
  EQ <- QueryState_EQ( StateInfo )
  
  HZ <- QueryState_HZ( StateInfo )
  
  
  ## querying model database
  
  QueryModel_HU <- function( HU ){
    
    con <- dbConnect( odbc(), Driver = "SQL Server", Server = "DESKTOP-QGE4DP3\\SQLEXPRESS", Database = "Hazus_model", Trusted_Connection = "True" )
    
    HU$WBC$list <- dbGetQuery( con, ' select * from "huListOfBldgChar" ' )
    
    HU$WBC$map <- dbGetQuery( con, ' select * from "huBldgMapping" ' )
    
    HU$WBT$list <- dbGetQuery( con, ' select * from "huListOfWindBldgTypes" ' )
    
    HU$SBT$list <- dbGetQuery( con, ' select * from "huGbsOccMappingList" ' )
    
    HU$SBT$map <- dbGetQuery( con, ' select * from "huGbsOccMapping" ' )
    
    return( HU )
    
  }
  
  QueryModel_EQ <- function( EQ ){
    
    con <- dbConnect( odbc(), Driver = "SQL Server", Server = "DESKTOP-QGE4DP3\\SQLEXPRESS", Database = "Hazus_model", Trusted_Connection = "True" )
    
    EQ$DL$list <- dbGetQuery( con, ' select * from "eqClDesignLevel" ' )
    
    return( EQ )
    
  }
  
  # HU <- QueryModel_HU( HU )
  
  EQ <- QueryModel_EQ( EQ )
  
  
  ## reading in CBECS
  
  ReadCBECS <- function( year, division ){
    
    # Note: need to add other years
    
    setwd( paste( pwd, "/DoE - CBECS", sep = "" ) )
    
    lines <- readLines( paste( "20", toString( year ), "_public_use_data.csv", sep = "" ) )
    
    lines <- gsub( "'", "", lines )
    
    CBECS <- read.csv( textConnection( lines ) )
    
    CBECS_ <- CBECS[ ( CBECS$CENDIV == division ), ]
    
    return( CBECS_ )
    
  }
  
  CBECS <- ReadCBECS( year, StateInfo$division )
  
  
  ## reading in Dodge
  
  ReadDodge <- function( year, state ){
    
    # Note: need to add other years
    
    setwd( paste( pwd, "/Dodge", sep = "" ) )
    
    lines <- readLines( paste( "Dodge_20", toString( year ), "09.csv", sep = "" ) )
    
    lines <- gsub( "'", "", lines )
    
    Dodge <- read.csv( textConnection( lines ) )
    
    Dodge_ <- Dodge[ ( Dodge$State == state ), ]
    
    return( Dodge_ )
    
  }
  
  Dodge <- ReadDodge( year, StateInfo$StateAbbrev )
  
  
  # ## assigning census tract
  # 
  # County2FIPS <- function( counties, state ){
  #   
  #   setwd( pwd )
  #   
  #   lines <- readLines( "County_FIPS_Codes.csv" )
  #   
  #   lines <- gsub( "'", "", lines )
  #   
  #   FIPS <- read.csv( textConnection( lines ) )
  #   
  #   FIPS <- FIPS[ ( FIPS$State == state ), ]
  #   
  #   FIPS_ <- c()
  #   
  #   for ( county in counties ){
  #     
  #     if ( length( FIPS$FIPS[ which( tolower( FIPS$Name ) == tolower( unlist( strsplit( toString( county ), "," ) )[ 1 ] ) & FIPS$State == state ) ] ) > 0 ){
  #       
  #       FIPS_ <- c( FIPS_, FIPS$FIPS[ which( tolower( FIPS$Name ) == tolower( unlist( strsplit( toString( county ), "," ) )[ 1 ] ) & FIPS$State == state ) ] )
  #       
  #     }else{
  #       
  #       if ( county == "Dade" ){
  #         
  #         FIPS_ <- c( FIPS_, FIPS$FIPS[ which( tolower( FIPS$Name ) == tolower( "Miami-Dade" ) & FIPS$State == state ) ] )
  #         
  #       }else{
  #         
  #         FIPS_ <- c( FIPS_, FIPS$FIPS[ 1 ] )
  #         
  #       }
  #       
  #     }
  #     
  #   }
  #   
  #   return( FIPS_ )
  #   
  # }
  # 
  # DistributeTract <- function( values, counties, tracts ){
  #   
  #   values_ <- rep( 0, length( tracts ) )
  #   
  #   for ( i in 1:length( values ) ){
  #     
  #     if ( sum( strtrim( tracts, 5 ) %in% counties[ i ] ) > 0 ){
  #       
  #       values_[ strtrim( tracts, 5 ) %in% counties[ i ] ] <- values[ i ] / sum( strtrim( tracts, 5 ) %in% counties[ i ] )
  #       
  #     }else{
  #       
  #       if ( sum( strtrim( tracts, 5 ) %in% paste( "0", counties[ i ], sep = "" ) ) > 0 ){
  #         
  #         values_[ strtrim( tracts, 5 ) %in% paste( "0", counties[ i ], sep = "" ) ] <- values[ i ] / sum( strtrim( tracts, 5 ) %in% paste( "0", counties[ i ], sep = "" ) )
  #         
  #       }else{
  #         
  #         print( counties[ i ] )
  #         
  #       }
  #       
  #     }
  #     
  #   }
  #   
  #   return( values_ )
  #   
  # }
  # 
  # for ( YB in 2014:2017 ){
  #   
  #   COM10_counts <- as.data.frame( table( Dodge$County[ Dodge$Project.Type == "Parking Garages and Automotive Services" & Dodge$Year == YB ] ) )
  #   
  #   colnames( COM10_counts ) <- c( "county", "count" )
  #   
  #   COM10_counts[ ,"FIPS" ] <- County2FIPS( COM10_counts$county, StateInfo$StateAbbrev )
  #   
  #   
  #   COM10_sqfts <- aggregate( Dodge$Area[ Dodge$Project.Type == "Parking Garages and Automotive Services" & Dodge$Year == YB ], list( Dodge$County[ Dodge$Project.Type == "Parking Garages and Automotive Services" & Dodge$Year == YB ] ), FUN = sum )
  #   
  #   colnames( COM10_sqfts ) <- c( "county", "sqft" )
  #   
  #   COM10_sqfts[ ,"FIPS" ] <- County2FIPS( COM10_sqfts$county, StateInfo$StateAbbrev )
  #   
  #   
  #   HZ$tract$count$COM10I <- DistributeTract( COM10_counts$count, COM10_counts$FIPS, HZ$tract$count$Tract )
  #   
  #   # Note: I'm confused whether sqft in terms of 1000s
  #   
  #   HZ$tract$sqft$COM10F <- DistributeTract( COM10_sqfts$sqft, COM10_sqfts$FIPS, HZ$tract$sqft$Tract )
  #   
  #   
  #   source( paste( pwd, "/CreateEQBldgScheme_COM10.R", sep = "" ) )
  #   
  #   COM10_ <- CreateEQBldgScheme_COM10()
  #   
  #   COM10_$counts$YB <- rep( YB, nrow( COM10_$counts ) )
  #   
  #   COM10_$sqfts$YB <- rep( YB, nrow( COM10_$sqfts ) )
  #   
  #   if ( YB == 2014 ){
  #     
  #     counts <- COM10_$counts
  #     
  #     sqfts <- COM10_$sqfts
  #     
  #   }else{
  #     
  #     counts <- rbind( counts, COM10_$counts )
  #     
  #     sqfts <- rbind( sqfts, COM10_$sqfts )
  #     
  #   }
  #   
  # }
  # 
  # write.csv( counts, file = paste( pwd, "/HAZUS - GBT - SBT/eqCount_COM10_", StateInfo$StateAbbrev, ".csv", sep = "" ) )
  # 
  # write.csv( sqfts, file = paste( pwd, "/HAZUS - GBT - SBT/eqSqft_COM10_", StateInfo$StateAbbrev, ".csv", sep = "" ) )
  
  
  ## calculating and saving volumes and surface areas
  
  source( paste( pwd, "/CalculateVolumeSurfaceArea_COM10.R", sep = "" ) )
  
  CalculateVolumeSurfaceArea_COM10()
  
}


## part 4: creating csvs

stock_C <- c()

stock_PC <- c()

stock_RM <- c()

stock_URM <- c()

for ( state in states ){
  
  source( paste( pwd, "/AssignStateInfo.R", sep = "" ) )
  
  StateInfo <- AssignStateInfo( state )
  
  print( StateInfo$StateAbbrev )
  
  
  ## combining and saving data
  
  load( file = paste( pwd, "/HAZUS - GBT - SBT/areas_COM10_", StateInfo$StateAbbrev, ".Rda",sep = "" ) )
  
  areas_COM10 <- areas
  
  load( file = paste( pwd, "/HAZUS - GBT - SBT/areas_", StateInfo$StateAbbrev, ".Rda",sep = "" ) )
  
  areas$areas_C[ areas$areas_C$OT == "COM10", ] <- areas_COM10$areas_C[ areas_COM10$areas_C$YB %in% unique( areas$areas_C$YB ), ]
  
  areas$areas_PC[ areas$areas_PC$OT == "COM10", ] <- areas_COM10$areas_PC[ areas_COM10$areas_PC$YB %in% unique( areas$areas_PC$YB ), ]
  
  areas$areas_RM[ areas$areas_RM$OT == "COM10", ] <- areas_COM10$areas_RM[ areas_COM10$areas_RM$YB %in% unique( areas$areas_RM$YB ), ]
  
  areas$areas_URM[ areas$areas_URM$OT == "COM10", ] <- areas_COM10$areas_URM[ areas_COM10$areas_URM$YB %in% unique( areas$areas_URM$YB ), ]
  
  StateName <- rep( StateInfo$StateName, nrow( areas$areas_C ) )
  
  StateAbbrev <- rep( StateInfo$StateAbbrev, nrow( areas$areas_C ) )
  
  StateFIPS <- rep( state, nrow( areas$areas_C ) )
  
  areas$areas_C$StateName <- StateName
  
  areas$areas_C$StateAbbrev <- StateAbbrev
  
  areas$areas_C$StateFIPS <- StateFIPS
  
  areas$areas_PC$StateName <- StateName
  
  areas$areas_PC$StateAbbrev <- StateAbbrev
  
  areas$areas_PC$StateFIPS <- StateFIPS
  
  areas$areas_RM$StateName <- StateName
  
  areas$areas_RM$StateAbbrev <- StateAbbrev
  
  areas$areas_RM$StateFIPS <- StateFIPS
  
  areas$areas_URM$StateName <- StateName
  
  areas$areas_URM$StateAbbrev <- StateAbbrev
  
  areas$areas_URM$StateFIPS <- StateFIPS
  
  if( which( states == state ) == 1 ){
    
    stock_C <- areas$areas_C
    
    stock_PC <- areas$areas_PC
    
    stock_RM <- areas$areas_RM
    
    stock_URM <- areas$areas_URM
    
  }else{
    
    stock_C <- rbind( stock_C, areas$areas_C )
    
    stock_PC <- rbind( stock_PC, areas$areas_PC )
    
    stock_RM <- rbind( stock_RM, areas$areas_RM )
    
    stock_URM <- rbind( stock_URM, areas$areas_URM )
    
  }
  
}

save( stock_C, file = paste( pwd, "/Carbon Uptake/stock_C.Rda",sep = "" ) )
save( stock_PC, file = paste( pwd, "/Carbon Uptake/stock_PC.Rda",sep = "" ) )
save( stock_RM, file = paste( pwd, "/Carbon Uptake/stock_RM.Rda",sep = "" ) )
save( stock_URM, file = paste( pwd, "/Carbon Uptake/stock_URM.Rda",sep = "" ) )

write.csv( stock_C, file = paste( pwd, "/Carbon Uptake/stock_C.csv",sep = "" ) )
write.csv( stock_PC, file = paste( pwd, "/Carbon Uptake/stock_PC.csv",sep = "" ) )
write.csv( stock_RM, file = paste( pwd, "/Carbon Uptake/stock_RM.csv",sep = "" ) )
write.csv( stock_URM, file = paste( pwd, "/Carbon Uptake/stock_URM.csv",sep = "" ) )


## part 5: calculating carbon uptake

# Note: move here