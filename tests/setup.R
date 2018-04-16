if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

acs_cat <-
	get_catalog( "acs" ,
		output_dir = file.path( getwd() ) )

# alabama one-year and all wyoming only
acs_cat <- subset( acs_cat , ( stateab == 'al' & time_period == '1-Year' ) | stateab == 'wy' )

acs_cat <- acs_cat[ split( seq( nrow( acs_cat ) ) , 1 + sort( seq( nrow( acs_cat ) ) %% 13 ) )[[ this_sample_break ]] , ]

# for alabama 2016, toss out other nearby states
if( any( acs_cat$stateab == 'al' & acs_cat$year == 2016 ) ){
	acs_cat <- acs_cat[ acs_cat$stateab == 'al' & acs_cat$year == 2016 , ]
}

acs_cat <- lodown( "acs" , acs_cat )

if( any( acs_cat$stateab == 'al' & acs_cat$year == 2016 & acs_cat$time_period == '1-Year' ) ){











# # alternative subsets:

# # nationwide merged table including puerto rico
# acs_cat <- subset( acs_cat , year == 2016 & time_period == '1-Year' )
# acs_cat <- lodown( "acs" , acs_cat )

# # nationwide merged table excluding puerto rico
# acs_cat <- subset( acs_cat , year == 2016 & time_period == '1-Year' & stateab != 'pr' )
# acs_cat <- lodown( "acs" , acs_cat )

library(survey)

acs_df <- 
	readRDS( 
		file.path( getwd() , 
			"acs2016_1yr.rds" ) 
	)

# because of the catalog subset above
# the `merged.rds` file is alabama only
acs_design <-
	svrepdesign(
		weight = ~pwgtp ,
		repweights = 'pwgtp[0-9]+' ,
		scale = 4 / 80 ,
		rscales = rep( 1 , 80 ) ,
		mse = TRUE ,
		type = 'JK1' ,
		data = acs_df
	)
acs_design <-
	update(
		
		acs_design ,
		
		relp = as.numeric( relp ) ,
		
		state_name =
			factor(
				as.numeric( st ) ,
				levels = 
					c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 
					11L, 12L, 13L, 15L, 16L, 17L, 18L, 
					19L, 20L, 21L, 22L, 23L, 24L, 25L, 
					26L, 27L, 28L, 29L, 30L, 31L, 32L, 
					33L, 34L, 35L, 36L, 37L, 38L, 39L, 
					40L, 41L, 42L, 44L, 45L, 46L, 47L, 
					48L, 49L, 50L, 51L, 53L, 54L, 55L, 
					56L, 72L) ,
				labels =
					c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
					"Colorado", "Connecticut", "Delaware", "District of Columbia", 
					"Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
					"Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
					"Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
					"Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
					"New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
					"Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
					"South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
					"Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico")
			) ,
		
		cit =
			factor( 
				cit , 
				levels = 1:5 , 
				labels = 
					c( 
						'born in the u.s.' ,
						'born in the territories' ,
						'born abroad to american parents' ,
						'naturalized citizen' ,
						'non-citizen'
					)
			) ,
		
		poverty_level = as.numeric( povpip ) ,
		
		married = as.numeric( mar %in% 1 ) ,
		
		sex = factor( sex , labels = c( 'male' , 'female' ) )
	)
sum( weights( acs_design , "sampling" ) != 0 )

svyby( ~ one , ~ cit , acs_design , unwtd.count )
svytotal( ~ one , acs_design )

svyby( ~ one , ~ cit , acs_design , svytotal )
svymean( ~ poverty_level , acs_design , na.rm = TRUE )

svyby( ~ poverty_level , ~ cit , acs_design , svymean , na.rm = TRUE )
svymean( ~ sex , acs_design )

svyby( ~ sex , ~ cit , acs_design , svymean )
svytotal( ~ poverty_level , acs_design , na.rm = TRUE )

svyby( ~ poverty_level , ~ cit , acs_design , svytotal , na.rm = TRUE )
svytotal( ~ sex , acs_design )

svyby( ~ sex , ~ cit , acs_design , svytotal )
svyquantile( ~ poverty_level , acs_design , 0.5 , na.rm = TRUE )

svyby( 
	~ poverty_level , 
	~ cit , 
	acs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ ssip , 
	denominator = ~ pincp , 
	acs_design ,
	na.rm = TRUE
)
sub_acs_design <- subset( acs_design , agep >= 65 )
svymean( ~ poverty_level , sub_acs_design , na.rm = TRUE )
this_result <- svymean( ~ poverty_level , acs_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ poverty_level , 
		~ cit , 
		acs_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( acs_design )
svyvar( ~ poverty_level , acs_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ poverty_level , acs_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ poverty_level , acs_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ married , acs_design ,
	method = "likelihood" )
svyttest( poverty_level ~ married , acs_design )
svychisq( 
	~ married + sex , 
	acs_design 
)
glm_result <- 
	svyglm( 
		poverty_level ~ married + sex , 
		acs_design 
	)

summary( glm_result )
library(convey)
acs_design <- convey_prep( acs_design )

svygini( ~ hincp , acs_design , na.rm = TRUE )
stopifnot( round( coef( svytotal( ~ one , acs_design ) ) , 0 ) == 4863300 )
pums_estimate <- 
	c( 285681 , 314701 , 300814 , 334318 , 327896 , 629329 , 599719 , 644212 , 
	342205 , 300254 , 464893 , 231293 , 87985 )

pums_standard_error <- 
	c( 2888 , 5168 , 5009 , 3673 , 3521 , 4825 , 4088 , 
	4398 , 5329 , 5389 , 1938 , 3214 , 2950 )

pums_margin_of_error <- 
	c( 4751 , 8501 , 8240 , 6043 , 5792 , 7937 , 6725 , 
	7234 , 8767 , 8865 , 3188 , 5287 , 4853 )

results <-
	svytotal( 
		~ as.numeric( agep %in% 0:4 ) +
		as.numeric( agep %in% 5:9 ) +
		as.numeric( agep %in% 10:14 ) +
		as.numeric( agep %in% 15:19 ) +
		as.numeric( agep %in% 20:24 ) +
		as.numeric( agep %in% 25:34 ) +
		as.numeric( agep %in% 35:44 ) +
		as.numeric( agep %in% 45:54 ) +
		as.numeric( agep %in% 55:59 ) +
		as.numeric( agep %in% 60:64 ) +
		as.numeric( agep %in% 65:74 ) +
		as.numeric( agep %in% 75:84 ) +
		as.numeric( agep %in% 85:100 ) , 
		acs_design
	)

stopifnot( all( round( coef( results ) , 0 ) == pums_estimate ) )

stopifnot( all( round( SE( results ) , 0 ) == pums_standard_error ) )

stopifnot( all( round( SE( results ) * 1.645 , 0 ) == pums_margin_of_error ) )

}
