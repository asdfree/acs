# one percent sample
# the decennial census
# in miniature
library(haven)

tf_household <- tempfile()

this_url_household <-
	"https://www2.census.gov/programs-surveys/acs/data/pums/2021/1-Year/sas_hal.zip"

download.file( this_url_household , tf_household , mode = 'wb' )

unzipped_files_household <- unzip( tf_household , exdir = tempdir() )

acs_sas_household <-
	grep( '\\.sas7bdat$' , unzipped_files_household , value = TRUE )

acs_df_household <- haven::read_sas( acs_sas_household )

names( acs_df_household ) <- tolower( names( acs_df_household ) )
tf_person <- tempfile()

this_url_person <-
	"https://www2.census.gov/programs-surveys/acs/data/pums/2021/1-Year/sas_pal.zip"

download.file( this_url_person , tf_person , mode = 'wb' )

unzipped_files_person <- unzip( tf_person , exdir = tempdir() )

acs_sas_person <-
	grep( '\\.sas7bdat$' , unzipped_files_person , value = TRUE )

acs_df_person <- haven::read_sas( acs_sas_person )

names( acs_df_person ) <- tolower( names( acs_df_person ) )

acs_df_household[ , 'rt' ] <- NULL

acs_df_person[ , 'rt' ] <- NULL

acs_df <- merge( acs_df_household , acs_df_person )
	
stopifnot( nrow( acs_df ) == nrow( acs_df_person ) )

acs_df[ , 'one' ] <- 1

library(survey)

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
	ci = TRUE , na.rm = TRUE
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
library(srvyr)
acs_srvyr_design <- as_survey( acs_design )
acs_srvyr_design %>%
	summarize( mean = survey_mean( poverty_level , na.rm = TRUE ) )

acs_srvyr_design %>%
	group_by( cit ) %>%
	summarize( mean = survey_mean( poverty_level , na.rm = TRUE ) )
stopifnot( round( coef( svytotal( ~ one , acs_design ) ) , 0 ) == 5039877 )
pums_estimate <- 
	c(288139L, 299245L, 336727L, 334606L, 327102L, 635004L, 641405L, 
	615709L, 335431L, 341926L, 538367L, 265742L, 80474L)

pums_standard_error <- 
	c(2727L, 5368L, 6067L, 4082L, 4485L, 5716L, 4420L, 3706L, 4836L, 
	5100L, 2158L, 3363L, 3186L)

pums_margin_of_error <- 
	c(4486L, 8830L, 9981L, 6715L, 7378L, 9402L, 7271L, 6096L, 7956L, 
	8389L, 3550L, 5532L, 5240L)

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

