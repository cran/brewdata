brewdata <-
function( years=2015, term="F", degree="phd", focus="statistics", 
	resolution=10, map=FALSE ) {
	
	#error handling
	tryCatch({
		message("Checking user-specified parameters for errors or typos...")

		#user specified variables
		TERM = toupper(term)
		YEARS = unique( substr(years, 3, 4) )
		DEGREE = tolower(degree)
		FOCUS = tolower(focus)
		},
		error=function(cond) {
			message( 
			"One or more brewdata inputs is invalid."
			)
			stop("brewdata stopped.", call.=FALSE )
		}
	)

	#Scape GradeCafe Results Search and fill the 'data' data.frame
	app_cycles = paste( TERM, YEARS,"|", sep="", collapse="")
	tmp = nchar(app_cycles)-1
	app_cycles = substring(app_cycles,0,tmp); rm(tmp)
	
	url = paste( "http://www.thegradcafe.com/survey/index.php?q=",
		FOCUS,"&t=a&pp=250&o=d&p=", sep="")
	keep_scrolling = fetch = TRUE
	df = results = data.frame(); i=1; record_ct=0
	while( fetch )
	{
		cat( "Downloading page", i, "from GradCafe Results Search.\n" )
		page_i = paste(url,i, sep="")
		raw_i = readHTMLTable( page_i )[[1]]
		df_i = data.frame( 
			raw_i$Institution,  
			raw_i$"Decision & Date", 
			raw_i$St1,
			raw_i$"Program (Season)"
			)[-1,]
	
		colnames(df_i) = c("school","results","status","program")
	
		#Next couple lines force the loop to advance until we visit the app year.
		#The keep_scrolling variable evaluates to TRUE until we see the first
		#records. Once we see that first record, the app_year_term variable
		#takes over. The loop will continue until we get to the end of the
		#dataset we want. Note that initalizing record_ct to 0 and using max
		#record_ct guarantees the loop terminates after we've created the
		#dataset we want--and not before.
		record_ct = max( sum( grep( min(YEARS), df_i$program ) ), record_ct )
		keep_scrolling = ifelse( record_ct==0, TRUE, FALSE )
		
		#test whether or not we should break out of the loop
		app_year_term = grep( app_cycles, df_i$program )
		if( keep_scrolling | length( app_year_term )>0 ) 
		{
			df = rbind( df, df_i[app_year_term,] )
			i=i+1
		} else {
			fetch=FALSE
			data = df[ grep( DEGREE, tolower( df$program ) ), ]     
		}
	}
	#dump all the temporary and index variables
	rm(list=c("fetch","i","page_i","raw_i","df_i","df","url","record_ct") )
	
	#Fill the 'results' data.frame with the parsed info. The results in the
	#'data' data.frame are unusable strings. Call the parseResults method to 
	#gather self-reported personal statistics (gpa, gre, decision date).
	for( results_i in data$results ){
		results = rbind( results, parseResults( results_i ) )
	}
	
	#Translate all pre-2011 scores to the current scale
	V=results$gre_v; Q=results$gre_q
	I_oldv = which( V>170 ); I_oldq = which( Q>170 )
	results$gre_v[ I_oldv ] = translateScore( V[I_oldv], "verbal" )
	results$gre_q[ I_oldq ] = translateScore( Q[I_oldq], "quant" )
	
	#Find the percentiles for the scores
	v_pct = findScorePercentile( results$gre_v, "verbal" )
	q_pct = findScorePercentile( results$gre_q, "quant" )
	aw_pct = findScorePercentile( results$gre_aw, "writing" )
	percentiles = cbind( v_pct, q_pct, aw_pct )
	
	#Replace unusable strings with the parsed data, then parse the names
	school_name = parseSchools( as.matrix( data[,1] ), resolution, map )
	
	dec_stat = data.frame( results[,8], data[,-c(1:2,4)] )
	colnames( dec_stat ) = c( "decision", "status" )
	out = cbind( school_name, dec_stat, results[,1:4], percentiles, 
		results[,5:7] )
	return( out )
}
