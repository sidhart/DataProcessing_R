### functions for summarizing and cleaning data

## function for summarizing data
summarize_data <- function(df)
{
  # loading libraries
  library(data.table)
  
  # converting to dataframe
  df <- as.data.frame(df)
  
  suppressWarnings(
  ldf <- lapply(1:ncol(df), function(k)
  {
    return(data.table(Column = colnames(df)[k],
                      Type = class(df[,k]),
                      Unique_Values = length(unique(df[,k])),
                      Missing_Values = sum(is.na(df[,k])),
                      Max = round(as.numeric(max(df[,k], na.rm=T)),4),
                      Mean = round(as.numeric(mean(df[,k], na.rm=T)),4),
                      Min = round(as.numeric(min(df[,k], na.rm=T)),4)
                      ))
  })
  )
  
  ldf <- rbindlist(ldf)
  
  return(ldf)
}


## function for removing constant columns
remove_redundant_columns <- function(df)
{
	count_unique <- lapply(df, function(k){length(unique(k))})
	constant_columns <- names(count_unique[count_unique == 1])

	if (length(constant_columns) > 0)
	{
		df <- df[, .SD, .SDcols=-c(constant_columns)]
    
    for (i in 1:length(constant_columns))
    {
      cat("Column:", constant_columns[i], "is a redundant column\n")      
    }
	}else
	{
    cat("No redundant columns found\n")
	}
	
	cat("\n")
  
  return(df)
}


## function for removing duplicate columns
remove_duplicate_columns <- function(df)
{
  dups <- sum(duplicated(lapply(df,c)))
  
  if (dups > 0)
  {
    df <- df[!duplicated(lapply(df,c))]
    
    cat(dups, "duplicate columns removed from data\n")
  }else
  {
    cat("No duplicate columns found\n")
  }
  
  cat("\n")
  
  return(df)
}


## function for converting two-element columns to binary
convert_binary_columns <- function(df)
{
	count_unique <- lapply(df, function(k){length(unique(k))})
	binary_columns <- names(count_unique[count_unique == 2])

	change <- 0

	for (i in which(colnames(df) %in% binary_columns))
	{
		if (all(unique(df[[i]]) %in% c(0,1)) != T)
		{
      # converting column to binary
			df[[i]] <- as.numeric(as.factor(df[[i]])) - 1

			change <- 1

			cat("Column", colnames(df)[i], "converted to binary column\n")
		}
	}

	if (change == 0)
	{
		cat("No binary columns found\n")
	}

	cat("\n")
  
  return(df)
}


## function for cleaning data by combining cleaning functions
clean_data <- function(df)
{
  # loading libraries
  library(data.table)
  
	df <- remove_redundant_columns(df)
  df <- remove_duplicate_columns(df)
  df <- convert_binary_columns(df)
  
  return(df)
}

