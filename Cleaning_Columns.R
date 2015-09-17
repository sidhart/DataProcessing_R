### functions for basic data processing
# 1. Removing redundant columns
# 2. Removing duplicate columns


## loading libraries
library(plyr)


## function for removing constant columns
remove_redundant_columns <- function(X_train,X_test=data.frame())
{
	cat("Preparing Data\n")

	ldf <- lapply(1:ncol(X_train), function(k)
					{
						return(data.frame("Column" = colnames(X_train)[k],
										  "Unique_Values" = length(unique(X_train[,k]))))
					})

	ldf <- ldply(ldf, data.frame)

	if (min(ldf$Unique_Values == 1))
	{
		X_train <- X_train[,!names(X_train) %in% ldf$Column[ldf$Unique_Values == 1]]
		X_test <- X_test[,!names(X_test) %in% ldf$Column[ldf$Unique_Values == 1]]]

		for (i in 1:nrow(ldf[ldf$Unique_Values == 1,]))
		{
			cat("Column", ldf$Column[ldf$Unique_Values == 1][i]), "is removed from data\n"
		}

		return(list(X_train,X_test))
	}else
	{
		cat("No redundant columns found/n")
	}
}

