### functions for basic data processing
# 1. Removing redundant columns
# 2. Converting two-element columns to binary
# 3. Removing duplicate columns
# 4. Removing duplicate rows


## loading libraries
library(plyr)


## function for removing constant columns
standard_cleaning <- function(X_train,X_test=data.frame())
{
	# finding number of unique values in columns
	ldf <- lapply(1:ncol(X_train), function(k)
					{
						return(data.frame("Column" = colnames(X_train)[k],
										  "Unique_Values" = length(unique(X_train[,k]))))
					})

	ldf <- ldply(ldf, data.frame)

	# removing redundant columns
	if (min(ldf$Unique_Values == 1))
	{
		X_train <- X_train[,!names(X_train) %in% ldf$Column[ldf$Unique_Values == 1]]
		
		if (nrow(X_test) > 0)
		{
			X_test <- X_test[,!names(X_test) %in% ldf$Column[ldf$Unique_Values == 1]]
		}

		for (i in 1:nrow(ldf[ldf$Unique_Values == 1,]))
		{
			cat("Column", ldf$Column[ldf$Unique_Values == 1][i], "is removed from data\n")
		}
	}else
	{
		cat("No redundant columns found\n")
	}

	cat("\n")

	# converting two-element columns to binary
	if (nrow(ldf[ldf$Unique_Values == 2,]) > 0)
	{
		for (i in 1:nrow(ldf[ldf$Unique_Values == 2,]))
		{
			X_train[,i] <- as.numeric(as.factor(X_train[,i])) - 1

			if (nrow(X_test) > 0)
			{
				X_test[,i] <- as.numeric(factor(X_test[,i], levels=X_train[,i])) - 1
			}

			cat("Column", ldf$Column[ldf$Unique_Values == 2][i], "is converted to binary\n")
		}
	}else
	{
		cat("No binary columns found\n")
	}

	cat("\n")

	# removing duplicate columns
	X_train_new <- X_train[!duplicated(lapply(X_train,c))]

	if (ncol(X_train_new) < ncol(X_train))
	{
		X_test <- X_test[,colnames(X_test) %in% colnames(X_train_new)]

		cat(ncol(X_train) - ncol(X_train_new), "duplicate columns removed from data\n")

		X_train <- X_train_new
	}else
	{
		cat("No duplicate columns found\n")
	}

	cat("\n")

	# removing duplicate rows
	if (length(which(duplicated(X_train))) > 0)
	{
		X_train <- X_train[!duplicated(X_train),]

		cat(length(which(duplicated(X_train))), "duplicate rows removed from data\n")
	}else
	{
		cat("No duplicate rows found\n")
	}

	return(list(X_train,X_test))
}

