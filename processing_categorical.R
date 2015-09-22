### functions for categorical data processing
# 1. One-hot encoding categorical features


## loading libraries
library(dummies)
library(plyr)


## function for one-hot encoding categorical features
onehot_encode_categories <- function(X_train,X_test=data.frame())
{
	# creating panel
	cat("Creating panel\n")

	if (nrow(X_test) > 0)
	{
		panel <- rbind(X_train,X_test)
	}else
	{
		panel <- X_train
	}

	# extracting categorical columns
	categorical_columns <- NULL

	for (i in 1:ncol(panel))
	{
		if (class(panel[,i]) %in% c("character", "factor"))
		{
			categorical_columns <- c(categorical_columns, colnames(panel)[i])
		}
	}

	# creating dummy features
	if (length(categorical_columns) > 0)
	{
		cat("One-hot encoding the categorical features\n")

		panel <- dummy.data.frame(panel, names=categorical_columns, sep="_")
		colnames(panel) <- gsub("[[:punct:]]", "", colnames(panel))
		colnames(panel) <- gsub("[[:space:]]+", " ", colnames(panel))
		colnames(panel) <- gsub(" ", "_", colnames(panel))

		cat("One-hot encoded", length(categorical_columns), "categorical features\n")
	}else
	{
		cat("No categorical features found\n")
	}

	X_train <- panel[1:nrow(X_train),]
	X_test <- panel[(nrow(X_train)+1):nrow(panel),]

	return(list(X_train,X_test))
}

