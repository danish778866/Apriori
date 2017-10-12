pincerSearch <- function(fileName, support) {
    library(checkmate)
    dataSet <- read.csv(fileName)
    numCols <- ncol(dataSet)
    numRows <- nrow(dataSet)
    candidateSet <- vector(mode="character")
    frequentSet <- vector(mode="character")
    infrequentSet <- vector(mode="character")
    k <- 1
    for(i in 1:numCols) {
        candidateSet[i] <- as.character(i) 
    }
    maximalFrequentSet <- vector(mode="character")
    maximalFrequentCandidateSet <- vector(mode="character")
    maximalFrequentCandidateSet <- gsub(", ", ",", toString(1:numCols))
    #print(candidateSet)
    #print(maximalFrequentCandidateSet)
    lengthCandidateSet <- length(candidateSet)
    lengthInfrequentSet <- length(infrequentSet)
    continueAlgo <- TRUE
    while(continueAlgo) {
        print("Pass No.:")
        print(k)
	maximalFrequentSet <- readDatabaseCountAndFindFrequentSets(maximalFrequentCandidateSet, support, numRows, dataSet)
        frequentSet <- readDatabaseCountAndFindFrequentSets(candidateSet, support, numRows, dataSet)
        infrequentSet <- candidateSet[!(candidateSet %in% frequentSet)]
        print("After reading the database counts for C and MFCS")
        print("Maximal FrequentSet:")
	myPrint(maximalFrequentSet)
        print("Frequent Set")
        myPrint(frequentSet)
	print("Infrequent Set")
	myPrint(infrequentSet)
        lengthInfrequentSet <- length(infrequentSet)
        if(lengthInfrequentSet > 0) {
	    print("MFCS_Gen is called")
	    maximalFrequentCandidateSet <- maximalFrequentCandidateSetGen(maximalFrequentCandidateSet, infrequentSet)
    	} else {
	    print("MFCS_Gen is not called")
        }
	print("Current MFCS")
	myPrint(maximalFrequentCandidateSet)
	lengthFrequentSetInitial <- length(frequentSet)
	previousFrequentSet <- frequentSet
	if(length(maximalFrequentSet) > 0) {
	    print("MFS_Pruning is called")
	    frequentSet <- maximalFrequentSetPruning(frequentSet, maximalFrequentSet)
	} else {
	    print("MFS_Pruning is not called")
        }
	print("Current Frequent set")
	myPrint(frequentSet)
	lengthFrequentSetFinal <- length(frequentSet)
	print("Find candidates for the next pass")
	candidateSet <- findCandidateSetAndPrune(frequentSet, k)
	print("Current Candidates:")
	myPrint(candidateSet)
	if(lengthFrequentSetFinal < lengthFrequentSetInitial) {
	    print("Recovery happens")
	    newCandidates <- recoverCandidateSets(previousFrequentSet, maximalFrequentSet, k)
	    candidateSet <- union(candidateSet, newCandidates)
	} else {
	    print("Recovery doesn't happen")
	}
	print("Candidate Set after recovery:")
	myPrint(candidateSet)
	print("Calling MFCS_Pruning")
        candidateSet <- maximalFrequentCandidateSetPrune(candidateSet, maximalFrequentCandidateSet)
	print("Candidate set after MFCS_Pruning")
	myPrint(candidateSet)
	k <- k + 1
	lengthCandidateSet <- length(candidateSet)
    	lengthInfrequentSet <- length(infrequentSet)
	if((lengthCandidateSet == 0) && (lengthInfrequentSet == 0)) {
	    continueAlgo <- FALSE
	}
	print("------------------------------------------------------------")
	print("------------------------------------------------------------")
        #printTheSets(maximalFrequentCandidateSet, maximalFrequentSet, candidateSet, infrequentSet, frequentSet, k)
    }
    print("The final maximal frequent set:")
    print(maximalFrequentSet)
    print("*******************************************************")
    print("*******************************************************")
}

myPrint <- function(obj) {
    if(length(obj) == 0) {
	print("Empty")
    } else {
	print(obj)
    }
}

printTheSets <- function(mfcs, mfs, c, s, l, k) {
    print("Pass No.:")
    print(k-1)
    print("Next candidate set:")
    if(length(c) == 0) {
	print("Empty")
    } else {
        print(c)
    }
    print("Current frequent set:")
    if(length(l) == 0) {
	print("Empty")
    } else {
        print(l)
    }
    print("Current infrequent set:")
    if(length(s) == 0) {
	print("Empty")
    } else {
        print(s)
    }
    print("Current MFCS:")
    if(length(mfcs) == 0) {
        print("Empty")
    } else {
        print(mfcs)
    }
    print("Current MFS:")
    if(length(mfs) == 0) {
	print("Empty")
    } else {
        print(mfs)
    }
}

maximalFrequentCandidateSetPrune <- function(c, mfcs) {
    itemsInMfcs <- strsplit(mfcs, ",", TRUE)
    for(i in c) {
	itemsInI <- strsplit(i, ",", TRUE)
	foundSubset <- FALSE
	for(j in itemsInMfcs) {
	    if(testSubset(itemsInI[[1]], j)) {
		foundSubset <- TRUE
		break
	    }
	}
	if(!foundSubset) {
	    removeElem <- c(i)
	    c <- c[!(c %in% removeElem)]
	}
    }
    return(c)
}

recoverCandidateSets <- function(f, mfs, k) {
    newCandidates <- vector(mode="character")
    itemsInMfs <- strsplit(mfs, ",", TRUE)
    if(k >= 2) { 
   	for(i in f) {
	    itemsInI <- strsplit(i, ",", TRUE)
	    for(j in itemsInMfs) {
	        times <- k - 1
		found <- TRUE
		for(l in 1:times) {
		    if(is.na(match(itemsInI[[1]][l], j))) {
			found <- FALSE
			break
		    } else if(l == times) {
			matchFoundAt <- match(itemsInI[[1]][l], j)
			#print(matchFoundAt)
			#print(i)
			#print(j)
		    }
		}
		if(found) {
		   remainingItemsInJ <- length(j) - matchFoundAt
		   if(remainingItemsInJ > 0) {
		       currentElem <- gsub(", ", ",", toString(itemsInI[[1]][1:times]))
		       intValueOfKthElemInI <- strtoi(itemsInI[[1]][k])
		       for(m in 1:remainingItemsInJ) {
		           intValueOfElemAtM <- strtoi(j[m + matchFoundAt])
			   if(intValueOfElemAtM > intValueOfKthElemInI) {
			       newElem <- paste(currentElem, intValueOfKthElemInI, sep=",")
			       newElem <- paste(newElem, intValueOfElemAtM, sep=",")
			       newCandidates[length(newCandidates)+1] <- newElem
			   } else if(intValueOfElemAtM < intValueOfKthElemInI) {
			       newElem <- paste(currentElem, intValueOfElemAtM, sep=",")
			       newElem <- paste(newElem, intValueOfKthElemInI, sep=",")
			       newCandidates[length(newCandidates)+1] <- newElem
			   }
		        }
		    }
	        }
	    }
	}
    }
    return(newCandidates)
}

findCandidateSetAndPrune <- function(frequentSetLocal, subsetLength) {
    cSet <- vector(mode="character")
    if(length(frequentSetLocal) > 1) {
        iTimes <- length(frequentSetLocal) - 1
        for(i in 1:iTimes) {
            jTimes <- length(frequentSetLocal) - i
            for(j in 1:jTimes) {
	        #print("I and J")
	        #print(j)	
	        retVal <- findCandidateSet(frequentSetLocal[i], frequentSetLocal[i+j])
	        #print(retVal)
                if(!(retVal == "")) {
	            if(pruneCandidate(retVal, frequentSetLocal, subsetLength)) {
	                cSet[length(cSet) + 1] <- retVal
                     }
	        }               		
            }
	}
    }
    return(cSet)
}

findCandidateSet <- function(x, y) {
    a <- strsplit(x, ",", TRUE)
    b <- strsplit(y, ",", TRUE)
    retVal <- ""
    currentIntX <- strtoi(a[[1]][1])
    currentIntY <- strtoi(b[[1]][1])
    len <- length(a[[1]])
    #print(len)
    #print("AND HEREEEE LEN")
    #print(currentIntX)
    #print(currentIntY)
    if(currentIntX == currentIntY && len >= 2) {
        retVal <- paste(retVal, a[[1]][1], sep = "")
        matching <- 1    
    } else if(currentIntX < currentIntY && len == 1) {
	retVal <- paste(retVal, a[[1]][length(a[[1]])], sep = "")
	retVal <- paste(retVal, b[[1]][length(a[[1]])], sep = ",")
        matching <- 0
    } else {
	matching <- 0
    }
    if(matching == 1) {
	times <- len - 2
	if(times >= 1) {
             for(i in 1:times) {
	        currentIntX <- strtoi(a[[1]][i+1])
	        currentIntY <- strtoi(b[[1]][i+1])	
	        if(currentIntX == currentIntY) {
	            retVal <- paste(retVal, a[[1]][i+1], sep = ",")
	        } else {
	            matching <- 0
	            retVal <- ""
	            break
	        }
	    }
        }
        if(matching == 1) {
	    currentIntX <- strtoi(a[[1]][length(a[[1]])])
	    currentIntY <- strtoi(b[[1]][length(a[[1]])])
	    #print("HAHA")
	    #print(currentIntX)
	    #print(currentIntY)
	    if(currentIntX < currentIntY) {
		retVal <- paste(retVal, a[[1]][length(a[[1]])], sep = ",")
	        retVal <- paste(retVal, b[[1]][length(a[[1]])], sep = ",")
	    } else {
		retVal <- ""
	    }
	}
    }
    return(retVal)
}

pruneCandidate <- function(candidate, frequentSets, len) {
    x <- strsplit(candidate, ",", TRUE)    
    allSubsetsOfLen <- combn(x[[1]], len)
    numSubsets <- ncol(allSubsetsOfLen)
    correctCandidate <- TRUE
    for(i in  1:numSubsets) {
	currentSubset <- gsub(", ", ",", toString(allSubsetsOfLen[, i]))
        if(!(currentSubset %in% frequentSets)) {
	    correctCandidate <- FALSE
            break
    	}
    }
    return(correctCandidate)
}

maximalFrequentSetPruning <- function(f, mfs) {
    itemsInMfs <- strsplit(mfs, ",", TRUE)
    for(i in f) {
	itemsInI <- strsplit(i, ",", TRUE)
        iIsSubsetOfElemInMfs <- FALSE
	for(j in itemsInMfs) {
	    if(testSubset(itemsInI[[1]], j)) {
		iIsSubsetOfElemInMfs <- TRUE
		break
	    }
	}
	if(iIsSubsetOfElemInMfs) {
	    removeItem <- c(i)
	    f <- f[!(f %in% removeItem)]
	}
    }
    return(f)
}

maximalFrequentCandidateSetGen <- function(mfcs, s) {
    itemsInMfcs <- strsplit(mfcs, ",", TRUE)
    for(i in s) {
	for(j in mfcs) {
	    itemsInI <- strsplit(i, ",", TRUE)
	    itemsInJ <- strsplit(j, ",", TRUE)
	    if(testSubset(itemsInI[[1]], itemsInJ[[1]])) {
		removeItem <- c(j)
		mfcs <- mfcs[!(mfcs %in% removeItem)]		
		for(k in 1:length(itemsInI[[1]])) {
		    matchOne <- paste(itemsInI[[1]][k], ",", sep="")
		    matchTwo <- paste(",", itemsInI[[1]][k], sep="")
		    newElemOfMfcs <- gsub(matchOne, "", j)
		    newElemOfMfcs <- gsub(matchTwo, "", newElemOfMfcs)
		    itemsInNewElem <- strsplit(newElemOfMfcs, ",", TRUE)
		    newElemIsSubset <- FALSE
		    itemsInMfcs <- strsplit(mfcs, ",", TRUE)
		    for(l in itemsInMfcs) {
			if(testSubset(itemsInNewElem[[1]], l)) {
			     newElemIsSubset <- TRUE
			    break
			}
		    }
		    if(!newElemIsSubset) {
		        mfcs <- c(mfcs, newElemOfMfcs)
		    }
		}
	    }
 	}
    }
    return(mfcs)
}

readDatabaseCountAndFindFrequentSets <-  function(candidateSet, support, numRows, dataSet) {
    fSetLocal <- vector(mode="character") 
    for(i in candidateSet) {
        count <- 0
        x <- strsplit(i, ",", TRUE)
        for(j in 1:numRows) {
	    found <- 1
            for(k in 1:length(x[[1]])) {
	        colName <- paste("l", x[[1]][k], sep="")
	        if(dataSet[j, colName] == 1) {
		    #print(j)
		    #print(colName)
                } else {
		    found <- 0	
	        }    
	    }
	    if(found == 1) {
	        count <- count + 1
            }
        }
        #print("Idiot")
        #print(length(frequentSetLocal))
        #print(count)
        if(count >= support) {
	    fSetLocal[length(fSetLocal) + 1] <- i
        }
    }
    return(fSetLocal)
}
pincerSearch("DataSet.csv", 3)
pincerSearch("DataSetTwo.csv", 2)
pincerSearch("DataSetThree.csv", 3)
