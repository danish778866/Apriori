apriori <- function(fileName, supportPercent) {
    print("-----------------------------------")
    dataSet <- read.csv(fileName)
    #print(dataSet$l1)
    numCols <- ncol(dataSet)
    numRows <- nrow(dataSet)
    support <- ceiling((supportPercent / 100) * numRows)
    #print(numCols)
    #print(numRows)
    candidateSet <- vector(mode="character")
    frequentSetLocal <- vector(mode="character")
    frequentSetGlobal <- vector(mode="character")
    for(i in 1:numCols) {
        candidateSet[i] <- as.character(i) 
    }
    #print(candidateSet)
    frequentSetLocal <- readDatabaseCountAndFindFrequentSets(candidateSet, support, numRows, dataSet)
    frequentSetGlobal <- c(frequentSetGlobal, frequentSetLocal)
    print("Candidate")
    print(candidateSet)
    numElems <- length(frequentSetLocal)
    subsetLength <- 1
    while(numElems > 1) {
        removalSet <- candidateSet
        candidateSet <- candidateSet[!(candidateSet %in% removalSet)]
        candidateSet <- findCandidateSetAndPrune(frequentSetLocal, subsetLength)

        #if(subsetLength == 2) {
	    #print(candidateSet)
        #}
        removalFrequentSet <- frequentSetLocal
        frequentSetLocal <- frequentSetLocal[!(frequentSetLocal %in% removalFrequentSet)]
        frequentSetLocal <- readDatabaseCountAndFindFrequentSets(candidateSet, support, numRows, dataSet)     
        frequentSetGlobal <- c(frequentSetGlobal, frequentSetLocal)

        numElems <- length(frequentSetLocal)
        subsetLength <- subsetLength + 1
        print("Pass No.:")
	print(subsetLength-1)
	print("Current Frequent set:")
        print(frequentSetLocal)
        print("Current Candidate Set:")
	print(candidateSet)
    }
    print("Final frequent set")
    print(frequentSetGlobal)
    print("-----------------------------------")
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

apriori("DataSet.csv", 20)
apriori("DataSetTwo.csv", 50)
apriori("DataSetThree.csv", 42)
