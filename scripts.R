#~#~#~#~#
# 2018.02.07
#   Script for assigning random groups of students, post-docs, 
#   staff and faculty for departmental lunch matrix
#   Sophie Tintori sophia.tintori@gmail.com


importRoster <- function(rosterIn){
    # Import the current list of people participating
    lunchMatrArch <- NULL
    if(rosterIn == "standard"){
        lunchMatrArch <- read.table("matrixRoster.txt", header = T, sep="\t", row.names = 1, stringsAsFactors = F)
    } else {
        lunchMatrArch <- read.table(rosterIn, header = T, sep = "\t", row.names = 1, stringsAsFactors = F)
    }
    # Archive this file
    dateAndTime <- Sys.time()
    dateAndTime <- as.character(format(dateAndTime, format="%Y-%m-%d_%H-%M"))
    dir.create("retired_files", showWarnings = F)
    write.table(x = lunchMatrArch, file = paste("retired_files/archive_", dateAndTime, ".txt", sep=""), sep = "\t", quote = F, col.names = NA)
    print(paste("The original matrixRoster file has been archived at: ", getwd(), "/retired_files/archive_", dateAndTime, ".txt", sep=""))
    # Make sure hold names are all types in correctly
    if(!all(holdNames %in% rownames(lunchMatrArch))){
        tempHoldTypos <- holdNames[which(!holdNames %in% rownames(lunchMatrArch))]
        print("The following names may be spelled incorrectly:")
        print(paste(tempHoldTypos, collapse = ", "))
        quit()
    }
    # Exclude the one-time-excluders
    lunchMatr <- lunchMatrArch[which(!rownames(lunchMatrArch) %in% holdNames),]
    
    # Make a big table of who has recently been matched with whom
    lunchHistory <- matrix(0, nrow=dim(lunchMatr[1]), ncol=dim(lunchMatr[1]), 
                           dimnames = list(rownames(lunchMatr), rownames(lunchMatr)))
    
    for(indiv in rownames(lunchMatr)){
        tempHist <- indiv
        for(event in setdiff(colnames(lunchMatr), c("position", "email", "yes_please"))){
            if(!is.na(lunchMatr[indiv, event])){
                tempHist <- c(tempHist, strsplit(lunchMatr[indiv, event], split=",")[[1]])
            }
        }
        for(ex in tempHist[!is.na(tempHist)]){
            if(ex %in% rownames(lunchMatr)){
                lunchHistory[indiv,ex] <- 1
                lunchHistory[ex,indiv] <- 1
            }
        }
    }
    return(list(lunchHistory, lunchMatr, lunchMatrArch))
}

sortLunchers <- function(miniLunchMatr){  
    faculty <- rownames(lunchMatr[which(lunchMatr$position=="faculty"),])
    postDocs <- rownames(lunchMatr[which(lunchMatr$position=="post-doc"),])
    students <- rownames(lunchMatr[which(lunchMatr$position=="student"),])
    staff <- rownames(lunchMatr[which(lunchMatr$position=="staff"),])
    numGroups <- 0
    miniGroups <- list()
    miniTally <- NULL
    totPartic <- dim(miniLunchMatr)[1]
    if((totPartic%%4)>0){numGroups <- ceiling(totPartic/4)
    } else {numGroups <- floor(totPartic/4)}
    miniTally <- matrix(0, ncol = 4, nrow = numGroups, dimnames = list(c(1:numGroups), c("faculty", "postDocs", "staff", "students")))
    
    # Keep trying new iterations until all participants can be 
    #   placed in groups that do not include recent matches
    
    counter=0
    
    while(length(c(faculty, students, postDocs, staff))>0){
        counter <- counter +1
        print(paste("ATTEMPT # ", counter, sep = ""))
        # Reset the lists of people
        faculty <- rownames(miniLunchMatr[which(miniLunchMatr$position=="faculty"),])
        postDocs <- rownames(miniLunchMatr[which(miniLunchMatr$position=="post-doc"),])
        students <- rownames(miniLunchMatr[which(miniLunchMatr$position=="student"),])
        staff <- rownames(miniLunchMatr[which(miniLunchMatr$position=="staff"),])
        miniGroups <- list()
        miniTally <- matrix(0, ncol = 4, nrow = numGroups, dimnames = list(c(1:numGroups), c("faculty", "postDocs", "students", "staff")))
        
        # Start with one person per group
        if(length(which(miniLunchMatr$position=="faculty"))>0){ 
            for(group in 1:numGroups){
                indiv <- sample(faculty, 1)
                miniGroups[[group]] <- indiv
                faculty <- faculty[-which(faculty==indiv)]
                miniTally[group,"faculty"] <- miniTally[group,"faculty"]+1
            }
        }else{
            for(group in 1:numGroups){
                indiv <- sample(students, 1)
                miniGroups[[group]] <- indiv
                students <- students[-which(students==indiv)]
                miniTally[group,"students"] <- miniTally[group,"students"]+1
            }
        }
        
        # Sort all the others into established groups
        for(persNum in 0:(length(rownames(miniLunchMatr))-(1+numGroups))){
            library('Biobase')
            group = NULL
            smllGrps <- which(listLen(miniGroups)==min(listLen(miniGroups)))
            if(length(smllGrps)==1){group <- smllGrps}
            else {group = sample(which(listLen(miniGroups)==min(listLen(miniGroups))), 1)}
            # Establish names that are not off-limits
            viable = rownames(miniLunchMatr)
            if(length(miniGroups[[group]])==1){
                viable <- names(which(lunchHistory[,miniGroups[[group]]]==0))
            } else if (length(miniGroups[[group]]>1)){
                viable <- names(which(apply(lunchHistory[,miniGroups[[group]]],1,sum)==0))
            }
            indiv <- NULL
            
            if(length(faculty)>0){
                if(length(intersect(faculty, viable))==0){next}
                indiv <- sample(intersect(faculty, viable), 1)
                miniGroups[[group]] <- c(miniGroups[[group]], indiv)
                faculty <- faculty[-which(faculty==indiv)]
                miniTally[group,"faculty"] <- miniTally[group, "faculty"]+1
            }
            else if (length(postDocs>0)){
                if(length(intersect(postDocs, viable))==0){next}
                indiv <- sample(intersect(postDocs, viable), 1)
                miniGroups[[group]] <- c(miniGroups[[group]], indiv)
                postDocs <- postDocs[-which(postDocs==indiv)]
                miniTally[group,"postDocs"] <- miniTally[group, "postDocs"]+1
            }
            else if (length(students>0)){
                if(length(intersect(students, viable))==0){next}
                indiv <- sample(intersect(students, viable), 1)
                miniGroups[[group]] <- c(miniGroups[[group]], indiv)
                students <- students[-which(students==indiv)]
                miniTally[group,"students"] <- miniTally[group, "students"]+1
            }
            else if (length(staff>0)){
                if(length(intersect(staff, viable))==0){next}
                indiv <- sample(intersect(staff, viable), 1)
                miniGroups[[group]] <- c(miniGroups[[group]], indiv)
                staff <- staff[-which(staff==indiv)]
                miniTally[group,"staff"] <- miniTally[group, "staff"]+1
            }
        }
    }
    return(list(miniTally, miniGroups))
}

printMatrix <- function(){
    dir.create("toEmail", showWarnings = F)
    outFile = paste("toEmail/matrix_groups_", as.character(Sys.Date()), ".txt", sep="")
    sink(outFile)
    for(group in 1:length(lunchGroups)){
        cat("\n***\n")
        for(indiv in sample(lunchGroups[[group]])){
            cat(paste(indiv, "\t", lunchMatr[indiv,"email"], "\n", sep=""))
        }
    }
    cat("\n\n\n")
    cat(paste(lunchMatr$email, collapse = "; "))
    cat("\n\n\n")
    sink()
    tempTally <- cbind(tempTally, "total"=apply(tempTally, 1, sum))
    suppressWarnings(write.table(x=tempTally, file=outFile, 
                                 append = T, col.names = T, row.names = F))
    print(paste("Your output file is ready at: ", getwd(), "/", outFile, sep=""))
    print("This file lists all your groups, and email addresses for all participants.")
}

replaceRoster <- function(){ 
    # Move all the histories over one slot
    histories = setdiff(colnames(lunchMatr), c("position", "email", "yes_please", "no_please"))
    lunchMatrArch[rownames(lunchMatr),histories[2:length(histories)]] <- lunchMatrArch[rownames(lunchMatr),histories[1:length(histories)-1]]
    
    # Add a new history column
    for(group in 1:length(lunchGroups)){
        for(indiv in lunchGroups[[group]]){
            newHist <- lunchGroups[[group]][-which(lunchGroups[[group]]==indiv)]
            lunchMatrArch[indiv,"history_1"] <- paste(newHist, collapse=",")
        }
    }
    
    # Overwrite old file
    write.table(x = lunchMatrArch, file = "matrixRoster.txt", sep = "\t", quote = F, col.names = NA)
    print(paste("A new matrixRoster file has been written to: ", getwd(), "/matrixRoster.txt", sep=""))
}