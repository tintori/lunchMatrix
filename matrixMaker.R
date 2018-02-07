#~#~#~#~#
# 2018.02.07
#   Script for assigning random groups of students, post-docs, 
#   staff and faculty for departmental lunch matrix
#   Sophie Tintori sophia.tintori@gmail.com


#~#~#~#~#
#
#   ENTER SETTINGS:
#

# Mixed groups? Or faculty separate from non-faculty?
arrngment <- "segr"         # options: "intg" "segr"

# Does anyone wish to be excluded this month?
holdNames <- c()            # if folks wish to be excluded, add their names as they are written in the matrixRoster
                            #   eg: holdNames <- c("Muhammad Ali", "Sally Ride")

# Are you running the program from the current matrixRoster file, or from an archive?
rosterIn <- "standard"      # other option is the full file path to an archived file.
                            #   eg: rosterIn <- "retired_files/2016-05-01_archive.txt"
                            #   or: rosterIn <- "~/Documents/lunchMatrix/retired_files/2016-12-01_archive.txt"

#
#
#~#~#~#~#~#~#




# Install packages and establish objects
if(as.logical(!"Biobase" %in% rownames(installed.packages()))){
    source("https://bioconductor.org/biocLite.R")
    biocLite("Biobase")
    install.packages('Biobase')
}
source('scripts.R')
lunchGroups <- list()
tempTally <- NULL
#set.seed(1)

# Import and parse roster file
rosterOutput <- importRoster(rosterIn)
lunchHistory <- rosterOutput[[1]]
lunchMatr <- rosterOutput[[2]]
lunchMatrArch <- rosterOutput[[3]]

# Make groups
if(arrngment=="intg"){
    temp <- sortLunchers(lunchMatr)
    tempTally <- temp[[1]]
    lunchGroups <- temp[[2]]
}
    
if(arrngment=="segr"){
    temp <- sortLunchers(lunchMatr[which(lunchMatr$position!="faculty"),])
    tempTally <- temp[[1]]
    lunchGroups <- temp[[2]]
    temp <- sortLunchers(lunchMatr[which(lunchMatr$position=="faculty"),])
    tempTally <- rbind(tempTally, temp[[1]])
    lunchGroups <- c(lunchGroups, temp[[2]])
}

# Evaluate whether or not the groups look even
tempTally
apply(tempTally, 1, sum)
lunchGroups

# Print out a document with the groups, names, email addresses, 
#   a "mail to" list, and a summary file to check by eye that 
#   the groups are pretty even

printMatrix()

# Slide over histories record and overwrite old roster
replaceRoster()
