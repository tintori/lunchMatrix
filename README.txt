#~#~#~#~#
# 2018.02.07
#   README for script to assign random groups of students, 
#	post-docs, staff and faculty for departmental lunch matrix
#   Sophie Tintori sophia.tintori@gmail.com


############
INSTRUCTIONS
############

CUSTOMIZE THE INPUT FILES:

Open "matrixRoster.txt" and populate it with real names, email addresses, and positions. 
This file can have as many rows as you wish, but must be saved as a tab delineated text file.

The first two columns after email, titled "yes_please" and "no_please" are for special requests. If someone wishes to never be paired with another specific member of the department, put that name under "no_please". ("yes_please" doesn't do anything yet, but may soon).

Open "matrixMaker.R" in your preferred text editor or R gui, and adjust the settings at the top of the script (arrngment, holdNames, and rosterIn). They are described in the comments of the script.

RUN THE SCRIPT:

Make sure your working directory is set to the folder containing all your files, including the scripts.R file. Run matrixMaker.R. It should crank out three new files: (1) This month's matrix groups to email out to participants, (2) a new matrixRoster, with the new groups included in the history_1 column, so these particular pairings don't get repeated within the next 6 months, and (3) an archive of the matrixRoster as it was before you ran this command, in case something got messed up and you need to go back and re-do it. 


########
ADVANCED
########

There are currently six columns for recent history. These columns prevent recent pairings (within the last six iterations) to be matched up again. Six columns worked well for 50-60 people, but if you have many fewer participants, six iterations may be too ambitious. If you have many more participants, perhaps you can promise no repeats for more iterations. 

If you'd like to adjust the program to guard against repeats from more history or less, just add or remove history columns from the matrixRoster. If you are adding new columns, give each column a name (i.e. "history_7"), and populate the column with NAs.
