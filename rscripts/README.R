################
#### Will Barnett, August 2016
################



################
#### Explanation of files
################
# The data reading and cleaning scripts
# should be run with the following
# scripts. The working directory is set with the .RProj
# file, and all the other scripts assume relative filepaths.
# 1) fileSummary.R - this script reads all the sub-directories and files
#    in the data folder, discards obvious duplicate files, and gets a handle
#    on the size of each sheet
# 2) masterScript.R - this script goes through all of the identified sheets
#    and reads in the data. This script calls a bunch of smaller scripts, which
#    are written for various types of data formats.
# 3) cleanAlgaeFile.R - this script deals with duplicates, substitutes
#    special characters, fixes some station names, merges the taxa
#    with the taxonomic and group info, and computes biovolume
#    for some instances where Cells/L is present but BV is absent.
