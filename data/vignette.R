# THIS CODE GENERATES AN EARLY (BLAND!) VERSION OF A FIELDNOTES PDF COMPENDIUM
# IT WAS DEVELOPED BEFORE THE FIELDNOTES FORM WAS CREATED IN THE PROJECT PLANNING TOOL
# IT IS RECOMMENDED THAT THE PROJECT PLANNING TOOL BE USED TO GENERATE PDFS INSTEAD
#
# #require(devtools)
# #set wd to directory in which DFOfieldnotes lives
# uninstall("DFOfieldnotes")
# install("DFOfieldnotes")
# #it gets installed here: C:/Users/keyserf/AppData/Local/R/win-library/4.4
# require(DFOfieldnotes)
# run_report(
#   fiscalyear = "2024-2025",
#   yourregion = "Maritimes",
#   yourpath = "C:/Users/keyserf/Documents/GitHub/DFOfieldnotes/raw_data",
#   token = read.csv(paste0(yourpath,"/cookie_token"), header = F)[1,1],
#   projectids = c(941, 2615)
# )
# #OUTPUT IS SAVED IN working directory/output.pdf
#
# #PREREQUISITES
# #In the yourpath folder:
# #1) Data retrieved from dataSPA package as a .rds file. Code commented out within run.R if needed. You will have used a "cookie" to retrieve this data.
# #    Enter the cookie as your "token" argument here. I saved mine as a string in a file called "cookie_token", and you can use the code below if you want to try that.
# #2) Until the sharepoint form is replaced by additional Project Planning Tool Fields, enter the sharepoint form path as "surveypath"
# #3) Move photos from Sharepoint (My Files/Apps/Microsoft Forms/DFO Maritimes Region Fieldnotes - Project Contribution) to "yourpath/images/Sharepoint photos"
#
# #testing
# fiscalyear = "2024-2025"
# yourregion = "Maritimes"
# yourpath = "C:/Users/keyserf/Documents/GitHub/DFOfieldnotes/raw_data"
# projectids = 2615
# token = read.csv(paste0(yourpath,"/cookie_token"), header = F)[1,1]
# surveypath = "C:/Users/keyserf/OneDrive - DFO-MPO/ARtS (MAR) Internal Files/Field Notes/DFO Maritimes Region Fieldnotes - Project Contribution Form.xlsx"
