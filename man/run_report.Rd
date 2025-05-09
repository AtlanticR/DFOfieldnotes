% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_report.R
\name{run_report}
\alias{run_report}
\title{the run function}
\usage{
run_report(
  fiscalyear = "2024-2025",
  yourpath = "C:/Users/keyserf/Documents/temp_data",
  yourregion = "Maritimes",
  token = "csrftoken=YOURCOOKIE; sessionid=YOURSESSIONID",
  projectids = NULL,
  surveypath =
    "C:/Users/keyserf/OneDrive - DFO-MPO/ARtS (MAR) Internal Files/Field Notes/DFO Maritimes Region Fieldnotes - Project Contribution Form.xlsx"
)
}
\arguments{
\item{fiscalyear}{"2024-2025" (default)}

\item{yourpath}{Location of data downloaded from PPT (default is working directory)}

\item{yourregion}{"Maritimes" (default)}

\item{token}{Cookie used by dataSPA to access PPT}

\item{projectids}{NULL (default), but can list as: c(2615, 941) etc.}

\item{surveypath}{"C:/Users/keyserf/OneDrive - DFO-MPO/ARtS (MAR) Internal Files/Field Notes/DFO Maritimes Region Fieldnotes - Project Contribution Form.xlsx" is default}
}
\value{
creates PDF document, default file name is output.pdf
}
\description{
This function allows you to run the report
}
\examples{

run_report(projectids=2615)
run_report(projectids=C(941, 2615))
}
