% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/easyKAF.R
\name{easyKAF}
\alias{easyKAF}
\title{easyKAFtool}
\usage{
easyKAF(venv, lang_model)
}
\arguments{
\item{venv}{The path to the initialized python virtual environment on your computer.}

\item{lang_model}{The name of your english language model.}
}
\description{
With the path to your virtual environment and the defined english language model of choice, this function will prompt you to entry a query to search in either PubMed or Europe PMC! If this function does not work, you probably defined the venv wrong. You will need to uninstall the package, and retry with the correct path.
}
\examples{
rules <- easyKAF(venv = "C:/Users/JohnDoe/venv/mar6", lang_model = "en_core_web_sm")
}
\keyword{ARM,}
\keyword{Association-rule}
\keyword{KAF,}
\keyword{KAFtool,}
\keyword{MeSH,}
\keyword{Systox}
\keyword{classification,}
\keyword{mining,}
\keyword{visualization,}
