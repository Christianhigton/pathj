\name{sem_import}
\alias{sem_import_parse}
\alias{sem_import_tokenize}
\title{Tokenize and parse imported SEM syntax}
\usage{
sem_import_tokenize(text, source = "lavaan")

sem_import_parse(text, source = "lavaan", variables = NULL)
}
\arguments{
\item{text}{Model syntax supplied as a character string.}
\item{source}{Syntax source such as \code{"lavaan"}, \code{"mermaid"},
\code{"mplus"}, or \code{"openmx"}.}
\item{variables}{Optional variable names used to validate imported nodes.}
}
\value{
\code{sem_import_tokenize()} returns tokens and
\code{sem_import_parse()} returns a parsed model representation.
}
\description{
Low-level helpers used by PathJ's syntax-import pipeline.
}
