\name{sem_schema}
\alias{sem_schema_new}
\alias{sem_schema_validate}
\alias{sem_schema_capability_map}
\title{Create and validate an engine-neutral SEM schema}
\usage{
sem_schema_new(nodes = NULL, edges = NULL, metadata = list())

sem_schema_validate(schema, data_vars = NULL, engine = "lavaan")

sem_schema_capability_map(schema = NULL)
}
\arguments{
\item{nodes}{A data frame describing model nodes.}
\item{edges}{A data frame describing model edges.}
\item{metadata}{A list of model metadata.}
\item{schema}{A PathJ SEM schema.}
\item{data_vars}{Optional names of variables available in the data.}
\item{engine}{Estimation engine against which capabilities are checked.}
}
\value{
\code{sem_schema_new()} returns a \code{pathj_sem_schema};
\code{sem_schema_validate()} returns validation errors and warnings; and
\code{sem_schema_capability_map()} returns a data frame of engine capabilities.
}
\description{
Helpers for representing imported SEM models independently of any one
estimation engine.
}
