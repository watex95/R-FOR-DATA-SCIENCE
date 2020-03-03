library(SparkR)
sparkR.session()

df=as.DataFrame(faithful)

head(df)

sparkR.session(sparkPackages = "com.databricks:spark-avro_2.11:3.0.0")

people <- read.df("./examples/src/main/resources/people.json", "json")
head(people)

write.df(people, path = "people.parquet", source = "parquet", mode = "overwrite")

library(parklyr)
library(tidyverse)
library(leaflet)
library(h2o)
library(DT)
library(rsparkling)

