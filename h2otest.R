
library(h2o)
h2o.init()
h2o.removeAll()
setwd("/practicum2")

data   <- h2o.importFile(path = normalizePath("/practicum2/data/county/2016CHR_CSV_Analytic_Data_v2.csv"))

names(data)

require(magrittr)

data %>% h2o.mut

library(sparklyr)
library(rsparkling)
library(h2o)
library(dplyr)

sc <- spark_connect("local")

mtcars_tbl <- copy_to(sc, mtcars, "mtcars")

partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  mutate(cyl8 = cyl == 8) %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 1099)

training <- as_h2o_frame(sc, partitions$training, strict_version_check = FALSE)
test <- as_h2o_frame(sc, partitions$test, strict_version_check = FALSE)

detach("package:rsparkling", unload = TRUE)
if ("package:h2o" %in% search()) { detach("package:h2o", unload = TRUE) }
if (isNamespaceLoaded("h2o")){ unloadNamespace("h2o") }
remove.packages("h2o")
install.packages("h2o", type = "source", repos =  "https://h2o-release.s3.amazonaws.com/h2o/rel-weierstrass/2/R")
