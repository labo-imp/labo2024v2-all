library(readr)
library(tidyverse)

## Con Meses de Pandemia ##
path = '~/buckets/b1/flow-05/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt'
ganancia <- read_delim(path,
                         delim = "\t", escape_double = FALSE,
                         trim_ws = TRUE)

ganancia = ganancia %>% filter(semilla != -1)

ganancia %>% group_by(semilla) %>% count()

ganancia %>% arrange(desc(ganancia))

## Meses Sin Meses de Pandemia

path = '~/buckets/b1/flow-03/wf_septiembre-005/011-KA_evaluate_kaggle/ganancias_log.txt'
ganancia_1 <- read_delim(path,
                       delim = "\t", escape_double = FALSE,
                       trim_ws = TRUE)

ganancia_1 = ganancia_1 %>% filter(semilla != -1)

ganancia_1 %>% group_by(semilla) %>% count()

ganancia_1 %>% arrange(desc(ganancia))

wilcox.test(ganancia$ganancia,ganancia_1$ganancia, paired = TRUE)
