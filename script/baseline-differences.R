load("data/europar_test.rds")
load("data/europar_taskset.rds")
load("data/europar_taskset_die2.rds")

regular <- rbind(europar_test, europar_taskset, europar_taskset_die2)

regular_base <- regular[ startsWith(regular$work, "base"),]
regular_base$work <- "regular"

load("data/icsm_hot_first.rds")
hot_first_base <- icsm_hot_first[ startsWith(icsm_hot_first$work,"base"), ]
hot_first_base$work <- "hot-first"

