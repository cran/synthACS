
ca_geo <- geo.make(state= "CA", county="*")
ca_dat <- pull_synth_data(2014, 5, geography= ca_geo)
syn <- derive_synth_datasets(ca_dat, parallel= TRUE)
test_micro <- syn[[1]][[2]];
test_macro <- syn[[1]][[1]]; 

g <- test_macro$age_by_sex[2:3]; names(test_macro) <- c("Male", "Female")
e <- round(tapply(test_micro$p, test_micro$edu_attain, sum) * test_macro$age_by_sex[1] ,0)
a <- unlist(test_macro$age_by_sex)[-c(1:3)]
a <- apply(cbind(a[1:16], a[17:32]), 1, sum); names(a) <- levels(test_micro$age)
n <- round(tapply(test_micro$p, test_micro$nativity, sum) * test_macro$age_by_sex[1] ,0)
r <- round(test_macro$pop_by_race[c(5,3,10,4,6,8,9)] * test_macro$age_by_sex[1] / 
             sum(test_macro$pop_by_race[c(5,3,10,4,6,8,9)]) ,0)
r[7] <- r[7]-1
names(r) <- levels(test_micro$race)
m <- round(tapply(test_micro$p, test_micro$marital_status, sum) * test_macro$age_by_sex[1] ,0)
m[1] <- m[1] + 1

# c_list <- add_cons

save(list= c("test_micro", "test_macro", "g", "e", "a", "n", "r", "m", "c_list", "ca_dat" ), 
     file= "C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")


##### build other test data set
rm(list=ls())

syn <- syn[c(2,4,6,8)]

g <- lapply(syn, function(l) {
  v <- l[[1]]$age_by_sex[2:3]
  names(v) <- c("Male", "Female")
  return(v)
})

a <- lapply(syn, function(l) {
  v <- l[[1]]$age_by_sex[-c(1:3)]
  v <- apply(cbind(v[1:16], v[17:32]), 1, sum)
  names(v) <- levels(l[[2]]$age)
  return(v)
})

e <- lapply(syn, function(l) {
  v <- round(tapply(l[[2]]$p, l[[2]]$edu_attain, sum) * l[[1]]$age_by_sex[1], 0)
  if (sum(v) == l[[1]]$age_by_sex[1]) {return(v)}
  else {
    v[min(v)] <- v[min(v)] + (l[[1]]$age_by_sex[1] - sum(v))
    return(v)
  }
})

save(list= c("a", "e", "g", "syn"),
     file= "C:/Github_projects/ACSpulls/synthACS/tests/testthat/par_sim_anneal.Rdata")