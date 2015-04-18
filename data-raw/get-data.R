library(RCurl)
library(dplyr)

# data from https://github.com/ledell/h2oEnsemble-benchmarks
# c subset of the original HIGGS.csv file
higgs_1M <- getURL("https://s3.amazonaws.com/uciml-higgs/higgs_1M.csv")

# last 500k observations of HIGGS.csv are the test set.
higgs_test <- getURL("https://s3.amazonaws.com/uciml-higgs/higgs_test.csv")
labels_higgs_test <- getURL("https://s3.amazonaws.com/uciml-higgs/labels_higgs_test.csv")

names(higgs_1M) <- c("class", "lepton_pT", "lepton_eta", "lepton_phi", "missing_energy_magnitude",
                     "missing_energy_phi", "jet_1_pt", "jet_1_eta", "jet_1_phi", "jet_1_b_tag", "jet_2_pt",
                     "jet_2_eta", "jet_2_phi", "jet_2_b_tag", "jet_3_pt", "jet_3_eta", "jet_3_phi",
                     "jet_3_b_tag", "jet_4_pt", "jet_4_eta", "jet_4_phi", "jet_4_b_tag", "m_jj", "m_jjj",
                     "m_lv", "m_jlv", "m_bb", "m_wbb", "m_wwbb")
names(higgs_test) <- names(higgs_1M)
names(labels_higgs_test) <- "class"

# create samples of varying unbalancedness
set.seed(503503) #reproducible samples
n <- 1e4
p <- c(.01, .02, .05, .1)

higgs_1 <- higgs_1M %>%
  filter(class == 0) %>%
  sample_n(n*(1-p[1])) %>%
  rbind(higgs_1M %>%
          filter(class == 1) %>%
          sample_n(n*(p[1])))

higgs_2 <- higgs_1M %>%
  filter(class == 0) %>%
  sample_n(n*(1-p[2])) %>%
  rbind(higgs_1M %>%
          filter(class == 1) %>%
          sample_n(n*(p[2])))

higgs_5 <- higgs_1M %>%
  filter(class == 0) %>%
  sample_n(n*(1-p[3])) %>%
  rbind(higgs_1M %>%
          filter(class == 1) %>%
          sample_n(n*(p[3])))

higgs_10 <- higgs_1M %>%
  filter(class == 0) %>%
  sample_n(n*(1-p[4])) %>%
  rbind(higgs_1M %>%
          filter(class == 1) %>%
          sample_n(n*(p[4])))


test_higgs_1 <- higgs_test %>%
  filter(class == 0) %>%
  sample_n(n*(1-p[1])/2) %>%
  rbind(higgs_test %>%
          filter(class == 1) %>%
          sample_n(n*(p[1])/2))

test_higgs_2 <- higgs_test %>%
  filter(class == 0) %>%
  sample_n(n*(1-p[2])/2) %>%
  rbind(higgs_test %>%
          filter(class == 1) %>%
          sample_n(n*(p[2])/2))

test_higgs_5 <- higgs_test %>%
  filter(class == 0) %>%
  sample_n(n*(1-p[3])/2) %>%
  rbind(higgs_test %>%
          filter(class == 1) %>%
          sample_n(n*(p[3])/2))

test_higgs_10 <- higgs_test %>%
  filter(class == 0) %>%
  sample_n(n*(1-p[4])/2) %>%
  rbind(higgs_test %>%
          filter(class == 1) %>%
          sample_n(n*(p[4])/2))

devtools::use_data(higgs_1, higgs_2, higgs_5, higgs_10, test_higgs_1, test_higgs_2, test_higgs_5, test_higgs_10)