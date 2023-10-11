############################### Data Import ################################### 
library(vegan)

ko_freq = read.table('Data/ko_frequency.tsv', header = T)
col_names = colnames(ko_freq)[2:8]
ko_freq = ko_freq[,-8]
colnames(ko_freq) = col_names

ko_freq_ra = rrarefy(t(ko_freq), min(colSums(ko_freq)))
pH = c(4, 4.5, 5, 5.5, 6, 6.5, 7)

############################# PERMANOVA #######################################

adonis2(ko_freq_ra ~ pH, method = 'euc', permutations = 1000)
