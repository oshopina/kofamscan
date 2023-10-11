############################### Data Import ################################### 

ko_freq = read.table('Data/ko_frequency.tsv', header = T)
col_names = colnames(ko_freq)[2:8]
ko_freq = ko_freq[,-8]
colnames(ko_freq) = col_names

ko_freq_ra = rrarefy(t(ko_freq), min(colSums(ko_freq)))
pH = c(4, 4.5, 5, 5.5, 6, 6.5, 7)

###################### Pearson correlation ###################################

cor_results = c()
p_results = c()

for(i in colnames(ko_freq_ra)) {
  R = cor.test(ko_freq_ra[, i], pH, formula = "", method = "pearson")$estimate # cor val
  P = cor.test(ko_freq_ra[, i], pH, formula = "", method = "pearson")$p.value # P val
  cor_results[i] = R
  p_results[i] = P
}

p_results_sign = p_results[p_results < 0.05]
p_results_sign = p_results_sign[!is.na(p_results_sign)]

cor_results_sign = cor_results[names(p_results_sign)]

############################ Pearson graph ###################################
cor_graph = list()

for (i in names(cor_results_sign)) {
  df = as.data.frame(ko_freq_ra)
  
  plot = ggplot(df, aes(y = df[[i]], x = pH)) +
    geom_line(group = 1) +
    geom_point() +
    ylab(i)
  
  cor_graph[[i]] = plot
}
