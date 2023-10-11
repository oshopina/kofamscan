############################### Data Import ################################### 

ko_freq = read.table('Data/ko_frequency.tsv', header = T)
col_names = colnames(ko_freq)[2:8]
ko_freq = ko_freq[,-8]
colnames(ko_freq) = col_names

ko_freq_ra = rrarefy(t(ko_freq), min(colSums(ko_freq)))
pH = c(4, 4.5, 5, 5.5, 6, 6.5, 7)

############################# Diversity ###############################
library(vegan)

shannon <- diversity(ko_freq_ra, index = "shannon")
chao <- estimateR(ko_freq_ra)
df_diversity = data.frame(shannon = shannon, chao = chao[3, ], sobs = chao[1, ])

library(ggplot2)

ggplot(df_diversity, aes(y = shannon, x = rownames(df_diversity))) +
  geom_line(group = 1) +
  geom_point()

ggplot(df_diversity, aes(y = chao, x = rownames(df_diversity))) +
  geom_line(group = 1) +
  geom_point()

ggplot(df_diversity, aes(y = sobs, x = rownames(df_diversity))) +
  geom_line(group = 1) +
  geom_point()

############################## Anova #####################################

anova(lm(df_diversity$shannon ~ pH))
anova(lm(df_diversity$chao ~ pH))
anova(lm(df_diversity$sobs ~ pH))
