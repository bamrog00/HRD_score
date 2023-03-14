library(devtools)
library(copynumber)
library(scarHRD)
library(ggplot2)


#### Comparing SARC results
## Summary:
# Number of files now 254
# Number of files then 253
# 0 wrong HRD
# 61 wrong TAI (difference of mostly 1 but up to 4)
# 0 Wrong LST
# 61 wrong HRD_sum (due to the TAI)
# 1 file had no match : "TCGA-SARC.d100649a-baa7-4c9d-b79c-6789b59aa12c.ascat2.allelic_specific.seg.txt"
# Same results where seen if ploidy 3.7 was added

# Load the formatted SCAR files
files = list.files(path = "../data/allele_specific_cnv/formatted_asc_files/", pattern = 'TCGA-SARC.*', all.files = FALSE,
                   full.names = FALSE, recursive = FALSE,
                   ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)


# Create empty data frame to be filled
path = '../data/allele_specific_cnv/formatted_asc_files/'
columns = c('file_name','HRD','TAI','LST','HRD_sum')
df = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(df) = columns

# Calculate the HRD scores for the SCAR files
for (file in files){
  scores = scar_score(paste(path,file,sep = ''),seqz = FALSE, outputdir = '../data/HRD_scores_SARC')
  df[nrow(df) + 1,] = list(file, scores[1], scores[2], scores[3], scores[4])
  
}

# Load the calculated values from Alicia
df_validation = data.frame(read.csv('../data/results_HRD_SARC_correct.csv'))

wrong_hrd = 0
wrong_tai = 0
wrong_lst = 0
wrong_sum = 0
no_match = 0

df_diff = data.frame(matrix(nrow = 0, ncol = 1))
colnames(df_diff) = 'Difference'
# Get file name, search for it in the validation dataframe and compare each score
for (i in 1:nrow(df)){
  name = df[i,"file_name"]
  HRD = df[i,'HRD']
  TAI = df[i,'TAI']
  LST = df[i,'LST']
  HRD_sum = df[i,'HRD_sum']
  correct_values = df_validation[df_validation$SampleID == name,]
  
  if (nrow(correct_values)==0){
    print('No match for:')
    print(name)
    no_match = no_match + 1
    next
  }
  
  if (!(HRD == correct_values$HRD)){
    print('Wrong HRD for:')
    print(name)
    print(HRD)
    print(correct_values$HRD)
    wrong_hrd = wrong_hrd + 1
  }
  if (!(TAI == correct_values$Telomeric.AI)){
    print('Wrong TAI for:')
    print(name)
    print(TAI)
    print(correct_values$Telomeric.AI)
    wrong_tai = wrong_tai + 1
    df_diff[nrow(df_diff) + 1,] = abs(TAI - correct_values$Telomeric.AI)
  }
  if (!(LST == correct_values$LST)){
    print('Wrong LST for:')
    print(name)
    print(LST)
    print(correct_values$LST)
    wrong_lst = wrong_lst + 1
  }
  if (!(HRD_sum == correct_values$HRD.sum)){
    print('Wrong HRD_sum for:')
    print(name)
    print(HRD_sum)
    print(correct_values$HRD.sum)
    wrong_sum = wrong_sum + 1
  }
}
# Summary of the differences
print('Total numbers')
print(nrow(df))
print(nrow(df_validation))
print('Number of wrong HRD')
print(wrong_hrd)
print('Number of wrong TAI')
print(wrong_tai)
print('Number of wrong LST')
print(wrong_lst)
print('Number of wrong sums')
print(wrong_sum)
print('Number of no matches')
print(no_match)

print(df)

#Save the computed scores
write.csv(df,'../data/validation_testing/results_HRD_SARC_new.csv',row.names = FALSE)
df = data.frame(read.csv('../data/validation_testing/results_HRD_SARC_new.csv'))

#Plotting:
diff_histo = ggplot(df_diff, aes(x=Difference)) + geom_histogram(binwidth = 1, color = 1, fill = 'grey') + scale_y_continuous(breaks = seq(0, 60, 5)) +
   labs(x = 'Difference value') + ggtitle("Difference in TAI scores between Alicia and Roger \n TCGA-SARC") +
  theme(plot.title = element_text(hjust = 0.5)) + stat_bin(aes(label = ..count..), geom = "text", binwidth = 1, vjust = -0.5)
diff_histo
ggsave('../data/figures_validation/difference_histo.png',diff_histo)

tai_plot = ggplot() + geom_density(data = df, aes(x=TAI, color = 'Roger'), alpha = 0.3) + geom_density(data = df_validation, aes(x=Telomeric.AI, color = 'Alicia'))+
  labs(x = "Telomeric allelic imbalance (TAI)", color = NULL) + ggtitle("TAI score value frequency from Alicia and Roger \n TCGA-SARC") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_color_manual(values = c('Roger' = 'red', 'Alicia' = 'blue')) +
  guides(color = guide_legend(override.aes = list(shape = 1)))
tai_plot
ggsave('../data/figures_validation/density_tai.png',tai_plot)

HRD_plot = ggplot() + geom_density(data = df, aes(x=HRD, color = 'Roger')) + geom_density(data = df_validation, aes(x=HRD, color = 'Alicia'))+
  labs(x = "Loss-of-heterozygosity (LOH)", color = NULL) + ggtitle("LOH score value frequency from Alicia and Roger \n TCGA-SARC") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_color_manual(values = c('Roger' = 'red', 'Alicia' = 'blue')) +
  guides(color = guide_legend(override.aes = list(shape = 1)))
HRD_plot
ggsave('../data/figures_validation/density_hrd.png',HRD_plot)

LST_plot = ggplot() + geom_density(data = df, mapping = aes(x=LST, color = 'Roger')) + geom_density(data = df_validation, aes(x=LST, color = 'Alicia'))+
  labs(x = "Large-scale transitions (LST)", color = NULL) + ggtitle("LST score value frequency from Alicia and Roger \n TCGA-SARC") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_color_manual(values = c('Roger' = 'red', 'Alicia' = 'blue')) +
  guides(color = guide_legend(override.aes = list(shape = 1)))
LST_plot
ggsave('../data/figures_validation/density_lst.png',LST_plot)

HRDs_plot = ggplot() + geom_density(data = df, mapping = aes(x=HRD_sum, color = 'Roger')) + geom_density(data = df_validation, aes(x=HRD.sum, color = 'Alicia'))+
  labs(x = "HRD sum/ HRD score", color = NULL) + ggtitle("HRDsum value frequency from Alicia and Roger \n TCGA-SARC") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_color_manual(values = c('Roger' = 'red', 'Alicia' = 'blue')) +
  guides(color = guide_legend(override.aes = list(shape = 1)))
HRDs_plot
ggsave('../data/figures_validation/density_hrds.png',HRDs_plot)
