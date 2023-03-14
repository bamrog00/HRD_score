library(devtools)
library(copynumber)
library(scarHRD)

# Get all formatted files
files = list.files(path = "../data/allele_specific_cnv/formatted_asc_files/", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)


#Create empty data frame for storage
path = '../data/allele_specific_cnv/formatted_asc_files/'
columns = c('File Name','LOH','TAI','LST','HRD_sum')
df = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(df) = columns

# Calculate scores for each file
i = 0
for (file in files){
  scores = scar_score(paste(path,file,sep = ''),seqz = FALSE, outputdir = '../data/HRD_scores')
  df[nrow(df) + 1,] = list(file, scores[1], scores[2], scores[3], scores[4])
  i = i+1
  if (i%%500 == 0){
    print(paste('Processed',as.character(i),sep=' '))
  }
}

# Save the computed scores
write.csv(df,'../data/HRD_scores_pan_cancer.csv',row.names = FALSE)

