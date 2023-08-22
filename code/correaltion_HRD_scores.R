library(ggplot2)
library(data.table)
library(corrplot)
library(gridGraphics)
library(gridExtra)
library(png)
library(cowplot)
library(ComplexHeatmap)
library(dplyr)
library(corrplot)
library(RColorBrewer)
library(circlize)
library(tidyr)
# Get the data
HRD_scores = data.frame(read.csv('../data/HRD_scores_pan_cancer_annotated_typecorrect.csv'))

scores = c("HRD_sum","TAI","LOH","LST")

# Plot linear correlation between HRDsum and LOH of all cancertypes
corr_hrds_loh = ggplot(data = HRD_scores, aes(x = HRD_sum, y = LOH, color = Project.ID)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

corr_hrds_loh

# Constructs a scatterplot matrix comparing each score with all the others
pairs(HRD_scores[c('HRD_sum','LST','LOH','TAI')])

# Extract scores and cancertypes
sub_df = HRD_scores[, c("TAI", "LOH", "LST", "Project.ID")]

table(sub_df$Project.ID)

# Split the dataframe into cancer specific dataframes
df_list = split(sub_df, sub_df$Project.ID)

cor_list = list()
cancer_types = list()

# Creates a list of correlation matrixes in which per cancertype, each score is compared to all the others
for (i in seq_along(df_list)){
  cor_mat = cor(df_list[[i]][,c('TAI','LOH','LST')], method = 'spearman')
  type = unique(df_list[[i]]$Project.ID)
  if (type == 'TARGET-CCSK'){
    # Cancer type with std of 0
    print(df_list[[i]])
  }
  cancer_types = c(cancer_types, type)
  
  cor_list[[i]] = cor_mat
}


# Function to grab the plots that are created with corrplot, in order to save them
grab_grob = function(){
  grid.echo()
  grid.grab()
}



# Create a correlation plot for each cancer type comparing the different scores. 
# The plots are saved using the function grab_grob()
for (i in seq_along(cor_list)){
  corr_plot = corrplot(cor_list[[i]], method = 'color', order = 'alphabet', title = cancer_types[[i]], mar=c(0,0,1,0))
  plot = grab_grob()
  ggsave(paste('../data/figures_pan_cancer/scores_correlation/',cancer_types[[i]],'.png',sep = ''),plot,width = 8.63, height = 5.71)
}

## This last part is to construct a overview with all correlation plots

# Get all the plot created before
files = list.files(path = "../data/figures_pan_cancer/scores_correlation/", pattern = NULL, all.files = FALSE,
                   full.names = TRUE, recursive = FALSE,
                   ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# Creates the grid for the images
rl = lapply(files, png::readPNG)
gl = lapply(rl, grid::rasterGrob)

# Defining parameters for the plot
n_plots <- length(gl)
n_rows <- ceiling(sqrt(n_plots))
n_cols <- ceiling(n_plots / n_rows)

widths <- rep(40, n_cols)
heights <- rep(40, n_rows)

total_width <- sum(widths)
total_height <- sum(heights)
output_width <- total_width * 0.1
output_height <- total_height * 0.1

# Plot the summary and save it
plot = arrangeGrob(grobs=gl, ncol=n_cols, nrow=n_rows, widths=widths, heights=heights)
ggsave("../data/figures_pan_cancer/summary_correlations_plots.png",plot,width = output_width, height = output_height,limitsize = FALSE)



HRD_scores = data.frame(read.csv('../data/HRD_scores_pan_cancer_annotated_v2.csv'))
HRD_scores_primary = HRD_scores[HRD_scores$Type == 'Primary',]

HRD_scores_primary$Cancertype <- gsub("^[^-]+-", "", HRD_scores_primary$Project.ID)

empty_df <- data.frame(matrix(nrow = length(unique(HRD_scores_primary$Cancertype)),ncol = ))

subset = HRD_scores_primary[,c('Cancertype', 'LST', 'LOH', 'TAI', 'HRD_sum')]
subset_t = t(subset)
colnames(subset_t) <- subset_t['Cancertype',]

# Remove the 'my_column' column since it's now the rownames
subset_t <- subset_t[-1, ]

cor_by_cancer_type <- by(subset[, -1], subset$CancerType, cor)

# Create a list to store the correlation data
correlation_list <- list()

# Get unique cancer types from the original dataframe
cancer_types <- unique(HRD_scores_primary$Cancertype)

# Iterate over each cancer type and calculate the correlations
for (cancer_type in cancer_types) {
  # Filter the dataframe for the current cancer type
  filtered_df <- filter(HRD_scores_primary, Cancertype == cancer_type)
  
  if (cancer_type == "CCSK") {
    # For CCSK, calculate correlation between TAI and LST only
    correlation_matrix <- cor(filtered_df[, c("TAI", "LST")], use="pairwise.complete.obs")
    
    # Fill NaN for LOH correlation
    correlation_matrix <- cbind(correlation_matrix, NaN)
    correlation_matrix <- rbind(correlation_matrix, NaN)
    correlation_matrix[3,3] = as.double(1.0000)
    
    #names(my_df)[names(my_df) == "Age"] <- "Age_Group"
    
  } else {
    # For other cancer types, calculate correlation between all three scores
    correlation_matrix <- cor(filtered_df[, c("TAI", "LST", "LOH")], use="pairwise.complete.obs")
  }
  
  # Extract correlation values and convert to a dataframe
  correlation_values <- as.data.frame((correlation_matrix))
  
  # Rename the columns of the correlation_values dataframe
  colnames(correlation_values) <- c(paste0(cancer_type, "-TAI"), paste0(cancer_type, "-LST"), paste0(cancer_type, "-LOH"))
  rownames(correlation_values) <- c(paste0(cancer_type, "-TAI"), paste0(cancer_type, "-LST"), paste0(cancer_type, "-LOH"))
  # Store the correlation values in the list
  correlation_list[[cancer_type]] <- correlation_values
}

# Combine the correlation values for all cancer types into a single dataframe
correlation_df <- do.call(bind_rows, correlation_list)

# Replace NA with NaN
correlation_df[is.na(correlation_df)] <- NaN

# Print the resulting dataframe
print(correlation_df)

mat <- as.matrix(correlation_df)

# Step 2: Set the upper triangle elements to NA
mat[upper.tri(mat)] <- NA

# Step 3: Convert the matrix back to a dataframe (if needed)
triangular_df <- as.data.frame(mat)

colnames(mat) = colnames(correlation_df)
rownames(mat) = rownames(correlation_df)

numbers = length(colnames(correlation_df))/3
#ha = HeatmapAnnotation(Cancertype = data_measurement$ProjectID, HRDType = data_measurement$HRDtype, col = list(Cancertype = color_vector,HRDType = c('High' = 'orange', 'Low' = 'blue')))

Heatmap(mat, cluster_rows = FALSE, cluster_columns = FALSE, show_column_dend = FALSE, show_column_names  = TRUE, column_title='Testing', name = 'Correlation')

if (save){
  png(file=paste('../data/figures/heatmap_',measure_short_name,'_',level, '.png', sep = ''))
}

draw(hm)

if (save){
  dev.off()
}


######## Heatmap with all correlations between all scores in each cancer type

empty_df <- data.frame(matrix(nrow = length(unique(HRD_scores_primary$Cancertype)),ncol = 6))
rownames(empty_df) = unique(HRD_scores_primary$Cancertype)
colnames(empty_df) = c('HRDs-LST','HRDs-LOH','HRDs-TAI','LST-LOH','LST-TAI','LOH-TAI')

for (cancertype in unique(HRD_scores_primary$Cancertype)){
  subset = HRD_scores_primary[HRD_scores_primary$Cancertype == cancertype,]
  hrd_lst = cor(subset$HRD_sum,subset$LST, method = 'spearman')
  hrd_tai = cor(subset$HRD_sum,subset$TAI, method = 'spearman')
  lst_tai = cor(subset$LST,subset$TAI, method = 'spearman')
  
  if (cancertype == 'CCSK'){
    hrd_loh = NaN
    lst_loh = NaN
    loh_tai = NaN
  }else{
    hrd_loh = cor(subset$HRD_sum,subset$LOH, method = 'spearman')
    lst_loh = cor(subset$LST,subset$LOH, method = 'spearman')
    loh_tai = cor(subset$LOH,subset$TAI, method = 'spearman')
  }
  
  empty_df[cancertype,'HRDs-LST'] = hrd_lst
  empty_df[cancertype,'HRDs-LOH'] = hrd_loh
  empty_df[cancertype,'HRDs-TAI'] = hrd_tai
  empty_df[cancertype,'LST-LOH'] = lst_loh
  empty_df[cancertype,'LST-TAI'] = lst_tai
  empty_df[cancertype,'LOH-TAI'] = loh_tai

}

mat = as.matrix(empty_df)

colnames(mat) = colnames(empty_df)
rownames(mat) = rownames(empty_df)

empty_df$RowNames <- rownames(empty_df)


df_long <- empty_df %>%
  pivot_longer(cols = -RowNames, names_to = "NumericColumn", values_to = "Value")

# Step 3 (Optional): Remove row names column if no longer needed
empty_df$RowNames <- NULL

plot = ggplot(df_long) +
  geom_bar(aes(x = NumericColumn, y = Value),
           position = "stack",
           stat = "identity") +
  facet_grid(~factor(RowNames, levels=unique(df_long$RowNames)),  switch = "x") +
  labs(title = paste("Precentage of events between HRD-high and HRD-low in Chromosome ", sep =''),
       x = "Cancer Type", y = "Precentages",
       fill = "Events")+
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = "white"),
        strip.text.x = element_text(angle = 90, hjust = 1),  # Rotate facet labels
        axis.text.x = element_text(angle = 90, hjust = 1) )

print(plot)
#"antiquewhite1"
col_fun = colorRamp2(c(0, 0.5, 1), c('purple','white', "tan2"))

Heatmap(mat, cluster_rows = TRUE, cluster_columns = FALSE, show_column_dend = FALSE,
        show_column_names  = TRUE, column_title='Correlation between HRD scores', name = 'Spearman correlation',
        rect_gp = gpar(col = "white", lwd = 2),row_names_gp = gpar(fontsize = 10), col = col_fun)

write.csv(empty_df,'../data/correlation_values.csv', row.names = TRUE)
