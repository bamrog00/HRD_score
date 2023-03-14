library(ggplot2)
library(data.table)
library(corrplot)
library(gridGraphics)
library(gridExtra)
library(png)
library(cowplot)

HRD_scores = data.frame(read.csv('../data/HRD_scores_pan_cancer_annotated_typecorrect.csv'))

scores = c("HRD_sum","TAI","LOH","LST")

corr_hrds_loh = ggplot(data = HRD_scores, aes(x = HRD_sum, y = LOH, color = Project.ID)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

corr_hrds_loh

pairs(HRD_scores[c('HRD_sum','LST','LOH','TAI')])


sub_df = HRD_scores[, c("TAI", "LOH", "LST", "Project.ID")]

table(sub_df$Project.ID)

df_list = split(sub_df, sub_df$Project.ID)

cor_list = list()

cancer_types = list()
heatmap_list <- list()


for (i in seq_along(df_list)){
  cor_mat = cor(df_list[[i]][,c('TAI','LOH','LST')], method = 'spearman')
  type = unique(df_list[[i]]$Project.ID)
  if (type == 'TARGET-CCSK'){
    print(df_list[[i]])
  }
  cancer_types = c(cancer_types, type)
  
  #colnames(cor_mat) = paste0(type, "-", colnames(cor_mat))
  #rownames(cor_mat) = colnames(cor_mat)
  
  cor_list[[i]] = cor_mat
}


grab_grob = function(){
  grid.echo()
  grid.grab()
}

plot_list = list()
for (i in seq_along(cor_list)){
  corr_plot = corrplot(cor_list[[i]], method = 'color', order = 'alphabet', title = cancer_types[[i]], mar=c(0,0,1,0))
  plot = grab_grob()
  ggsave(paste('../data/figures_pan_cancer/scores_correlation/',cancer_types[[i]],'.png',sep = ''),plot,width = 8.63, height = 5.71)
}

files = list.files(path = "../data/figures_pan_cancer/scores_correlation/", pattern = NULL, all.files = FALSE,
                   full.names = TRUE, recursive = FALSE,
                   ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

rl = lapply(files, png::readPNG)
gl = lapply(rl, grid::rasterGrob)

n_plots <- length(gl)
n_rows <- ceiling(sqrt(n_plots))
n_cols <- ceiling(n_plots / n_rows)

widths <- rep(40, n_cols)
heights <- rep(40, n_rows)

total_width <- sum(widths)
total_height <- sum(heights)
output_width <- total_width * 0.1
output_height <- total_height * 0.1
#grid.arrange(grobs=gl, ncol=n_cols, nrow=n_rows, widths=widths, heights=heights)
#plot = grab_grob()
plot = arrangeGrob(grobs=gl, ncol=n_cols, nrow=n_rows, widths=widths, heights=heights)
ggsave("../data/figures_pan_cancer/summary_correlations_plots.png",plot,width = output_width, height = output_height,limitsize = FALSE)

