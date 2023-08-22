library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df1 <- read.csv('./data/20230820231833/analysis_data.csv')
df2 <- read.csv('./data/20230822113023/analysis_data.csv')
df3 <- rbind(df1,df2)
write.csv(df3, './data/20230820231833/analysis_data.csv')
