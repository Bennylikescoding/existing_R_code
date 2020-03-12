# a. if not transformed
# 1.import files
file_path_coeff<-choose.files()
coeffi_df<-read.csv(file_path_coeff,row.names=1)
coeffi_df_vector <- as.vector(coeffi_df[,1])

file_path_matrix<-choose.files("c:\\新建文件夹(2)\\*.*")
matrix_df<-read.csv(file_path_matrix,row.names=1)

# 2. transform matrix value to percent of sum values * 100
percent_sum_df <- apply(matrix_df,2,function(x){x/sum(x)*100})

# 3. select target pathway from transformed matrix
selected_percent_matrix_df <- percent_sum_df[row.names(coeffi_df),]


# 3. matrix multiplication
## 3.1 transpose matrix
percent_sum_df_T <- t(selected_percent_matrix_df)

## 3.2 matrix multiplicatn
score = percent_sum_df_T %*% coeffi_df_vector
colnames(score) <- "Score"

score




# b. if transformed
# 1.import files
file_path_coeff<-choose.files()
coeffi_df<-read.csv(file_path_coeff,row.names=1)
coeffi_df_vector <- as.vector(coeffi_df[,1])

file_path_matrix<-choose.files()
matrix_df<-read.csv(file_path_matrix,row.names=1)

# 2. select target pathway from transformed matrix
selected_percent_matrix_df <- matrix_df[row.names(coeffi_df),]


# 3. matrix multiplication
## 3.1 transpose matrix
percent_sum_df_T <- t(selected_percent_matrix_df)

## 3.2 matrix multiplicatn
score = percent_sum_df_T %*% coeffi_df_vector
colnames(score) <- "Score"

score
