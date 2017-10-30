source("ggcorplot.R")

train_features = read.csv("dengue_features_train.csv")
train_label = read.csv("dengue_labels_train.csv")
train_data = cbind(train_features, train_label$total_cases)
crm = c(4,5,6,7,8)
tsub = train_data[,-crm]
tsub_na = na.omit(tsub)

test_features = read.csv("dengue_features_test.csv")
testsub = test_features[,-crm]
testsub_na = na.omit(testsub)

a = cbind(tsub$station_avg_temp_c, tsub$reanalysis_avg_temp_k)

apply(tsub_na[,-c(1,2,3,20)], 2, function(x) cor(x, tsub_na$station_precip_mm))


re_avg_st_avg = subset(tsub, select = c("reanalysis_avg_temp_k", "station_avg_temp_c"))
re_tdtr_st_diur = subset(tsub, select = c("reanalysis_tdtr_k", "station_diur_temp_rng_c"))

# re_avg_st_avg = cbind(tsub$reanalysis_avg_temp_k, tsub$station_avg_temp_c)
# re_tdtr_st_diur = cbind(tsub$reanalysis_tdtr_k, tsub$station_diur_temp_rng_c)

id_st_avg = which(is.na(tsub$station_avg_temp_c))
id_re_avg = which(is.na(tsub$reanalysis_avg_temp_k))
available_ids = setdiff(id_st_avg, id_re_avg)

re_avg_st_avg_available = re_avg_st_avg[-id_st_avg,]
re_tdtr_st_diur_available = re_tdtr_st_diur[-id_st_avg,]
re_avg_st_avg_missing_one = re_avg_st_avg[available_ids,]
re_tdtr_st_diur_missing_one = re_tdtr_st_diur[available_ids,]

st_avg_fit = lm(station_avg_temp_c ~ reanalysis_avg_temp_k, data = re_avg_st_avg_available)
pred_st_avg = predict(st_avg_fit, re_avg_st_avg_missing_one, type = "response")

st_diur_fit = lm(station_diur_temp_rng_c ~ reanalysis_tdtr_k, data = re_tdtr_st_diur_available)
pred_st_diur = predict(st_diur_fit, re_tdtr_st_diur_missing_one, type = "response")

tsub$station_avg_temp_c[available_ids] = pred_st_avg
tsub$station_diur_temp_rng_c[available_ids] = pred_st_diur

tsub_na = na.omit(tsub)
tr.pca = prcomp(tsub_na[,-c(1,2,3,20)], center = TRUE, scale. = FALSE)
pcs = tr.pca$x[,1:3]
reduced_tr = cbind(tsub_na[,1:3], pcs, total_cases = tsub_na[,20])
reduced_tr$city = as.factor(reduced_tr$city)
reduced_tr$year = as.factor(reduced_tr$year)
reduced_tr$weekofyear = as.factor(reduced_tr$weekofyear)

tree.fit = rpart(total_cases ~ ., data = reduced_tr)
plot(tree.fit)
text(tree.fit)

test.pca = prcomp(testsub_na[,-c(1,2,3)], center = TRUE, scale. = FALSE)
test_pcs = test.pca$x[,1:3]
reduced_test = cbind(testsub_na[,1:3], test_pcs)
reduced_test$city = as.factor(reduced_test$city)
reduced_test$year = as.factor(reduced_test$year)
reduced_test$weekofyear = as.factor(reduced_test$weekofyear)


pairs(tsub_na[,-c(1,2,3,20)])
ggcorplot(
  data = tsub_na[,-c(1,2,3,20)],
  var_text_size = 5,
  cor_text_limits = c(5,10))
