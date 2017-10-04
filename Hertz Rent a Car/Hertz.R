library(readr)
library(ROCR)
library(olsrr)
library(perturb)

##########Subtracted by 1 because 1 was added to perform segmentation in Latent Gold##########
Final_Segmentation <- read_csv("Final Segmentation.csv", col_types = cols(Crafts.Hobby.Buyer = col_skip(), DWELLING.TYPE = col_skip(), DWELLING.UNIT.SIZE = col_skip(), EDUCATION.MODEL = col_skip(), GPR_ENROLL_DATE = col_skip(), HTZ_CITY = col_skip(), HTZ_CUSTID = col_skip(), HTZ_ZIP = col_skip(), ID_RNTLS = col_skip(), RevenuePerDay = col_skip()))
Final_Segmentation = Final_Segmentation[-c(1998, 7257, 8096), ]
Final_Segmentation$Political.Contributor <- Final_Segmentation$Political.Contributor - 1
Final_Segmentation$Religious.Contributor <- Final_Segmentation$Religious.Contributor - 1
Final_Segmentation$Opportunity.seeker <- Final_Segmentation$Opportunity.seeker - 1
Final_Segmentation$Religious.Magazine <- Final_Segmentation$Religious.Magazine - 1
Final_Segmentation$General.Contributor <- Final_Segmentation$General.Contributor - 1
Final_Segmentation$General.Merch.buyer <- Final_Segmentation$General.Merch.buyer - 1
Final_Segmentation$Gifts.and.Gadgets.Buyer <- Final_Segmentation$Gifts.and.Gadgets.Buyer - 1
Final_Segmentation$Gardening.Buyer <- Final_Segmentation$Gardening.Buyer - 1
Final_Segmentation$Gardening.Magazinr <- Final_Segmentation$Gardening.Magazinr - 1
Final_Segmentation$Sports.Magazine.Male <- Final_Segmentation$Sports.Magazine.Male - 1
Final_Segmentation$News.and.finance <- Final_Segmentation$News.and.finance - 1
Final_Segmentation$fitness.magazine <- Final_Segmentation$fitness.magazine - 1
Final_Segmentation$Health.Contributor <- Final_Segmentation$Health.Contributor - 1
Final_Segmentation$Male.Merch.Buyer <- Final_Segmentation$Male.Merch.Buyer - 1
Final_Segmentation$Female.Merch.buyer <- Final_Segmentation$Female.Merch.buyer - 1
Final_Segmentation$Female.Orient.Magazine <- Final_Segmentation$Female.Orient.Magazine - 1
Final_Segmentation$Culinary.Magazine <- Final_Segmentation$Culinary.Magazine - 1
Final_Segmentation$D.I.Yers <- Final_Segmentation$D.I.Yers - 1
Final_Segmentation$Special.Foods.Buyer <- Final_Segmentation$Special.Foods.Buyer - 1
Final_Segmentation$Family.Magazine <- Final_Segmentation$Family.Magazine - 1
Final_Segmentation$STATE.ABBREVIATION = as.factor(Final_Segmentation$STATE.ABBREVIATION)
Final_Segmentation$MAIL.RESPONDER = as.factor(Final_Segmentation$MAIL.RESPONDER)
Final_Segmentation$MARITAL.STATUS = as.factor(Final_Segmentation$MARITAL.STATUS)
Final_Segmentation$CHILDREN..AGE.0.18.VERSION.3 = as.factor(Final_Segmentation$CHILDREN..AGE.0.18.VERSION.3)
Final_Segmentation$CURRENT_TIER = as.factor(Final_Segmentation$CURRENT_TIER)
Final_Segmentation$EMAIL_OPTIN = as.factor(Final_Segmentation$EMAIL_OPTIN)
Final_Segmentation$EST.HOUSEHOLD.INCOME.V5 = as.factor(Final_Segmentation$EST.HOUSEHOLD.INCOME.V5)
Final_Segmentation$GENDER = as.factor(Final_Segmentation$GENDER)
Final_Segmentation$HTZ_STATE = as.factor(Final_Segmentation$HTZ_STATE)
names(Final_Segmentation)[names(Final_Segmentation) == 'clu#'] <- 'clu_no'
Final_Segmentation$TotalUnit = Final_Segmentation$NUM_1D_RNTLS * 1 + Final_Segmentation$NUM_2D_RNTLS * 2 + Final_Segmentation$NUM_3D_RNTLS * 3 + Final_Segmentation$NUM_4D_RNTLS * 4 + Final_Segmentation$NUM_5D_RNTLS * 5 + Final_Segmentation$NUM_GT5D_RNTLS * 6.5
Final_Segmentation$Price = Final_Segmentation$REVENUE / (Final_Segmentation$TotalUnit)
Final_Segmentation$EMAIL_OPTIN = NULL
Final_Segmentation = Final_Segmentation[Final_Segmentation$Price!=Inf, ]

############Evaluate elasticity on each clusters##############
set.seed(144)
clus_no_1 = subset(Final_Segmentation, Final_Segmentation$clu_no == 1)
clus_no_1$clu_no = NULL
clus_no_1$NUM_RENTALS = NULL
clus_no_1$REVENUE = NULL
clus_no_1$NUM_1D_RNTLS = NULL
clus_no_1$NUM_2D_RNTLS = NULL
clus_no_1$NUM_3D_RNTLS = NULL
clus_no_1$NUM_4D_RNTLS = NULL
clus_no_1$NUM_5D_RNTLS = NULL
clus_no_1$NUM_GT5D_RNTLS = NULL
clus_no_1$HTZ_STATE = NULL
clus_no_1$NONINS_ANCIL_RNTLS = NULL
clus_no_1$STATE.ABBREVIATION = NULL
clus_no_1$COMBINED.AGE = NULL
clus_no_1$EST.HOUSEHOLD.INCOME.V5 = NULL
#clus_no_1$D_RNTLS = NULL
#clus_no_1$Special.Foods.Buyer = NULL
#nobs1 <- nrow(clus_no_1)
#train1 <- sample(nrow(clus_no_1), 0.7*nobs1)
#train_clu1 = clus_no_1[train1,]
#test_clu1 <- clus_no_1[-train1,]
avg_price1 = mean(clus_no_1$Price)
avg_units1 = mean(clus_no_1$TotalUnit)
model1 <- lm(TotalUnit ~ ., data=clus_no_1)
price_coeff1 = coef(model1)[44]
elas_1 = price_coeff1 * (avg_price1/avg_units1)
ci1 = ols_eigen_cindex(model1)
vif1 = ols_vif_tol(model1)
summary(model1)


clus_no_2 = subset(Final_Segmentation, Final_Segmentation$clu_no == 2)
clus_no_2$clu_no = NULL
clus_no_2$NUM_RENTALS = NULL
clus_no_2$REVENUE = NULL
clus_no_2$NUM_1D_RNTLS = NULL
clus_no_2$NUM_2D_RNTLS = NULL
clus_no_2$NUM_3D_RNTLS = NULL
clus_no_2$NUM_4D_RNTLS = NULL
clus_no_2$NUM_5D_RNTLS = NULL
clus_no_2$NUM_GT5D_RNTLS = NULL
clus_no_2$HTZ_STATE = NULL
clus_no_2$NONINS_ANCIL_RNTLS = NULL
clus_no_2$STATE.ABBREVIATION = NULL
clus_no_2$COMBINED.AGE = NULL
clus_no_2$EST.HOUSEHOLD.INCOME.V5 = NULL
#clus_no_2 = clus_no_2[clus_no_2$Price!=Inf, ]
#nobs2 <- nrow(clus_no_2)
#train2 <- sample(nrow(clus_no_2), 0.7*nobs2)
#train_clu2 = clus_no_2[train2,]
#test_clu2 <- clus_no_2[-train2,]
avg_price2 = mean(clus_no_2$Price)
avg_units2 = mean(clus_no_2$TotalUnit)
model2 <- lm(TotalUnit ~ ., data=clus_no_2)
price_coeff2 = coef(model2)[44]
elas_2 = price_coeff2 * (avg_price2/avg_units2)
ci2 = ols_eigen_cindex(model2)
vif2 = ols_vif_tol(model2)
summary(model2)

clus_no_3 = subset(Final_Segmentation, Final_Segmentation$clu_no == 3)
clus_no_3$clu_no = NULL
clus_no_3$NUM_RENTALS = NULL
clus_no_3$REVENUE = NULL
clus_no_3$NUM_1D_RNTLS = NULL
clus_no_3$NUM_2D_RNTLS = NULL
clus_no_3$NUM_3D_RNTLS = NULL
clus_no_3$NUM_4D_RNTLS = NULL
clus_no_3$NUM_5D_RNTLS = NULL
clus_no_3$NUM_GT5D_RNTLS = NULL
clus_no_3$HTZ_STATE = NULL
clus_no_3$NONINS_ANCIL_RNTLS = NULL
clus_no_3$STATE.ABBREVIATION = NULL
clus_no_3$COMBINED.AGE = NULL
clus_no_3$EST.HOUSEHOLD.INCOME.V5 = NULL
#clus_no_3 = clus_no_3[clus_no_3$Price!=Inf, ]
#nobs3 <- nrow(clus_no_3)
#train3 <- sample(nrow(clus_no_3), 0.7*nobs3)
#train_clu3 = clus_no_3[train3,]
#test_clu3 <- clus_no_3[-train3,]
avg_price3 = mean(clus_no_3$Price)
avg_units3 = mean(clus_no_3$TotalUnit)
model3 <- lm(TotalUnit ~ ., data=clus_no_3)
price_coeff3 = coef(model3)[44]
elas_3 = price_coeff3 * (avg_price3/avg_units3)
ci3 = ols_eigen_cindex(model3)
vif3 = ols_vif_tol(model3)
summary(model3)

clus_no_4 = subset(Final_Segmentation, Final_Segmentation$clu_no == 4)
clus_no_4$clu_no = NULL
clus_no_4$Religious.Magazine = NULL
clus_no_4$NUM_RENTALS = NULL
clus_no_4$REVENUE = NULL
clus_no_4$NUM_1D_RNTLS = NULL
clus_no_4$NUM_2D_RNTLS = NULL
clus_no_4$NUM_3D_RNTLS = NULL
clus_no_4$NUM_4D_RNTLS = NULL
clus_no_4$NUM_5D_RNTLS = NULL
clus_no_4$NUM_GT5D_RNTLS = NULL
clus_no_4$HTZ_STATE = NULL
clus_no_4$NONINS_ANCIL_RNTLS = NULL
clus_no_4$STATE.ABBREVIATION = NULL
clus_no_4$COMBINED.AGE = NULL
clus_no_4$EST.HOUSEHOLD.INCOME.V5 = NULL
#clus_no_4 = clus_no_4[clus_no_4$Price!=Inf, ]
#nobs4 <- nrow(clus_no_4)
#train4 <- sample(nrow(clus_no_4), 0.7*nobs4)
#train_clu4 = clus_no_4[train4,]
#test_clu4 <- clus_no_4[-train4,]
avg_price4 = mean(clus_no_4$Price)
avg_units4 = mean(clus_no_4$TotalUnit)
model4 <- lm(TotalUnit ~ ., data=clus_no_4)
price_coeff4 = coef(model4)[44]
elas_4 = price_coeff4 * (avg_price4/avg_units4)
ci4 = ols_eigen_cindex(model4)
vif4 = ols_vif_tol(model4)
summary(model4)

clus_no_5 = subset(Final_Segmentation, Final_Segmentation$clu_no == 5)
clus_no_5$clu_no = NULL
clus_no_5$Religious.Magazine = NULL
clus_no_5$NUM_RENTALS = NULL
clus_no_5$REVENUE = NULL
clus_no_5$NUM_1D_RNTLS = NULL
clus_no_5$NUM_2D_RNTLS = NULL
clus_no_5$NUM_3D_RNTLS = NULL
clus_no_5$NUM_4D_RNTLS = NULL
clus_no_5$NUM_5D_RNTLS = NULL
clus_no_5$NUM_GT5D_RNTLS = NULL
clus_no_5$HTZ_STATE = NULL
clus_no_5$NONINS_ANCIL_RNTLS = NULL
clus_no_5$STATE.ABBREVIATION = NULL
clus_no_5$COMBINED.AGE = NULL
clus_no_5$EST.HOUSEHOLD.INCOME.V5 = NULL
#clus_no_5 = clus_no_5[clus_no_5$Price!=Inf, ]
#nobs5 <- nrow(clus_no_5)
#train5 <- sample(nrow(clus_no_5), 0.7*nobs5)
#train_clu5 = clus_no_5[train5,]
#test_clu5 <- clus_no_5[-train5,]
avg_price5 = mean(clus_no_5$Price)
avg_units5 = mean(clus_no_5$TotalUnit)
model5 <- lm(TotalUnit ~ ., data=clus_no_5)
price_coeff5 = coef(model5)[41]
elas_5 = price_coeff5 * (avg_price5/avg_units5)
ci5 = ols_eigen_cindex(model5)
vif5 = ols_vif_tol(model5)
summary(model5)