library(tidyverse)
library(broom)
library(tidyr)
library(caret)
library(readxl)
library(pROC)

## Read in the Data ##

data <- C:/Users/carle/Downloads/Vaccine Hesitancy Data.xlsx

View(data)

## Rename Columns ##

names(data) <- c(“Age”,”Gender”,”Education”,”Occupation”,”Occupation - Other”,
                  “Marital Status”,”Marital Status - Other”, “Income”,
                  “Race”,”Race - Other”,”Geographic Location”,”Number of Children”,
                  “Number of Children - Other”,”VH1”,”VH2”,”VH3”,”VH4”,”VH5”,”VH6”,
                  “VH7”,”VH8”,”VH9”,paste(“RDM”,1:5,sep=““),
                  paste(“IDM”,1:5,sep=““),
                  paste(“DDM”,1:5,sep=““),
                  paste(“ADM”,1:5,sep=““),
                  paste(“SDM”,1:5,sep=““))

## Clean up columns ##

data <- data |>
  select(-c(“Occupation - Other”,”Marital Status - Other”,
             “Number of Children - Other”,”Race - Other”)
  )

## Throw Out Rows with 100% Missing Data ##

data1 <- data[which(
  
  apply(data,1,FUN=function(x){
    
    k <- ifelse(sum(is.na(x)) != ncol(data),T,F)
    return(k)
    
  })
),]

## Calcualte Mean & Summed Scores ##

apply(data1,1,FUN=function(x){
  
  k <- ifelse(sum(is.na(x)) > 0,1,0)
  return(k)
  
})

naniar::vis_miss(data1)

## Consider Only Complete Cases ##

data2 <- na.omit(data1)

## Calculate Mean of VH columns ##

## First, Recode VH Columns to numeric ##

data2 <- data2 |>
  mutate(across(starts_with(“VH”),~recode(.x,”Strongly Disagree”=1,
                                          “Disagree”=2,
                                          “Agree”=3,
                                          “Strongly Agree”=4)))

## Reverse Code VH8 & VH9 ##

data2 <- data2 |>
  mutate(across(c(“VH8”,”VH9”),~5-.x))

## Now, Calculate VHS Mean ##

vhs <- data2 |>
  select(starts_with(“VH”))

VHSMEAN <- rowMeans(vhs,na.rm=T)

data2 <- bind_cols(
  data2,VHSMEAN
)

names(data2)[44] <- “VHSMEAN”

data2 <- data2 |>
  mutate(FINALVHS = if_else(VHSMEAN >= 3,0,1))

## Calculate RDM total score ##

rdm <- data2 |>
  select(starts_with(“RDM”))

## Recode ##

rdm <- rdm |>
  mutate(across(everything(),~recode(.x,”Strongly Disagree”=1,
                                     “Disagree”=2,
                                     “Agree”=3,
                                     “Strongly Agree”=4)))

RDMSCORE <- rowSums(rdm,na.rm=T)

data2 <- bind_cols(
  data2,RDMSCORE
)

names(data2)[46] <- “RDMSCORE”

## IDM ##

idm <- data2 |>
  select(starts_with(“IDM”))

## Recode ##

idm <- idm |>
  mutate(across(everything(),~recode(.x,”Strongly Disagree”=1,
                                     “Disagree”=2,
                                     “Agree”=3,
                                     “Strongly Agree”=4)))

IDMSCORE <- rowSums(idm,na.rm=T)

data2 <- bind_cols(
  data2,IDMSCORE
)

names(data2)[47] <- “IDMSCORE”

## DDM ##

ddm <- data2 |>
  select(starts_with(“DDM”))
## Recode ##

ddm <- ddm |>
  mutate(across(everything(),~recode(.x,”Strongly Disagree”=1,
                                     “Disagree”=2,
                                     “Agree”=3,
                                     “Strongly Agree”=4)))

DDMSCORE <- rowSums(ddm,na.rm=T)

data2 <- bind_cols(
  data2,DDMSCORE
)

names(data2)[48] <- “DDMSCORE”

## ADM ##

adm <- data2 |>
  select(starts_with(“ADM”))

## Recode ##

adm <- adm |>
  mutate(across(everything(),~recode(.x,”Strongly Disagree”=1,
                                     “Disagree”=2,
                                     “Agree”=3,
                                     “Strongly Agree”=4)))

ADMSCORE <- rowSums(adm,na.rm=T)

data2 <- bind_cols(
  data2,ADMSCORE
)

names(data2)[49] <- “ADMSCORE”

## SDM ##

sdm <- data2 |>
  select(starts_with(“SDM”))

## Recode ##

sdm <- sdm |>
  mutate(across(everything(),~recode(.x,”Strongly Disagree”=1,
                                     “Disagree”=2,
                                     “Agree”=3,
                                     “Strongly Agree”=4)))

SDMSCORE <- rowSums(sdm,na.rm=T)

data2 <- bind_cols(
  data2,SDMSCORE
)

names(data2)[50] <- “SDMSCORE”

## Subset to only analyzed variables ##

df_final <- data2 |>
  select(Age,Gender,Education,Occupation,`Mariatal Status`,
         Income,Race,`Geographic Location`,`Number of Children`,
         FINALVHS,RDMSCORE,IDMSCORE,DDMSCORE,ADMSCORE,SDMSCORE)

## Determine Decision Making Style ##

## Subset RDMSCORE - SDMSCORE ##

scores <- df_final |>
  select(RDMSCORE, IDMSCORE, DDMSCORE, ADMSCORE, SDMSCORE)

## Determine which column has the max value ##

## Return column name associated with max value ##

DMSTYLE <- vector(‘integer’,nrow(scores))

for(i in 1:nrow(scores)){
  
  DMSTYLE[i] <- which.max(scores[i,])
  
}

## Rejoin to df_final ##

df_final <- bind_cols(
  df_final,DMSTYLE
)

names(df_final)[16] <- “DMSTYLE”

## Collapse Levels of the Categorical Predictors ##

## Age - Collapse to 18-34 & 35+ ##

table(df_final$Age)

df_final <- df_final |>
  mutate(Age = if_else(Age %in% c(“18-24”,”25-34”),”18-34”,”35+”))

## Gender - Male, Female & Nonbinary ##

table(df_final$Gender)

## Remove Nonbinary ##

df_final <- df_final |>
  filter(Gender != “Nonbinary”)

## Education ##

table(df_final$Education)

df_final <- df_final |>
  mutate(Education = if_else(Education %in% c(“Less than high school”,
                                               “High school graduate”,
                                               “Some college, no degree”),
                             “College Experience or Less”,Education))

table(df_final$Education)


## Occupation ##

table(df_final$Occupation)

df_final <- df_final |>
  mutate(Occupation = if_else(Occupation == “Employed full-time”,
                              “Employed full-time”,”Other”))

table(df_final$Occupation)

## Marital Status ##

table(df_final$`Mariatal Status`)

df_final <- df_final |>
  mutate(`Marital Status` = if_else(`Marital Status` == “Married”,
                                    “Married”,”Other”))

table(df_final$`Marital Status`)

## Income ##

table(df_final$Income)

df_final <- df_final |>
  mutate(Income = if_else(Income == “$100,000 and Over”,
                          “$100K+”,”Less than $100K”))

table(df_final$Income)

## Ethnicity ##

table(df_final$Race)

df_final <- df_final |>
  mutate(Race = if_else(Race == “White/Caucasian”,
                        “White”,”Non-White”))

table(df_final$Race)

## Geographic Location ##

table(df_final$`Geographic Location`)

## Leave as is ##

## Number of Children ##

table(df_final$`Number of Children`)

df_final <- df_final |>
  filter(`Number of Children` != “None”) |>
  mutate(`Number of Children` = if_else(`Number of Children` %in%
                                          c(“3”,”4 or more (Please specify how many children)”),
                                        “3+”,`Number of Children`))

table(df_final$`Number of Children`)

## DMSTYLE ##

table(df_final$DMSTYLE)
## Since 5 == “Spontaneous”, is 1 best to omit that person and
## then recode ##

df_final <- df_final |>
  filter(DMSTYLE != 5) |>
  mutate(DMSTYLE = recode(DMSTYLE,`1`=“Rational”,
                          `2`=“Intuitive”,
                          `3`=“Dependent”,
                          `4`=“Avoidant”))

table(df_final$DMSTYLE)

## FINALVHS ##

table(df_final$FINALVHS)

## Recode ##

df_final <- df_final |>
  mutate(FINALVHS = recode(FINALVHS,`0`=“Vaccine Hesitant”,
                           `1`=“Non-Vaccine Hesitant”))

table(df_final$FINALVHS)

## Subset only analyzed variables ##

df_final2 <- df_final |>
  select(FINALVHS,Age,Gender,Education,Occupation,`Marital Status`,
         Income,Race,`Geographic Location`,`Number of Children`,
         DMSTYLE) |>
  mutate(FINALVHS = recode(FINALVHS,”Vaccine Hesitant”=1,
                           “Non-Vaccine Hesitant”=0))

## Build Logistic Regression Model ##

mod <- glm(FINALVHS ~ .,data=df_final2,family=“binomial”)

mod |>
  glance() -> model_fit_tests

## Cooks D##
# Load necessary library
install.packages(“ggplot2”)
library(ggplot2)

# Calculate Cook’s distance
cooksd <- cooks.distance(mod)

# Define a threshold for influential observations (e.g., 4/(n-k-1))
threshold <- 4 / (nrow(mod$model) - length(coef(mod)) - 1)

# Identify influential observations
influential_obs <- which(cooksd > threshold)

# Print the influential observations
influential_table <- data.frame(Observation = influential_obs, CookD = cooksd[influential_obs])
print(influential_table)

# Convert to a data frame for ggplot2
cooksd_df <- data.frame(Observation = 1:length(cooksd), CooksD = cooksd)

# Plot Cook’s distance
ggplot(cooksd_df, aes(x = Observation, y = CooksD)) +
  geom_bar(stat = “identity”) +
  geom_hline(yintercept = threshold, linetype = “dashed”, color = “red”) +
  geom_text(aes(label = ifelse(CooksD > threshold, as.character(Observation), ““)),
            vjust = -0.5, size = 3) +
  labs(title = “Cook’s Distance for Influential Observations”,
       x = “Observation Number”,
       y = “Cook’s Distance”) +
  theme_minimal()

## Create Log Likelihood for Null Model ##

null_mod <- glm(FINALVHS ~ 1,data=df_final2,family=“binomial”)

model_fit_tests <- model_fit_tests |>
  mutate(McFaddenR2 = 1 - logLik/as.numeric(logLik(null_mod)))

mod |>
  tidy(conf.int=T,exponentiate=T) -> model_summary

## ROC Curve ##

roc_curve <- roc(df_final2$FINALVHS,predict(mod,df_final2,type=“response”))
plot(roc_curve)
auc(roc_curve)

## AIC and BIC ##
aic_bic <- function(mod) {
  list(AIC = AIC(model), BIC = BIC(mod))
}
aic_bic_full <- aic_bic(mod)

print(aic_bic_full)

## McFaddens Pseudo R-Squared ##

# Install and load the pscl package
install.packages(“pscl”) library(pscl)

# Calculate McFadden’s pseudo R-squared
mcfadden_r2 <- pR2(mod)[“McFadden”]

# Print McFadden’s pseudo R-squared
print(mcfadden_r2)

## Likelihood Ratio Test ##
lmtest::lrtest(null_mod,mod)

## GVIF ##
df_final2 <- vif(mod)
print(df_final2)

## Sensitivity – Specificity ##
sensitivity_specificity <- function(mod, threshold = 0.5) {
  predictions <- ifelse(fitted(mod) > threshold, 1, 0)
  confusion_matrix <- confusionMatrix(as.factor(predictions), as.factor(mod$y))
  list(Sensitivity = confusion_matrix$byClass[“Sensitivity”], Specificity = confusion_matrix$byClass[“Specificity”])
}

sens_spec_full <- sensitivity_specificity(mod)
print(sens_spec_full)

## Hosmer Lemeshow Test ##
hoslem_test <- function(mod) {
  hoslem.test(mod$y, fitted(mod), g = 10)
}

# Fit a logistic regression model
model <- glm(vs ~ wt + hp + drat, family = binomial, data = mtcars)

# Perform the Hosmer-Lemeshow test
hoslem_full <- hoslem_test(mod)

# Print the test results
print(hoslem_full)
## Tables by Vaccine-Hesitant or Non-Vaccine-Hesitant ##
table(df_final$Age, df_final$FINALVHS)
table(df_final$Gender, df_final$FINALVHS)
table(df_final$Education, df_final$FINALVHS)
table(df_final$Occupation, df_final$FINALVHS)
table(df_final$`Geographic Location`, df_final$FINALVHS)
table(df_final$`Marital Status`, df_final$FINALVHS)
table(df_final$Income, df_final$FINALVHS)
table(df_final$Race, df_final$FINALVHS)
table(df_final$`Number of Children`, df_final$FINALVHS)
table(df_final$DMSTYLE, df_final$FINALVHS)
