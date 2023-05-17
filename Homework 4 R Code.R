# ITM 818 Homework 4 R Code
# Eric Voisine

setwd("/Users/evoisine/Documents/MSBDSA - SS22/ITM 818/Homework")
airfare <- read.csv('Airfares.csv')
View(airfare)

# Question 1
num_cols <- c(5, 6, 9, 10, 11, 12, 13, 16, 17, 18)

corr <- cor(airfare[, num_cols])
plot(airfare[, num_cols])

plot(airfare[, c(16, 18)])


# Question 2

# for vacation
pct_vac <- c(sum(airfare$VACATION=="Yes") / length(airfare$VACATION), 
             (1 - (sum(airfare$VACATION=="Yes") / length(airfare$VACATION))))
names(pct_vac) <- c("Yes", "No")
pct_vac

avg_fare_vac <- c(mean(airfare[airfare$VACATION=="Yes", "FARE"]), 
                  mean(airfare[airfare$VACATION=="No", "FARE"]))

barplot(avg_fare_vac, names.arg=c("Yes", "No"), xlab="Vacation Route (y/n)", 
        ylab="Avg Fare Price", ylim=c(0,200))

# for Southwest
pct_SW <- c(sum(airfare$SW=="Yes") / length(airfare$SW), 
            1 - (sum(airfare$SW=="Yes") / length(airfare$SW)))
names(pct_SW) <- c("Yes", "No")
pct_SW

avg_fare_SW <- c(mean(airfare[airfare$SW=="Yes", "FARE"]), 
                  mean(airfare[airfare$SW=="No", "FARE"]))

barplot(avg_fare_SW, names.arg=c("Yes", "No"), xlab="Southwest Route (y/n)", 
        ylab="Avg Fare Price", ylim=c(0,200))

# for SLOT
pct_slot <- c(sum(airfare$SLOT=="Free") / length(airfare$SLOT),
              sum(airfare$SLOT=="Controlled") / length(airfare$SLOT))
names(pct_slot) <- c("Free", "Controlled")
pct_slot

avg_fare_SLOT <- c(mean(airfare[airfare$SLOT=="Free", "FARE"]), 
                 mean(airfare[airfare$SLOT=="Controlled", "FARE"]))

barplot(avg_fare_SLOT, names.arg=c("Free", "Controlled"), xlab="Slot Type", 
        ylab="Avg Fare Price", ylim=c(0,200))

# for Gate
pct_gate <- c(sum(airfare$GATE=="Free") / length(airfare$GATE),
              sum(airfare$GATE=="Constrained") / length(airfare$GATE))
names(pct_gate) <- c("Free", "Constrained")
pct_gate

avg_fare_gate <- c(mean(airfare[airfare$GATE=="Free", "FARE"]), 
                   mean(airfare[airfare$GATE=="Constrained", "FARE"]))

barplot(avg_fare_gate, names.arg=c("Free", "Constrained"), xlab="Gate Type", 
        ylab="Avg Fare Price", ylim=c(0,200))


# Question 3

# Part (i)
# set categorical variables as factors
airfare$VACATION <- as.factor(airfare$VACATION)
airfare$SW <- as.factor(airfare$SW)
airfare$SLOT <- as.factor(airfare$SLOT)
airfare$GATE <- as.factor(airfare$GATE)

# split data into 60/40 train/validate
set.seed(1)
pop_size <- length(airfare[[1]])
train.index=sample(c(1:pop_size), 0.6*pop_size, replace=FALSE)
train <- airfare[train.index,] 
valid <- airfare[-train.index,]

# Part (ii)

step_model <- regsubsets(FARE~. -S_CODE -S_CITY -E_CODE - E_CITY,
         data=train,
         nvmax=13,
         method="forward")

valid.mat_step=model.matrix(FARE~. -S_CODE -S_CITY -E_CODE - E_CITY, valid)

val.errors_step=numeric(13)
for(i in 1:13){
  coefi=coef(step_model,id=i)
  pred=valid.mat_step[,names(coefi)]%*%coefi
  val.errors_step[i]=mean((valid$FARE-pred)^2)
}
val.errors_step
best_step=which.min(val.errors_step)
best_step
coef(step_model,best_step)

# Part (iii)
exhaust_model <- regsubsets(FARE~. -S_CODE -S_CITY -E_CODE - E_CITY,
                         data=train,
                         nvmax=13,
                         method="exhaustive")

valid.mat_exh=model.matrix(FARE~. -S_CODE -S_CITY -E_CODE - E_CITY, valid)

val.errors_exh=numeric(13)
for(i in 1:13){
  coefi=coef(exhaust_model,id=i)
  pred=valid.mat_exh[,names(coefi)]%*%coefi
  val.errors_exh[i]=mean((valid$FARE-pred)^2)
}
val.errors_exh
best_exh=which.min(val.errors_exh)
best_exh
coef(exhaust_model,best_exh)

# Part (iv)

# for stepwise model
step_MSE=val.errors_step[best_step]
step_RMSE=sqrt(step_MSE)

coefi_step=coef(step_model,id=best_step)
pred_step=valid.mat_step[,names(coefi_step)]%*%coefi_step
step_error=valid$FARE-pred_step

step_MAE=mean(abs(step_error))
step_MAPE=mean(abs(step_error/valid$FARE))*100

step_measures <- c(step_MSE, step_RMSE, step_MAE, step_MAPE)
names(step_measures) <- c("MSE", "RMSE", "MAE", "MAPE")

# for exhaustive model
exh_MSE=val.errors_exh[best_exh]
exh_RMSE=sqrt(exh_MSE)

coefi_exh=coef(exhaust_model,id=best_exh)
pred_exh=valid.mat_exh[,names(coefi_exh)]%*%coefi_exh
exh_error=valid$FARE-pred_exh

exh_MAE=mean(abs(exh_error))
exh_MAPE=mean(abs(exh_error/valid$FARE))*100

exh_measures <- c(exh_MSE, exh_RMSE, exh_MAE, exh_MAPE)
names(exh_measures) <- c("MSE", "RMSE", "MAE", "MAPE")

# Part (v)

new_FARE <- -33.4966 - (34.3351*0) - (41.4734*0) + (.0097*4442.141) + (.0018*28760) +
  (.0022*27664) + (.000004*4557004) + (.000004*3195503) - (12.9106*1) - (22.6017*1) +
  (.0753*1976) - (.0011*12782)

# Part (vi)
coefi_exh

# Part (vii) in Word doc

# Part (viii)

new_model <- regsubsets(FARE~VACATION+S_INCOME+E_INCOME+S_POP+E_POP+DISTANCE,
                            data=train,
                            nvmax=6,
                            method="exhaustive")

valid.mat_new=model.matrix(FARE~VACATION+S_INCOME+E_INCOME+S_POP+E_POP+DISTANCE, valid)

val.errors_new=numeric(6)
for(i in 1:6){
  coefi=coef(new_model,id=i)
  pred=valid.mat_new[,names(coefi)]%*%coefi
  val.errors_new[i]=mean((valid$FARE-pred)^2)
}
val.errors_new
best_new=which.min(val.errors_new)
best_new
coef(new_model,best_new)

# Part (ix)

new_pred <- -109.3986 - (46.1058*0) + (.0039*28760) +(.0030*27664) +
  (.000002*4557004) + (.000004*3195503) + (0.0745*1976)
new_pred

# Part (x)

min(val.errors_exh)
new_MSE = min(val.errors_new)

new_RMSE=sqrt(new_MSE)

coefi_new=coef(new_model,id=best_new)
pred_new=valid.mat_new[,names(coefi_new)]%*%coefi_new
new_error=valid$FARE-pred_new

new_MAE=mean(abs(new_error))
new_MAPE=mean(abs(new_error/valid$FARE))*100

new_measures <- c(new_MSE, new_RMSE, new_MAE, new_MAPE)
names(new_measures) <- c("MSE", "RMSE", "MAE", "MAPE")
