# Decisions Decisions..


    ## “Suppose a farmer has 75 acres on which to plant two crops: wheat and barley. 
    ## To produce these crops, it costs the farmer (for seed, fertilizer, etc.) 
    ## $120 per acre for the wheat and $210 per acre for the barley. 
    ## The farmer has $15000 available for expenses. But after the harvest, the 
    ## farmer must store the crops while awaiting avourable market conditions. 
    ## The farmer has storage space for 4000 bushels. Each acre yields an average 
    ## of 110 bushels of wheat or 30 bushels of barley. 
    ## If the net profit per bushel of wheat (after all expenses have been subtracted) 
    ## is $1.30 and for barley is $2.00, how should the farmer plant the 75 acres to maximize profit?”


library(lpSolve)

obj <- data.frame( 
  obj = c("Wheat","Barley"),
  coefs = c(110*1.3,30*2.00), # Maximize
  acres = c(1,1), # 75
  storage = c(110,30), # 4000
  costs = c(120,210) # 15000
  )


limits <- c(
  acres= 75, 
  storage = 4000,
  costs = 15000,
  barley = 0,
  wheat = 0)

opts <- lp(
  direction = "max",
  objective.in = obj$coefs,
  const.mat = t(obj[,-c(1:2)]),
  const.dir = c("<=","<=","<="),
  const.rhs = limits[-c(4:5)]
  )

opts$solution
opts$objval # sum(opts$solution*obj$coefs)


#######################################################################################################################################


    ## A trading company is looking for a way to maximize profit per transportation of their goods. 
    ## The company has a train available with 3 wagons. 
    ## When stocking the wagons they can choose between 4 types of cargo, each with its own specifications. 
    ## How much of each cargo type should be loaded on which wagon in order to maximize profit?

library(lpSolveAPI)

wagon <- data.frame(
  wagons = c("w1","w2","w3"),
  weight = c(10, 8, 12),
  space = c(5000,4000,8000)
)

cargo <- data.frame(
  cargos = c("c1","c2","c3","c4"),
  amount = c(18,10,5,20),
  volume = c(400,300,200,500),
  profit = c(2000,2500,5000,3500)
)


2*nrow(wagon)+nrow(cargo)

# Since there are 3 wagons and 4 types of cargo, there are 12 decisions. 
# Furthermore, we will be making 10 constraints because there are 2*# of wagon possible train constraints and 4 cargo constraints.

# Building Model per cargo column. We are transposing the cargo dataframe and initializing a matrix of wagonsXcargo
col <- 1
for (i in 1:nrow(wagon)) {
  for (j in 1:nrow(cargo)) {
    set.column(lpm,col,c(1, cargo[j,'volume'],1), 
               indices=c(i,nrow(wagon)+i, nrow(wagon)*2+j))
    col <- col + 1
  }
}



# Right hand side weight constraints
set.constr.value(lpm, rhs=wagon$weight, constraints=1:nrow(wagon))

# Right hand side volume constraints
set.constr.value(lpm, rhs=wagon$space, constraints=seq(nrow(wagon)+1,nrow(wagon)*2))


#set rhs volume constraints
set.constr.value(lpm, rhs=cargo$amount, constraints=seq(nrow(wagon)*2+1,nrow(wagon)*2+nrow(cargo)))

#set objective coefficients
set.objfn(lpm, rep(cargo$profit,nrow(wagon)))

#set objective direction
lp.control(lpm,sense='max')

write.lp(lpm,'model.lp',type='lp')

#solve the model, if this return 0 an optimal solution is found
solve(lpm)

#this return the proposed solution
get.objective(lpm) # 107500

