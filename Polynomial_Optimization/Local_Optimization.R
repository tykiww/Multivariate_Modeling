# Optimizing using Polynomial Fits

x <- c(1,4,4,3,6,6,6,7,8)
y <- c(12,45,33,44,55,65,58,57,55)
yes <- lm(y~poly(x,3, raw = TRUE)) # Note use of raw=TRUE, otherwise poly returns orthogonal polynomials
no <- lm(y~x)
plot(x,y, col = "steel blue")
lines(sort(x),fitted(yes)[order(x)], col = "darkred", type = 'l',cex = 10)


# Copy
G = seq(min(x), max(x), length=9)
pred = data.frame(G,
                  Yield=predict(yes, newdata=data.frame(G)))


# Vector of model coefficients
cf = coef(yes)

# First derivative of fit. This is just for Illustration; we won't plot this
#  equation directly, but we'll find its roots to get the locations of 
#  local maxima and minima.
D1 = cf[2] + 2*cf[3]*pred$G + 3*cf[4]*pred$G^2



# Roots of first derivative (these are locations where first derivative = 0).
#  Use quadratic formula to find roots.
D1roots = (-2*cf[3] + c(-1,1)*sqrt((2*cf[3])^2 - 4*3*cf[4]*cf[2]))/(2*3*cf[4])

# Second derivative at the two roots. 
D2atD1roots =  2*cf[3] + 6*cf[4]*D1roots

# Local maxima are where the second derivative is negative
max_x = D1roots[which(D2atD1roots < 0)]
max_y = cf %*% max_x^(0:3)

# Plot local maxima
points(max_x, max_y, pch=16, col="red")

# Add values of Yield at local maxima
text(max_x, max_y, label=paste("(",round(max_x,2),",",round(max_y,2),")"), adj=c(0.5,-1), cex=0.8)


