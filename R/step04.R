########################################################
# Step 4: 1D Burgers' Equation
########################################################

X <- 101                      # Number of spatial points
T <- 100                      # Number of time steps
v <- 0.07                     # Diffusion coefficient
dx <- 2 * pi / (X - 1)        # Spatial step size
dt <- dx * v                  # Time step size

# Initialise spatial grid and initial condition
x <- seq(0, 2 * pi, length.out = X)
u <- rep(0, X)
uA <- rep(0, X)

AnalyticalSolution <- function(t, v, x) {
  phi <- exp(-(4 * t - x)^2 / (4 * v * (1 + t))) + exp(-(2 * pi + 4 * t - x)^2 / (4 * v * (1 + t)))
  dphi <- (exp(-(2 * pi + 4 * t - x)^2 / (4 * v * (1 + t))) * (4 * pi + 8 * t - 2 * x)) / (4 * v * (1 + t)) +
    (exp(-(4 * t - x)^2 / (4 * v * (1 + t))) * (8 * t - 2 * x)) / (4 * v * (1 + t))
  u <- 4 - (2 * dphi * v) / phi
  return(u)
}

for (i in 1:X) {
  u[i] <- AnalyticalSolution(0, v, x[i])
  uA[i] <- AnalyticalSolution(100 * dt, v, x[i])
}

# Setup the plot
ani.options(interval = 50)

saveHTML({
  for (n in 1:T) {
    un <- u
    for (i in 2:(X - 1)) {
      u[i] <- (v * dt / dx^2 * (un[i + 1] - 2 * un[i] + un[i - 1])) - 
        (un[i] * dt / dx * (un[i] - un[i - 1])) + un[i]
    }
    u[1] <- (v * dt / (dx * dx) * (un[2] - 2 * un[1] + un[X - 1])) - 
      (un[1] * dt / dx * (un[1] - un[X - 1])) + un[1]
    u[X] <- u[1]
    plot(x, u, type = 'l', col = 'red', xlab = 'x', ylab = 'u', 
         main = "1D Burgers' Equation")
  }
}, autoplay = FALSE, verbose = FALSE, interval = 0.05, outdir = getwd())
