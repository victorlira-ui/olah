# olah
a ver q sale
# Enfriamiento de magma - Euler explícito (unidad de tiempo: días)
euler_cooling <- function(T0, T_env, k, dt, t_max) {
  times <- seq(0, t_max, by = dt)
  T <- numeric(length(times))
  T[1] <- T0
  for (i in seq_len(length(times)-1)) {
    dTdt <- -k * (T[i] - T_env)
    T[i+1] <- T[i] + dt * dTdt
  }
  data.frame(time = times, T = T)
}

analytic_T <- function(T0, T_env, k, times) {
  T_env + (T0 - T_env) * exp(-k * times)
}

# Parámetros ejemplo (ajusta según tu caso)
T0    <- 1200    # °C (temperatura inicial del magma)
T_env <- 25      # °C (ambiente)
k     <- 0.05    # day^-1 (constante de enfriamiento)
dt    <- 0.1     # days (paso de tiempo)
t_max <- 200     # days (duración de la simulación)

sim <- euler_cooling(T0, T_env, k, dt, t_max)
sim$T_analytical <- analytic_T(T0, T_env, k, sim$time)
sim$error <- abs(sim$T - sim$T_analytical)

# Gráfico comparativo
plot(sim$time, sim$T_analytical, type = "l", col = "blue", lwd = 2,
     xlab = "Tiempo (d)", ylab = "Temperatura (°C)",
     main = "Enfriamiento de magma — Euler explícito vs analítico")
lines(sim$time, sim$T, col = "red", lty = 2)
legend("topright", legend = c("Analítico", "Euler explícito"),
       col = c("blue","red"), lty = c(1,2), lwd = c(2,1))

# Resumen de error y estabilidad
cat("Max error (°C):", round(max(sim$error), 4), "\n")
cat("Condición de estabilidad (Euler explícito): dt < 2/k ->", 2 / k, "días\n")
cat("Regla práctica de precisión: dt << 1/k (a menudo dt <= 0.1*(1/k))\n")
