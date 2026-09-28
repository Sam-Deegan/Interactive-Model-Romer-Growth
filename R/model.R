################################################################################
## Project: ECON42550 Macroeconomics                                          ##
## Romer Endogenous Growth: Model                                             ##
################################################################################

## Author:      Sam Deegan
## Affiliation: University College Dublin
## Email:       sam.deegan@ucdconnect.ie

## Usage:
##   Sourced automatically by app.R. Can be sourced alone from a lecture
##   .qmd so slide figures come from the same model:
##     source("R/model.R")
##
## Inputs:
##   None. Every function is a pure function of a parameter list "par"
##   built by the app.
##
## Outputs:
##   C_01_* functions: the technology block (growth rate, dynamics, path,
##   research share, readouts). C_02_* functions: the capital block.
##
## The model (notation of the Part 3 exam questions; Romer 2019, ch. 3):
##   Goods:      Y = A (1 - a) L                        (stages 1 to 5)
##               Y = K^alpha (A L_Y)^(1 - alpha),  L_Y = (1 - a) L
##   Research:   Adot = theta (a L)^lambda A^phi
##   Population: Ldot / L = n
##   Capital:    Kdot = s Y - delta K
##
##   Dividing the research equation by A gives g_A = theta (a L)^lambda
##   A^(phi - 1), and differentiating that gives the dynamics of the growth
##   rate in g_A alone:
##       gdot_A = g_A [ lambda n + (phi - 1) g_A ].
##   With phi < 1 it settles at g_A* = lambda n / (1 - phi), which has no
##   research share in it: semi-endogenous growth (Jones 1995). With phi = 1
##   and n = 0 it is theta (a L)^lambda, set by the research share and by the
##   size of the workforce: endogenous growth, with a scale effect.
##
##   The balanced level of technology is A = [theta (a L)^lambda /
##   g_A*]^(1/(1 - phi)), so output per worker A (1 - a) is proportional to
##   (1 - a) a^(lambda/(1 - phi)) and has an interior maximum in a.
##
##   Writing k = K / (A L) and y = Y / (A L), the capital side is
##       kdot = s k^alpha (1 - a)^(1 - alpha) - (n + g_A + delta) k,
##       y    = k^alpha (1 - a)^(1 - alpha),
##   with k* = [s (1 - a)^(1 - alpha) / (n + g_A* + delta)]^(1/(1 - alpha))
##   and y* = [s / (n + g_A* + delta)]^(alpha/(1 - alpha)) (1 - a). On the
##   balanced path Y/L and K/L both grow at g_A; the research share has a
##   level effect through (1 - a) and, with phi < 1, no growth effect.
##
## Parameter list (par) elements:
##   theta, lambda, phi, a_res, n, a_start, l_start, n_periods, shift_at,
##   shift_by, alpha, saving, delta, k_start
##
## References:
##   Romer, D. (2019). Advanced Macroeconomics, 5th ed. Ch. 3, sections 3.2
##     (the model without capital) and 3.3 (the model with capital).
##   Romer, P. (1990). Endogenous Technological Change. JPE 98(5).
##   Jones, C. (1995). R&D-Based Models of Economic Growth. JPE 103(4).

#-------------------------------- Script Begin --------------------------------#

################################################################################
## A: Table of Contents ########################################################
################################################################################
# Note: C_01 and C_02 hold the model; app.R holds sections B, D, E, F and G.
#
#   C: Model
#     C_01  Ideas, growth and the research share
#       C_01_01  Growth rate of technology     C_01_07  Best research share
#       C_01_02  Steady growth rate            C_01_08  A change in the share
#       C_01_03  Dynamics of the growth rate   C_01_09  Readouts
#       C_01_04  The path                      C_01_10  Calibration notes
#       C_01_05  Balanced level of technology  C_01_11  Nothing to draw
#       C_01_06  Growth and level against a
#     C_02  Capital, output and the balanced path
#       C_02_01  Long-run growth rate          C_02_07  Transition path
#       C_02_02  Output per effective worker   C_02_08  Balanced path against a
#       C_02_03  Balanced stock of capital     C_02_09  Readouts
#       C_02_04  Balanced level of output      C_02_10  Calibration notes
#       C_02_05  The two differential eqs      C_02_11  Nothing to draw
#       C_02_06  One Runge-Kutta step

################################################################################
## C: Model ####################################################################
################################################################################
# Note: Pure functions. Nothing here touches Shiny.

#### C_01: Ideas, Growth and the Research Share ################################
# Note: The technology equation, its dynamics, and what the research share
#   does to the level and the growth rate. Romer (2019) section 3.2.

###### C_01_01: The Growth Rate of Technology ##################################
# Note: Part (a) of the exam question: the research equation divided by A.

C_01_01_growth_fn <- function(par, tech, labour) {
  par$theta * (par$a_res * labour)^par$lambda * tech^(par$phi - 1)
}

###### C_01_02: The Steady Growth Rate #########################################
# Note: Part (c). With phi < 1 the growth rate converges to lambda n /
#   (1 - phi), with no research share in it (Jones 1995). NA otherwise.

C_01_02_steady_fn <- function(par) {
  if (par$phi >= 1) return(NA_real_)
  par$lambda * par$n / (1 - par$phi)
}

###### C_01_03: The Dynamics of the Growth Rate ################################
# Note: Part (b): gdot_A = g_A (lambda n + (phi - 1) g_A) on a grid of g_A.
#   The crossing from above is the steady rate; it is stable when phi < 1.

C_01_03_dynamics_fn <- function(par, g_max = NULL) {
  top  <- if (is.null(g_max)) {
    max(C_01_02_steady_fn(par) * 3, 0.06, na.rm = TRUE)
  } else {
    g_max
  }
  grid <- seq(0, top, length.out = 300)
  data.frame(
    g_a  = grid,
    gdot = grid * (par$lambda * par$n + (par$phi - 1) * grid)
  )
}

###### C_01_04: The Path #######################################################
# Note: The model forward in Euler steps of one year on the research
#   equation, with the workforce growing at n.

C_01_04_path_fn <- function(par) {
  n_t <- par$n_periods
  a   <- numeric(n_t)
  l   <- numeric(n_t)
  a[1] <- par$a_start
  l[1] <- par$l_start

  for (t in seq_len(n_t)[-1]) {
    a[t] <- a[t - 1] + par$theta * (par$a_res * l[t - 1])^par$lambda *
      a[t - 1]^par$phi
    l[t] <- l[t - 1] * (1 + par$n)
  }

  g_a <- c(diff(a) / a[-n_t], NA_real_)
  out <- a * (1 - par$a_res) * l

  data.frame(
    period   = seq_len(n_t),
    tech     = a,
    labour   = l,
    g_tech   = g_a,
    output   = out,
    y_worker = a * (1 - par$a_res),
    research = par$a_res * l
  )
}

###### C_01_05: The Balanced Level of Technology ###############################
# Note: The level of A consistent with growing at the steady rate; it rises
#   with the research share to the power lambda / (1 - phi).

C_01_05_level_fn <- function(par, a_res = NULL) {
  share <- if (is.null(a_res)) par$a_res else a_res
  g_ss  <- C_01_02_steady_fn(par)
  if (is.na(g_ss) || g_ss <= 0) return(NA_real_)
  (par$theta * (share * par$l_start)^par$lambda /
     g_ss)^(1 / (1 - par$phi))
}

###### C_01_06: Growth and Level Against the Research Share ####################
# Note: Long-run growth and balanced output per worker on a grid of the
#   research share. Flat and humped with phi < 1; rising with phi = 1.

C_01_06_share_fn <- function(par, n = 200) {
  grid <- seq(0.01, 0.85, length.out = n)

  growth <- vapply(grid, function(s) {
    if (par$phi >= 1) {
      par$theta * (s * par$l_start)^par$lambda
    } else {
      C_01_02_steady_fn(par)
    }
  }, 0)

  level <- vapply(grid, function(s) {
    lv <- C_01_05_level_fn(par, s)
    if (is.na(lv)) NA_real_ else lv * (1 - s)
  }, 0)

  data.frame(a_res = grid, growth = growth, y_worker = level)
}

###### C_01_07: The Best Research Share ########################################
# Note: The share that maximises balanced output per worker: (1 - a) a^kappa
#   with kappa = lambda / (1 - phi) peaks at a = kappa / (1 + kappa).

C_01_07_best_fn <- function(par) {
  if (par$phi >= 1) return(list(share = NA_real_, kappa = NA_real_))
  kappa <- par$lambda / (1 - par$phi)
  list(share = kappa / (1 + kappa), kappa = kappa)
}

###### C_01_08: A Change in the Research Share #################################
# Note: The path with the research share raised by shift_by from year
#   shift_at and held there.

C_01_08_shift_fn <- function(par) {
  n_t   <- par$n_periods
  when  <- max(2, min(round(par$shift_at), n_t - 1))
  share <- c(rep(par$a_res, when - 1),
             rep(par$a_res + par$shift_by, n_t - when + 1))
  share <- pmin(pmax(share, 0.01), 0.9)

  a <- numeric(n_t)
  l <- numeric(n_t)
  a[1] <- par$a_start
  l[1] <- par$l_start
  for (t in seq_len(n_t)[-1]) {
    a[t] <- a[t - 1] + par$theta * (share[t - 1] * l[t - 1])^par$lambda *
      a[t - 1]^par$phi
    l[t] <- l[t - 1] * (1 + par$n)
  }

  data.frame(
    period   = seq_len(n_t),
    share    = share,
    tech     = a,
    g_tech   = c(diff(a) / a[-n_t], NA_real_),
    y_worker = a * (1 - share),
    shifted  = seq_len(n_t) >= when
  )
}

###### C_01_09: Readouts #######################################################
# Note: The numbers shown in the tiles above the figures.

C_01_09_diagnostics_fn <- function(par, n_live = TRUE) {
  path <- C_01_04_path_fn(par)
  best <- C_01_07_best_fn(par)
  g_ss <- C_01_02_steady_fn(par)
  n_t  <- nrow(path)

  list(
    g_steady   = g_ss,
    g_now      = path$g_tech[1],
    g_end      = path$g_tech[n_t - 1],
    endogenous = par$phi >= 1,
    tech_end   = path$tech[n_t],
    y_end      = path$y_worker[n_t],
    researchers = par$a_res * par$l_start,
    best_share = best$share,
    kappa      = best$kappa,
    at_best    = !is.na(best$share) && abs(par$a_res - best$share) < 0.02,
    problems   = C_01_10_problems_fn(par, n_live)
  )
}

###### C_01_10: Notes on the Calibration #######################################
# Note: Remarks shown above the figures at the model's corner cases; each
#   still has a path to draw. n_live is FALSE while the app pins n at zero.

C_01_10_problems_fn <- function(par, n_live = TRUE) {
  out <- character(0)

  if (isTRUE(par$phi > 1)) {
    out <- c(out, paste(
      "With phi above one each new idea makes the next one more than",
      "proportionally easier, so technology explodes in finite time. The",
      "model has no balanced path. Bring phi back to one or below."
    ))
  }
  if (isTRUE(par$phi >= 1) && isTRUE(par$n > 0)) {
    out <- c(out, paste(
      "With phi = 1 and a growing population the growth rate itself keeps",
      "rising, which is the scale effect the evidence does not support. Set",
      "population growth to zero to see the clean endogenous growth case."
    ))
  }
  if (isTRUE(n_live) && isTRUE(par$phi < 1) && isTRUE(par$n <= 0)) {
    out <- c(out, paste(
      "With phi below one and no population growth, long-run growth is",
      "zero: ideas get harder to find and nobody new arrives to look for",
      "them. The figures show exactly that \u2014 the growth rate falling",
      "towards zero while the stock of ideas keeps rising. Raise population",
      "growth, or set phi to one, to see the alternatives."
    ))
  }
  out
}

###### C_01_11: Calibrations With Nothing to Draw ##############################
# Note: Parameter values at which there is no path to draw. Only phi above
#   one qualifies: A diverges in finite time and every series overflows.

C_01_11_fatal_fn <- function(par) {
  out <- character(0)

  if (isTRUE(par$phi > 1)) {
    out <- c(out, paste(
      "Technology diverges in finite time at phi above one, so there is no",
      "path to draw. Bring phi back to one or below."
    ))
  }
  out
}

#### C_02: Capital, Output and the Balanced Path ###############################
# Note: Capital accumulates out of saving; per effective worker the model
#   is two differential equations, in g_A and in k. Romer (2019) section 3.3.

###### C_02_01: The Long-Run Growth Rate of Technology #########################
# Note: The rate g_A heads for: lambda n / (1 - phi) below phi = 1, theta
#   (a L)^lambda at phi = 1 with a constant workforce, NA anywhere else.

C_02_01_glong_fn <- function(par) {
  if (par$phi < 1) return(C_01_02_steady_fn(par))
  if (par$phi > 1) return(NA_real_)
  if (par$n > 0) return(NA_real_)
  par$theta * (par$a_res * par$l_start)^par$lambda
}

###### C_02_02: Output per Effective Worker ####################################
# Note: y = Y / (A L) = k^alpha (1 - a)^(1 - alpha). The (1 - a) term is
#   where research is paid for.

C_02_02_ytilde_fn <- function(par, k_tilde) {
  pmax(k_tilde, 0)^par$alpha * (1 - par$a_res)^(1 - par$alpha)
}

###### C_02_03: The Balanced Stock of Capital ##################################
# Note: kdot = 0 gives s k^alpha (1 - a)^(1 - alpha) = (n + g_A + delta) k:
#   the Solow steady state with g_A handed to it by the ideas block.

C_02_03_kstar_fn <- function(par, g_a = NULL) {
  g_use <- if (is.null(g_a)) C_02_01_glong_fn(par) else g_a
  if (is.na(g_use)) return(NA_real_)
  wedge <- par$n + g_use + par$delta
  if (wedge <= 0) return(NA_real_)
  (par$saving * (1 - par$a_res)^(1 - par$alpha) /
     wedge)^(1 / (1 - par$alpha))
}

###### C_02_04: The Balanced Level of Output ###################################
# Note: The balanced stock of capital substituted back into y. Capital
#   deepening is a constant multiplier; the research share's cost is (1 - a).

C_02_04_ystar_fn <- function(par, g_a = NULL) {
  k_ss <- C_02_03_kstar_fn(par, g_a)
  if (is.na(k_ss)) return(NA_real_)
  C_02_02_ytilde_fn(par, k_ss)
}

###### C_02_05: The Two Differential Equations #################################
# Note: The two differential equations: the dynamics of g_A from part (b)
#   and capital accumulation per effective worker.

C_02_05_deriv_fn <- function(par, g_a, k_tilde) {
  k <- max(k_tilde, 1e-9)
  c(
    g_a     = g_a * (par$lambda * par$n + (par$phi - 1) * g_a),
    k_tilde = par$saving * k^par$alpha * (1 - par$a_res)^(1 - par$alpha) -
      (par$n + g_a + par$delta) * k
  )
}

###### C_02_06: One Runge-Kutta Step ###########################################
# Note: Fourth-order Runge-Kutta on the state (g_A, k, log A). Capital needs
#   a finer step than a year to land on its balanced stock cleanly.

C_02_06_step_fn <- function(par, state, h) {
  slope <- function(s) {
    d <- C_02_05_deriv_fn(par, s[["g_a"]], s[["k_tilde"]])
    c(g_a = d[["g_a"]], k_tilde = d[["k_tilde"]], log_tech = s[["g_a"]])
  }
  s1  <- slope(state)
  s2  <- slope(state + h / 2 * s1)
  s3  <- slope(state + h / 2 * s2)
  s4  <- slope(state + h * s3)
  out <- state + h / 6 * (s1 + 2 * s2 + 2 * s3 + s4)
  out[["k_tilde"]] <- max(out[["k_tilde"]], 1e-9)
  out
}

###### C_02_07: The Transition Path ############################################
# Note: The joint path of technology and capital, recorded once a year.
#   Growth in Y/L is g_A + alpha kdot/k and in K/L is g_A + kdot/k.

C_02_07_capital_fn <- function(par, a_res = NULL) {
  use <- par
  if (!is.null(a_res)) use$a_res <- a_res
  n_t   <- use$n_periods
  steps <- 20L
  h     <- 1 / steps

  g_a      <- numeric(n_t)
  k_tilde  <- numeric(n_t)
  log_tech <- numeric(n_t)
  g_a[1]      <- C_01_01_growth_fn(use, use$a_start, use$l_start)
  k_tilde[1]  <- use$k_start
  log_tech[1] <- log(use$a_start)

  state <- c(g_a = g_a[1], k_tilde = k_tilde[1], log_tech = log_tech[1])
  for (t in seq_len(n_t)[-1]) {
    for (i in seq_len(steps)) {
      state <- C_02_06_step_fn(use, state, h)
    }
    g_a[t]      <- state[["g_a"]]
    k_tilde[t]  <- state[["k_tilde"]]
    log_tech[t] <- state[["log_tech"]]
  }

  k_dot <- vapply(seq_len(n_t), function(t) {
    C_02_05_deriv_fn(use, g_a[t], k_tilde[t])[["k_tilde"]]
  }, 0)
  g_k     <- k_dot / pmax(k_tilde, 1e-9)
  tech    <- exp(log_tech)
  y_tilde <- C_02_02_ytilde_fn(use, k_tilde)

  data.frame(
    period   = seq_len(n_t),
    g_tech   = g_a,
    k_tilde  = k_tilde,
    y_tilde  = y_tilde,
    tech     = tech,
    y_worker = tech * y_tilde,
    k_worker = tech * k_tilde,
    g_ktilde = g_k,
    g_y      = g_a + use$alpha * g_k,
    g_kw     = g_a + g_k
  )
}

###### C_02_08: The Balanced Path Against the Research Share ###################
# Note: Long-run growth and the balanced level of output per effective
#   worker on a grid of the research share.

C_02_08_share_fn <- function(par, n = 200) {
  grid <- seq(0.01, 0.85, length.out = n)

  growth <- vapply(grid, function(s) {
    use <- par
    use$a_res <- s
    C_02_01_glong_fn(use)
  }, 0)

  level <- vapply(grid, function(s) {
    use <- par
    use$a_res <- s
    C_02_04_ystar_fn(use)
  }, 0)

  data.frame(a_res = grid, growth = growth, y_tilde = level)
}

###### C_02_09: Readouts for the Capital Side ##################################
# Note: The numbers shown in the tiles once capital is in the model.

C_02_09_capdiag_fn <- function(par) {
  path <- C_02_07_capital_fn(par)
  n_t  <- nrow(path)
  g_ss <- C_02_01_glong_fn(par)
  k_ss <- C_02_03_kstar_fn(par)
  none <- par
  none$a_res <- 0

  list(
    g_long    = g_ss,
    k_star    = k_ss,
    k_end     = path$k_tilde[n_t],
    y_star    = C_02_04_ystar_fn(par),
    y_none    = C_02_04_ystar_fn(none),
    y_end     = path$y_tilde[n_t],
    g_y_end   = path$g_y[n_t],
    g_kw_end  = path$g_kw[n_t],
    g_tech_end = path$g_tech[n_t],
    settled   = !is.na(k_ss) &&
      abs(path$k_tilde[n_t] - k_ss) < 0.01 * k_ss,
    problems  = C_02_10_problems_fn(par)
  )
}

###### C_02_10: Notes on the Capital Calibration ###############################
# Note: Remarks for the capital block alone; the app shows them with
#   C_01_10. Each still leaves a transition path to draw.

C_02_10_problems_fn <- function(par) {
  out <- character(0)

  if (isTRUE(par$alpha <= 0) || isTRUE(par$alpha >= 1)) {
    out <- c(out, paste(
      "The capital share has to sit strictly between zero and one for the",
      "production function to have constant returns to capital and effective",
      "labour together. Bring alpha back inside (0, 1)."
    ))
  }
  if (isTRUE(par$phi >= 1) && isTRUE(par$n > 0)) {
    out <- c(out, paste(
      "With phi = 1 and a growing population the growth rate of technology",
      "keeps rising, so the (n + g_A + delta) that capital has to keep up",
      "with rises with it and capital per effective worker is driven to",
      "zero. There is no balanced path for capital either."
    ))
  }

  wedge <- par$n + C_02_01_glong_fn(par) + par$delta
  if (isTRUE(wedge <= 0)) {
    out <- c(out, paste(
      "Capital per effective worker only settles somewhere if n + g_A +",
      "delta is positive: something has to wear the capital out or spread",
      "it thinner. Raise depreciation."
    ))
  }
  out
}

###### C_02_11: Capital Calibrations With Nothing to Draw ######################
# Note: The capital block's half of C_01_11: outside (0, 1) the capital
#   share gives the integrator nothing sensible to step through.

C_02_11_fatal_fn <- function(par) {
  out <- character(0)

  if (isTRUE(par$alpha <= 0) || isTRUE(par$alpha >= 1)) {
    out <- c(out, paste(
      "The capital share has to sit strictly between zero and one for there",
      "to be a path to draw. Bring alpha back inside (0, 1)."
    ))
  }
  out
}
