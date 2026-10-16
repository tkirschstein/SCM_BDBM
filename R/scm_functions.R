# =============================================================================
# SCM Course — Reusable R Functions
# Prof. Dr. Thomas Kirschstein | HS RheinMain
# Bachelor Supply Chain Management
# =============================================================================
# Fachmodelle und Algorithmen, gruppiert nach Themen:
#   0. Bestandsmanagement ((t, S)-Politik)
#   1. Location Planning (CoG, Weiszfeld, Haversine, AHP)
#   2. Transportation Problem (Vogel's Approximation)
#   3. Inventory Models (Newsvendor, EOQ, Pooling)
#   4. Warehouse Location Problem (Add / Drop Heuristics)
#   5. Transportplanung: Eröffnungsverfahren mit Protokoll
#   6. Kürzeste Wege (Dijkstra)
#   7. Tourenplanung (TSP-Heuristiken, Savings)
#   8. Lager und Kommissionierung (Picker Routing, Lagerplatzvergabe)
#   9. Transportkosten
# Darstellungs- und Formatierungshilfen stehen in R/scm_helpers.R.
# =============================================================================

# 0. Inventor management ─────────────────────────────────────────────────────

#' Simulation einer periodischen (t, S)-Bestandspolitik
#'
#' Zeitliche Reihenfolge innerhalb einer Periode k:
#'   1. Eingang der Bestellung aus Periode k - wbz
#'   2. Realisierung des Bedarfs
#'   3. Fortschreibung von Nettobestand und physischem Bestand
#'   4. Ermittlung der offenen Bestellungen
#'   5. Bestellentscheidung in jeder t-ten Periode
#'
#' @param bedarf Numerischer Vektor der periodischen Bedarfe d[k].
#' @param t Positives ganzzahliges Bestell- bzw. Überprüfungsintervall.
#' @param S Order-up-to-Level beziehungsweise Zielbestand.
#' @param anfangsbestand Physischer Anfangsbestand vor Periode 1.
#' @param wbz Feste Wiederbeschaffungszeit in ganzen Perioden.
#' @param backorders Logisch. Bei TRUE werden Fehlmengen als Backorders
#'   fortgeschrieben. Bei FALSE handelt es sich um Lost Sales.
#'
#' @return Ein tibble mit Bestands-, Bestell- und Lieferinformationen.
#'
#' @examples
#' simuliere_tS(
#'   bedarf = c(8, 9, 13, 16, 12, 8, 9, 11),
#'   t = 2,
#'   S = 30,
#'   anfangsbestand = 20,
#'   wbz = 2
#' )
simuliere_tS <- function(bedarf,
                         t,
                         S,
                         anfangsbestand,
                         wbz,
                         backorders = TRUE) {
  
  if (!is.numeric(bedarf) || length(bedarf) == 0L) {
    stop("'bedarf' muss ein nicht-leerer numerischer Vektor sein.")
  }
  
  if (any(is.na(bedarf)) || any(bedarf < 0)) {
    stop("'bedarf' darf keine fehlenden oder negativen Werte enthalten.")
  }
  
  if (length(t) != 1L || is.na(t) || t < 1 || t != as.integer(t)) {
    stop("'t' muss eine positive ganze Zahl sein.")
  }
  
  if (length(S) != 1L || is.na(S) || S < 0) {
    stop("'S' muss eine nicht-negative Zahl sein.")
  }
  
  if (length(anfangsbestand) != 1L ||
      is.na(anfangsbestand) ||
      anfangsbestand < 0) {
    stop("'anfangsbestand' muss eine nicht-negative Zahl sein.")
  }
  
  if (length(wbz) != 1L ||
      is.na(wbz) ||
      wbz < 0 ||
      wbz != as.integer(wbz)) {
    stop("'wbz' muss eine nicht-negative ganze Zahl sein.")
  }
  
  n_perioden <- length(bedarf)
  
  periode <- seq_len(n_perioden)
  
  liefermenge <- numeric(n_perioden)
  bestellmenge <- numeric(n_perioden)
  offene_bestellungen <- numeric(n_perioden)
  
  physischer_bestand_vor_bedarf <- numeric(n_perioden)
  physischer_bestand <- numeric(n_perioden)
  
  nettobestand_vor_bestellung <- numeric(n_perioden)
  nettobestand <- numeric(n_perioden)
  
  fehlmenge <- numeric(n_perioden)
  disponibler_bestand_vor_bestellung <- numeric(n_perioden)
  disponibler_bestand_nach_bestellung <- numeric(n_perioden)
  
  # n_0 ist bei Backorders identisch mit dem physischen Anfangsbestand.
  netto_vorperiode <- anfangsbestand
  
  for (k in periode) {
    
    # 1. Lieferung zu Periodenbeginn:
    # Bestellung o_(k-wbz) trifft in Periode k ein.
    if (wbz == 0L) {
      liefermenge[k] <- 0
    } else if (k > wbz) {
      liefermenge[k] <- bestellmenge[k - wbz]
    }
    
    # 2. Nettobestand nach Lieferung und Bedarfsrealisierung.
    nettobestand_vor_bestellung[k] <-
      netto_vorperiode + liefermenge[k] - bedarf[k]
    
    # Bei Backordering wird der Nettobestand einschließlich Fehlmengen
    # fortgeschrieben. Bei Lost Sales geht nicht bedienter Bedarf verloren.
    if (backorders) {
      nettobestand[k] <- nettobestand_vor_bestellung[k]
    } else {
      nettobestand[k] <- max(
        0,
        nettobestand_vor_bestellung[k]
      )
    }
    
    # Physischer Bestand ist nie negativ.
    physischer_bestand[k] <- max(
      0,
      nettobestand[k]
    )
    
    # Der Bestand unmittelbar vor Bedarfsrealisierung ergibt sich aus
    # dem Nettobestand der Vorperiode zuzüglich der eingetroffenen Menge.
    physischer_bestand_vor_bedarf[k] <- max(
      0,
      netto_vorperiode + liefermenge[k]
    )
    
    # Fehlmenge ist nur bei Backordering relevant.
    fehlmenge[k] <- if (backorders) {
      max(0, -netto_vorperiode - liefermenge[k] + bedarf[k])
    } else {
      max(0, -netto_vorperiode - liefermenge[k] + bedarf[k])
    }
    
    # 3. Noch offene Bestellungen vor der Neubestellung:
    # Alle Bestellungen der letzten wbz Perioden, die noch nicht
    # eingetroffen sind.
    if (wbz == 0L) {
      offene_bestellungen[k] <- 0
    } else {
      start_offen <- max(1L, k - wbz + 1L)
      ende_offen <- k - 1L
      
      offene_bestellungen[k] <- if (start_offen <= ende_offen) {
        sum(bestellmenge[start_offen:ende_offen])
      } else {
        0
      }
    }
    
    # 4. Disponibler Bestand bzw. Bestandsposition vor Neubestellung.
    disponibler_bestand_vor_bestellung[k] <-
      nettobestand[k] + offene_bestellungen[k]
    
    # 5. Bestellentscheidung:
    # In Perioden 1, 1+t, 1+2t, ... wird auf S aufgefüllt.
    if ((k - 1L) %% t == 0L) {
      bestellmenge[k] <- max(
        0,
        S - disponibler_bestand_vor_bestellung[k]
      )
    }
    
    # Bestandsposition unmittelbar nach der Bestellung.
    disponibler_bestand_nach_bestellung[k] <-
      disponibler_bestand_vor_bestellung[k] + bestellmenge[k]
    
    # Fortschreibung für die nächste Periode.
    netto_vorperiode <- nettobestand[k]
  }
  
  tibble::tibble(
    Periode = periode,
    Bedarf = bedarf,
    Lieferung = liefermenge,
    Physischer_Bestand_vor_Bedarf = physischer_bestand_vor_bedarf,
    Physischer_Bestand = physischer_bestand,
    Nettobestand = nettobestand,
    Fehlmenge = fehlmenge,
    Offene_Bestellungen = offene_bestellungen,
    Disponibler_Bestand_vor_Bestellung =
      disponibler_bestand_vor_bestellung,
    Bestellmenge = bestellmenge,
    Disponibler_Bestand_nach_Bestellung =
      disponibler_bestand_nach_bestellung,
    Bestellperiode = (periode - 1L) %% t == 0L
  )
}






#' (t,S) Inventory control function
#'
#' calculates inventory stocks for a (t,S)-controlled storage
#'
#' @param d  numeric vector with period demands
#' @param t  numeric scalar with order interval
#' @param S  numeric scalar with order-up-to-level
#' @param l.ini  numeric initial inventory level
#' @param wbz  integer number of  order lead time
#' @return        Named matrix with demand, stock levels, orders and deliveries
#' @examples
#' demand <- rpois(25, lambda = 25)
#' tS.lager(d = demand, t =3, S = 65, l.ini = 50, wbz = 2)

tS.lager <- function(d, t, S, l.ini,  wbz){
  n <- length(d)
  tmp.mat <- matrix(0, nrow= n+1+wbz, ncol= 5 )
  colnames(tmp.mat) <- c("Bedarf","Lagerbestand", "Bestellung", "Lieferung", "dis.LB")
  
  tmp.mat[,"Bedarf"] <- c(0,d, rep(0,wbz))
  tmp.mat[1,"Lagerbestand"] <- l.ini
  tmp.mat[1,"dis.LB"] <- l.ini
  
  for(i in 2:(n+1)){
    if((i-1) %% t == 0){
      tmp.mat[i, "Bestellung"] <- S - tmp.mat[i-1, "dis.LB"]
      tmp.mat[i+wbz, "Lieferung"] <- tmp.mat[i, "Bestellung"]
    }
    
    tmp.mat[i, "dis.LB"] <- tmp.mat[i-1, "dis.LB"] - tmp.mat[i, "Bedarf"] + tmp.mat[i, "Bestellung"]
    tmp.mat[i, "Lagerbestand"] <- tmp.mat[i-1, "Lagerbestand"] - tmp.mat[i, "Bedarf"] + tmp.mat[i, "Lieferung"]
    
  }
  return(tmp.mat[2:(n+1),]) 
}

tS.lager.new <- function(d, t, S, l.ini,  wbz){
  n <- length(d)
  tmp.mat <- matrix(0, nrow= n+1+wbz, ncol= 7 )
  colnames(tmp.mat) <- c("Bedarf","Lagerbestand","Nettobestand", "offene_Bestellungen", "Bestellmenge", "Lieferung", "disp_LB", "disp_LB_vor_Nachfrage")
  
  tmp.mat[,"Bedarf"] <- c(0,d, rep(0,wbz))
  tmp.mat[1,"Lagerbestand"] <- tmp.mat[1,"dis.LB"] <- tmp.mat[1,"Nettobestand"] <- l.ini
  tmp.mat[1,"offene_Bestellungen"] <- 0
  
  for(i in 2:(n+1)){
    
    tmp.mat[i, "Nettobestand"] <- tmp.mat[i-1, "Nettobestand"] - tmp.mat[i, "Bedarf"] + tmp.mat[i, "Lieferung"]
    
    tmp.mat[i, "offene_Bestellungen"] <- tmp.mat[i-1, "offene_Bestellungen"] - tmp.mat[i, "Lieferung"]
    
    
    if((i-1) %% t == 0){
      tmp.mat[i, "Bestellung"] <- S - tmp.mat[i-1, "dis.LB"] #+ tmp.mat[i, "Bedarf"] + tmp.mat[i, "Lieferung"]
      tmp.mat[i+wbz, "Lieferung"] <- tmp.mat[i, "Bestellung"]
    }
    
    tmp.mat[i, "dis.LB"] <- tmp.mat[i-1, "dis.LB"] - tmp.mat[i, "Bedarf"] + tmp.mat[i, "Bestellung"]
    
    
    tmp.mat[i, "Lagerbestand"] <- max(0,tmp.mat[i, "Nettobestand"])
    
  }
  return(tmp.mat[2:(n+1),]) 
}



# ─── 1. LOCATION PLANNING ─────────────────────────────────────────────────────

#' Center of Gravity (CoG) Heuristic
#'
#' Minimises the sum of weighted squared Euclidean distances.
#' Fast closed-form solution — a reasonable starting point for Weiszfeld.
#'
#' @param coords  data.frame with columns \code{x}, \code{y}, \code{demand}
#' @return        Named numeric vector c(x = ..., y = ...)
#' @examples
#' sites <- data.frame(x = c(0,-13,5,-18,8), y = c(0,3,-25,-5,-5),
#'                     demand = c(800,450,600,350,500))
#' center_of_gravity(sites)
center_of_gravity <- function(coords) {
  stopifnot(all(c("x", "y", "demand") %in% names(coords)))
  if (any(coords$demand < 0)) stop("Demand values must be non-negative.")
  total_demand <- sum(coords$demand)
  if (total_demand == 0) stop("Total demand must be positive.")
  c(
    x = sum(coords$demand * coords$x) / total_demand,
    y = sum(coords$demand * coords$y) / total_demand
  )
}


#' Weiszfeld Algorithm for the Euclidean Steiner-Weber Problem
#'
#' Minimises the sum of weighted Euclidean distances (not squared) by
#' iteratively re-weighted least-squares updates.
#'
#' @param coords    data.frame with columns \code{x}, \code{y}, \code{demand}
#' @param max_iter  Maximum number of iterations (default 100)
#' @param tol       Convergence tolerance — stop when step length < tol (default 1e-6)
#' @param start     Optional starting point c(x, y). Defaults to CoG.
#' @return          List with fields:
#'   \describe{
#'     \item{x}{Optimal x-coordinate}
#'     \item{y}{Optimal y-coordinate}
#'     \item{iterations}{Number of iterations performed}
#'     \item{history}{data.frame of (iter, x, y, twd) per iteration}
#'     \item{converged}{Logical — did the algorithm converge?}
#'   }
#' @examples
#' sites <- data.frame(x = c(0,-13,5,-18,8), y = c(0,3,-25,-5,-5),
#'                     demand = c(800,450,600,350,500))
#' weiszfeld(sites)
weiszfeld <- function(coords, max_iter = 100, tol = 1e-6, start = NULL) {
  stopifnot(all(c("x", "y", "demand") %in% names(coords)))

  # Internal helper: total weighted Euclidean distance
  twd <- function(fx, fy) sum(coords$demand * sqrt((coords$x - fx)^2 +
                                                      (coords$y - fy)^2))

  # Starting point
  if (is.null(start)) {
    cog   <- center_of_gravity(coords)
    x <- cog["x"]; y <- cog["y"]
  } else {
    x <- start[1]; y <- start[2]
  }

  history   <- data.frame(iter = 0L, x = x, y = y, twd = twd(x, y))
  converged <- FALSE

  for (k in seq_len(max_iter)) {
    # Euclidean distances from current estimate
    d <- sqrt((coords$x - x)^2 + (coords$y - y)^2)

    # Guard against coincidence with a demand site (avoids division by zero)
    d[d < 1e-10] <- 1e-10

    w_d   <- coords$demand / d
    x_new <- sum(w_d * coords$x) / sum(w_d)
    y_new <- sum(w_d * coords$y) / sum(w_d)

    history <- rbind(history,
                     data.frame(iter = k, x = x_new, y = y_new,
                                twd  = twd(x_new, y_new)))

    step_len <- sqrt((x_new - x)^2 + (y_new - y)^2)
    x <- x_new; y <- y_new

    if (step_len < tol) {
      converged <- TRUE
      break
    }
  }

  list(x = x, y = y, iterations = nrow(history) - 1L,
       history = history, converged = converged)
}


#' Haversine Distance Between Two Geographic Points
#'
#' Returns the great-circle distance in kilometres.
#'
#' @param lat1  Latitude  of point 1 (decimal degrees)
#' @param lon1  Longitude of point 1 (decimal degrees)
#' @param lat2  Latitude  of point 2 (decimal degrees)
#' @param lon2  Longitude of point 2 (decimal degrees)
#' @return      Distance in km (scalar or vector)
#' @examples
#' haversine(51.5, 0, 48.8, 2.35)  # London to Paris ≈ 341 km
haversine <- function(lat1, lon1, lat2, lon2) {
  R     <- 6371.0                  # Earth's mean radius (km)
  phi1  <- lat1 * pi / 180
  phi2  <- lat2 * pi / 180
  dphi  <- (lat2 - lat1) * pi / 180
  dlam  <- (lon2 - lon1) * pi / 180
  a     <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  R * 2 * atan2(sqrt(a), sqrt(1 - a))
}


#' AHP Priority Vector via Eigenvector Method
#'
#' Computes the normalised principal eigenvector of a pairwise comparison
#' matrix — the standard AHP priority derivation.
#'
#' @param mat  n×n pairwise comparison matrix (must be positive, square).
#'             Entry [i,j] = "how much more important is criterion i than j?"
#' @return     Normalised priority vector (sums to 1)
#' @examples
#' mat <- matrix(c(1, 3, 5,
#'                 1/3, 1, 2,
#'                 1/5, 1/2, 1), nrow = 3, byrow = TRUE)
#' ahp_priority_vector(mat)
ahp_priority_vector <- function(mat) {
  if (!is.matrix(mat) || nrow(mat) != ncol(mat))
    stop("'mat' must be a square matrix.")
  if (any(mat <= 0))
    stop("All entries must be strictly positive.")

  n      <- nrow(mat)
  # Geometric mean method as a stable approximation to the eigenvector method
  geo_means <- apply(mat, 1, function(row) prod(row)^(1/n))
  weights   <- geo_means / sum(geo_means)
  names(weights) <- rownames(mat)
  weights
}


#' AHP Consistency Check
#'
#' Computes lambda_max, CI, and CR for a pairwise comparison matrix.
#'
#' @param mat      n×n pairwise comparison matrix
#' @param weights  Priority vector from \code{ahp_priority_vector()} — if NULL,
#'                 computed internally.
#' @return         List with lambda_max, CI (Consistency Index), and CR
#'                 (Consistency Ratio). CR < 0.10 is acceptable.
#' @examples
#' mat <- matrix(c(1, 3, 5, 1/3, 1, 2, 1/5, 1/2, 1), 3, byrow = TRUE)
#' ahp_consistency(mat)
ahp_consistency <- function(mat, weights = NULL) {
  # Random Index table (Saaty, n = 1..10)
  RI <- c(0, 0, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49)

  if (is.null(weights)) weights <- ahp_priority_vector(mat)
  n <- nrow(mat)

  # Weighted sum vector
  Aw        <- as.numeric(mat %*% weights)
  lambda_max <- mean(Aw / weights)
  CI        <- (lambda_max - n) / (n - 1)
  ri        <- if (n <= 10) RI[n] else 1.49
  CR        <- CI / ri

  list(lambda_max = lambda_max, CI = CI, CR = CR,
       consistent = CR < 0.10)
}


# ─── 2. TRANSPORTATION PROBLEM ────────────────────────────────────────────────

#' Vogel's Approximation Method (VAM) for the Transportation Problem
#'
#' Finds an initial feasible allocation for the classical transportation
#' problem. VAM typically yields solutions within 0–5% of optimum.
#'
#' The problem may be unbalanced: if sum(supply) != sum(demand), a dummy
#' source or destination with zero cost is added automatically.
#'
#' @param cost_mat  m×n cost matrix (supply nodes as rows, demand nodes as cols)
#' @param supply    Numeric vector of supply quantities (length m)
#' @param demand    Numeric vector of demand quantities (length n)
#' @param verbose   Print iteration details (default FALSE)
#' @return          List with:
#'   \describe{
#'     \item{allocation}{m×n allocation matrix}
#'     \item{total_cost}{Total transportation cost}
#'     \item{balanced}{Was the problem balanced? (logical)}
#'   }
#' @examples
#' cost <- matrix(c(2,3,1,7, 5,2,4,3, 4,6,3,5), 3, byrow = TRUE)
#' vogels_approximation(cost, supply = c(120,80,100), demand = c(70,90,60,80))
vogels_approximation <- function(cost_mat, supply, demand, verbose = FALSE) {
  # ── Balance the problem ──────────────────────────────────────────────────
  balanced <- TRUE
  total_s  <- sum(supply); total_d <- sum(demand)
  if (total_s > total_d) {
    # Excess supply → add dummy destination with zero cost
    cost_mat <- cbind(cost_mat, Dummy = 0)
    demand   <- c(demand, Dummy = total_s - total_d)
    balanced <- FALSE
  } else if (total_d > total_s) {
    # Excess demand → add dummy source with zero cost
    cost_mat <- rbind(cost_mat, Dummy = 0)
    supply   <- c(supply, Dummy = total_d - total_s)
    balanced <- FALSE
  }

  m <- nrow(cost_mat); n <- ncol(cost_mat)
  alloc       <- matrix(0, m, n,
                        dimnames = list(rownames(cost_mat), colnames(cost_mat)))
  sup         <- supply
  dem         <- demand
  active_rows <- seq_len(m)
  active_cols <- seq_len(n)
  iter        <- 0L

  while (length(active_rows) > 0 && length(active_cols) > 0) {
    iter <- iter + 1L

    # Penalty = difference between two smallest costs (0 if only one active)
    row_pen <- sapply(active_rows, function(i) {
      v <- sort(cost_mat[i, active_cols]); if (length(v) >= 2) v[2] - v[1] else 0
    })
    col_pen <- sapply(active_cols, function(j) {
      v <- sort(cost_mat[active_rows, j]); if (length(v) >= 2) v[2] - v[1] else 0
    })

    if (verbose) {
      cat(sprintf("\n[VAM iter %d]\n", iter))
      cat("Row penalties:", setNames(row_pen, rownames(cost_mat)[active_rows]), "\n")
      cat("Col penalties:", setNames(col_pen, colnames(cost_mat)[active_cols]), "\n")
    }

    if (max(row_pen, na.rm = TRUE) >= max(col_pen, na.rm = TRUE)) {
      sel_r_idx <- which.max(row_pen)
      i <- active_rows[sel_r_idx]
      j <- active_cols[which.min(cost_mat[i, active_cols])]
    } else {
      sel_c_idx <- which.max(col_pen)
      j <- active_cols[sel_c_idx]
      i <- active_rows[which.min(cost_mat[active_rows, j])]
    }

    qty        <- min(sup[i], dem[j])
    alloc[i,j] <- alloc[i,j] + qty
    sup[i]     <- sup[i] - qty
    dem[j]     <- dem[j] - qty

    if (verbose) cat(sprintf("  Allocate %g: [%s] → [%s]\n",
                             qty, rownames(cost_mat)[i], colnames(cost_mat)[j]))

    if (sup[i] <= 0) active_rows <- setdiff(active_rows, i)
    if (dem[j] <= 0) active_cols <- setdiff(active_cols, j)
  }

  list(
    allocation = alloc,
    total_cost = sum(alloc * cost_mat),
    balanced   = balanced
  )
}


# ─── 3. INVENTORY MODELS ──────────────────────────────────────────────────────

#' Newsvendor Model with Normal Demand
#'
#' Solves the single-period newsvendor problem assuming normally distributed demand.
#'
#' @param mu    Mean demand
#' @param sigma Standard deviation of demand (must be > 0)
#' @param p     Selling price per unit
#' @param c     Purchase / production cost per unit
#' @param s     Salvage value per unit (must satisfy s < c < p)
#' @return      Named list with CR, Q_star, expected profit, and service level.
#' @examples
#' newsvendor_normal(mu = 500, sigma = 100, p = 200, c = 120, s = 60)
newsvendor_normal <- function(mu, sigma, p, c, s) {
  if (sigma <= 0)  stop("sigma must be positive.")
  if (!(s < c && c < p)) stop("Must satisfy s < c < p.")

  cu     <- p - c          # underage cost (margin lost per unit of stockout)
  co     <- c - s          # overage cost  (loss per unsold unit)
  cr     <- cu / (cu + co) # critical ratio = (p-c)/(p-s)

  z_star <- qnorm(cr)
  q_star <- mu + sigma * z_star

  # Loss function: L(z) = E[max(Z-z,0)] for standard normal Z
  L <- function(z) dnorm(z) - z * (1 - pnorm(z))

  z_q          <- (q_star - mu) / sigma
  exp_leftover <- sigma * L(-z_q)          # E[max(Q-D,0)]
  exp_stockout <- sigma * L(z_q)           # E[max(D-Q,0)]
  exp_sales    <- mu - exp_stockout

  exp_revenue  <- p * exp_sales + s * exp_leftover
  exp_cost     <- c * q_star
  exp_profit   <- exp_revenue - exp_cost

  list(
    mu = mu, sigma = sigma, p = p, c = c, s = s,
    cu = cu, co = co, cr = cr,
    z_star       = z_star,
    q_star       = q_star,
    exp_sales    = exp_sales,
    exp_leftover = exp_leftover,
    exp_stockout = exp_stockout,
    exp_revenue  = exp_revenue,
    exp_profit   = exp_profit,
    service_level = pnorm(z_star)
  )
}


#' EOQ — Economic Order Quantity
#'
#' Classic Wilson formula for the economic order quantity.
#'
#' @param K  Fixed ordering (setup) cost per order (€)
#' @param D  Demand rate (units per time period, same period as h)
#' @param h  Holding cost per unit per time period (€)
#' @return   Named list with Q_star (EOQ), cycle time T_star, and total cost TC.
#' @examples
#' eoq(K = 200, D = 1000, h = 5)
eoq <- function(K, D, h) {
  if (any(c(K, D, h) <= 0)) stop("K, D, h must all be strictly positive.")
  q_star <- sqrt(2 * K * D / h)
  t_star <- q_star / D           # cycle length (same time unit as D)
  tc     <- sqrt(2 * K * D * h) # total annual cost (holding + ordering)
  list(
    K = K, D = D, h = h,
    q_star = q_star,
    t_star = t_star,
    orders_per_period = D / q_star,
    total_cost = tc
  )
}


#' Inventory Pooling — Safety Stock Comparison
#'
#' Compares the safety stock required under (a) independent ordering at
#' n decentralised locations vs. (b) a single centralised pool.
#' Assumes independent, identically distributed demand at each location.
#'
#' @param n      Number of locations
#' @param sigma  Standard deviation of demand at each individual location
#'               (same period as lead time)
#' @param z      Service level quantile (e.g. \code{qnorm(0.95)} for 95\%)
#' @return       Named list with:
#'   \describe{
#'     \item{ss_individual}{Safety stock per location (individual policy)}
#'     \item{ss_total_individual}{Total safety stock across all n locations}
#'     \item{ss_central}{Safety stock at the central warehouse}
#'     \item{savings}{Absolute reduction in safety stock units}
#'     \item{savings_pct}{Percentage reduction}
#'     \item{pooling_factor}{sqrt(n) — the theoretical pooling factor}
#'   }
#' @examples
#' pooling_safety_stock(n = 10, sigma = 40, z = qnorm(0.95))
pooling_safety_stock <- function(n, sigma, z) {
  if (n < 1 || sigma <= 0 || z < 0)
    stop("n >= 1, sigma > 0, and z >= 0 required.")

  ss_individual       <- z * sigma
  ss_total_individual <- n * ss_individual
  sigma_central       <- sqrt(n) * sigma    # combined std under independence
  ss_central          <- z * sigma_central

  savings     <- ss_total_individual - ss_central
  savings_pct <- savings / ss_total_individual * 100

  list(
    n                   = n,
    sigma               = sigma,
    z                   = z,
    ss_individual       = ss_individual,
    ss_total_individual = ss_total_individual,
    sigma_central       = sigma_central,
    ss_central          = ss_central,
    savings             = savings,
    savings_pct         = savings_pct,
    pooling_factor      = sqrt(n)
  )
}


# ─── 4. WAREHOUSE LOCATION PROBLEM ────────────────────────────────────────────

#' Internal Helper — Total Cost for a Given Set of Open Warehouses
#'
#' Each customer is served by the cheapest open warehouse.
#'
#' @param open_wh           Integer indices of open warehouses
#' @param fixed_costs       Named vector of annual fixed costs per warehouse
#' @param transport_cost_mat (warehouses × customers) transport cost per unit
#' @param demand            Customer demand vector
#' @return                  List with fixed, transport, and total cost, plus assignment
.wlp_cost <- function(open_wh, fixed_costs, transport_cost_mat, demand) {
  if (length(open_wh) == 0)
    return(list(fixed = 0, transport = Inf, total = Inf, assignment = integer(0)))

  sub         <- transport_cost_mat[open_wh, , drop = FALSE]
  min_tc      <- apply(sub, 2, min)
  assignment  <- apply(sub, 2, which.min)   # index within open_wh
  fc          <- sum(fixed_costs[open_wh])
  tc          <- sum(min_tc * demand)
  list(fixed = fc, transport = tc, total = fc + tc, assignment = open_wh[assignment])
}


#' Add Heuristic for the Warehouse Location Problem (WLP)
#'
#' Starting from no open warehouses, iteratively opens the warehouse that
#' achieves the greatest reduction in total cost. Stops when no further
#' improvement is possible.
#'
#' @param fixed_costs         Named numeric vector of fixed costs per warehouse
#' @param transport_cost_mat  (warehouses × customers) matrix of per-unit transport costs
#' @param demand              Numeric vector of customer demands
#' @param verbose             Print iteration details (default TRUE)
#' @return                    List with open_warehouses (indices), fixed_cost,
#'                            transport_cost, total_cost, and assignment.
#' @examples
#' fc  <- c(5000,7000,5000,6000,4000)
#' tc  <- matrix(c(2,3,1,5,4,6,8, 3,2,4,3,5,7,1, 1,4,2,6,3,5,9,
#'                 5,3,7,2,6,4,3, 4,1,3,4,2,8,5), 5, byrow=TRUE)
#' d   <- c(100,80,120,60,90,70,110)
#' add_heuristic_wlp(fc, tc, d)
add_heuristic_wlp <- function(fixed_costs, transport_cost_mat, demand,
                               verbose = TRUE) {
  nWH      <- length(fixed_costs)
  open     <- integer(0)
  all_wh   <- seq_len(nWH)
  wh_names <- if (!is.null(names(fixed_costs))) names(fixed_costs) else paste0("WH", all_wh)
  iter     <- 0L

  repeat {
    iter       <- iter + 1L
    candidates <- setdiff(all_wh, open)
    if (length(candidates) == 0) break

    # Current cost (Inf if no warehouse open yet)
    cost_cur <- if (length(open) > 0)
      .wlp_cost(open, fixed_costs, transport_cost_mat, demand)$total
    else Inf

    # Resulting total cost for each candidate addition
    cand_cost <- sapply(candidates, function(wh) {
      .wlp_cost(c(open, wh), fixed_costs, transport_cost_mat, demand)$total
    })
    names(cand_cost) <- wh_names[candidates]
    savings <- cost_cur - cand_cost

    # Auswahl über die resultierenden Gesamtkosten (robust auch wenn
    # cost_cur = Inf ist: sonst sind im ersten Schritt alle savings
    # gleich "Inf" und which.max() waere nur Zufall der Reihenfolge)
    best_idx    <- which.min(cand_cost)
    best_saving <- savings[best_idx]
    if (best_saving <= 0 && length(open) > 0) break   # no improvement

    best_wh <- candidates[best_idx]
    open    <- c(open, best_wh)

    if (verbose) {
      res <- .wlp_cost(open, fixed_costs, transport_cost_mat, demand)
      cat(sprintf("[Add iter %d] Open %s | Total cost: %.0f\n",
                  iter, wh_names[best_wh], res$total))
    }
  }

  res <- .wlp_cost(open, fixed_costs, transport_cost_mat, demand)
  list(
    open_warehouses = open,
    open_names      = wh_names[open],
    fixed_cost      = res$fixed,
    transport_cost  = res$transport,
    total_cost      = res$total,
    assignment      = res$assignment
  )
}


#' Drop Heuristic for the Warehouse Location Problem (WLP)
#'
#' Starting from all warehouses open, iteratively closes the warehouse whose
#' removal yields the smallest cost increase (or the largest decrease). Stops
#' when closing any remaining warehouse would increase total cost.
#'
#' @param fixed_costs         Named numeric vector of fixed costs per warehouse
#' @param transport_cost_mat  (warehouses × customers) matrix of per-unit transport costs
#' @param demand              Numeric vector of customer demands
#' @param verbose             Print iteration details (default TRUE)
#' @return                    Same structure as \code{add_heuristic_wlp}
#' @examples
#' fc  <- c(5000,7000,5000,6000,4000)
#' tc  <- matrix(c(2,3,1,5,4,6,8, 3,2,4,3,5,7,1, 1,4,2,6,3,5,9,
#'                 5,3,7,2,6,4,3, 4,1,3,4,2,8,5), 5, byrow=TRUE)
#' d   <- c(100,80,120,60,90,70,110)
#' drop_heuristic_wlp(fc, tc, d)
drop_heuristic_wlp <- function(fixed_costs, transport_cost_mat, demand,
                                verbose = TRUE) {
  nWH      <- length(fixed_costs)
  open     <- seq_len(nWH)                   # start with all open
  wh_names <- if (!is.null(names(fixed_costs))) names(fixed_costs) else paste0("WH", seq_len(nWH))
  iter     <- 0L

  repeat {
    if (length(open) <= 1) break
    iter     <- iter + 1L
    cost_cur <- .wlp_cost(open, fixed_costs, transport_cost_mat, demand)$total

    # Cost after dropping each open warehouse
    costs_after_drop <- sapply(open, function(wh) {
      remaining <- setdiff(open, wh)
      if (length(remaining) == 0) return(Inf)
      .wlp_cost(remaining, fixed_costs, transport_cost_mat, demand)$total
    })
    names(costs_after_drop) <- wh_names[open]

    best_drop_cost <- min(costs_after_drop)
    if (best_drop_cost >= cost_cur) break    # no improvement from dropping

    wh_to_drop <- open[which.min(costs_after_drop)]
    open       <- setdiff(open, wh_to_drop)

    if (verbose) {
      cat(sprintf("[Drop iter %d] Close %s | New total cost: %.0f\n",
                  iter, wh_names[wh_to_drop], best_drop_cost))
    }
  }

  res <- .wlp_cost(open, fixed_costs, transport_cost_mat, demand)
  list(
    open_warehouses = open,
    open_names      = wh_names[open],
    fixed_cost      = res$fixed,
    transport_cost  = res$transport,
    total_cost      = res$total,
    assignment      = res$assignment
  )
}


# ─── UTILITY ──────────────────────────────────────────────────────────────────

#' DuPont ROI Decomposition
#'
#' @param revenue       Total revenue
#' @param mat_cost      Material costs
#' @param pers_cost     Personnel costs
#' @param fixed_assets  Net fixed assets
#' @param cash          Cash and equivalents
#' @param inventory     Inventory
#' @param receivables   Trade receivables
#' @return Named list with all DuPont components (profit, ROS, asset turnover, ROI)
#' @examples
#' dupont_roi(11000, 5500, 4950, 2000, 500, 3300, 1075)
dupont_roi <- function(revenue, mat_cost, pers_cost,
                       fixed_assets, cash, inventory, receivables) {
  profit         <- revenue - mat_cost - pers_cost
  total_assets   <- fixed_assets + cash + inventory + receivables
  ros            <- profit / revenue
  asset_turnover <- revenue / total_assets
  roi            <- ros * asset_turnover

  list(
    revenue        = revenue,
    mat_cost       = mat_cost,
    pers_cost      = pers_cost,
    profit         = profit,
    total_assets   = total_assets,
    fixed_assets   = fixed_assets,
    cash           = cash,
    inventory      = inventory,
    receivables    = receivables,
    ros            = ros,
    asset_turnover = asset_turnover,
    roi            = roi
  )
}


# ─── 5. TRANSPORTPLANUNG: ERÖFFNUNGSVERFAHREN ──────────────────────────────────

#' Nordwestecken-Regel
#' @param supply,demand Benannte Vektoren Angebot und Nachfrage.
#' @return Allokationsmatrix.
nw_corner <- function(supply, demand) {
  a <- matrix(0, length(supply), length(demand),
              dimnames = list(names(supply), names(demand)))
  i <- 1; j <- 1
  while (i <= length(supply) && j <= length(demand)) {
    q <- min(supply[i], demand[j])
    a[i, j] <- q
    supply[i] <- supply[i] - q
    demand[j] <- demand[j] - q
    if (supply[i] == 0) i <- i + 1 else j <- j + 1
  }
  a
}

#' Vogel'sche Approximationsmethode mit Protokoll aller Iterationen
#' (gleiche Logik wie vogels_approximation(), zusätzlich steps für Tableaus)
#' @return Liste mit steps, allocation und total_cost.
vam_trace <- function(cost, supply, demand) {
  m <- nrow(cost); n <- ncol(cost)
  alloc <- matrix(0, m, n, dimnames = dimnames(cost))
  sup <- supply; dem <- demand
  ar <- seq_len(m); ac <- seq_len(n)
  pen <- function(v) { v <- sort(v); if (length(v) >= 2) v[2] - v[1] else 0 }
  steps <- list()
  while (length(ar) > 0 && length(ac) > 0) {
    rp <- sapply(ar, function(i) pen(cost[i, ac]))
    cp <- sapply(ac, function(j) pen(cost[ar, j]))
    if (max(rp) >= max(cp)) {
      typ <- "Zeile"; i <- ar[which.max(rp)]
      j <- ac[which.min(cost[i, ac])]; pmax_val <- max(rp)
    } else {
      typ <- "Spalte"; j <- ac[which.max(cp)]
      i <- ar[which.min(cost[ar, j])]; pmax_val <- max(cp)
    }
    q <- min(sup[i], dem[j])
    steps[[length(steps) + 1]] <- list(
      ar = ar, ac = ac, sup = sup, dem = dem,
      rp = setNames(rp, rownames(cost)[ar]),
      cp = setNames(cp, colnames(cost)[ac]),
      alloc_before = alloc, typ = typ, i = i, j = j, q = q, pmax = pmax_val
    )
    alloc[i, j] <- alloc[i, j] + q
    sup[i] <- sup[i] - q; dem[j] <- dem[j] - q
    if (sup[i] <= 0) ar <- setdiff(ar, i)
    if (dem[j] <= 0) ac <- setdiff(ac, j)
  }
  list(steps = steps, allocation = alloc, total_cost = sum(alloc * cost))
}


# ─── 6. KÜRZESTE WEGE ──────────────────────────────────────────────────────────

#' Dijkstra-Algorithmus: kürzeste Entfernungen ab einem Startknoten
#' @param W Gewichtsmatrix mit Knotennamen (0 = keine Kante).
#' @param start Startknoten.
#' @return Benannter Vektor der kürzesten Entfernungen.
dijkstra <- function(W, start) {
  ids <- rownames(W)
  d <- setNames(rep(Inf, nrow(W)), ids)
  perm <- setNames(rep(FALSE, nrow(W)), ids)
  d[start] <- 0
  while (!all(perm)) {
    v <- names(which.min(ifelse(perm, Inf, d)))
    perm[v] <- TRUE
    for (w in ids[W[v, ] > 0 & !perm]) d[w] <- min(d[w], d[v] + W[v, w])
  }
  d
}

#' Dijkstra mit Protokoll (Marken, Vorgänger, Markenkorrekturen je Iteration)
#' @return Liste mit steps, d und pred.
dijkstra_trace <- function(W, start) {
  n <- nrow(W); ids <- rownames(W)
  d <- setNames(rep(Inf, n), ids); pred <- setNames(rep(NA_character_, n), ids)
  perm <- setNames(rep(FALSE, n), ids)
  d[start] <- 0
  steps <- list()
  while (!all(perm)) {
    cand <- ifelse(perm, Inf, d)
    u <- names(which.min(cand))
    perm[u] <- TRUE
    updates <- character(0); old <- c()
    for (v in ids[W[u, ] > 0 & !perm]) {
      if (d[u] + W[u, v] < d[v]) {
        old[v] <- d[v]
        d[v] <- d[u] + W[u, v]; pred[v] <- u
        updates <- c(updates, v)
      }
    }
    steps[[length(steps) + 1]] <- list(u = u, d = d, pred = pred,
                                       perm = perm, updates = updates,
                                       old = old)
  }
  list(steps = steps, d = d, pred = pred)
}

#' Knotenfolge des kürzesten Wegs aus dem Vorgängervektor
#' @param pred Vorgänger je Knoten (aus dijkstra_trace()).
#' @param ziel Zielknoten.
pfad <- function(pred, ziel) {
  p <- ziel
  while (!is.na(pred[p[1]])) p <- c(pred[p[1]], p)
  unname(p)
}


# ─── 7. TOURENPLANUNG ──────────────────────────────────────────────────────────

#' Länge einer Rundtour (Rückkehr zum ersten Knoten wird mitgezählt)
#' @param tour Knotenfolge (Namen oder Indizes).
#' @param D Distanzmatrix.
tour_length <- function(tour, D) {
  sum(D[cbind(tour, c(tour[-1], tour[1]))])
}

#' Länge einer Tour ab und bis Depot (Depot = Index 1, nicht in t enthalten)
depot_tour_length <- function(t, D) { tt <- c(1, t, 1); sum(D[cbind(head(tt, -1), tt[-1])]) }

#' Nearest-Neighbor-Heuristik
#' @param D Distanzmatrix.
#' @param start Startknoten (Index).
#' @return Knotennamen der Tour bzw. Indizes, falls D keine Zeilennamen hat.
nearest_neighbor <- function(D, start = 1) {
  tour <- start
  while (length(tour) < nrow(D)) {
    offen <- setdiff(seq_len(nrow(D)), tour)
    letzte <- tail(tour, 1)
    tour <- c(tour, offen[which.min(D[letzte, offen])])
  }
  if (is.null(rownames(D))) tour else rownames(D)[tour]
}

#' 2-opt-Verbesserungsverfahren (bester Zug je Iteration)
#' @param tour Ausgangstour.
#' @param D Distanzmatrix.
#' @param tol Mindestverbesserung je Zug.
#' @return Liste mit verbesserter tour und log der ausgeführten Züge.
two_opt <- function(tour, D, tol = 1e-9) {
  n <- length(tour); log <- list()
  repeat {
    best <- 0
    for (i in 1:(n - 1)) for (k in (i + 1):n) {
      a <- tour[i]; b <- tour[i + 1]
      c <- tour[k]; d <- tour[k %% n + 1]
      if (b == c || a == d) next          # benachbarte Kanten
      delta <- D[a, b] + D[c, d] - D[a, c] - D[b, d]
      if (delta > best + tol) { best <- delta; bi <- i; bk <- k }
    }
    if (best <= 0) break                  # lokales Optimum
    log[[length(log) + 1]] <- list(tour = tour, i = bi, k = bk, gain = best)
    tour <- c(tour[1:bi], rev(tour[(bi + 1):bk]),
              if (bk < n) tour[(bk + 1):n])
  }
  list(tour = tour, log = log)
}

#' Alle Permutationen eines Vektors (rekursiv; nur für kleine n geeignet)
perms <- function(v) if (length(v) <= 1) list(v) else
  do.call(base::c, lapply(seq_along(v), function(i)
    lapply(perms(v[-i]), function(p) c(v[i], p))))

#' Savings-Verfahren nach Clarke & Wright (parallele Variante, Kapazität)
#' @param D Distanzmatrix mit Knotennamen.
#' @param q Benannte Bedarfe der Kunden.
#' @param cap Fahrzeugkapazität.
#' @param depot Name des Depots.
#' @return Liste mit touren und Savings-Liste.
savings_cw <- function(D, q, cap, depot = "R") {
  kunden <- names(q)
  touren <- as.list(kunden)                       # Start: Pendeltouren
  s <- subset(expand.grid(i = kunden, j = kunden, stringsAsFactors = FALSE),
              i < j)
  s$s <- D[depot, s$i] + D[depot, s$j] - D[cbind(s$i, s$j)]
  s <- s[order(-s$s), ]
  for (r in seq_len(nrow(s))) {
    i <- s$i[r]; j <- s$j[r]
    ti <- which(sapply(touren, function(t) i %in% t))
    tj <- which(sapply(touren, function(t) j %in% t))
    if (ti == tj) next
    A <- touren[[ti]]; B <- touren[[tj]]
    if (sum(q[c(A, B)]) > cap) next
    if (tail(A, 1) == i && B[1] == j) new <- c(A, B)
    else if (A[1] == i && tail(B, 1) == j) new <- c(B, A)
    else if (tail(A, 1) == i && tail(B, 1) == j) new <- c(A, rev(B))
    else if (A[1] == i && B[1] == j) new <- c(rev(A), B)
    else next                                     # i oder j innen → nicht verknüpfbar
    touren[[ti]] <- new; touren <- touren[-tj]
  }
  list(touren = touren, savings = s)
}

#' Savings-Verfahren mit Kapazitäts- und Dauerrestriktion (Indexform, Depot = 1)
#' @param D Distanzmatrix (km).
#' @param q Bedarfe der Kunden 2..n.
#' @param cap Kapazität je Tour.
#' @param t_max Maximale Tourdauer (h).
#' @param v Geschwindigkeit (km/h).
#' @param t_stop,t_load Stopp- und Ladezeit (h).
#' @return Liste von Touren (Knotenindizes).
savings_vrp <- function(D, q, cap, t_max, v = v_kmh, t_stop = t_stopp,
                        t_load = t_lade) {
  k <- length(q); touren <- as.list(2:(k + 1))
  dauer <- function(t) {
    tt <- c(1, t, 1)
    sum(D[cbind(head(tt, -1), tt[-1])]) / v + length(t) * t_stop + t_load
  }
  s <- which(upper.tri(D[-1, -1]), arr.ind = TRUE)
  s <- data.frame(i = s[, 1] + 1, j = s[, 2] + 1)
  s$s <- D[1, s$i] + D[1, s$j] - D[cbind(s$i, s$j)]
  s <- s[order(-s$s), ]
  for (r in seq_len(nrow(s))) {
    i <- s$i[r]; j <- s$j[r]
    ti <- which(sapply(touren, function(t) i %in% t))
    tj <- which(sapply(touren, function(t) j %in% t))
    if (ti == tj) next
    A <- touren[[ti]]; B <- touren[[tj]]
    if (sum(q[c(A, B) - 1]) > cap) next
    if (tail(A, 1) == i && B[1] == j) neu <- c(A, B)
    else if (A[1] == i && tail(B, 1) == j) neu <- c(B, A)
    else if (tail(A, 1) == i && tail(B, 1) == j) neu <- c(A, rev(B))
    else if (A[1] == i && B[1] == j) neu <- c(rev(A), B)
    else next
    if (dauer(neu) > t_max) next
    touren[[ti]] <- neu; touren <- touren[-tj]
  }
  touren
}


# ─── 8. LAGER UND KOMMISSIONIERUNG ─────────────────────────────────────────────

#' Wegdistanz zwischen zwei Positionen im Ein-Block-Lager
#' @param p,q Positionen (x = Gangmitte, y = Tiefe).
#' @param y_max Lage des hinteren Quergangs.
dist_lager <- function(p, q, y_max = Y) {
  if (p[1] == q[1]) return(abs(p[2] - q[2]))
  abs(p[1] - q[1]) + min(p[2] + q[2], 2 * y_max - p[2] - q[2])
}

#' Weglänge eines rechtwinkligen Wegpunktzugs (Matrix x, y)
weg_laenge <- function(wp) sum(abs(diff(wp[, 1])) + abs(diff(wp[, 2])))

#' Routingstrategie S-Shape: Gänge mit Picks vollständig durchlaufen
#' @param pk Picks mit Spalten gang und y.
#' @param gx x-Position der Gänge.
#' @param y_max Hinterer Quergang.
#' @return Wegpunkte ab/bis Depot (0, 0).
route_sshape <- function(pk, gx = gang_x, y_max = Y) {
  g <- sort(unique(pk$gang)); m <- length(g)
  wp <- list(c(0, 0)); vorne <- TRUE
  for (k in seq_len(m)) {
    x <- gx[g[k]]; ys <- pk$y[pk$gang == g[k]]
    if (k == m && m %% 2 == 1) {
      wp <- c(wp, list(c(x, 0), c(x, max(ys)), c(x, 0)))
    } else if (vorne) {
      wp <- c(wp, list(c(x, 0), c(x, y_max))); vorne <- FALSE
    } else {
      wp <- c(wp, list(c(x, y_max), c(x, 0))); vorne <- TRUE
    }
  }
  do.call(rbind, c(wp, list(c(0, 0))))
}

#' Routingstrategie Return: jeden Gang von vorne bis zum hintersten Pick
route_return <- function(pk, gx = gang_x) {
  g <- sort(unique(pk$gang)); wp <- list(c(0, 0))
  for (j in g) {
    x <- gx[j]
    wp <- c(wp, list(c(x, 0), c(x, max(pk$y[pk$gang == j])), c(x, 0)))
  }
  do.call(rbind, c(wp, list(c(0, 0))))
}

#' Routingstrategie Largest Gap: größte Lücke je Gang nicht laufen
route_lgap <- function(pk, gx = gang_x, y_max = Y) {
  g <- sort(unique(pk$gang)); m <- length(g)
  if (m == 1) return(route_return(pk, gx))
  wp <- list(c(0, 0), c(gx[g[1]], 0), c(gx[g[1]], y_max))
  vorne_max <- c()
  mitte <- if (m > 2) g[2:(m - 1)] else integer(0)
  for (j in mitte) {
    pts <- c(0, sort(pk$y[pk$gang == j]), y_max); k <- which.max(diff(pts))
    x <- gx[j]; wp <- c(wp, list(c(x, y_max)))
    if (pts[k + 1] < y_max) wp <- c(wp, list(c(x, pts[k + 1]), c(x, y_max)))
    vorne_max[as.character(j)] <- pts[k]
  }
  xl <- gx[g[m]]; wp <- c(wp, list(c(xl, y_max), c(xl, 0)))
  for (j in rev(mitte)) {
    x <- gx[j]; fm <- vorne_max[as.character(j)]
    wp <- c(wp, list(c(x, 0)))
    if (fm > 0) wp <- c(wp, list(c(x, fm), c(x, 0)))
  }
  do.call(rbind, c(wp, list(c(0, 0))))
}

#' Distanzmatrix Depot + Pickpositionen
#' @return Liste mit Koordinaten P und Distanzmatrix D.
dist_matrix <- function(pk, y_max = Y) {
  P <- rbind(c(0, 0), as.matrix(pk[, c("x", "y")])); n <- nrow(P)
  D <- matrix(0, n, n)
  for (i in 1:n) for (j in 1:n) D[i, j] <- dist_lager(P[i, ], P[j, ], y_max)
  list(P = P, D = D)
}

#' Wegpunkte einer TSP-Tour im Lager (Gangwechsel über den kürzeren Quergang)
tour_wp <- function(t, P, y_max = Y) {
  tt <- c(t, t[1]); wp <- list(P[tt[1], ])
  for (k in 2:length(tt)) {
    p <- P[tt[k - 1], ]; q <- P[tt[k], ]
    if (p[1] != q[1]) {
      yy <- if (p[2] + q[2] <= 2 * y_max - p[2] - q[2]) 0 else y_max
      wp <- c(wp, list(c(p[1], yy), c(q[1], yy)))
    }
    wp <- c(wp, list(q))
  }
  do.call(rbind, wp)
}

#' Klassenbasierte Lagerplatzvergabe (ABC-Zonen, zufällig innerhalb der Zone)
#' @param fach_reihenfolge Fächer von gut nach schlecht erreichbar.
#' @param kl ABC-Klasse je Artikel.
#' @return Fachnummer je Artikel.
zuordnung_klasse <- function(fach_reihenfolge, kl = klasse) {
  out <- integer(length(kl))
  for (k in c("A", "B", "C")) {
    idx <- which(kl == k)
    out[idx] <- sample(fach_reihenfolge[idx])
  }
  out
}


# ─── 9. TRANSPORTKOSTEN ────────────────────────────────────────────────────────

#' Fahrzeugkostenrechnung: fixe und variable Kosten, Kostensätze
#' @param p Liste mit A, RW, N, i, versicherung, steuer, sonst_fix,
#'   energie_km, wartung_km, reifen_km, maut_km.
#' @param km_jahr Jahresfahrleistung.
fahrzeugkosten <- function(p, km_jahr) {
  afa    <- (p$A - p$RW) / p$N                 # lineare Abschreibung
  zins   <- (p$A + p$RW) / 2 * p$i             # kalk. Zinsen auf Ø Kapital
  fix    <- afa + zins + p$versicherung + p$steuer + p$sonst_fix
  var_km <- p$energie_km + p$wartung_km + p$reifen_km + p$maut_km
  list(afa = afa, zins = zins, fix = fix, var_km = var_km,
       fix_km = fix / km_jahr, gesamt_km = fix / km_jahr + var_km,
       gesamt_jahr = fix + var_km * km_jahr)
}

#' Hauptlauf vom Depot zu einem Zielknoten (Pendelfahrten mit vollen Lkw)
#' @param ziel,von Koordinaten (lon, lat).
#' @param boxen Menge.
#' @param kap Boxen je Lkw.
#' @param fix,kkm,kh Kostensätze (€/Tag, €/km, €/h).
#' @param v,t_load Geschwindigkeit, Ladezeit.
hauptlauf <- function(ziel, boxen, von = depot, umweg_f = umweg, kap = 900,
                      fix = fix_tag, kkm = k_km, kh = k_h, v = v_kmh,
                      t_load = t_lade) {
  d <- umweg_f * haversine(von["lat"], von["lon"], ziel["lat"], ziel["lon"])
  fahrten <- ceiling(boxen / kap)           # kap Boxen je Lkw (Einheit Tourenplanung)
  km <- 2 * d * fahrten; h <- km / v + fahrten * 2 * t_load
  list(d = unname(d), fahrten = fahrten, km = unname(km),
       kosten = unname(fahrten * fix + kkm * km + kh * h))
}
