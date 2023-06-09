new_algor <- function (p, 
                       data, 
                       f = maxnet.formula(p, data), 
                       regmult = 1, 
                       regfun = maxnet::maxnet.default.regularization, 
                       addsamplestobackground = T, 
                       ...) 
{
	# browser()
	if (anyNA(data)) 
		stop("NA values in data table. Please remove them and rerun.")
	
	### this takes ages to run!! 
	# if (addsamplestobackground) {
	# 	pdata <- data[p == 1, ]
	# 	ndata <- data[p == 0, ]
	# 	toadd <- apply(pdata, 1, function(rr) !any(apply(ndata,
	# 													 1, function(r) identical(r, rr))))
	# 	p <- c(p, rep(0, sum(toadd)))
	# 	data <- rbind(data, pdata[toadd, ])
	# }
	
	###########################################
	# massive increase in speed using setdiff
	if (addsamplestobackground) {
		pdata <- data[p == 1, ]
		ndata <- data[p == 0, ]
		toadd <- dplyr::setdiff(pdata, ndata)
		p <- c(p, rep(0, nrow(toadd)))
		data <- rbind(data, toadd)
	}
	
	mm <- model.matrix(f, data)
	reg <- regfun(p, mm) * regmult
	weights <- p + (1 - p) * 100
	glmnet::glmnet.control(pmin = 1e-08, fdev = 0)
	model <- glmnet::glmnet(x = mm, y = as.factor(p), family = "binomial", 
							standardize = F, penalty.factor = reg, lambda = 10^(seq(4, 
																					0, length.out = 200)) * sum(reg)/length(reg) * sum(p)/sum(weights), 
							weights = weights, ...)
	class(model) <- c("maxnet", class(model))
	if (length(model$lambda) < 200) {
		msg <- "Error: glmnet failed to complete regularization path.  Model may be infeasible."
		if (!addsamplestobackground) 
			msg <- paste(msg, " Try re-running with addsamplestobackground=T.")
		stop(msg)
	}
	bb <- model$beta[, 200]
	model$betas <- bb[bb != 0]
	model$alpha <- 0
	rr <- predict(model, data[p == 0, , drop = FALSE], 
				  type = "exponent", clamp = F)
	raw <- rr/sum(rr)
	model$entropy <- -sum(raw * log(raw))
	model$alpha <- -log(sum(rr))
	model$penalty.factor <- reg
	model$featuremins <- apply(mm, 2, min)
	model$featuremaxs <- apply(mm, 2, max)
	vv <- (sapply(data, class) != "factor")
	model$varmin <- apply(data[, vv, drop = FALSE], 2, min)
	model$varmax <- apply(data[, vv, drop = FALSE], 2, max)
	means <- apply(data[p == 1, vv, drop = FALSE], 2, mean)
	majorities <- sapply(names(data)[!vv], function(n) which.max(table(data[p == 
																				1, n, drop = FALSE])))
	names(majorities) <- names(data)[!vv]
	model$samplemeans <- unlist(c(means, majorities))
	model$levels <- lapply(data, levels)
	model
}


my_lookup.enm <- function (algorithm, logger){
	x <- switch(algorithm, maxent.jar = enm.maxent.jar, maxnet = enm.maxnet, 
				bioclim = enm.bioclim)
	logger %>% writeLog("Note: NOT adding samples to background\n")
	# here we just swap in the function with the code removed
	x@fun <- new_algor
	return(x)
	
	
}

my_model_maxent <- function (occs, bg, user.grp, bgMsk, rms, rmsStep, fcs, clampSel, 
		  algMaxent, catEnvs = NULL, parallel = FALSE, numCores = NULL, 
		  logger = NULL, spN = NULL) 
{
	if (is.null(user.grp)) {
		logger %>% writeLog(type = "error", "Before building a model, please partition occurrences for cross-validation.")
		return()
	}
	if (algMaxent == "maxent.jar") {
		jar <- paste(system.file(package = "dismo"), "/java/maxent.jar", 
					 sep = "")
		if (!file.exists(jar)) {
			logger %>% writeLog(type = "error", "To use Maxent, make sure you download, ", 
								strong("maxent.jar"), " from the ", a("AMNH Maxent webpage", 
																	  href = "http://biodiversityinformatics.amnh.org/open_source/maxent/", 
																	  target = "_blank"), " and place it in this directory:", 
								br(), em(jar))
			return()
		}
		if (!requireNamespace("rJava")) {
			logger %>% writeLog(type = "error", paste0("Package rJava cannot load. Please download the latest version of ", 
													   "Java, and make sure it is the correct version (e.g. 64-bit for a ", 
													   "64-bit system). After installing, try \"library(rJava)\". If it ", 
													   "loads properly, restart Wallace and try again. If it does not, ", 
													   "please consult www.github.com/wallaceecomod/wallace for more ", 
													   "tips on getting rJava to work."))
			return()
		}
		if (is.null(getOption("dismo_rJavaLoaded"))) {
			Sys.setenv(NOAWT = TRUE)
			if (requireNamespace("rJava")) {
				rJava::.jpackage("dismo")
				options(dismo_rJavaLoaded = TRUE)
			}
			else {
				stop("rJava cannot be loaded")
			}
		}
		mxe <- rJava::.jnew("meversion")
		maxentJARversion <- try(rJava::.jcall(mxe, "S", "meversion"))
		if (maxentJARversion < "3.4.3") {
			logger %>% writeLog(type = "error", "Please, use the updated version of Maxent (v3.4.4). Currently, you are ", 
								"using (", maxentJARversion, ").")
			return()
		}
		if (maxentJARversion == "3.4.3") {
			logger %>% writeLog("Please, consider to updated version of Maxent (v3.4.4). Currently, you are ", 
								"using (", maxentJARversion, ").")
		}
	}
	rms.interval <- seq(rms[1], rms[2], rmsStep)
	tune.args = list(fc = fcs, rm = rms.interval)
	if (!is.null(logger)) {
		progress <- shiny::Progress$new()
		progress$set(message = paste0("Building/Evaluating ENMs for ", 
									  spName(spN), "..."), value = 0)
		on.exit(progress$close())
		n <- length(rms.interval) * length(fcs)
		updateProgress <- function(value = NULL, detail = NULL) {
			progress$inc(amount = 1/n, detail = detail)
		}
	}
	else {
		n <- length(rms.interval) * length(fcs)
		updateProgress <- FALSE
	}
	occs.xy <- occs %>% dplyr::select(.data$longitude, .data$latitude)
	bg.xy <- bg %>% dplyr::select(.data$longitude, .data$latitude)
	# get the new algorithm with the code block commented out
	my_alg <- my_lookup.enm(algMaxent, logger)
	# now pass the new algorithm directly, via user.enm = my_alg
	e <- ENMeval::ENMevaluate(occs = as.data.frame(occs.xy), user.enm = my_alg,
							  bg = as.data.frame(bg.xy), partitions = "user", user.grp = user.grp, 
							  envs = bgMsk, tune.args = tune.args, doClamp = clampSel, 
							  algorithm = algMaxent, categoricals = catEnvs, parallel = parallel, 
							  numCores = numCores, parallelType = "doSNOW", updateProgress = updateProgress, 
							  quiet = FALSE)
	occPredVals <- raster::extract(e@predictions, occs.xy)
	endTxt <- paste("]), using", algMaxent, "with clamping", 
					ifelse(clampSel, "on.", "off."))
	logger %>% writeLog(hlSpp(spN), "Maxent ran successfully and output evaluation ", 
						"results for ", nrow(e@results), " models (Regularization multiplier values: [", 
						paste(rms.interval, collapse = ", "), "]; Feature classes: [", 
						paste(fcs, collapse = ", "), endTxt, "")
	return(e)
}

# env.breadth from ENMTools;
# slightly changed, instead of "response" set to "cloglog" and prevent plot generation
env.breadth.cloglog <- function (model, env = env_values, tolerance = 1e-04, max.reps = 10, chunk.size = 1e+05) {
  
  if (inherits(model, "enmtools.model")) {
    model <- model$model
  }
  if (inherits(env, c("raster", "RasterStack", "RasterBrick", 
                      "RasterLayer"))) {
    mins <- minValue(env)
    maxes <- maxValue(env)
  }
  else if (inherits(env, "list")) {
    mins <- unlist(lapply(env, min))
    maxes <- unlist(lapply(env, max))
  }
  continue <- FALSE
  n.reps <- 0
  while (continue == FALSE & n.reps < max.reps) {
    gens <- chunk.size
    this.lhs <- lhs::randomLHS(chunk.size, length(names(env)))
    predict.table <- t(t(this.lhs) * (maxes - mins) + mins)
    colnames(predict.table) <- names(env)
    if (inherits(model, "DistModel")) {
      pred <- as.numeric(predict(model, x = data.frame(predict.table), 
                                 type = "response"))
    }
    else {
      if (inherits(model, "ranger")) {
        pred <- as.numeric(predict(model, data = data.frame(predict.table), 
                                   type = "response")$predictions[, 2, drop = TRUE])
      }
      else {
        pred <- as.numeric(predict(model, newdata = data.frame(predict.table), 
                                   type = "cloglog"))
      }
    }
    if (max(pred) == 0) {
      this.B2 <- NA
    }
    else {
      this.B2 <- calc.B2(pred)
    }
    if (!is.na(this.B2)) {
      continue <- TRUE
    }
    else {
      n.reps <- n.reps + 1
    }
  }
  if (n.reps == max.reps) {
    warning("\n\nCould not find suitable starting conditions for environmental breadth, returning NA\n\n")
    return(list(env.B2 = NA, B2.plot = NA))
  }
  else {
    delta <- 1
    while (delta > tolerance) {
      this.lhs <- lhs::randomLHS(chunk.size, length(names(env)))
      predict.table <- t(t(this.lhs) * (maxes - mins) + 
                           mins)
      colnames(predict.table) <- names(env)
      if (inherits(model, "DistModel")) {
        pred <- c(pred, as.numeric(predict(model, x = data.frame(predict.table), 
                                           type = "response")))
      }
      else {
        if (inherits(model, "ranger")) {
          pred <- as.numeric(predict(model, data = data.frame(predict.table), 
                                     type = "response")$predictions[, 2, drop = TRUE])
        }
        else {
          pred <- as.numeric(predict(model, newdata = data.frame(predict.table), 
                                     type = "cloglog"))
        }
      }
      if (max(pred) == 0) {
        next
      }
      else {
        this.B2 <- c(this.B2, calc.B2(pred))
        gens <- c(gens, max(gens) + chunk.size)
      }
      delta <- abs(mean(this.B2) - mean(this.B2[-length(this.B2)]))
    }
  }
  output <- list(env.B2 = mean(this.B2)#, B2.plot = qplot(gens, this.B2, ylab = "B2", xlab = "Samples")
  )
  return(output)
}