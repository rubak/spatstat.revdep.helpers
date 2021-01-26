## Functions assume a couple of global variables (fundf, DIR and DEPENDS at least)
download_from_cran <- function(pkg, download_dir = "downloads", src_dir = "sources"){
  dir.create(file.path(DIR, download_dir), showWarnings = FALSE)
  dir.create(file.path(DIR, src_dir), showWarnings = FALSE)
  dl <- download.packages(pkg, destdir = file.path(DIR, download_dir))
  untar(dl[1,2], exdir = file.path(DIR, src_dir))
  ### Before continuing I joined importFrom(spatstat, ...) in surveillance 
  ### NAMESPACE in directory "sources" to a single line
  if(pkg == "surveillance"){
    surveil <- file.path(DIR, src_dir, "surveillance", "NAMESPACE")
    ns <- readLines(surveil)
    start <- grep("importFrom.*area.owin", ns)
    end <- grep("spatstat.options", ns)
    if(end>start){
      writeLines(head(ns, n = start -1), surveil)
      cat(trimws(ns[start:end]), file = surveil, append = TRUE)
      cat("", tail(ns, n = -end), file = surveil, append = TRUE, sep = "\n")
    }
  }
}

check_pkg <- function(pkg, tar_dir, check_dir){
  dir.create(check_dir, showWarnings = FALSE)
  Sys.setenv("_R_CHECK_FORCE_SUGGESTS_"="false")
  tarfiles <- list.files(tar_dir, full.names = TRUE)
  index <- grep(paste0(pkg, "_"), tarfiles)
  pkgcheck <- rcmdcheck::rcmdcheck(
    tarfiles[index], args = "--no-manual",  check_dir = check_dir)
  saveRDS(pkgcheck, file = file.path(check_dir, paste0(pkg, ".Rdata")))
}

checksummary <- function(checkdir = "checks_original"){
  files <- list.files(file.path(DIR, checkdir), pattern = ".Rdata")
  for(f in files){
    pkgcheck <- readRDS(file.path(DIR, checkdir, f))
    cat(substr(f, 0, nchar(f)-6), ": ", sep = "")
    rcmdcheck:::print.rcmdcheck_summary(summary(pkgcheck), line = FALSE)
  }
}

findfun <- function(x){
  # Find spatstat:: and split on it. Split the string after spatstat:: on:
  # - `(` for usual function call
  # - `)` or `,` for when called with lapply and friends
  # - space for a special case where `plot <- spatstat::plot.owin`
  # - end of line ($) for a special case by end of line `spatstat::Kest`
  xx <- regmatches(x, regexpr("spatstat::.*", x))
  words <- strsplit(xx, "spatstat::")[[1]]
  words <- words[nchar(words)>0]
  words <- words[grepl("\\(|,|\\)|}| |$", words, fixed = FALSE)]
  if(length(words)==0L){
    return(words)
  }
  words <- sapply(strsplit(words, "\\(|,|\\)|}| |$", fixed = FALSE), function(y) y[1])
  # gsub("`", "", words, fixed = TRUE)
}

fix_lengths_psp <- function(pkg){
  files <- system(paste0('grep -rl "lengths.psp" ', DIR, '/updates/', pkg), intern = TRUE)
  for(i in seq_along(files)){
    code <- readLines(files[i])
    code <- gsub("lengths.psp", "lengths_psp", code, fixed = TRUE) ## Fix lenghts.psp no longer exported
    writeLines(code, files[i])
  }
}

fixdoublecolon <- function(pkg){
  ### USES GLOBAL VARIABLE fundf
  files <- system(paste0('grep -lr "spatstat::" ', DIR, '/updates/', pkg), intern = TRUE)
  print(paste(pkg, ": found", length(files), "files"))
  for(i in seq_along(files)){
    code <- readLines(files[i])
    index <- which(grepl("spatstat::", code, fixed = TRUE))
    for(j in index){
      line <- code[j]
      fun <- findfun(line)
      for(k in seq_along(fun)){
        tmp <- fundf[fundf$fun == fun[k],]
        if(nrow(tmp)>0){
          line <- gsub(tmp$old, tmp$new, line, fixed = TRUE)
        }
      }
      code[j] <- line
    }
    writeLines(code, files[i])
  }
}

fix_s3_methods <- function(pkg){
  ### USES GLOBAL VARIABLE fundf
  files <- system(paste0('grep -l "register_s3_method.*spatstat" ', DIR, '/updates/', pkg, "/R/*.R"), intern = TRUE)
  print(paste(pkg, ": found", length(files), "files"))
  splitstring <- r'(register_s3_method("spatstat", ")'
  for(i in seq_along(files)){
    code <- readLines(files[i])
    index <- which(grepl("register_s3_method.*spatstat", code))
    for(j in index){
      line <- code[j]
      parts <- strsplit(line, splitstring, fixed = TRUE)
      fun <- lapply(parts, function(x) x[2])
      fun <- sapply(fun, function(x) strsplit(x, '\"', fixed = TRUE)[[1]][1])
      for(k in seq_along(fun)){
        tmp <- fundf[fundf$fun == fun[k],]
        if(nrow(tmp)>0){
          line <- gsub("spatstat", tmp$pkg, line, fixed = TRUE)
        }
      }
      code[j] <- line
    }
    writeLines(code, files[i])
  }
}

split_one_importFrom <- function(x){
  ### USES GLOBAL VARIABLE fundf
  xx <- strsplit(x, ",")[[1]]
  imp <- xx[1]
  fun <- xx[-1]
  lastfun <- tail(fun, 1)
  fun[length(fun)] <- strsplit(lastfun, ")", fixed = TRUE)[[1]][1]
  fun_trim <- trimws(gsub('"', "", fun))
  paste0("importFrom(", fundf$pkg[match(fun_trim, fundf$fun)], ", ", fun, ")")
}

split_importFrom <- function(pkg){
  f <- file.path(DIR, 'updates', pkg, 'NAMESPACE')
  ns <- readLines(f)
  # files <- system(paste0('grep -l "importFrom.*spatstat" ', DIR, '/updates/', pkg, '/NAMESPACE'), intern = TRUE)
  # print(paste("found", length(files), "files"))
  # for(i in seq_along(files)){
    # print(files[i])
    # ns <- readLines(files[i])
  if(any(grepl("importFrom.*spatstat", ns))){
    print(f)
    # ns <- readLines(files[i])
    ns <- gsub("lengths.psp", "lengths_psp", ns, fixed = TRUE) ## Fix lenghts.psp no longer exported
    index <- which(grepl("spatstat", ns, fixed = TRUE) & 
                     !grepl("spatstat.utils", ns, fixed = TRUE) & 
                     !grepl("spatstat.data", ns, fixed = TRUE) &
                     !grepl("export(", ns, fixed = TRUE) &
                     !grepl("import(spatstat)", ns, fixed = TRUE))
    # writeLines(ns[-index], files[i])
    if(length(index)>0){
      writeLines(ns[-index], f)
    }
    for(j in index){
      imports <- split_one_importFrom(ns[j])
      # cat(imports, file = files[i], append = TRUE, sep = "\n")
      cat(imports, file = f, append = TRUE, sep = "\n")
    }
  } 
}


fix_namespace <- function(pkg){
  ### USES GLOBAL VARIABLE `DEPENDS`
  dir <- file.path(DIR, "updates")
  filename <- file.path(dir, pkg, "NAMESPACE")
  ns <- parseNamespaceFile(pkg, dir, mustExist = TRUE)
  index <- which(sapply(ns$imports, function(x) x[[1]]) == "spatstat")
  if(length(index)>0 && any(sapply(ns$imports[index], length))==1){
    lines <- readLines(filename)
    ii <- which(grepl("spatstat", lines) & !grepl("importFrom", lines) & !grepl("useDynLib", lines))
    spst <- gsub(" ", "", lines[ii])
    print(paste0(filename, ": line ", ii, ": ", spst))
    for(j in seq_along(spst)){
      if(grepl('"spatstat"', spst[j])){
        if(pkg %in% DEPENDS){
          spst[j] <- sub('"spatstat"', '"spatstat.geom", "spatstat.core", "spatstat.linnet", "spatstat"', spst[j])
        } else{
          spst[j] <- sub('"spatstat"', '"spatstat.geom", "spatstat.core", "spatstat.linnet"', spst[j])
        }
      } else{
        if(pkg %in% DEPENDS){
          spst[j] <- gsub("spatstat,", "spatstat.geom,spatstat.core,spatstat.linnet,spatstat,", spst[j], fixed = TRUE)
          spst[j] <- gsub(",spatstat)", ",spatstat.geom,spatstat.core,spatstat.linnet,spatstat)", spst[j], fixed = TRUE)
          spst[j] <- gsub("(spatstat)", "(spatstat.geom,spatstat.core,spatstat.linnet,spatstat)", spst[j], fixed = TRUE)
        } else{
          spst[j] <- gsub("spatstat,", "spatstat.geom,spatstat.core,spatstat.linnet,", spst[j], fixed = TRUE)
          spst[j] <- gsub(",spatstat)", ",spatstat.geom,spatstat.core,spatstat.linnet)", spst[j], fixed = TRUE)
          spst[j] <- gsub("(spatstat)", "(spatstat.geom,spatstat.core,spatstat.linnet)", spst[j], fixed = TRUE)
        }
      }
    }
    lines[ii] <- spst
    writeLines(lines, filename)
  }
}

fix_roxygen <- function(pkg){
  ### USES GLOBAL VARIABLE `DEPENDS`
  files <- system(paste0('grep -l "import.*spatstat" ', DIR, '/updates/', pkg, "/R/*.R"), intern = TRUE)
  for(i in seq_along(files)){
    lines <- readLines(files[i])
    ## Substitutions to avoid confusion
    lines <- gsub("spatstat.utils", "SPST_UTILS", lines)
    lines <- gsub("spatstat.data", "SPST_DATA", lines)
    ## Pure imports first:
    import <- grep("@import spatstat", lines, fixed = TRUE)
    if(length(import)>0){
      if(pkg %in% DEPENDS){
        lines[import] <- gsub("@import spatstat", "@import spatstat.geom spatstat.core spatstat.linnet spatstat", lines[import])
      } else{
        lines[import] <- gsub("@import spatstat", "@import spatstat.geom spatstat.core spatstat.linnet", lines[import])
      }
    }
    ## Now importFrom:
    importFrom <- grep("@importFrom spatstat ", lines, fixed = TRUE)
    if(length(importFrom)>0){
      if(max(importFrom)==length(lines) | min(importFrom)==1){
        stop("Edge case probably doesn't occur -- well it did if the error appears.")
      }
      start <- c(1, importFrom+1)
      end <- c(importFrom-1, length(lines))
      newlines <- lines[start[1]:end[1]]
      for(j in seq_along(importFrom)){
        funlist <- strsplit(lines[importFrom[j]], "@importFrom spatstat ", fixed = TRUE)[[1]][2]
        funs <- strsplit(funlist, " ", fixed = TRUE)[[1]]
        df <- fundf[fundf$fun %in% funs,]
        splitdf <- split(df, df$pkg)
        for(k in seq_along(splitdf)){
          string <- paste("#' @importFrom", splitdf[[k]]$pkg[1], paste0(splitdf[[k]]$fun, collapse = " "))
          newlines <- c(newlines, string)
        }
        if(start[j+1]<end[j+1]){
          newlines <- c(newlines, lines[start[j+1]:end[j+1]])
        }
      }
      ## Backsubstitutions
      lines <- newlines
      lines <- gsub("SPST_UTILS", "spatstat.utils", lines)
      lines <- gsub("SPST_DATA", "spatstat.data", lines)
      writeLines(lines, files[i])
    }
  }
}

fix_description <- function(pkg){
  ### USES GLOBAL VARIABLE `DEPENDS`
  f <- file.path(DIR, "updates", pkg, "DESCRIPTION")
  desc <- readLines(f)
  if(pkg == "GmAMisc"){
    desc[14] <- paste(desc[14:15], collapse = "")
    desc <- desc[-15]
  }
  desc <- gsub("spatstat[ ]*\\([>]*[<]*[=]*[ ]*[0-9\\.-]*\\)", "spatstat", desc ) # Remove version requirement
  if(pkg=="surveillance"){ # Weird encoding makes some strings `invalid in this locale``
    desc[56] <- paste(desc[56], "spatstat.core, spatstat.geom")
    desc <- desc[-57]
  } else{
    desc <- gsub("spatstat.utils", "SPST_UTILS", desc, fixed = TRUE)
    desc <- gsub("spatstat.data", "SPST_DATA", desc, fixed = TRUE)
    desc <- gsub("'spatstat'", "SPST_QUOTE", desc, fixed = TRUE)
    desc <- gsub("'spatstat.R'", "SPST.R_QUOTE", desc, fixed = TRUE)
    j <- which(grepl("spatstat", desc) & !grepl("Package|Title|Description|BugReports", desc))
    if(pkg %in% DEPENDS){
      desc[j] <- gsub("spatstat", "spatstat.geom, spatstat.core, spatstat.linnet, spatstat", desc[j], fixed = TRUE)
    } else{
      geom <- system(paste0('grep -lFr "spatstat.geom" ', DIR, '/updates/', pkg), intern = TRUE)
      core <- system(paste0('grep -lFr "spatstat.core" ', DIR, '/updates/', pkg), intern = TRUE)
      linnet <- system(paste0('grep -lFr "spatstat.linnet" ', DIR, '/updates/', pkg), intern = TRUE)
      if(length(geom)>0) geom <- "spatstat.geom"
      if(length(core)>0) core <- "spatstat.core"
      if(length(linnet)>0) linnet <- "spatstat.linnet"
      if(pkg=="sf") linnet <- "spatstat.linnet"
      spst_string <- paste(c(geom, core, linnet), collapse = ", ")
      desc[j] <- gsub("spatstat", spst_string, desc[j], fixed = TRUE)
    }
    desc <- gsub("SPST_UTILS", "spatstat.utils", desc)
    desc <- gsub("SPST_DATA", "spatstat.data", desc)
    desc <- gsub("SPST.R_QUOTE", "'spatstat.R'", desc, fixed = TRUE)
    desc <- gsub("SPST_QUOTE", "'spatstat'", desc, fixed = TRUE)
    if(pkg=="stars"){
      desc <- gsub("spatstat.geom, spatstat.core, spatstat.linnet", "spatstat.geom", desc, fixed = TRUE)
    }
  }
  writeLines(desc, f)
  if(pkg=="sf"){
    cat("Remotes: spatstat/spatstat.utils, spatstat/spatstat.geom, spatstat/spatstat.core, baddstats/spatstat.linnet", 
        file = f, append = TRUE)
  }
}

fix_pkg_loading <- function(pkg){
  files1 <- system(paste0('grep -lr "library(.*spatstat" ', DIR, '/updates/', pkg), intern = TRUE)
  files2 <- system(paste0('grep -lr "require(.*spatstat" ', DIR, '/updates/', pkg), intern = TRUE)
  files3 <- system(paste0('grep -lr "requireNamespace(.*spatstat" ', DIR, '/updates/', pkg), intern = TRUE)
  files <- union(union(files1, files2), files3)
  print(paste("found", length(files), "files"))
  for(i in seq_along(files)){
    print(files[i])
    code <- readLines(files[i])
    code <- gsub('library(spatstat)', 'library(spatstat.core)', code, fixed = TRUE)
    code <- gsub('library("spatstat")', 'library("spatstat.core")', code, fixed = TRUE)
    code <- gsub('require(spatstat)', 'require(spatstat.core)', code, fixed = TRUE)
    code <- gsub('require("spatstat")', 'require("spatstat.core")', code, fixed = TRUE)
    code <- gsub('require(spatstat,', 'require(spatstat.core,', code, fixed = TRUE)
    code <- gsub('require("spatstat",', 'require("spatstat.core",', code, fixed = TRUE)
    code <- gsub('requireNamespace(spatstat)', 'requireNamespace(spatstat.core)', code, fixed = TRUE)
    code <- gsub('requireNamespace("spatstat")', 'requireNamespace("spatstat.core")', code, fixed = TRUE)
    code <- gsub('requireNamespace(spatstat,', 'requireNamespace(spatstat.core,', code, fixed = TRUE)
    code <- gsub('requireNamespace("spatstat",', 'requireNamespace("spatstat.core",', code, fixed = TRUE)
    if(pkg == "stars"){
      code <- gsub('library(spatstat.core)', 'library(spatstat.geom)', code, fixed = TRUE)
      code <- gsub('requireNamespace("spatstat.core",', 'requireNamespace("spatstat.geom",', code, fixed = TRUE)
    }
    if(pkg == "sf"){
      code <- gsub('require(spatstat.core', 'require(spatstat.linnet', code, fixed = TRUE)
      code <- gsub('requireNamespace("spatstat.core",', 'requireNamespace("spatstat.linnet",', code, fixed = TRUE)
    }
    if(pkg == "maptools"){
      code <- gsub('require(spatstat.core', 'require(spatstat.linnet', code, fixed = TRUE)
      code <- gsub('requireNamespace("spatstat.core",', 'requireNamespace("spatstat.linnet",', code, fixed = TRUE)
    }
    writeLines(code, files[i])
  } 
}

fix_std_link_line <- function(x){
  ### USES GLOBAL VARIABLE fundf
  link_item <- "link[spatstat]{"
  xx <- strsplit(x, link_item, fixed = TRUE)[[1]]
  fun <- sapply(strsplit(xx[-1], "}", fixed = TRUE), function(x) x[1])
  out <- xx[1]
  for(i in seq_along(fun)){
    pkg <- fundf$pkg[fundf$fun == fun[i]]
    if(length(pkg)>0){
      out <- paste0(out, gsub("spatstat", pkg, link_item, fixed = TRUE), xx[i+1])
    } else{
      stop("Function not found:", fun)
    }
  }
  return(out)
}

fix_colon_link_line <- function(x){
  ### USES GLOBAL VARIABLE fundf
  link_item <- "link[spatstat:"
  xx <- strsplit(x, link_item, fixed = TRUE)[[1]]
  fun <- sapply(strsplit(xx[-1], "]", fixed = TRUE), function(x) x[1])
  out <- xx[1]
  for(i in seq_along(fun)){
    pkg <- fundf$pkg[fundf$fun == fun[i]]
    if(length(pkg)>0){
      out <- paste0(out, gsub("spatstat", pkg, link_item, fixed = TRUE), xx[i+1])
    }
  }
  return(out)
}

fix_links <- function(pkg){
  files <- system(paste0('grep -lr "link\\[spatstat" ', DIR, '/updates/', pkg), intern = TRUE)
  print(paste(pkg, ": found", length(files), "files"))
  for(i in seq_along(files)){
    code <- readLines(files[i])
    std_index <- which(grepl("link[spatstat]", code, fixed = TRUE))
    for(j in std_index){
      code[j] <- fix_std_link_line(code[j])
    }
    colon_index <- which(grepl("link[spatstat:", code, fixed = TRUE))
    for(j in colon_index){
      code[j] <- fix_colon_link_line(code[j])
    }
    writeLines(code, files[i])
  }
}

spst_fun_df <- function(){
  library(spatstat.linnet)
  s.data <- ls("package:spatstat.data")
  s.core <- ls("package:spatstat.core")
  s.geom <- ls("package:spatstat.geom")
  s.linnet <- ls("package:spatstat.linnet")
  fundf <- rbind(
    data.frame(fun = "spatstat-package", pkg = "spatstat", old = "spatstat::spatstat-package", new = "spatstat::spatstat-package"),
    data.frame(fun = "spatstat", pkg = "spatstat", old = "spatstat::spatstat", new = "spatstat::spatstat"),
    data.frame(fun = "`marks<-`", pkg = "spatstat.geom:`marks<-`", old = "spatstat::`marks<-`", new = "spatstat.geom::`marks<-`"),
    data.frame(fun = "ppp.object", pkg = "spatstat.geom:ppp", old = "spatstat::ppp.object", new = "spatstat.geom::ppp.object"),
    data.frame(fun = "psp.object", pkg = "spatstat.geom:psp", old = "spatstat::psp.object", new = "spatstat.geom::psp.object"),
    data.frame(fun = "owin.object", pkg = "spatstat.geom", old = "spatstat::owin.object", new = "spatstat.geom::owin.object"),
    data.frame(fun = "fv.object", pkg = "spatstat.core", old = "spatstat::fv.object", new = "spatstat.geom::fv.object"),
    data.frame(fun = "im.object", pkg = "spatstat.geom", old = "spatstat::im.object", new = "spatstat.geom::im.object"),
    data.frame(fun = s.data, pkg = "spatstat.data", old = paste0("spatstat::", s.data), new = paste0("spatstat.data::", s.data)),
    data.frame(fun = s.core, pkg = "spatstat.core", old = paste0("spatstat::", s.core), new = paste0("spatstat.core::", s.core)),
    data.frame(fun = s.geom, pkg = "spatstat.geom", old = paste0("spatstat::", s.geom), new = paste0("spatstat.geom::", s.geom)),
    data.frame(fun = s.linnet, pkg = "spatstat.linnet", old = paste0("spatstat::", s.linnet), new = paste0("spatstat.linnet::", s.linnet)))
  return(fundf)
}

dep_update <- function(pkg, download = FALSE, check_original = FALSE, 
                       fix_src = FALSE, build = FALSE, check_updates = FALSE){
  if(download){download_from_cran(pkg)}
  if(check_original){check_pkg(pkg, tar_dir = "downloads", check_dir = "checks_original")}
  if(fix_src){
    system(paste("rm -rf", file.path(DIR, "updates", pkg)))
    system(paste("cp -R", file.path(DIR, "sources", pkg), file.path(DIR, "updates", pkg)))
    split_importFrom(pkg)
    fix_pkg_loading(pkg)
    fix_lengths_psp(pkg)
    fixdoublecolon(pkg)
    fix_namespace(pkg)
    fix_links(pkg)
    fix_s3_methods(pkg)
    fix_roxygen(pkg)
    fix_description(pkg)
  }
  if(build){
    buildDIR <- file.path(DIR, "builds")
    dir.create(buildDIR, showWarnings = FALSE)
    system(paste("cd", buildDIR, "; R CMD build", file.path(DIR, "updates", pkg)))
  }
  if(check_updates){check_pkg(pkg, tar_dir = "builds", check_dir = "checks_updates")}
  
}
