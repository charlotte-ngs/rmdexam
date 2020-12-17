#' ---
#' title: Create Rmarkdown Exam
#' date:  2020-12-15
#' ---
#'
#' @title Generate Rmd Solutions and Exams
#'
#' @description
#' Given a vector of problem names and a vector of student names, a series of
#' individualised solutions and exams are created. The output currently consists
#' of Rmarkdown source files and pdf-files for the exam questions and the solutions.
#' Each problem requires a Rmarkdown source file which is stored in a file named
#' the same way as the problem name. Hence the source of the problem 'nrm' must
#' be stored in a file called 'nrm.Rmd'. Any problem setup should be included in
#' an R-script called 'nrm.R' which contains a single function 'generate_problem_setup()'
#'
#' @details
#' Solutions to questions must be tagged with the begin-tag 'solstart' and the
#' end-tag 'solend'. These tags have to be defined in the yaml header or in a
#' preamble.tex file
#'
#' @param pvec_problem vector of problem names
#' @param pvec_names vector of student names
#' @param pn_nr_exam number of exams to be generated, if pvec_names is missing
#' @param ps_rmd_dir directory where rmd-sources for the problems are stored
#' @param ps_rsrc_dir directory where R-scripts for problem-setup are stored
#' @param ps_tex_dir directory containing additional tex files
#' @param ps_out_dir directory where output should be stored
#' @param pb_force flag to first delete old output directories
#'
#' @example
#' \dontrun{
#' exam2rmd(pvec_problem = c('nrm'), pvec_names = c('test_student'), pn_nr_exam = 1)
#' }
#'
#' @export exam2rmd
exam2rmd <- function(pvec_problem,
                     pvec_names  = NULL,
                     pn_nr_exam  = 1,
                     ps_rmd_dir  = 'rmd',
                     ps_rsrc_dir = 'R',
                     ps_tex_dir  = 'tex',
                     ps_out_dir   = 'out',
                     pb_force    = FALSE){
  # with pb_force, existing output is deleted
  if (pb_force && dir.exists(ps_out_dir)){
    cat(" * Removing existing output directory: ", ps_out_dir, "\n")
    unlink(ps_out_dir)
  }
  # check whether ps_out_dir exists
  if (!dir.exists(ps_out_dir)) {
    cat(" * Create directory: ", ps_out_dir, "\n")
    dir.create(path = ps_out_dir)
  }
  # get a time-stamp
  s_cur_ts <- format(Sys.time(), '%Y%m%d%H%M%S')
  # loop over number of exams to produce
  for (eidx in 1:pn_nr_exam){
    # create output directory for current exam
    if (!is.null(pvec_names) && eidx <= length(pvec_names)) {
      cur_outdir <- file.path(ps_out_dir, paste0('exam_', pvec_names[eidx]))
      cur_exam_rmd_file <- paste0(s_cur_ts, '_exam_', pvec_names[eidx], '.Rmd', collapse = '')
      cur_sol_rmd_file <- paste0(s_cur_ts, '_sol_', pvec_names[eidx], '.Rmd', collapse = '')
    } else {
      cur_outdir <- file.path(ps_out_dir, paste0('exam_', eidx))
      cur_exam_rmd_file <- paste0(s_cur_ts, '_exam_', eidx, '.Rmd', collapse = '')
      cur_sol_rmd_file <- paste0(s_cur_ts, '_sol_', eidx, '.Rmd', collapse = '')
    }
    if (!dir.exists(cur_outdir)){
      cat(" ** Current exam directory: ", cur_outdir, "\n")
      dir.create(cur_outdir)
    }
    # copy tex dir
    if (dir.exists(ps_tex_dir)) {
      cat(" ** Copy ", ps_tex_dir, " to: ", cur_outdir, "\n")
      fs::dir_copy(path = ps_tex_dir, new_path = cur_outdir)
    }
    # name of current exam-rmdfile
    cur_exam_rmd_path <- file.path(cur_outdir, cur_exam_rmd_file)
    cur_sol_rmd_path <- file.path(cur_outdir, cur_sol_rmd_file)
    # put together current exam-rmdfile
    s_index_rmd <- file.path(ps_rmd_dir, 'index.Rmd')
    if (file.exists(s_index_rmd)){
      con_index <- file(description = s_index_rmd)
      vec_index <- readLines(con = con_index)
      close(con_index)
    } else {
      cat (" ** Cannot find index file: ", s_index_rmd, "\n")
    }
    # write the index to solution-rmd file
    cat(" ** Writing index.rmd to: ", cur_sol_rmd_path, "\n")
    cat(paste0(vec_index, collapse = '\n'), '\n', file = cur_sol_rmd_path, append = FALSE)
    # write the index to the exam-rmd file
    vec_index_exam <- gsub('Solutions To ', '', vec_index, fixed = TRUE)
    cat(paste0(vec_index_exam, collapse = '\n'), '\n', file = cur_exam_rmd_path, append = FALSE)

    # add problems to solution and exam rmd files
    for (pidx in seq_along(pvec_problem)){
      # obtain name of current problem
      cur_problem <- pvec_problem[pidx]
      # write page break to exam rmd file
      cat("\n\\clearpage\n\\pagebreak\n\n", file = cur_sol_rmd_path, append = TRUE)
      cat("\n\\clearpage\n\\pagebreak\n\n", file = cur_exam_rmd_path, append = TRUE)

      # check wether there is an R-script for the current problem
      cur_rscript <- file.path(ps_rsrc_dir, paste0(cur_problem, '.R'))
      if (file.exists(cur_rscript)){
        cat(" *** Sourcing R-script: ", cur_rscript, "\n")
        source(cur_rscript)
        l_problem_setup <- generate_problem_setup()
        # write problem setup statement
        cat(" *** Writing problem setup to: ", cur_sol_rmd_path, "\n")
        cat("```{r problem setup, echo=FALSE, results='hide'}\n", file = cur_sol_rmd_path, append = TRUE)
        cat(l_problem_setup$rstmt, "\n", file = cur_sol_rmd_path, append = TRUE)
        cat("```\n\n", file = cur_sol_rmd_path, append = TRUE)
        cat(" *** Writing problem setup to: ", cur_exam_rmd_path, "\n")
        cat("```{r problem setup, echo=FALSE, results='hide'}\n", file = cur_exam_rmd_path, append = TRUE)
        cat(l_problem_setup$rstmt, "\n", file = cur_exam_rmd_path, append = TRUE)
        cat("```\n\n", file = cur_exam_rmd_path, append = TRUE)
      }
      # write problem rmd to solution and exam rmd
      cur_problem_rmd_path <- file.path(ps_rmd_dir, paste0(cur_problem, '.Rmd'))
      if (file.exists(cur_problem_rmd_path)){
        con_problem <- file(description = cur_problem_rmd_path)
        vec_problem <- readLines(con = con_problem)
        close(con = con_problem)
        cat(" *** Writing problem rmd: ", cur_problem_rmd_path, " to solution: ", cur_sol_rmd_path, "\n")
        cat(paste0(vec_problem, collapse = "\n"), "\n", file = cur_sol_rmd_path, append = TRUE)
        # write exam-version without solution part
        l_sol_pos <- list(start = which(vec_problem == '\\solstart'),
                          end   = which(vec_problem == '\\solend'))
        if (length(l_sol_pos$start) != length(l_sol_pos$end))
          stop(" * ERROR: not the same number of solution start positions and end positions")
        n_nr_section <- length(l_sol_pos$start)
        vec_exam <- vec_problem[1:l_sol_pos$start[1]]
        for (idx in 2:n_nr_section){
          vec_exam <- c(vec_exam, vec_problem[(l_sol_pos$end[idx-1]+1):l_sol_pos$start[idx]])
        }
        # remaining
        if ((l_sol_pos$start[n_nr_section]+1) < length(vec_problem)){
          vec_exam <- c(vec_exam, vec_problem[(l_sol_pos$end[n_nr_section]+1):length(vec_problem)])
        }
        cat(" *** Writing problem rmd: ", cur_problem_rmd_path, " to exam: ", cur_exam_rmd_path, "\n")
        cat(paste0(vec_exam, collapse = '\n'), '\n', file = cur_exam_rmd_path, append = TRUE)

      } else {
        cat(" *** Cannot find problem rmd: ", cur_problem_rmd_path, "\n")
      }

    }
    # render exam rmd
    cat(" ** Render solution rmd: ", cur_sol_rmd_path, '\n')
    rmarkdown::render(input = cur_sol_rmd_path)
    cat(" ** Render exam: ", cur_exam_rmd_path, '\n')
    rmarkdown::render(input = cur_exam_rmd_path)

  }
  return(invisible())

}

