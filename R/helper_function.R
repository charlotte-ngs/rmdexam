#' ---
#' title: Helper Functions For Exam Generation
#' date:  2020-12-15
#' ---
#'
#' @title Convert Numeric Vector to Defining String
#'
#' @description
#' Given a numeric vector, we want to return  the string
#' that defines the same vector using 'c()'
#'
#' @details
#' This is needed when producing R-statments for
#' exercises and exams
#'
#' @param pvec vector to be converted to definition string
#' @return s_result defining string for pvec
#+
numeric_vector_as_def_string <- function(pvec){
  return(paste0('c(', paste0(pvec, collapse = ','), ')', collapse = ''))
}

#'
#' @title Convert Character Vector to Defining String
#'
#' @description
#' Given a character vector, we want to return  the string
#' that defines the same vector using 'c()'
#'
#' @details
#' This is needed when producing R-statments for
#' exercises and exams
#'
#' @param pvec vector to be converted to definition string
#' @return s_result defining string for pvec
#+
character_vector_as_def_string <- function(pvec){
  return(paste0("c('", paste0(pvec, collapse = "','"), "')", collapse = ''))
}
