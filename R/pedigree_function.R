#' ---
#' title: Pedigree Creation Functions
#' date:  2020-12-16
#' ---
#'
#'
#' @title Create Pedigree Using AlphaSimR
#'
#' @description
#' Pedigree creation with AlphaSimR is wrapped into this function
#'
#' @param pn_nr_founder number of founder animals (animals without parents)
#' @param pn_nr_cross_f1 nuber of animals in f1 (offspring of founders)
#' @param pn_nr_gen number of generation to simulate
#' @param pvec_nr_cross the number of crosses in each generation
#' @param ps_set_sex how sex should be set in AlphaSimR
#' @return pedigree as tibble
#'
#' @example
#' \dontrun{
#' create_pedigree(pn_nr_founder = 4,
#'                 pn_nr_cross_f1 = 2,
#'                 pn_nr_gen      = 3,
#'                 pvec_nr_cross  = c(2, 1, 1)
#' }
#'
#' @export create_pedigree
create_pedigree <- function(pn_nr_founder,
                            pn_nr_cross_f1,
                            pn_nr_gen,
                            pvec_nr_cross,
                            ps_set_sex = 'yes_sys'){

    # Founder Population Haplotypes
  founderPop  <-  AlphaSimR::quickHaplo(nInd=pn_nr_founder, nChr=1, segSites=10)

  # Set simulation parameters
  SP  <-  AlphaSimR::SimParam$new(founderPop)
  SP$setSexes(ps_set_sex)

  # Create founder population
  pop  <-  AlphaSimR::newPop(founderPop, simParam=SP)

  # F1 population with founder parents
  pop2 <- AlphaSimR::randCross(pop, pn_nr_cross_f1, simParam=SP)

  # loop over a number of subsequenct generations
  for (idx in 1:pn_nr_gen){
    pop2 <- c(pop2, AlphaSimR::randCross(pop2, pvec_nr_cross[idx], simParam=SP))
  }

  # put pedigree information into a tibble
  return(tibble::tibble(ID = pop2@id, Sex = pop2@sex, Sire = pop2@father, Dam = pop2@mother))

}

## -------------------------------------------------------------------------- ##

#'
#' @title Extend Pedigree
#'
#' @description
#' A given pedigree is extended with its founder animals.
#'
#' @details
#' The founder animals are determined by taking the set-difference between
#' the vector of parents and the vector of animal IDs
#'
#' @param ptbl_ped input pedigree to be extended by founders
#' @return pedigree extended by founder animals
#'
#' @export extend_pedigree
extend_pedigree <- function(ptbl_ped){
  vec_founder_sire <- setdiff(ptbl_ped$Sire, ptbl_ped$ID)
  n_nr_founder_sire <- length(vec_founder_sire)
  vec_founder_dam <- setdiff(ptbl_ped$Dam, ptbl_ped$ID)
  n_nr_founder_dam <- length(vec_founder_dam)

  # check that founder_sire and founder_dam are not the same
  if (length(intersect(vec_founder_sire, vec_founder_dam)) != 0)
    stop(" * ERROR: Founder sires and founder dams are not exclusive")

  tbl_ped_ext <- ptbl_ped
  # extending pedigree with founder sires, if there are any
  if (n_nr_founder_sire > 0){
    tbl_ped_ext <- dplyr::bind_rows(
      tibble::tibble(ID = vec_founder_sire[order(vec_founder_sire)],
                     Sex = rep('M', n_nr_founder_sire),
                     Sire = rep(NA, n_nr_founder_sire),
                     Dam  = rep(NA, n_nr_founder_sire)),
      tbl_ped_ext)
  }
  if (n_nr_founder_dam > 0){
    tbl_ped_ext <- dplyr::bind_rows(
      tibble::tibble(ID = vec_founder_dam[order(vec_founder_dam)],
                     Sex = rep('F', n_nr_founder_dam),
                     Sire = rep(NA, n_nr_founder_dam),
                     Dam  = rep(NA, n_nr_founder_dam)),
      tbl_ped_ext)

  }
  # return result
  return(tbl_ped_ext)
}
