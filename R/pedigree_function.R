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
  founderPop  <-  AlphaSimR::quickHaplo(nInd=n_nr_founder, nChr=1, segSites=10)

  # Set simulation parameters
  SP  <-  AlphaSimR::SimParam$new(founderPop)
  SP$setSexes(ps_set_sex)

  # Create founder population
  pop  <-  AlphaSimR::newPop(founderPop, simParam=SP)

  # F1 population with founder parents
  pop2 <- AlphaSimR::randCross(pop, n_nr_cross_f1, simParam=SP)

  # loop over a number of subsequenct generations
  for (idx in 1:n_nr_gen){
    pop2 <- c(pop2, AlphaSimR::randCross(pop2, vec_nr_cross[idx], simParam=SP))
  }

  # put pedigree information into a tibble
  return(tibble::tibble(ID = pop2@id, Sex = pop2@sex, Sire = pop2@father, Dam = pop2@mother))

}
