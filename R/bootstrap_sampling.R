#' @title Bootstrap sample community masses from uniform
#'
#' @description Randomly draw N masses, where N is the total number of individuals in a community, from a uniform distribution with min and max corresponding the min and max of the entire community.
#'
#' @param nind N
#' @param min_mass Min mass
#' @param max_mass Max mass
#'
#' @return vector of masses for a community of N individuals
#'
#' @export

boostrap_unif_bsed <- function(nind,
                                  min_mass,
                                  max_mass)
{
  community_masses <- runif(n = nind, min = min_mass, max = max_mass)
  
  return(community_masses)
}

#' @title Bootstrap sample community masses from two communities
#'
#' @description Pool all individual masses from communities and randomly re-draw communities with the original number of individuals from the pool, with replacement.
#'
#' @param community_a vector of masses for first community
#' @param community_b vector of masses for second community
#'
#' @return list of 2 vectors: masses for first resample community, masses for second resample community. 
#'
#' @export

boostrap_crosscomm_bseds <- function(community_a,
                               community_b)
{

  nind_a = as.integer(length(community_a))
  nind_b = as.integer(length(community_b))

  pool = c(community_a, community_b)
    
  nind_tot = as.integer(length(pool))
  
  random_indices_a = sample.int(pool, size = nind_a, replace = T)
  random_indices_b = sample.int(pool, size = nind_b, replace = T)
  
  resampled_a = pool[random_indices_a]
  resampled_b = pool[random_indices_b]
  
  resampled_communities = list(resampled_a, resampled_b)
  
  return(resampled_communities)
}