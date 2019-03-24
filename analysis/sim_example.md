Example on simulated data
================
Renata Diaz
3/21/2019

``` r
library(revisitbecs)
library(ggplot2)
```

Simulate a set of 9 "real" communities
======================================

To develop & test the pipeline, we will work with a set of simulated communities. We will randomly generate community data frames, in the form of records of individual "rodents" with species ID (1:nspecies) and mass (in g).

For our simulated communities, we will pick a mean mass for each species in the community. Then we will randomly assign a species ID to each individual in the community, and randomly give a mass to each individual by drawing from a normal distribution centered on that species' mean mass.

``` r
ncommunities = 9
set.seed(20)

real_communities <- list()

for(i in 1:ncommunities) {
  nspecies = sample(5:20, size = 1)
  nind = sample(50:250, size = 1)
  min_mass = sample(5:15, size= 1)
  max_mass = sample(40:250, size=1)
  
  real_communities[[i]] <- simulate_community(nspecies = nspecies,
                                              nind = nind,
                                              min_mass = min_mass,
                                              max_mass = max_mass)
}
```

Many parts of this analysis use the "raw" community data, which is what we would collect in the field or simulate above, to construct body size-energy distributions. For this we need to 1) estimate each individual's energy use (as *m*<sup>3/4</sup> where *m* is mass in g) and 2) assign each individual to a size class. We will divide the communities into size classes of .2 log units.

``` r
community_tables <- list()

for(i in 1:ncommunities){
  community_tables[[i]] <- make_community_table(sim_community = real_communities[[i]])
}
```

Construct BSEDs for those communities
=====================================

Now we can construct a body size-energy distribution for each community. This is a summary, for each size class, of how much energy all the individuals (regardless of species ID) in that size class use. We will work with these distributions standardized according to the total energy used by the whole community.

``` r
real_bseds <- list()

for(i in 1:ncommunities){
  real_bseds[[i]] <- make_bsed(community_tables[[i]], decimals = 1)
}
```

BSED plots
==========

![](sim_example_files/figure-markdown_github/plot%20real%20BSEDS-1.png)![](sim_example_files/figure-markdown_github/plot%20real%20BSEDS-2.png)![](sim_example_files/figure-markdown_github/plot%20real%20BSEDS-3.png)![](sim_example_files/figure-markdown_github/plot%20real%20BSEDS-4.png)![](sim_example_files/figure-markdown_github/plot%20real%20BSEDS-5.png)![](sim_example_files/figure-markdown_github/plot%20real%20BSEDS-6.png)![](sim_example_files/figure-markdown_github/plot%20real%20BSEDS-7.png)![](sim_example_files/figure-markdown_github/plot%20real%20BSEDS-8.png)![](sim_example_files/figure-markdown_github/plot%20real%20BSEDS-9.png)

Calculate and plot energetic dominance
======================================

``` r
dominance_values <- vector(mode = "numeric")

for(i in 1:ncommunities) {
  these_modes <- energetic_dominance(community_tables[[i]])
  
  these_modes <- these_modes %>%
    dplyr::select(mode_id, e_dominance) %>%
    dplyr::distinct()
  
  dominance_values <- c(dominance_values, these_modes$e_dominance)
}

anyNA(dominance_values)
```

    ## [1] FALSE

``` r
dominance_values <- as.data.frame(dominance_values)

e_dominance_plot <- ggplot(data = dominance_values) + 
  geom_histogram(binwidth = 0.1, aes(x = dominance_values)) +
  xlim(-0.1, 1.1) + 
  theme_bw()

e_dominance_plot
```

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](sim_example_files/figure-markdown_github/energetic%20dominance%20for%20real%20communities-1.png)

Compare each real BSED to 10000 bootstraps (DOI 95% interval)
=============================================================

``` r
nsamples = 100
for(i in 1:ncommunities){ 
  sampled_communities_doi <- replicate(nsamples, boostrap_unif_bsed_doi(real_communities[[i]]))
  
  real_doi <- doi(real_bseds[[i]]$total_energy_proportional)
  
  p_greater_doi <- length(which(sampled_communities_doi > real_doi)) / nsamples
  
  print(p_greater_doi)
  
}
```

Compare all pairwise communities BSEDs
======================================

``` r
nsamples = 100

all_pairs_matrix <- combn(1:ncommunities, m = 2)

p_comparison <- vector(length = ncol(all_pairs_matrix), mode = 'numeric')

for(i in 1:ncol(all_pairs_matrix)) {
  
  first = all_pairs_matrix[1, i]
    second = all_pairs_matrix[2, i]
sampled_pair_doi <- replicate(nsamples, boostrap_crosscomm_bseds(real_communities[[first]], real_communities[[second]]))


both_bseds <- real_bseds[[first]] %>%
  dplyr::full_join(real_bseds[[second]], by = c("size_class", "size_class_g")) %>%
  dplyr::mutate(total_energy_proportional.x = replace(total_energy_proportional.x, is.na(total_energy_proportional.x), 0),
                total_energy_proportional.y = replace(total_energy_proportional.y, is.na(total_energy_proportional.y), 0))

real_doi <- doi(both_bseds$total_energy_proportional.x,
                both_bseds$total_energy_proportional.y)

p_greater_doi <- length(which(sampled_pair_doi > real_doi)) / nsamples

p_comparison[i] <- p_greater_doi
}

p_comparison

length(which(p_comparison > 0.05)) / length(p_comparison)
```

Construct BSDs for real communities
===================================

``` r
real_bsds <- list()
for(i in 1:ncommunities) {
  real_bsds[[i]] <- make_bsd(community_tables[[i]], decimals = 2)
}
```

Plot them
=========

``` r
for(i in 1:ncommunities){ 
  this_bsd <- real_bsds[[i]]
  bsd_plot <- ggplot(data = this_bsd, aes(x = size_class, y =  n_species_proportional)) +
    geom_point(data = this_bsd, aes(x = as.factor(size_class_g), y= n_species_proportional)) + 
    theme_bw()
  print(bsd_plot)
  }
```

![](sim_example_files/figure-markdown_github/plot%20real%20BSDs-1.png)![](sim_example_files/figure-markdown_github/plot%20real%20BSDs-2.png)![](sim_example_files/figure-markdown_github/plot%20real%20BSDs-3.png)![](sim_example_files/figure-markdown_github/plot%20real%20BSDs-4.png)![](sim_example_files/figure-markdown_github/plot%20real%20BSDs-5.png)![](sim_example_files/figure-markdown_github/plot%20real%20BSDs-6.png)![](sim_example_files/figure-markdown_github/plot%20real%20BSDs-7.png)![](sim_example_files/figure-markdown_github/plot%20real%20BSDs-8.png)![](sim_example_files/figure-markdown_github/plot%20real%20BSDs-9.png)

Compare each real BSD to uniform (d-corrected KS)
=================================================

``` r
for (i in 1:ncommunities) {
  this_bsd <- real_bsds[[i]]
  this_ks <- ks.test(this_bsd$n_species_proportional, punif)
  print(this_ks$p.value)
}
```

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 3.353937e-07

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 8.686056e-06

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 0.002341759

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 0.001580981

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 0.001966263

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 6.893074e-05

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 2.683806e-05

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 0.1122497

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 0.1466813

Compare all pairwise communities BSDs (KS)
==========================================

``` r
all_pairs_matrix <- combn(1:ncommunities, m = 2)

ks_p_comparison <- vector(length = ncol(all_pairs_matrix), mode = 'numeric')

for(i in 1:ncol(all_pairs_matrix)) {
  
  first = all_pairs_matrix[1, i]
    second = all_pairs_matrix[2, i]

both_bsds <- real_bsds[[first]] %>%
  dplyr::full_join(real_bsds[[second]], by = c("size_class", "size_class_g")) %>%
  dplyr::mutate(n_species_proportional.x = replace(n_species_proportional.x, is.na(n_species_proportional.x), 0),
                n_species_proportional.y = replace(n_species_proportional.y, is.na(n_species_proportional.y), 0))

ks_comparison <- ks.test(both_bsds$n_species_proportional.x,
                both_bsds$n_species_proportional.y)
ks_p_comparison[i] <- ks_comparison$p.value
}
```

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

``` r
ks_p_comparison
```

    ##  [1] 0.33378427 0.24854822 0.56961321 0.66038602 0.56961321 0.99882209
    ##  [7] 0.04613904 0.04613904 0.46107176 0.87932440 0.51755084 0.09956185
    ## [13] 0.29142522 0.07580170 0.07580170 0.80792416 0.99335600 0.75909784
    ## [19] 0.75909784 0.96394524 0.98826108 0.51755084 0.84748845 0.75909784
    ## [25] 0.69937420 0.62716705 0.84748845 0.99789663 0.69937420 0.69937420
    ## [31] 0.46107176 0.12432316 0.20583624 0.16407920 0.20583624 0.93750270

``` r
length(which(ks_p_comparison < 0.05))
```

    ## [1] 2
