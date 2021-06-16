# ppcount: extract ages and posterior probabilities from a phylogenetic tree set.

The goal of ppcount is to get the age and posterior probability of given clades from a
posterior probability distribution of phylogenies.

## Installation

You can install the preliminary version of `ppcount` from [github](https://github.com/SimonGreenhill/ppcount) with:

``` r
library(devtools)
install_github("SimonGreenhill/ppcount", dependencies = TRUE)
```

## Example

``` r
library(ape)
library(ppcount)

> tree <- ape::rtree(9)
# first extract ages from the tree
> ages <- get_ages(tree)
> print(ages)
# A tibble: 8 x 6
   parental.node daughter.node dist.root        BL  dist.tip  mrca.age
1              6             7 0.5564967 0.5564967 1.5869747 2.1434714
t2             7             1 0.6205180 0.0640213 1.5229534 1.5869747
t1             7             2 1.4206500 0.8641533 0.7228215 1.5869747
4              6             8 0.6543045 0.6543045 1.4891670 2.1434714
t5             8             3 0.8962347 0.2419303 1.2472367 1.4891670
6              8             9 1.3053281 0.6510236 0.8381433 1.4891670
t3             9             4 1.4601615 0.1548334 0.6833099 0.8381433
t4             9             5 2.1434714 0.8381433 0.0000000 0.8381433

# get nodeages from the tree
> ages <- get_nodeages(tree)
> print(ages)
# A tibble: 4 x 3
   node   age tips              
  <dbl> <dbl> <chr>             
1     6    19 t1, t2, t3, t4, t5
2     7    16 t4, t5            
3     8    11 t1, t2, t3        
4     9     5 t1, t3           


# To get the ages of a specific set of taxa across one/more tree(s):
# first, specify the clades you want:
> clades <- list(A = c('t1', 't3'), B = c('t1', 't2', 't3'))

# then 
> process_trees(clades, trees)

# A tibble: 4 x 4
   Tree Clade   Age Monophyly
  <int> <chr> <dbl> <lgl>    
1     1 A     2.68  FALSE    
2     1 B     0.853 TRUE     
3     2 A     2.68  FALSE    
4     2 B     0.853 TRUE     
...

# i.e. clade "A" is 2.68 and not monophyletic in both trees, while
# clade "B" is 0.853, and is monophyletic in both trees.

```
### Read in a list of clades from a file:

Given a file like this, `ppcount`, can read this and process it too:

```
taxset A = t1 t2 t3;
taxset B = t4 t5;
```

```r
> clades <- read_clade_file(filename)
> results <- process_trees(clades, trees)
```