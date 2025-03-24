#!/usr/bin/env Rscript
library(ape)
library(ppcount)

tree <- ape::read.tree(text="(Tora:593.0470437,((Jaru:194.3342836,Urupa:184.3342836):259.8784746,((Wari:286.7558372,OroWin:286.7558372):178.7732388,Wanyam:465.529076):68.68368228):148.8342854);")

plot(tree)
axisPhylo()
nodelabels()

rh <- ppcount::get_rootheight(tree)
ages <- ppcount::get_ages(tree)
# parental.node - the parent node
# daughter.node - the daughter node
# dist.root - the distance from the root
# BL - this branch length
# dist.tip - the distance of the tip.
# mrca.age - age of the most recent common ancestor
#   -> mrca.age is BL + dist.tip to get age
# BL is mrca.age - dist.tip
