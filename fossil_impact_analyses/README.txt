This is a very basic pipeline I've bodged together to implement some of the tests performed in:  Cobbett, A., Wilkinson, M., and Wills, M. (2007). Fossils impact as hard as living taxa in parsimony analyses of morphology. Systematic Biology, 56(5):753-766. http://dx.doi.org/10.1080/10635150701627296

It certainly could do with a lot of improving... but it works.
ksepka.tnt is an 'easy' (quick) test data file, and resultsR.csv is example output

OTHER FILES, run in this order:
1.) tntdb.txt 2.) paupcmds.txt 3.) makerefs.txt 4.) makejacks.txt 5.) temp.R

The first script does the single taxon jackknife maximum parsimony analyses 
and spits out 'orig.nex' (with trees) and 'j01.nex' ... (jackknifed matrices with jackknifed trees)

#SECOND SCRIPT NOT TECHNICALLY NEEDED, JUST FOR BACKWARDS COMPATIBILITY WITH DELBAT
The second script just converts the nex files (with matrix) into phylip tree files 
a.nexorg.tre + a.nex.1.tre ...  

The third script reads in orig.nex, deletes single taxa from trees, and spits out pruned-ref trees ref2.tre  IMPORTANT, numbering here starts from 2 because of PAUP numbering (1=outgroup)

The fourth script reads in all the j01.nex files and spits out treeset files jack1.tre
numbering here starts from 1 reflecting numbering of TNT (0=outgroup)

The PAUP* batch scripts can be run together in shell like this:
paup -n paupcmds.txt; paup -n makerefs.txt; paup -n makejacks.txt;

The fifth script calculates the tree2tree distances in R and spits out a 'resultsR.csv' file of results for both Robinson-Foulds & PD distances

