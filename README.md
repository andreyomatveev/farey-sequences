# FareySequncesModule
A Haskell module FareySequencesModule.hs exporting a dozen of functions coded after a dozen of selected formulas presented 
in Farey Sequences: Dulaity and Maps Between Subsequences, by A.O. Matveev, De Gruyter, 2017. 

# Our Dramatis Personae (see Table 1.1 of the monograph):
- (Standard) Farey Sequence Fm ( in LaTeX: $\mathcal{F}_m$ ), where m > 0;
- Farey Subsequence Fml ( in LaTeX: $\mathcal{F}{}_m^l$ ), where m > 1, and 0 < l < m;
- Farey Subsequence Gml ( in LaTeX: $\mathcal{G}{}_m^l$ ), where m > 1, and 0 < l < m;
- Farey Subsequence FB2mm ( in LaTeX: $\mathcal{F}(\mathbb{B}(2m),m)$ ), where m > 0;
- Farey Subsequence FBnm ( in LaTeX: $\mathcal{F}(\mathbb{B}(n),m)$ ), where n > 1, and 0 < m < n.

# Fractions
In healthy situations, all of the exported functions return NONNEGATIVE reduced fractions (h % k) such that (0 % 1) <= (h % k) <= (1 % 1).
If you get a NEGATIVE fraction, it means that something went wrong, and the denominator of the resulting negative fraction has 
no computational meaning, since it just reports a reason of the problem---see the source code of the function you have used.

# In the beginning
If you would like to build up a certain subsequence of neighboring (one after another) fractions in one of the above Personae, 
then first you should realize what you hold in your hands in the beginning.

If you have two neighboring fractions, then you proceed in the recurrent manner by means of a relatively fast function 
of the form `predecessorOfPairOfNeighborsInPersonage' or `successorOfPairOfNeighborsInPersonage', depending on which direction (descending 
or ascending) you take.

If you have one fraction then, in order to obtain your starting pair of neighboring fractions, you make one-time use of a relatively slow 
function of the form `predecessorInPersonage' or `successorInPersonage', and then you proceed by calling, in the recurrent manner, 
the corresponding relatively fast function of the form  `predecessorOfPairOfNeighborsInPersonage' or `successorOfPairOfNeighborsInPersonage'.
