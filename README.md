# FareySequencesModule.hs #
A Haskell `FareySequencesModule.hs` module exporting a dozen of functions coded after several selected formulas presented 
in the monograph [A.O. Matveev, Farey Sequences: Duality and Maps Between Subsequences](https://doi.org/10.1515/9783110547665), Berlin, Boston: De Gruyter, 2017. 

## Our Dramatis Personae ##
  (See Table 1.1 of the monograph):
- (Standard) Farey Sequence Fm ( in LaTeX: $\mathcal{F}_m$ ), where $m > 0$;
- Farey Subsequence Fml ( in LaTeX: $\mathcal{F}{}_m^l$ ), where $m > 1$, and $0 < l < m$;
- Farey Subsequence Gml ( in LaTeX: $\mathcal{G}{}_m^l$ ), where $m > 1$, and $0 < l < m$;
- Farey Subsequence FB2mm ( in LaTeX: $\mathcal{F}(\mathbb{B}(2m),m)$ ), where $m > 0$;
- Farey Subsequence FBnm ( in LaTeX: $\mathcal{F}(\mathbb{B}(n),m)$ ), where $n > 1$, and $0 < m < n$.

## Fractions ##
In healthy situations, all of the exported functions return reduced fractions (h % k) such that (0 % 1) <= (h % k) <= (1 % 1).
If you get a *negative* fraction, it means that something went wrong, and the *denominator* of the resulting negative fraction has 
no computational meaning, since it just reports a reason of the problem---see the source code of the function you have used.

## In the Beginning ##
If you would like to build up a certain subsequence of neighboring (one after another) fractions in one of the above Personae, 
then first you should realize what you hold in your hands in the beginning.

If you have two neighboring fractions, then you proceed in the recurrent manner by means of a relatively fast function 
of the form `predecessorOfPairOfNeighborsInPersonage` or `successorOfPairOfNeighborsInPersonage`, depending on which direction (descending 
or ascending) you take.

If you have one fraction then, in order to obtain your starting pair of neighboring fractions, you make one-time use of a relatively slow 
function of the form `predecessorInPersonage` or `successorInPersonage`, and then you proceed by calling, in the recurrent manner, 
the corresponding relatively fast function of the form `predecessorOfPairOfNeighborsInPersonage` or `successorOfPairOfNeighborsInPersonage`.
