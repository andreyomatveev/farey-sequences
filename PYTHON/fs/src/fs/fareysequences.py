# Andrey O. Matveev
# After several selected formulas from the monograph A.O. Matveev, Farey Sequences: Duality and Maps Between Subsequences,
# Berlin: De Gruyter, 2017, https://doi.org/10.1515/9783110547665 .
#
# Our Dramatis Personae (see Table 1.1 of the monograph):
#
# (Standard) Farey Sequence Fm ( in LaTeX: $\mathcal{F}_m$ ), where m > 0;
# Farey Subsequence Fml ( in LaTeX: $\mathcal{F}_m^l$ ), where m > 1, and 0 < l < m;
# Farey Subsequence Gml ( in LaTeX: $\mathcal{G}_m^l$ ), where m > 1, and 0 < l < m;
# Farey Subsequence FB2mm ( in LaTeX: $\mathcal{F}(\mathbb{B}(2m),m)$ ), where m > 0;
# Farey Subsequence FBnm ( in LaTeX: $\mathcal{F}(\mathbb{B}(n),m)$ ), where n > 1, and 0 < m < n.
#
# In healthy situations, all of the exported functions below return reduced fractions Fraction(h, k)
# such that Fraction(0, 1) <= Fraction(h, k) <= Fraction(1, 1).
# If you get a NEGATIVE fraction, it means that something went wrong,
# and the denominator of the resulting negative fraction has no computational meaning,
# since it just reports a reason of the problem---see the source code of the function you have used.
#
# If you would like to build up a certain subsequence of neighboring (one after another) fractions
# in one of the above Personae, then first you should realize what you hold in your hands in the beginning.
#
# If you have two neighboring fractions, then you proceed in the recurrent manner
# by means of a relatively fast function of the form `predecessor_of_pair_of_neighbors_in_personage' or
# `successor_of_pair_of_neighbors_in_personage', depending on which direction (descending or ascending) you take.
#
# If you have one fraction then, in order to obtain your starting pair of neighboring fractions,
# you make one-time use of a relatively slow function of the form `predecessor_in_personage' or `successor_in_personage',
# and then you proceed by calling, in the recurrent manner, the corresponding relatively fast function
# of the form  `predecessor_of_pair_of_neighbors_in_personage' or `successor_of_pair_of_neighbors_in_personage'.
#
# In theory, Farey Subsequences $\mathcal{F}(\mathbb{B}(n),m)$ exhibit different behavior
# depending on whether you have n == 2*m, or n != 2*m. Nevertheless, if you deal with a Farey Subsequence $\mathcal{F}(\mathbb{B}(2m),m)$,
# then in this module it is suffices to call the predecessor_in_FBnm, successor_in_FBnm, predecessor_of_pair_of_neighbors_in_FBnm,
# and successor_of_pair_of_neighbors_in_FBnm functions by giving the value of (2*m) for the parameter n.
#
# If, for some reason, you prefer more narrowed names of functions, make the following functions public (and import and use them):
# __predecessor_in_FB2mm
# __successor_in_FB2mm
# __predecessor_of_pair_of_neighbors_in_FB2mm
# __successor_of_pair_of_neighbors_in_FB2mm


from ast import Tuple
from fractions import Fraction
from math import ceil, floor


def predecessor_in_Fm(m: int, successor: Fraction) -> Fraction:
    # See Lemma 2.9(i) and Table 2.1 of the monograph. Call for instance:
    #    >>> predecessor_in_Fm(6, Fraction(2, 3))
    # to get the result:
    #    Fraction(3, 5)
    if m < 1:
        # "N/A: Order m of the sequence should be > 0"
        return Fraction(1, -1)
    elif (successor <= Fraction(0, 1)) or (successor > Fraction(1, 1)):
        # "N/A: successor should be between (0/1) (excluded) and (1/1) (included)"
        return Fraction(1, -2)
    elif successor.denominator > m:
        # "N/A: Denominator of the successor should not exceed the order m of the sequence"
        return Fraction(1, -3)
    else:
        if successor == Fraction(1, 1):
            return __predecessor_of_one_first_in_Fm(m)
        else:
            ref_point = ceil((successor.numerator * m) /
                             successor.denominator)
            return __get_numerator_and_return_predecessor(
                __find_numerator_of_predecessor((ref_point - successor.numerator, ref_point - 1), successor), successor)


def successor_in_Fm(m: int, predecessor: Fraction) -> Fraction:
    # See Lemma 2.9(ii) and Table 2.3 of the monograph. Call for instance:
    #    >>> successor_in_Fm(6, Fraction(1, 3))
    # to get the result:
    #    Fraction(, )
    if m < 1:
        # "N/A: Order m of the sequence should be > 0"
        return Fraction(1, -1)
    elif (predecessor < Fraction(0, 1)) or (predecessor >= Fraction(1, 1)):
        # "N/A: predecessor should be between (0/1) (included) and (1/1) (excluded)"
        return Fraction(1, -2)
    elif predecessor.denominator > m:
        # "N/A: Denominator of the predecessor should not exceed the order m of the sequence"
        return Fraction(1, -3)
    else:
        if predecessor == Fraction(0, 1):
            return __successor_of_zero_first_in_Fm(m)
        else:
            ref_point = ceil((predecessor.numerator * m + 2) /
                             predecessor.denominator)
            return __get_numerator_and_return_successor(
                __find_numerator_of_successor(
                    (ref_point - predecessor.numerator, ref_point - 1), predecessor),
                predecessor)


def predecessor_in_Fml(m: int, l: int, successor: Fraction) -> Fraction:
    # See Lemma 2.13(i)(a)-(b) and Table 2.1 of the monograph. Call for instance:
    #    >>> predecessor_in_Fml(6, 4, Fraction(1, 1))
    # to get the result:
    #    Fraction(4, 5)
    if m < 2:
        # "N/A: Parameter m of the sequence should be > 1"
        return Fraction(1, -1)
    elif (l <= 0) or (l >= m):
        # "N/A: Parameter l should be between 0 (excluded) and m (excluded)"
        return Fraction(1, -2)
    elif (successor <= Fraction(0, 1)) or (successor > Fraction(1, 1)):
        # "N/A: successor should be between (0/1) (excluded) and (1/1) (included)"
        return Fraction(1, -3)
    elif successor.denominator > m:
        # "N/A: Denominator of the successor should not exceed the parameter m of the sequence"
        return Fraction(1, -4)
    elif l < successor.numerator:
        # "N/A: Numerator of the successor should be between 1 (included) and l (included)"
        return Fraction(1, -5)
    else:
        if successor == Fraction(1, 1):
            return __predecessor_of_one_first_in_Fml(l)
        elif successor.numerator * m - successor.denominator * l >= 1:
            return __get_numerator_and_return_predecessor(
                __find_numerator_of_predecessor((l - successor.numerator + 1, l), successor), successor)
        else:
            ref_point = ceil((successor.numerator * m) / successor.denominator)
            return __get_numerator_and_return_predecessor(
                __find_numerator_of_predecessor((ref_point - successor.numerator, ref_point - 1), successor), successor)


def successor_in_Fml(m: int, l: int, predecessor: Fraction) -> Fraction:
    # See Lemma 2.13(ii)(a)-(b) and Table 2.3 of the monograph. Call for instance:
    #    >>> successor_in_Fml(6, 4, Fraction(4, 5))
    # to get the result:
    #    Fraction(1, 1)
    if m < 2:
        # "N/A: Parameter m of the sequence should be > 1"
        return Fraction(1, -1)
    elif (l <= 0) or (l >= m):
        # "N/A: Parameter l should be between 0 (excluded) and m (excluded)"
        return Fraction(1, -2)
    elif (predecessor < Fraction(0, 1)) or (predecessor >= Fraction(1, 1)):
        # "N/A: predecessor should be between (0/1) (included) and (1/1) (excluded)"
        return Fraction(1, -3)
    elif predecessor.denominator > m:
        # "N/A: Denominator of the predecessor should not exceed the parameter m of the sequence"
        return Fraction(1, -4)
    elif l < predecessor.numerator:
        # "N/A: "N/A: Numerator of the predecessor should be between 1 (included) and l (included)"
        return Fraction(1, -5)
    else:
        if predecessor == Fraction(0, 1):
            return __successor_of_zero_first_in_Fml(m)
        elif predecessor.denominator * l - predecessor.numerator * m >= 1:
            ref_point = ceil((predecessor.numerator * m + 2) /
                             predecessor.denominator)
            return __get_numerator_and_return_successor(
                __find_numerator_of_successor(
                    (ref_point - predecessor.numerator, ref_point - 1), predecessor),
                predecessor)
        else:
            return __get_numerator_and_return_successor(
                __find_numerator_of_successor((l - predecessor.numerator + 1, l), predecessor), predecessor)


def predecessor_in_Gml(m: int, l: int, successor: Fraction) -> Fraction:
    # See Lemma 2.15(i)(a)-(b) and Table 2.1 of the monograph. Call for instance:
    #    >>> predecessor_in_Gml(6, 4, Fraction(1, 3))
    # to get the result:
    #    Fraction(0, 1)
    if m < 2:
        # "N/A: Parameter m of the sequence should be > 1"
        return Fraction(1, -1)
    elif (l <= 0) or (l >= m):
        # "N/A: Parameter l should be between 0 (excluded) and m (excluded)"
        return Fraction(1, -2)
    elif (successor <= Fraction(0, 1)) or (successor > Fraction(1, 1)):
        # "N/A: successor should be between (0/1) (excluded) and (1/1) (included)"
        return Fraction(1, -3)
    elif successor.denominator > m:
        # "N/A: Denominator of the successor should not exceed the parameter m of the sequence"
        return Fraction(1, -4)
    elif l + successor.denominator - m > successor.numerator:
        # "N/A: The quantity (l + denominator - m) should not exceed the numerator of the successor"
        return Fraction(1, -5)
    else:
        if successor == Fraction(1, 1):
            return __predecessor_of_one_first_in_Gml(l)
        else:
            if successor.numerator * m - successor.denominator * l >= 1:
                ref_point = ceil((successor.numerator * m) /
                                 successor.denominator)
                return __get_numerator_and_return_predecessor(
                    __find_numerator_of_predecessor(
                        (ref_point - successor.numerator, ref_point - 1), successor),
                    successor)
            else:
                ref_point = ceil(
                    (successor.numerator * (m - l)) / (successor.denominator - successor.numerator))
                return __get_numerator_and_return_predecessor(
                    __find_numerator_of_predecessor(
                        (ref_point - successor.numerator, ref_point - 1), successor),
                    successor)


def successor_in_Gml(m: int, l: int, predecessor: Fraction) -> Fraction:
    # See Lemma 2.15(ii)(a)-(b) and Table 2.3 of the monograph. Call for instance:
    #    >>> successor_in_Gml(6, 4, Fraction(1, 3))
    # to get the result:
    #    Fraction(1, 2)
    if m < 2:
        # "N/A: Parameter m of the sequence should be > 1"
        return Fraction(1, -1)
    elif (l <= 0) or (l >= m):
        # "N/A: Parameter l should be between 0 (excluded) and m (excluded)"
        return Fraction(1, -2)
    elif (predecessor < Fraction(0, 1)) or (predecessor >= Fraction(1, 1)):
        # "N/A: predecessor should be between (0/1) (included) and (1/1) (excluded)"
        return Fraction(1, -3)
    elif predecessor.denominator > m:
        # "N/A: Denominator of the predecessor should not exceed the parameter m of the sequence"
        return Fraction(1, -4)
    elif l + predecessor.denominator - m > predecessor.numerator:
        # "N/A: Denominator of the predecessor minus its numerator should not exceed (m - l)"
        return Fraction(1, -5)
    else:
        if predecessor == Fraction(0, 1):
            return __successor_of_zero_first_in_Gml(m, l)
        elif predecessor.denominator * l - predecessor.numerator * m >= 1:
            ref_point = ceil((predecessor.numerator * (m - l) + 2) /
                             (predecessor.denominator - predecessor.numerator))
            return __get_numerator_and_return_successor(
                __find_numerator_of_successor(
                    (ref_point - predecessor.numerator, ref_point - 1), predecessor),
                predecessor)
        else:
            ref_point = ceil((predecessor.numerator * m + 2) /
                             predecessor.denominator)
            return __get_numerator_and_return_successor(
                __find_numerator_of_successor(
                    (ref_point - predecessor.numerator, ref_point - 1), predecessor),
                predecessor)


def __predecessor_in_FB2mm(m: int, successor: Fraction) -> Fraction:
    # See Remark 1.17 and Table 1.5;
    # see Remark 2.25 and Table 2.8;
    # see Remark 2.11 and Table 2.5;
    # see Remark 2.24 and Table 2.7;
    # see Proposition 2.12 (i) (a) and Proposition 2.12 (ii) (a), and Table 2.1 of the monograph. Call for instance:
    #    >>> __predecessor_in_FB2mm(3, Fraction(2, 5))
    # to get the result:
    #    Fraction(1, 3)
    if m < 1:
        # "N/A: Parameter m of the sequence should be > 0"
        return Fraction(1, -1)
    elif (successor <= Fraction(0, 1)) or (successor > Fraction(1, 1)):
        # "N/A: successor should be between (0/1) (excluded) and (1/1) (included)"
        return Fraction(1, -2)
    elif successor.denominator > 2 * m:
        # "N/A: Denominator of the successor should not exceed (2 * m)"
        return Fraction(1, -3)
    elif (successor.denominator - m > successor.numerator) or (successor.numerator > m):
        # "N/A: Numerator of the successor should be between (denominator - m) (included) and m (included)"
        return Fraction(1, -4)
    else:
        match successor:
            case Fraction(numerator=1, denominator=1):
                return __predecessor_of_one_first_in_FB2mm(m)
            case Fraction(numerator=2, denominator=3):
                return __predecessor_of_two_thirds_in_FB2mm(m)
            case Fraction(numerator=1, denominator=2):
                return __predecessor_of_one_second_in_FB2mm(m)
            case Fraction(numerator=1, denominator=3):
                return __predecessor_of_one_third_in_FB2mm(m)
            case _:
                if successor > Fraction(1, 2):
                    return __get_numerator_and_return_predecessor(
                        __find_numerator_of_predecessor((m - successor.numerator + 1, m), successor), successor)
                else:
                    ref_point = ceil((successor.numerator * m) /
                                     (successor.denominator - successor.numerator))
                    return __get_numerator_and_return_predecessor(
                        __find_numerator_of_predecessor(
                            (ref_point - successor.numerator, ref_point - 1), successor),
                        successor)


def __successor_in_FB2mm(m: int, predecessor: Fraction) -> Fraction:
    # See Remark 1.17 and Table 1.5;
    # see Remark 2.24 and Table 2.7;
    # see Remark 2.11 and Table 2.5;
    # see Remark 2.25 and Table 2.8;
    # see Proposition 2.12 (i) (b) and Proposition 2.12 (ii) (b), and Table 2.3 of the monograph. Call for instance:
    #    >>> __successor_in_FB2mm(3, Fraction(3, 5))
    # to get the result:
    #    Fraction(2, 3)
    if m < 1:
        # "N/A: Parameter m of the sequence should be > 0"
        return Fraction(1, -1)
    elif (predecessor < Fraction(0, 1)) or (predecessor >= Fraction(1, 1)):
        # "N/A: predecessor should be between (0/1) (included) and (1/1) (excluded)"
        return Fraction(1, -2)
    elif predecessor.denominator > 2 * m:
        # "N/A: Denominator of the predecessor should not exceed (2 * m)"
        return Fraction(1, -3)
    elif (predecessor.denominator - m > predecessor.numerator) or (predecessor.numerator > m):
        # "N/A: Numerator of the predecessor should be between (denominator - m) (included) and m (included)"
        return Fraction(1, -4)
    else:
        match predecessor:
            case Fraction(numerator=0, denominator=1):
                return __successor_of_zero_first_in_FB2mm(m)
            case Fraction(numerator=1, denominator=3):
                return __successor_of_one_third_in_FB2mm(m)
            case Fraction(numerator=1, denominator=2):
                return __successor_of_one_second_in_FB2mm(m)
            case Fraction(numerator=2, denominator=3):
                return __successor_of_two_thirds_in_FB2mm(m)
            case _:
                if predecessor < Fraction(1, 2):
                    ref_point = ceil((predecessor.numerator * m + 2) /
                                     (predecessor.denominator - predecessor.numerator))
                    return __get_numerator_and_return_successor(
                        __find_numerator_of_successor(
                            (ref_point - predecessor.numerator, ref_point - 1), predecessor),
                        predecessor)
                else:
                    return __get_numerator_and_return_successor(
                        __find_numerator_of_successor((m - predecessor.numerator + 1, m), predecessor), predecessor)


def predecessor_in_FBnm(n: int, m: int, successor: Fraction) -> Fraction:
    # See Remark 1.17 and Table 1.5 of the monograph;
    # see CORRECTED Remark 2.43(i) and Remark 2.43(ii) and CORRECTED Table 2.8;
    # see Remark 2.17 and Table 2.5;
    # See CORRECTED Remark 2.42 and Table 2.7;
    # see Propositions 2.18(i)(a) and 2.18(ii)(a), and Table 2.1;
    # see Propositions 2.19(i)(a) and 2.19(ii)(a), and Table 2.1.  Call for instance:
    #    >>> predecessor_in_FBnm(6, 4, Fraction(3, 4))
    # to get the result:
    #    Fraction(2, 3)
    if n == 2 * m:
        return __predecessor_in_FB2mm(m, successor)
    elif n < 2:
        # "N/A: Parameter n of the sequence should be > 1"
        return Fraction(1, -1)
    elif (m < 1) or (m >= n):
        # "N/A: Parameter m of the sequence should be between 0 (excluded) and n (excluded)"
        return Fraction(1, -2)
    elif (successor <= Fraction(0, 1)) or (successor > Fraction(1, 1)):
        # "N/A: successor should be between (0/1) (excluded) and (1/1) (included)"
        return Fraction(1, -3)
    elif successor.denominator > n:
        # "N/A: Denominator of the successor should not exceed the parameter n of the sequence"
        return Fraction(1, -4)
    elif (m + successor.denominator - n > successor.numerator) or (successor.numerator > m):
        # "N/A: Numerator of the successor should be between (m + denominator - n) (included) and m (included)"
        return Fraction(1, -5)
    else:
        match successor:
            case Fraction(numerator=1, denominator=1):
                return __predecessor_of_one_first_in_FBnm(m)
            case Fraction(numerator=2, denominator=3):
                return __predecessor_of_two_thirds_in_FBnm(n, m)
            case Fraction(numerator=1, denominator=2):
                return __predecessor_of_one_second_in_FBnm(n, m)
            case Fraction(numerator=1, denominator=3):
                return __predecessor_of_one_third_in_FBnm(n, m)
            case _:
                if n < 2 * m:
                    if (successor > Fraction(1, 2)) and (successor.numerator * n - successor.denominator * m >= 1):
                        return __get_numerator_and_return_predecessor(
                            __find_numerator_of_predecessor((m - successor.numerator + 1, m), successor), successor)
                    else:
                        ref_point = ceil(
                            successor.numerator * (n - m) / (successor.denominator - successor.numerator))
                        return __get_numerator_and_return_predecessor(
                            __find_numerator_of_predecessor((ref_point - successor.numerator, ref_point - 1),
                                                            successor), successor)
                else:
                    if (successor > Fraction(1, 2)) or (successor.numerator * n - successor.denominator * m >= 1):
                        return __get_numerator_and_return_predecessor(
                            __find_numerator_of_predecessor((m - successor.numerator + 1, m), successor), successor)
                    else:
                        ref_point = ceil(
                            successor.numerator * (n - m) / (successor.denominator - successor.numerator))
                        return __get_numerator_and_return_predecessor(
                            __find_numerator_of_predecessor((ref_point - successor.numerator, ref_point - 1),
                                                            successor), successor)


def successor_in_FBnm(n: int, m: int, predecessor: Fraction) -> Fraction:
    # See Remark 1.17 and Table 1.5 of the monograph;
    # see CORRECTED Remark 2.42 and Table 2.7;
    # see Remark 2.17 and Table 2.5;
    # see Remark 2.43(i)-(ii) and Table 2.8;
    # see Propositions 2.18(i)(b) and 2.18(ii)(b), and Table 2.3;
    # see Propositions 2.19(i)(b) and 2.19(ii)(b), and Table 2.3.  Call for instance:
    #    >>> successor_in_FBnm(6, 4, Fraction(4, 5))
    # to get the result:
    #    Fraction(1, 1)
    if n == 2 * m:
        return __successor_in_FB2mm(m, predecessor)
    elif n < 2:
        # "N/A: Parameter n of the sequence should be > 1"
        return Fraction(1, -1)
    elif (m < 1) or (m >= n):
        # "N/A: Parameter m of the sequence should be between 0 (excluded) and n (excluded)"
        return Fraction(1, -2)
    elif (predecessor < Fraction(0, 1)) or (predecessor >= Fraction(1, 1)):
        # "N/A: predecessor should be between (0 % 1) (included) and (1 % 1) (excluded)"
        return Fraction(1, -3)
    elif predecessor.denominator > n:
        # "N/A: Denominator of the predecessor should not exceed the parameter n of the sequence"
        return Fraction(1, -4)
    elif (m + predecessor.denominator - n > predecessor.numerator) or (predecessor.numerator > m):
        # "N/A: Numerator of the predecessor should be between (m + denominator - n) (included) and m (included)"
        return Fraction(1, -5)
    else:
        match predecessor:
            case Fraction(numerator=0, denominator=1):
                return __successor_of_zero_first_in_FBnm(n, m)
            case Fraction(numerator=1, denominator=3):
                return __successor_of_one_third_in_FBnm(n, m)
            case Fraction(numerator=1, denominator=2):
                return __successor_of_one_second_in_FBnm(n, m)
            case Fraction(numerator=2, denominator=3):
                return __successor_of_two_thirds_in_FBnm(n, m)
            case _:
                if n < 2 * m:
                    if (predecessor > Fraction(1, 2)) and (
                            predecessor.denominator * m - predecessor.numerator * n <= 1):
                        return __get_numerator_and_return_successor(
                            __find_numerator_of_successor((m - predecessor.numerator + 1, m), predecessor), predecessor)
                    else:
                        ref_point = ceil((predecessor.numerator * (n - m) + 2) /
                                         (predecessor.denominator - predecessor.numerator))
                        return __get_numerator_and_return_successor(
                            __find_numerator_of_successor((ref_point - predecessor.numerator, ref_point - 1),
                                                          predecessor), predecessor)
                else:
                    if (predecessor > Fraction(1, 2)) or (predecessor.denominator * m - predecessor.numerator * n <= 1):
                        return __get_numerator_and_return_successor(
                            __find_numerator_of_successor((m - predecessor.numerator + 1, m), predecessor), predecessor)
                    else:
                        ref_point = ceil((predecessor.numerator * (n - m) + 2) /
                                         (predecessor.denominator - predecessor.numerator))
                        return __get_numerator_and_return_successor(
                            __find_numerator_of_successor((ref_point - predecessor.numerator, ref_point - 1),
                                                          predecessor), predecessor)


def __get_numerator_and_return_predecessor(a: int, successor: Fraction) -> Fraction:
    return Fraction(a, (successor.denominator * a + 1) // successor.numerator)


def __get_numerator_and_return_successor(a: int, predecessor: Fraction) -> Fraction:
    return Fraction(a, (predecessor.denominator * a - 1) // predecessor.numerator)


def __find_numerator_of_predecessor(search_interval: Tuple(int, int), successor: Fraction) -> int:
    # Here we follow a suggestion found at https://code-maven.com/python-find-first-element-in-list-matching-condition
    return next(filter(lambda x: (successor.denominator * x + 1) % successor.numerator == 0, range(search_interval[0], search_interval[1] + 1)))


def __find_numerator_of_successor(search_interval: Tuple(int, int), predecessor: Fraction) -> int:
    # Here we follow a suggestion found at https://code-maven.com/python-find-first-element-in-list-matching-condition
    return next(filter(lambda x: (predecessor.denominator * x - 1) % predecessor.numerator == 0, range(search_interval[0], search_interval[1] + 1)))


def __predecessor_of_one_first_in_Fm(m: int) -> Fraction:
    return Fraction(m - 1, m)


def __successor_of_zero_first_in_Fm(m: int) -> Fraction:
    # See Remark 1.6 and Table 1.5  of the monograph
    return Fraction(1, m)


def __predecessor_of_one_first_in_Fml(l: int) -> Fraction:
    # See Remark 1.9 and Table 1.5  of the monograph
    return Fraction(l, l + 1)


def __successor_of_zero_first_in_Fml(m: int) -> Fraction:
    # See Remark 1.9 and Table 1.5  of the monograph
    return Fraction(1, m)


def __predecessor_of_one_first_in_Gml(m: int) -> Fraction:
    # See Remark 1.13 and Table 1.5  of the monograph
    return Fraction(m - 1, m)


def __successor_of_zero_first_in_Gml(m: int, l: int) -> Fraction:
    # See Remark 1.13 and Table 1.5  of the monograph
    return Fraction(1, m - l + 1)


def __predecessor_of_one_first_in_FB2mm(m: int) -> Fraction:
    # See Remark 1.17 and Table 1.5 of the monograph
    return Fraction(m, m + 1)


def __predecessor_of_two_thirds_in_FB2mm(m: int) -> Fraction:
    # See Remark 2.25 and Table 2.8 of the monograph
    if m % 2 == 0:
        return Fraction((m - 1), ((3 * m - 2) // 2))
    else:
        return Fraction(m, (3 * m + 1) // 2)


def __predecessor_of_one_second_in_FB2mm(m: int) -> Fraction:
    # See Remark 2.11 and Table 2.5 of the monograph
    return Fraction(m - 1, 2 * m - 1)


def __predecessor_of_one_third_in_FB2mm(m: int) -> Fraction:
    # See Remark 2.24 and Table 2.7 of the monograph
    if m % 2 == 0:
        return Fraction((m - 2) // 2, ((3 * m - 4) // 2))
    else:
        return Fraction((m - 1) // 2, ((3 * m - 1) // 2))


def __successor_of_zero_first_in_FB2mm(m: int) -> Fraction:
    # See Remark 1.17 and Table 1.5 of the monograph
    return Fraction(1, m + 1)


def __successor_of_one_third_in_FB2mm(m: int) -> Fraction:
    # See Remark 2.24 and Table 2.7 of the monograph
    if m % 2 == 0:
        return Fraction(m // 2, (3 * m - 2) // 2)
    else:
        return Fraction((m + 1) // 2, (3 * m + 1) // 2)


def __successor_of_one_second_in_FB2mm(m: int) -> Fraction:
    # See Remark 2.11 and Table 2.5 of the monograph
    return Fraction(m, 2 * m - 1)


def __successor_of_two_thirds_in_FB2mm(m: int) -> Fraction:
    # See Remark 2.25 and Table 2.8 of the monograph
    if m % 2 == 0:
        return Fraction(m - 1, (3 * m - 4) // 2)
    else:
        return Fraction(m, (3 * m - 1) // 2)


def __predecessor_of_one_first_in_FBnm(m: int) -> Fraction:
    # See Remark 1.17 and Table 1.5 of the monograph
    return Fraction(m, m + 1)


def __predecessor_of_two_thirds_in_FBnm(n: int, m: int) -> Fraction:
    # See CORRECTED Remark 2.43(i) and Remark 2.43(ii) and CORRECTED Table 2.8 of the monograph
    if n < 2 * m:
        if 2 * n - 3 * m >= 1:
            if m % 2 == 0:
                return Fraction(m - 1, (3 * m - 2) // 2)
            else:
                return Fraction(m, (3 * m + 1) // 2)
        else:
            return Fraction(2 * (n - m) - 1, 3 * (n - m) - 1)
    else:
        if m % 2 == 0:
            return Fraction(m - 1, (3 * m - 2) // 2)
        else:
            return Fraction(m, (3 * m + 1) // 2)


def __predecessor_of_one_second_in_FBnm(n: int, m: int) -> Fraction:
    # See Remark 2.17 and Table 2.5 of the monograph
    if n < 2 * m:
        return Fraction(n - m - 1, 2 * (n - m) - 1)
    else:
        return Fraction(m, 2 * m + 1)


def __predecessor_of_one_third_in_FBnm(n: int, m: int) -> Fraction:
    # See CORRECTED Remark 2.42 and Table 2.7 of the monograph
    if n < 2 * m:
        if (n - m) % 2 == 0:
            return Fraction((n - m - 2) // 2, (3 * (n - m) - 4) // 2)
        else:
            return Fraction((n - m - 1) // 2, (3 * (n - m) - 1) // 2)
    elif n - 3 * m >= 1:
        return Fraction(m, 3 * m + 1)
    elif (n - m) % 2 == 0:
        return Fraction((n - m - 2) // 2, (3 * (n - m) - 4) // 2)
    else:
        return Fraction((n - m - 1) // 2, (3 * (n - m) - 1) // 2)


def __successor_of_zero_first_in_FBnm(n: int, m: int) -> Fraction:
    # See Remark 1.17 and Table 1.5 of the monograph
    return Fraction(1, n - m + 1)


def __successor_of_one_third_in_FBnm(n: int, m: int) -> Fraction:
    # See CORRECTED Remark 2.42 and Table 2.7 of the monograph
    if n < 2 * m:
        if (n - m) % 2 == 0:
            return Fraction((n - m) // 2, (3 * (n - m) - 2) // 2)
        else:
            return Fraction((n - m + 1) // 2, (3 * (n - m) + 1) // 2)
    elif 3 * m - n <= 1:
        return Fraction(m, 3 * m - 1)
    elif (n - m) % 2 == 0:
        return Fraction((n - m) // 2, (3 * (n - m) - 2) // 2)
    else:
        return Fraction((n - m + 1) // 2, (3 * (n - m) + 1) // 2)


def __successor_of_one_second_in_FBnm(n: int, m: int) -> Fraction:
    # See Remark 2.17 and Table 2.5 of the monograph
    if n < 2 * m:
        return Fraction(n - m + 1, 2 * (n - m) + 1)
    else:
        return Fraction(m, 2 * m - 1)


def __successor_of_two_thirds_in_FBnm(n: int, m: int) -> Fraction:
    # See Remarks 2.43(i)-(ii) and Table 2.8 of the monograph
    if n < 2 * m:
        if 3 * m - 2 * n >= 1:
            return Fraction(2 * (n - m) + 1, 3 * (n - m) + 1)
        else:
            if m % 2 == 0:
                return Fraction(m - 1, (3 * m - 4) // 2)
            else:
                return Fraction(m, (3 * m - 1) // 2)
    elif m % 2 == 0:
        return Fraction(m - 1, (3 * m - 4) // 2)
    else:
        return Fraction(m, (3 * m - 1) // 2)


def predecessor_of_pair_of_neighbors_in_Fm(m: int, successor: Fraction, right_neighbor_of_successor: Fraction,
                                           check_pair: bool) -> Fraction:
    # If check_pair == True, then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Fm
    # See Proposition 1.25 and Table 1.6 of the monograph. Call for instance:
    #    >>> predecessor_of_pair_of_neighbors_in_Fm(6, Fraction(1, 3),  Fraction(2, 5), True)
    # to get the result:
    #    Fraction(1, 4)
    # Also call:
    #    >>> predecessor_of_pair_of_neighbors_in_Fm(6, predecessor_in_Fm(6, Fraction(2, 5)), Fraction(2, 5), False)
    # to get the same result:
    #    Fraction(1, 4)
    if m < 1:
        # "N/A: Order m of the sequence should be > 0"
        return Fraction(1, -1)
    elif successor >= right_neighbor_of_successor:
        # "N/A: We should have successor < right_neighbor_of_successor"
        return Fraction(1, -2)
    elif (successor <= Fraction(0, 1)) or (successor >= Fraction(1, 1)):
        # "N/A: successor should be between (0/1) (excluded) and (1/1) (excluded)"
        return Fraction(1, -3)
    elif successor.denominator > m:
        # "N/A: Denominator of the successor should not exceed the order m of the sequence"
        return Fraction(1, -4)
    elif right_neighbor_of_successor > Fraction(1, 1):
        # "N/A: right_neighbor_of_successor should be between successor (excluded) and (1/1) (included)"
        return Fraction(1, -5)
    elif right_neighbor_of_successor.denominator > m:
        # "N/A: Denominator of the right_neighbor_of_successor should not exceed the order m of the sequence"
        return Fraction(1, -6)
    else:
        if (not check_pair) or (check_pair and (successor == predecessor_in_Fm(m, right_neighbor_of_successor))):
            farey_index = floor(
                (m + right_neighbor_of_successor.denominator) / successor.denominator)
            return Fraction(farey_index * successor.numerator - right_neighbor_of_successor.numerator,
                            farey_index * successor.denominator - right_neighbor_of_successor.denominator)
        else:
            # "N/A: The input pair is not a pair of neighboring fractions in this Farey sequence"
            return Fraction(1, -7)


def successor_of_pair_of_neighbors_in_Fm(m: int, left_neighbor_of_predecessor: Fraction, predecessor: Fraction,
                                         check_pair: bool) -> Fraction:
    # If check_pair == True, then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Fm
    # See Proposition 1.25 and Table 1.6 of the monograph. Call for instance:
    #    >>> successor_of_pair_of_neighbors_in_Fm(6, Fraction(3, 5),  Fraction(2, 3), True)
    # to get the result:
    #    Fraction(3, 4)
    # Also call:
    #    >>> successor_of_pair_of_neighbors_in_Fm(6, Fraction(3, 5), successor_in_Fm(6, Fraction(3, 5)),  False)
    # to get the same result:
    #    Fraction(3, 4)
    if m < 1:
        # "N/A: Order m of the sequence should be > 0"
        return Fraction(1, -1)
    elif left_neighbor_of_predecessor >= predecessor:
        # "N/A: We should have left_neighbor_of_predecessor < predecessor"
        return Fraction(1, -2)
    elif (predecessor <= Fraction(0, 1)) or (predecessor >= Fraction(1, 1)):
        # "N/A: predecessor should be between (0/1) (excluded) and (1/1) (excluded)"
        return Fraction(1, -3)
    elif predecessor.denominator > m:
        # "N/A: Denominator of the predecessor should not exceed the order m of the sequence"
        return Fraction(1, -4)
    elif left_neighbor_of_predecessor < Fraction(0, 1):
        # "N/A: left_neighbor_of_predecessor should be between (0/1) (included) and predecessor (excluded)"
        return Fraction(1, -5)
    elif left_neighbor_of_predecessor.denominator > m:
        # "N/A: Denominator of the left_neighbor_of_predecessor should not exceed the order m of the sequence"
        return Fraction(1, -6)
    else:
        if (not check_pair) or (check_pair and (predecessor == successor_in_Fm(m, left_neighbor_of_predecessor))):
            farey_index = floor(
                (m + left_neighbor_of_predecessor.denominator) / predecessor.denominator)
            return Fraction(farey_index * predecessor.numerator - left_neighbor_of_predecessor.numerator,
                            farey_index * predecessor.denominator - left_neighbor_of_predecessor.denominator)
        else:
            # "N/A: The input pair is not a pair of neighboring fractions in this Farey sequence"
            return Fraction(1, -7)


def predecessor_of_pair_of_neighbors_in_Fml(m: int, l: int, successor: Fraction, right_neighbor_of_successor: Fraction,
                                            check_pair: bool) -> Fraction:
    # If check_pair == True, then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Fml
    # See Proposition 1.26 (ii) (a) and Table 1.6 of the monograph. Call for instance:
    #    >>> predecessor_of_pair_of_neighbors_in_Fml(6, 4, Fraction(4, 5),  Fraction(1, 1), True)
    # to get the result:
    #    Fraction(3, 4)
    # Also call:
    #    >>> predecessor_of_pair_of_neighbors_in_Fml(6, 4, predecessor_in_Fml(6, 4, Fraction(1, 1)), Fraction(1, 1), False)
    # to get the same result:
    #    Fraction(3, 4)
    if m < 2:
        # "N/A: Parameter m of the sequence should be > 1"
        return Fraction(1, -1)
    elif (l <= 0) or (l >= m):
        # "N/A: Parameter l should be between 0 (excluded) and m (excluded)"
        return Fraction(1, -2)
    elif successor >= right_neighbor_of_successor:
        # "N/A: We should have successor < right_neighbor_of_successor"
        return Fraction(1, -3)
    elif (successor <= Fraction(0, 1)) or (successor >= Fraction(1, 1)):
        # "N/A: successor should be between (0/1) (excluded) and (1/1) (excluded)"
        return Fraction(1, -4)
    elif successor.denominator > m:
        # "N/A: Denominator of the successor should not exceed the parameter m of the sequence"
        return Fraction(1, -5)
    elif l < successor.numerator:
        # "N/A: Numerator of the successor should be between 1 (included) and l (included)"
        return Fraction(1, -6)
    elif right_neighbor_of_successor > Fraction(1, 1):
        # "N/A: right_neighbor_of_successor should be between successor (excluded) and (1/1) (included)"
        return Fraction(1, -7)
    elif right_neighbor_of_successor.denominator > m:
        # "N/A: Denominator of the right_neighbor_of_successor should not exceed the parameter m of the sequence"
        return Fraction(1, -8)
    elif l < right_neighbor_of_successor.numerator:
        # "N/A: Numerator of the right_neighbor_of_successor should be between 1 (included) and l (included)"
        return Fraction(1, -9)
    else:
        if (not check_pair) or (check_pair and (successor == predecessor_in_Fml(m, l, right_neighbor_of_successor))):
            if successor.numerator * m - successor.denominator * l >= 1:
                farey_index = floor(
                    (l + right_neighbor_of_successor.numerator) / successor.numerator)
                return Fraction(farey_index * successor.numerator - right_neighbor_of_successor.numerator,
                                farey_index * successor.denominator - right_neighbor_of_successor.denominator)
            else:
                farey_index = floor(
                    (m + right_neighbor_of_successor.denominator) / successor.denominator)
                return Fraction(farey_index * successor.numerator - right_neighbor_of_successor.numerator,
                                farey_index * successor.denominator - right_neighbor_of_successor.denominator)
        else:
            # "N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"
            return Fraction(1, -10)


def successor_of_pair_of_neighbors_in_Fml(m: int, l: int, left_neighbor_of_predecessor: Fraction, predecessor: Fraction,
                                          check_pair: bool) -> Fraction:
    # If check_pair == True, then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Fml
    # See Proposition 1.26 (ii) (a) and Table 1.6 of the monograph. Call for instance:
    #    >>> successor_of_pair_of_neighbors_in_Fml(6, 4, Fraction(3, 4),  Fraction(4, 5), True)
    # to get the result:
    #    Fraction(, )
    # Also call:
    #    >>> successor_of_pair_of_neighbors_in_Fml(6, 4, Fraction(3, 4), successor_in_Fml(6, 4, Fraction(3, 4)),  False)
    # to get the same result:
    #    Fraction(, )
    if m < 2:
        # "N/A: Parameter m of the sequence should be > 1"
        return Fraction(1, -1)
    elif (l <= 0) or (l >= m):
        # "N/A: Parameter l should be between 0 (excluded) and m (excluded)"
        return Fraction(1, -2)
    elif left_neighbor_of_predecessor >= predecessor:
        # "N/A: We should have left_neighbor_of_predecessor < predecessor"
        return Fraction(1, -3)
    elif (predecessor <= Fraction(0, 1)) or (predecessor >= Fraction(1, 1)):
        # "N/A: predecessor should be between (0/1) (excluded) and (1/1) (excluded)"
        return Fraction(1, -4)
    elif predecessor.denominator > m:
        # "N/A: Denominator of the predecessor should not exceed the parameter m of the sequence"
        return Fraction(1, -5)
    elif l < predecessor.numerator:
        # "N/A: Numerator of the predecessor should be between 1 (included) and l (included)"
        return Fraction(1, -6)
    elif left_neighbor_of_predecessor < Fraction(0, 1):
        # "N/A: left_neighbor_of_predecessor should be between (0/1) (included) and predecessor (excluded)"
        return Fraction(1, -7)
    elif left_neighbor_of_predecessor.denominator > m:
        # "N/A: Denominator of the left_neighbor_of_predecessor should not exceed the parameter m of the sequence"
        return Fraction(1, -8)
    elif l < left_neighbor_of_predecessor.numerator:
        # "N/A: Numerator of the left_neighbor_of_predecessor should be between 1 (included) and l (included)"
        return Fraction(1, -9)
    else:
        if (not check_pair) or (check_pair and (predecessor == successor_in_Fml(m, l, left_neighbor_of_predecessor))):
            if predecessor.denominator * l - predecessor.numerator * m >= 1:
                farey_index = floor(
                    (m + left_neighbor_of_predecessor.denominator) / predecessor.denominator)
                return Fraction(farey_index * predecessor.numerator - left_neighbor_of_predecessor.numerator,
                                farey_index * predecessor.denominator - left_neighbor_of_predecessor.denominator)
            else:
                farey_index = floor(
                    (l + left_neighbor_of_predecessor.numerator) / predecessor.numerator)
                return Fraction(farey_index * predecessor.numerator - left_neighbor_of_predecessor.numerator,
                                farey_index * predecessor.denominator - left_neighbor_of_predecessor.denominator)
        else:
            # "N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"
            return Fraction(1, -10)


def predecessor_of_pair_of_neighbors_in_Gml(m: int, l: int, successor: Fraction, right_neighbor_of_successor: Fraction,
                                            check_pair: bool) -> Fraction:
    # If ckeck_pair == True, then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Gml
    # See Proposition 1.27 (ii) (a) and Table 1.6 of the monograph. Call for instance:
    #    >>> predecessor_of_pair_of_neighbors_in_Gml(6, 4, Fraction(1, 2),  Fraction(3, 5), True)
    # to get the result:
    #    Fraction(1, 3)
    # Also call:
    #    >>> predecessor_of_pair_of_neighbors_in_Gml(6, 4, predecessor_in_Gml(6, 4, Fraction(3, 5)), Fraction(3, 5), False)
    # to get the same result:
    #    Fraction(1, 3)
    if m < 2:
        # "N/A: Parameter m of the sequence should be > 1"
        return Fraction(1, -1)
    elif (l <= 0) or (l >= m):
        # "N/A: Parameter l should be between 0 (excluded) and m (excluded)"
        return Fraction(1, -2)
    elif successor >= right_neighbor_of_successor:
        # "N/A: We should have successor < right_neighbor_of_successor"
        return Fraction(1, -3)
    elif (successor <= Fraction(0, 1)) or (successor >= Fraction(1, 1)):
        # "N/A: successor should be between (0/1) (excluded) and (1/1) (excluded)"
        return Fraction(1, -4)
    elif successor.denominator > m:
        # "N/A: Denominator of the successor should not exceed the parameter m of the sequence"
        return Fraction(1, -5)
    elif l + successor.denominator - m > successor.numerator:
        # "N/A: The quantity (l + denominator - m) should not exceed the numerator of the successor"
        return Fraction(1, -6)
    elif right_neighbor_of_successor > Fraction(1, 1):
        # "N/A: right_neighbor_of_successor should be between successor (excluded) and (1/1) (included)"
        return Fraction(1, -7)
    elif right_neighbor_of_successor.denominator > m:
        # "N/A: Denominator of the right_neighbor_of_successor should not exceed the parameter m of the sequence"
        return Fraction(1, -8)
    elif l + right_neighbor_of_successor.denominator - m > right_neighbor_of_successor.numerator:
        # "N/A: The quantity (l + denominator - m) should not exceed the numerator of the right_neighbor_of_successor"
        return Fraction(1, -9)
    else:
        if (not check_pair) or (check_pair and (successor == predecessor_in_Gml(m, l, right_neighbor_of_successor))):
            if successor.numerator * m - successor.denominator * l >= 1:
                farey_index = floor(
                    (m + right_neighbor_of_successor.denominator) / successor.denominator)
                return Fraction(farey_index * successor.numerator - right_neighbor_of_successor.numerator,
                                farey_index * successor.denominator - right_neighbor_of_successor.denominator)
            else:
                farey_index = floor((m - l + right_neighbor_of_successor.denominator -
                                     right_neighbor_of_successor.numerator) / (
                    successor.denominator - successor.numerator))
                return Fraction(farey_index * successor.numerator - right_neighbor_of_successor.numerator,
                                farey_index * successor.denominator - right_neighbor_of_successor.denominator)
        else:
            # "N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"
            return Fraction(1, -10)


def successor_of_pair_of_neighbors_in_Gml(m: int, l: int, left_neighbor_of_predecessor: Fraction, predecessor: Fraction,
                                          check_pair: bool) -> Fraction:
    # If check_pair == True, then to check whether the input pair is indeed a pair of neighboring fractions in the sequence Gml
    # See Proposition 1.27 (ii) (b) and Table 1.6 of the monograph. Call for instance:
    #    >>> successor_of_pair_of_neighbors_in_Gml(6, 4, Fraction(1, 3),  Fraction(1, 2), True)
    # to get the result:
    #    Fraction(3, 5)
    # Also call:
    #    >>> successor_of_pair_of_neighbors_in_Gml(6, 4, Fraction(1, 3), successor_in_Gml(6, 4, Fraction(1, 3)),  False)
    # to get the same result:
    #    Fraction(3, 5)
    if m < 2:
        # "N/A: Parameter m of the sequence should be > 1"
        return Fraction(1, -1)
    elif (l <= 0) or (l >= m):
        # "N/A: Parameter l should be between 0 (excluded) and m (excluded)"
        return Fraction(1, -2)
    elif left_neighbor_of_predecessor >= predecessor:
        # "N/A: We should have left_neighbor_of_predecessor < predecessor"
        return Fraction(1, -3)
    elif (predecessor <= Fraction(0, 1)) or (predecessor >= Fraction(1, 1)):
        # "N/A: predecessor should be between (0/1) (excluded) and (1/1) (excluded)"
        return Fraction(1, -4)
    elif predecessor.denominator > m:
        # "N/A: Denominator of the predecessor should not exceed the parameter m of the sequence"
        return Fraction(1, -5)
    elif l + predecessor.denominator - m > predecessor.numerator:
        # "N/A: The quantity (l + denominator - m) should not exceed the numerator of the predecessor"
        return Fraction(1, -6)
    elif left_neighbor_of_predecessor < Fraction(0, 1):
        # "N/A: left_neighbor_of_predecessor should be between (0/1) (included) and predecessor (excluded)"
        return Fraction(1, -7)
    elif left_neighbor_of_predecessor.denominator > m:
        # "N/A: Denominator of the left_neighbor_of_predecessor should not exceed the parameter m of the sequence"
        return Fraction(1, -8)
    elif l + left_neighbor_of_predecessor.denominator - m > left_neighbor_of_predecessor.numerator:
        # "N/A: The quantity (l + denominator - m) should not exceed the numerator of the left_neighbor_of_predecessor"
        return Fraction(1, -9)
    else:
        if (not check_pair) or (check_pair and (predecessor == successor_in_Gml(m, l, left_neighbor_of_predecessor))):
            if predecessor.denominator * l - predecessor.numerator * m >= 1:
                farey_index = floor(
                    (m - l + left_neighbor_of_predecessor.denominator - left_neighbor_of_predecessor.numerator) / (
                        predecessor.denominator - predecessor.numerator))
                return Fraction(farey_index * predecessor.numerator - left_neighbor_of_predecessor.numerator,
                                farey_index * predecessor.denominator - left_neighbor_of_predecessor.denominator)
            else:
                farey_index = floor(
                    (m + left_neighbor_of_predecessor.denominator) / predecessor.denominator)
                return Fraction(farey_index * predecessor.numerator - left_neighbor_of_predecessor.numerator,
                                farey_index * predecessor.denominator - left_neighbor_of_predecessor.denominator)
        else:
            # "N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"
            return Fraction(1, -10)


def predecessor_of_pair_of_neighbors_in_FBnm(n: int, m: int, successor: Fraction, right_neighbor_of_successor: Fraction,
                                             check_pair: bool) -> Fraction:
    # If check_pair == True, then to check whether the input pair is indeed a pair of neighboring fractions in the sequence FBnm
    # See Proposition 1.28 (ii) (a) and Table 1.6 of the monograph. Call for instance:
    #    >>> predecessor_of_pair_of_neighbors_in_FBnm(6, 4, Fraction(4, 5),  Fraction(1, 1), True)
    # to get the result:
    #    Fraction(3, 4)
    # Also call:
    #    >>> predecessor_of_pair_of_neighbors_in_FBnm(6, 4, predecessor_in_FBnm(6, 4, Fraction(1, 1)), Fraction(1, 1), False)
    # to get the same result:
    #    Fraction(3, 4)
    if n < 2:
        # "N/A: Parameter n of the sequence should be > 1"
        return Fraction(1, -1)
    elif (m < 1) or (m >= n):
        # "N/A: Parameter m of the sequence should be between 0 (excluded) and n (excluded)"
        return Fraction(1, -2)
    elif successor >= right_neighbor_of_successor:
        # "N/A: We should have successor < right_neighbor_of_successor"
        return Fraction(1, -3)
    elif (successor <= Fraction(0, 1)) or (successor >= Fraction(1, 1)):
        # "N/A: successor should be between (0/1) (excluded) and (1/1) (excluded)"
        return Fraction(1, -4)
    elif successor.denominator > n:
        # "N/A: Denominator of the successor should not exceed n"
        return Fraction(1, -5)
    elif (m + successor.denominator - n > successor.numerator) or (successor.numerator > m):
        # "N/A: Numerator of the successor should be between (m + denominator - n) (included) and m (included)"
        return Fraction(1, -6)
    elif right_neighbor_of_successor > Fraction(1, 1):
        # "N/A: right_neighbor_of_successor should be between successor (excluded) and (1/1) (included)"
        return Fraction(1, -7)
    elif right_neighbor_of_successor.denominator > n:
        # "N/A: Denominator of the right_neighbor_of_successor should not exceed n"
        return Fraction(1, -8)
    elif (m + right_neighbor_of_successor.denominator - n > right_neighbor_of_successor.numerator) or (
            right_neighbor_of_successor.numerator > m):
        # "N/A: Numerator of the right_neighbor_of_successor should be between (m + denominator - n) (included) and m (included)"
        return Fraction(1, -9)
    else:
        if (not check_pair) or (check_pair and (successor == predecessor_in_FBnm(n, m, right_neighbor_of_successor))):
            if successor.numerator * n - successor.denominator * m >= 1:
                farey_index = floor(
                    (m + right_neighbor_of_successor.numerator) / successor.numerator)
                return Fraction(farey_index * successor.numerator - right_neighbor_of_successor.numerator,
                                farey_index * successor.denominator - right_neighbor_of_successor.denominator)
            else:
                farey_index = floor((n - m + right_neighbor_of_successor.denominator -
                                     right_neighbor_of_successor.numerator) / (
                    successor.denominator - successor.numerator))
                return Fraction(farey_index * successor.numerator - right_neighbor_of_successor.numerator,
                                farey_index * successor.denominator - right_neighbor_of_successor.denominator)
        else:
            # "N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"
            return Fraction(1, -10)


def __predecessor_of_pair_of_neighbors_in_FB2mm(m: int, successor: Fraction, right_neighbor_of_successor: Fraction,
                                                check_pair: bool) -> Fraction:
    return predecessor_of_pair_of_neighbors_in_FBnm((2 * m), m, successor, right_neighbor_of_successor, check_pair)


def successor_of_pair_of_neighbors_in_FBnm(n: int, m: int, left_neighbor_of_predecessor: Fraction,
                                           predecessor: Fraction, check_pair: bool) -> Fraction:
    # If check_pair == True, then to check whether the input pair is indeed a pair of neighboring fractions in the sequence FBnm
    # See Proposition 1.28 (ii) (b) and Table 1.6 of the monograph. Call for instance:
    #    >>> successor_of_pair_of_neighbors_in_FBnm(6, 4, Fraction(1, 3),  Fraction(1, 2), True)
    # to get the result:
    #    Fraction(3, 5)
    # Also call:
    #    >>> successor_of_pair_of_neighbors_in_FBnm(6, 4, Fraction(1, 3), successor_in_FBnm(6, 4, Fraction(1, 3)),  False)
    # to get the same result:
    #    Fraction(3, 5)
    if n < 2:
        # "N/A: Parameter n of the sequence should be > 1"
        return Fraction(1, -1)
    elif (m < 1) or (m >= n):
        # "N/A: Parameter m of the sequence should be between 0 (excluded) and n (excluded)"
        return Fraction(1, -2)
    elif left_neighbor_of_predecessor >= predecessor:
        # "N/A: We should have left_neighbor_of_predecessor < predecessor"
        return Fraction(1, -3)
    elif (predecessor <= Fraction(0, 1)) or (predecessor >= Fraction(1, 1)):
        # "N/A: predecessor should be between (0/1) (excluded) and (1/1) (excluded)"
        return Fraction(1, -4)
    elif predecessor.denominator > n:
        # "N/A: Denominator of the predecessor should not exceed n"
        return Fraction(1, -5)
    elif (m + predecessor.denominator - n > predecessor.numerator) or (predecessor.numerator > m):
        # "N/A: Numerator of the predecessor should be between (m + denominator - n) (included) and m (included)"
        return Fraction(1, -6)
    elif left_neighbor_of_predecessor < Fraction(0, 1):
        # "N/A: left_neighbor_of_predecessor should be between (0/1) (included) and predecessor (excluded)"
        return Fraction(1, -7)
    elif left_neighbor_of_predecessor.denominator > n:
        # "N/A: Denominator of the left_neighbor_of_predecessor should not exceed n"
        return Fraction(1, -8)
    elif (m + left_neighbor_of_predecessor.denominator - n > left_neighbor_of_predecessor.numerator) or (
            left_neighbor_of_predecessor.numerator > m):
        # "N/A: Numerator of the left_neighbor_of_predecessor should be between (m + denominator - n) (included) and m (included)"
        return Fraction(1, -9)
    else:
        if (not check_pair) or (check_pair and (predecessor == successor_in_FBnm(n, m, left_neighbor_of_predecessor))):
            if predecessor.denominator * m - predecessor.numerator * n >= 1:
                farey_index = floor(
                    (n - m + left_neighbor_of_predecessor.denominator - left_neighbor_of_predecessor.numerator) / (
                        predecessor.denominator - predecessor.numerator))
                return Fraction(farey_index * predecessor.numerator - left_neighbor_of_predecessor.numerator,
                                farey_index * predecessor.denominator - left_neighbor_of_predecessor.denominator)
            else:
                farey_index = floor(
                    (m + left_neighbor_of_predecessor.numerator) / predecessor.numerator)
                return Fraction(farey_index * predecessor.numerator - left_neighbor_of_predecessor.numerator,
                                farey_index * predecessor.denominator - left_neighbor_of_predecessor.denominator)
        else:
            # "N/A: The input pair is not a pair of neighboring fractions in this Farey subsequence"
            return Fraction(1, -10)


def __successor_of_pair_of_neighbors_in_FB2mm(m: int, left_neighbor_of_predecessor: Fraction, predecessor: Fraction,
                                              check_pair: bool) -> Fraction:
    return successor_of_pair_of_neighbors_in_FBnm((2 * m), m, left_neighbor_of_predecessor, predecessor, check_pair)
