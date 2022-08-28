from fractions import Fraction
from fareysequences import (predecessor_in_Fm,
                            successor_in_Fm,
                            predecessor_in_Fml,
                            successor_in_Fml,
                            predecessor_in_Gml,
                            successor_in_Gml,
                            predecessor_in_FBnm,
                            successor_in_FBnm,
                            predecessor_of_pair_of_neighbors_in_Fm,
                            successor_of_pair_of_neighbors_in_Fm,
                            predecessor_of_pair_of_neighbors_in_Fml,
                            successor_of_pair_of_neighbors_in_Fml,
                            predecessor_of_pair_of_neighbors_in_Gml,
                            successor_of_pair_of_neighbors_in_Gml,
                            predecessor_of_pair_of_neighbors_in_FBnm,
                            successor_of_pair_of_neighbors_in_FBnm)


def main():
    print("predecessor_in_Fm(6, Fraction(2, 3))      returns:     ",
          predecessor_in_Fm(6, Fraction(2, 3)))
    print("successor_in_Fm(6, Fraction(1, 3))      returns:     ",
          successor_in_Fm(6, Fraction(1, 3)), "\n")
#
    print("predecessor_in_Fml(6, 4, Fraction(1, 1))      returns:     ",
          predecessor_in_Fml(6, 4, Fraction(1, 1)))
    print("successor_in_Fml(6, 4, Fraction(4, 5))      returns:     ",
          successor_in_Fml(6, 4, Fraction(4, 5)), "\n")
#
    print("predecessor_in_Gml(6, 4, Fraction(1, 3))      returns:     ",
          predecessor_in_Gml(6, 4, Fraction(1, 3)))
    print("successor_in_Gml(6, 4, Fraction(1, 3))      returns:     ",
          successor_in_Gml(6, 4, Fraction(1, 3)), "\n")
#
    print("predecessor_in_FBnm(6, 4, Fraction(3, 4))      returns:     ",
          predecessor_in_FBnm(6, 4, Fraction(3, 4)))
    print("successor_in_FBnm(6, 4, Fraction(4, 5))      returns:     ",
          successor_in_FBnm(6, 4, Fraction(4, 5)), "\n")
#
    print("predecessor_of_pair_of_neighbors_in_Fm (6, Fraction(1, 3),  Fraction(2, 5), True)      returns:     ",
          predecessor_of_pair_of_neighbors_in_Fm(6, Fraction(1, 3),  Fraction(2, 5), True))
    print("predecessor_of_pair_of_neighbors_in_Fm (6, predecessor_in_Fm(6, Fraction(2, 5)), Fraction(2, 5), False)      returns:     ",
          predecessor_of_pair_of_neighbors_in_Fm(6, predecessor_in_Fm(6, Fraction(2, 5)), Fraction(2, 5), False))
    print("successor_of_pair_of_neighbors_in_Fm (6, Fraction(3, 5),  Fraction(2, 3), True)      returns:     ",
          successor_of_pair_of_neighbors_in_Fm(6, Fraction(3, 5),  Fraction(2, 3), True))
    print("successor_of_pair_of_neighbors_in_Fm (6, Fraction(3, 5), successor_in_Fm(6, Fraction(3, 5)),  False)      returns:     ",
          successor_of_pair_of_neighbors_in_Fm(6, Fraction(3, 5), successor_in_Fm(6, Fraction(3, 5)),  False), "\n")
#
    print("predecessor_of_pair_of_neighbors_in_Fml (6, 4, Fraction(4, 5),  Fraction(1, 1), True)      returns:     ",
          predecessor_of_pair_of_neighbors_in_Fml(6, 4, Fraction(4, 5),  Fraction(1, 1), True))
    print("predecessor_of_pair_of_neighbors_in_Fml (6, 4, predecessor_in_Fml(6, 4, Fraction(1, 1)), Fraction(1, 1), False)      returns:     ",
          predecessor_of_pair_of_neighbors_in_Fml(6, 4, predecessor_in_Fml(6, 4, Fraction(1, 1)), Fraction(1, 1), False))
    print("successor_of_pair_of_neighbors_in_Fml (6, 4, Fraction(3, 4),  Fraction(4, 5), True)      returns:     ",
          successor_of_pair_of_neighbors_in_Fml(6, 4, Fraction(3, 4),  Fraction(4, 5), True))
    print("successor_of_pair_of_neighbors_in_Fml (6, 4, Fraction(3, 4), successor_in_Fml(6, 4, Fraction(3, 4)),  False)      returns:     ",
          successor_of_pair_of_neighbors_in_Fml(6, 4, Fraction(3, 4), successor_in_Fml(6, 4, Fraction(3, 4)),  False), "\n")
#
    print("predecessor_of_pair_of_neighbors_in_Gml (6, 4, Fraction(1, 2),  Fraction(3, 5), True)      returns:     ",
          predecessor_of_pair_of_neighbors_in_Gml(6, 4, Fraction(1, 2),  Fraction(3, 5), True))
    print("predecessor_of_pair_of_neighbors_in_Gml (6, 4, predecessor_in_Gml(6, 4, Fraction(3, 5)), Fraction(3, 5), False)      returns:     ",
          predecessor_of_pair_of_neighbors_in_Gml(6, 4, predecessor_in_Gml(6, 4, Fraction(3, 5)), Fraction(3, 5), False))
    print("successor_of_pair_of_neighbors_in_Gml(6, 4, Fraction(1, 3),  Fraction(1, 2), True)      returns:     ",
          successor_of_pair_of_neighbors_in_Gml(6, 4, Fraction(1, 3),  Fraction(1, 2), True))
    print("successor_of_pair_of_neighbors_in_Gml(6, 4, Fraction(1, 3), successor_in_Gml(6, 4, Fraction(1, 3)),  False)      returns:     ",
          successor_of_pair_of_neighbors_in_Gml(6, 4, Fraction(1, 3), successor_in_Gml(6, 4, Fraction(1, 3)),  False), "\n")
#
    print("predecessor_of_pair_of_neighbors_in_FBnm(6, 4, Fraction(4, 5),  Fraction(1, 1), True)      returns:     ",
          predecessor_of_pair_of_neighbors_in_FBnm(6, 4, Fraction(4, 5),  Fraction(1, 1), True))
    print("predecessor_of_pair_of_neighbors_in_FBnm(6, 4, predecessor_in_FBnm(6, 4, Fraction(1, 1)), Fraction(1, 1), False)      returns:     ",
          predecessor_of_pair_of_neighbors_in_FBnm(6, 4, predecessor_in_FBnm(6, 4, Fraction(1, 1)), Fraction(1, 1), False))
    print("successor_of_pair_of_neighbors_in_FBnm(6, 4, Fraction(1, 3),  Fraction(1, 2), True)      returns:     ",
          successor_of_pair_of_neighbors_in_FBnm(6, 4, Fraction(1, 3),  Fraction(1, 2), True))
    print("successor_of_pair_of_neighbors_in_FBnm(6, 4, Fraction(1, 3), successor_in_FBnm(6, 4, Fraction(1, 3)),  False)      returns:     ",
          successor_of_pair_of_neighbors_in_FBnm(6, 4, Fraction(1, 3), successor_in_FBnm(6, 4, Fraction(1, 3)),  False), "\n")


if __name__ == "__main__":
    main()
