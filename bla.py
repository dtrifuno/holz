from itertools import combinations

prime_ranks = { "Two"   :  2,
                "Three" :  3,
                "Four"  :  5,
                "Five"  :  7,
                "Six"   : 11,
                "Seven" : 13,
                "Eight" : 17,
                "Nine"  : 19,
                "Ten"   : 23,
                "Jack"  : 29,
                "Queen" : 31,
                "King"  : 37,
                "Ace"   : 41 }

rank_to_enum = {}
enum_to_rank = {}
inverted = []
for rank, prime in prime_ranks.items():
    inverted.append((prime, rank))
inverted.sort()

i = 0
for tuple_ in inverted:
    _, rank = tuple_
    rank_to_enum[rank] = i
    enum_to_rank[i] = rank
    i += 1

suits = { "Spades"   : 0,
          "Hearts"   : 1,
          "Diamonds" : 2,
          "Clubs"    : 3}

def card_to_int(card):
    rank, suit = card
    return prime_ranks[rank] + (rank_to_enum[rank] << 8) \
            + (1 << (12 + suits[suit])) + (1 << (16 + rank_to_enum[rank]))


flushes_table = {}
uniques_table = {}
multiples_table = {}
threes_table = {}
val = 7760

def is_flush(c1, c2, c3, c4, c5):
    return c1 & c2 & c3 & c4 & c5 & 0xf000 != 0

def eval_flush(c1, c2, c3, c4, c5):
    return (c1 | c2 | c3 | c4 | c5) >> 16

def straight_flushes():
    suit = "Spades"
    global val

    # A high to 6 high straight flush
    for i in range(12, 3, -1):
        c1, c2, c3, c4, c5 = ((enum_to_rank[j], suit) for j in range(i, i-5, -1))
        i1, i2, i3, i4, i5 = map(card_to_int, (c1, c2, c3, c4, c5))
        flushes_table[eval_flush(i1, i2, i3, i4, i5)] = val
        #print(c1, c2, c3, c4, c5, val)
        val -= 1

    # Steel Wheel
    c1 = (enum_to_rank[12], suit)
    c2, c3, c4, c5 = ((enum_to_rank[j], suit) for j in range(0, 4))
    i1, i2, i3, i4, i5 = map(card_to_int, (c1, c2, c3, c4, c5))
    flushes_table[eval_flush(i1, i2, i3, i4, i5)] = val
    #print(c1, c2, c3, c4, c5, val)



def eval_mult(c1, c2, c3, c4, c5):
    return (c1 & 0xff) * (c2 & 0xff) * (c3 & 0xff) * (c4 & 0xff) * (c5 & 0xff)

def eval_mult3(i1, i2, i3):
    return (i1 & 0xff) * (i2 & 0xff) * (i3 & 0xff)


def quads():
    global val
    s1, s2, s3, s4, s5 = "Spades", "Hearts", "Diamonds", "Clubs", "Clubs"
    for i in range(12, -1, -1):
        for j in range(12, -1, -1):
            if i == j:
                continue
            val -= 1
            rank  = enum_to_rank[i]
            rank_ = enum_to_rank[j]
            c1 = (rank, s1)
            c2 = (rank, s2)
            c3 = (rank, s3)
            c4 = (rank, s4)
            c5 = (rank_, s5)
            i1, i2, i3, i4, i5 = map(card_to_int, (c1, c2, c3, c4, c5))
            multiples_table[eval_mult(i1, i2, i3, i4, i5)] = val
            #print(c1, c2, c3, c4, c5, val)


def full_houses():
    s1, s2, s3, s4, s5 = "Spades", "Hearts", "Diamonds", "Clubs", "Spades"
    global val

    for i in range(12, -1, -1):
        for j in range(12, -1, -1):
            if i == j:
                continue
            val -= 1
            rank  = enum_to_rank[i]
            rank_ = enum_to_rank[j]
            c1 = (rank, s1)
            c2 = (rank, s2)
            c3 = (rank, s3)
            c4 = (rank_, s4)
            c5 = (rank_, s5)
            i1, i2, i3, i4, i5 = map(card_to_int, (c1, c2, c3, c4, c5))
            multiples_table[eval_mult(i1, i2, i3, i4, i5)] = val
            #print(c1, c2, c3, c4, c5, val)

#
def flushes():
    suit = "Spades"
    global val

    for r1 in range(12, -1, -1):
        for r2 in range(r1-1, -1, -1):
            for r3 in range(r2-1, -1, -1):
                for r4 in range(r3-1, -1, -1):
                    for r5 in range(r4-1, -1, -1):
                        c1 = (enum_to_rank[r1], suit)
                        c2 = (enum_to_rank[r2], suit)
                        c3 = (enum_to_rank[r3], suit)
                        c4 = (enum_to_rank[r4], suit)
                        c5 = (enum_to_rank[r5], suit)
                        i1, i2, i3, i4, i5 = map(card_to_int, (c1, c2, c3, c4, c5))
                        key = eval_flush(i1, i2, i3, i4, i5)
                        if key in flushes_table:
                            continue
                        val -= 1
                        #print(c1, c2, c3, c4, c5, val)
                        flushes_table[key] = val

def straights():
    suit1 = "Spades"
    suit2 = "Hearts"
    global val

    # A high to 6 high straight flush
    for i in range(12, 3, -1):
        val -= 1
        c1, c2, c3, c4 = ((enum_to_rank[j], suit1) for j in range(i, i-4, -1))
        c5 = (enum_to_rank[i-4], suit2)
        i1, i2, i3, i4, i5 = map(card_to_int, (c1, c2, c3, c4, c5))
        uniques_table[eval_flush(i1, i2, i3, i4, i5)] = val
        #print(c1, c2, c3, c4, c5, val)

    # Steel Wheel
    val -= 1
    c1 = (enum_to_rank[12], suit1)
    c2, c3, c4, c5 = ((enum_to_rank[j], suit2) for j in range(0, 4))
    i1, i2, i3, i4, i5 = map(card_to_int, (c1, c2, c3, c4, c5))
    uniques_table[eval_flush(i1, i2, i3, i4, i5)] = val
    print(c1, c2, c3, c4, c5, val)

def sets():
    s1, s2, s3 = "Spades", "Hearts", "Diamonds"
    global val

    for r1 in range(12, -1, -1):
        c1, c2, c3 = (enum_to_rank[r1], s1), (enum_to_rank[r1], s2), (enum_to_rank[r1], s3)
        for r2 in range(12, -1, -1):
            if r1 == r2:
                continue
            for r3 in range(r2-1, -1, -1):
                if r1 == r3:
                    continue
                val -= 1
                c4 = (enum_to_rank[r2], s1)
                c5 = (enum_to_rank[r3], s1)
                i1, i2, i3, i4, i5 = map(card_to_int, (c1, c2, c3, c4, c5))
                print(c1, c2, c3, c4, c5, val)
                multiples_table[eval_mult(i1, i2, i3, i4, i5)] = val
        val -= 1
        print(c1, c2, c3, val)
        i1, i2, i3 = map(card_to_int, (c1, c2, c3))
        threes_table[eval_mult3(i1, i2, i3)] = val
        print(c1, c2, c3, val)

def two_pairs():
    s1, s2 = "Spades", "Hearts"
    global val

    for r1 in range(12, -1, -1):
        for r2 in range(r1-1, -1, -1):
            for r3 in range(12, -1, -1):
                if r1 == r3 or r2 == r3:
                    continue
                val -= 1
                c1, c2 = (enum_to_rank[r1], s1), (enum_to_rank[r1], s2),
                c3, c4 = (enum_to_rank[r2], s1), (enum_to_rank[r2], s2)
                c5 = enum_to_rank[r3], s1
                i1, i2, i3, i4, i5 = map(card_to_int, (c1, c2, c3, c4, c5))
                # print(c1, c2, c3, c4, c5, val)
                multiples_table[eval_mult(i1, i2, i3, i4, i5)] = val

def pairs():
    s1, s2 = "Spades", "Hearts"
    global val

    for r1 in range(12, -1, -1):
        for r2 in range(12, -1, -1):
            if r1 == r2:
                continue
            for r3 in range(r2-1, -1, -1):
                if r1 == r3:
                    continue
                for r4 in range(r3-1, -1, -1):
                    if r1 == r4:
                        continue
                    val -= 1
                    c1, c2 = (enum_to_rank[r1], s1), (enum_to_rank[r1], s2),
                    c3, c4, c5 = (enum_to_rank[r2], s1), (enum_to_rank[r3], s1), (enum_to_rank[r4], s1)
                    i1, i2, i3, i4, i5 = map(card_to_int, (c1, c2, c3, c4, c5))
                    multiples_table[eval_mult(i1, i2, i3, i4, i5)] = val
                    print(c1, c2, c3, c4, c5, val)
            val -= 1
            c1, c2, c3 = (enum_to_rank[r1], s1), (enum_to_rank[r1], s2), (enum_to_rank[r3], s1)
            print(c1, c2, c3, val)
            i1, i2, i3 = map(card_to_int, (c1, c2, c3))
            threes_table[eval_mult3(i1, i2, i3)] = val

def high_cards():
    s1, s2 = "Spades", "Hearts"
    global val

    for r1 in range(12, 1, -1):
        for r2 in range(r1-1, 0, -1):
            for r3 in range(r2-1, -1, -1):
                c1 = (enum_to_rank[r1], s1)
                c2 = (enum_to_rank[r2], s1)
                c3 = (enum_to_rank[r3], s2)
                if (r3 > 1):
                    for r4 in range(r3-1, -1, -1):
                        for r5 in range(r4-1, -1, -1):
                            c4 = (enum_to_rank[r4], s1)
                            c5 = (enum_to_rank[r5], s1)
                            i1, i2, i3, i4, i5 = map(card_to_int, (c1, c2, c3, c4, c5))
                            key = eval_flush(i1, i2, i3, i4, i5)
                            if key in uniques_table:
                                continue # skip if it is a straight
                            val -= 1
                            uniques_table[eval_flush(i1, i2, i3, i4, i5)] = val
                            print(c1, c2, c3, c4, c5, val)
                val -= 1
                i1, i2, i3 = map(card_to_int, (c1, c2, c3))
                threes_table[eval_mult3(i1, i2, i3)] = val
                print(c1, c2, c3, val)


straight_flushes()
quads()
full_houses()
flushes()
straights()
sets()
two_pairs()
pairs()
#high_cards()

"""
with open('bla.hs', 'w') as fp:
    max_key = max(flushes_table.keys())
    for i in range(0, max_key+1):
        val = flushes_table.get(i, 0)
        fp.write("{}, ".format(val))

    fp.write('\n')
    max_key = max(uniques_table.keys())
    for i in range(0, max_key+1):
        val = uniques_table.get(i, 0)
        fp.write("{}, ".format(val))

    fp.write('\n')
    max_key = max(multiples_table.keys())
    for i in range(0, max_key+1):
        if i in multiples_table:
            fp.write("({}, {}), ".format(i, multiples_table[i]))
"""
