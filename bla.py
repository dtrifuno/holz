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



def is_flush(c1, c2, c3, c4, c5):
    return c1 & c2 & c3 & c4 & c5 & 0xf000 != 0

def eval_flush(c1, c2, c3, c4, c5):
    return (c1 | c2 | c3 | c4 | c5) >> 16

def straight_flushes():
    val = 0
    suit = "Spades"
    for i in range(12, 4, -1):
        c1, c2, c3, c4, c5 = (card_to_int((enum_to_rank[j], suit)) for j in range(i, i-5, -1))
        print(eval_flush(c1, c2, c3, c4, c5), val)
        val += 1
    # wheel
    c1 = card_to_int((enum_to_rank[12], suit))
    c2, c3, c4, c5 = (card_to_int((enum_to_rank[j], suit)) for j in range(0, 4))
    print(eval_flush(c1, c2, c3, c4, c5), val)


def eval_mult(c1, c2, c3, c4, c5):
    (c1 & 0xff) * (c2 & 0xff) * (c3 & 0xff) * (c4 & 0xff) * (c5 & 0xff)

def quads():
    val = 0
    s1, s2, s3, s4, s5 = "Spades", "Hearts", "Diamonds", "Clubs", "Clubs"
    for i in range(12, -1, -1):
        for j in range(12, -1, -1):
            if i == j:
                continue
            val += 1
            rank  = enum_to_rank[i]
            rank_ = enum_to_rank[j]
            c1 = (rank, s1)
            c2 = (rank, s2)
            c3 = (rank, s3)
            c4 = (rank, s4)
            c5 = (rank_, s5)
            #print((c1, c2, c3, c4, c5))
            i1, i2, i3, i4, i5 = map(card_to_int, (c1, c2, c3, c4, c5))

def full_houses():
    val = 0
    s1, s2, s3, s4, s5 = "Spades", "Hearts", "Diamonds", "Clubs", "Clubs"
    for i in range(12, -1, -1):
        for j in range(12, -1, -1):
            if i == j:
                continue
            val += 1
            rank  = enum_to_rank[i]
            rank_ = enum_to_rank[j]
            c1 = (rank, s1)
            c2 = (rank, s2)
            c3 = (rank, s3)
            c4 = (rank_, s4)
            c5 = (rank_, s5)
            print((c1, c2, c3, c4, c5))
            i1, i2, i3, i4, i5 = map(card_to_int, (c1, c2, c3, c4, c5))

