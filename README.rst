=========
Holz
=========


Open-face Chinese poker (`OFC <https://en.wikipedia.org/wiki/Open-face_Chinese_poker>`_) is a game where players set three rows of cards (bottom, middle and top) with the goal of making the best poker hand in every row. Each player starts with an initial five cards to set and are afterwards dealt one card at a time. Players receive royalties (extra points) for particularly strong hands, but lose all points (foul) the round if one of their higher rows is stronger than a lower row. Pursuing larger royalties increases the risk of fouling, so a good player has to be able to determine if the possible reward of a higher royalty is worth the risk of fouling. Holz assists players with difficult decisions by using `Monte Carlo simulation <https://en.wikipedia.org/wiki/Monte_Carlo_method>`_ with a variant of `Cactus Kev's fast hand evaluation algorithm <http://suffe.cool/poker/evaluator.html>`_ to compute the expected value of playing a card on a particular row.

Holz is still in very early stages of development. Use at your own risk.

Installation
=====================

To install just clone the repository and run cabal.

::

    git clone git@github.com/dtrifuno/holz.git
    cabal build
    cabal install

Usage
=====================

Run the executable holz in a console and follow the directions in the prompts. Here's an example of a Holz session.

::

    dtrifuno@ubuntu:~/haskell/holz$ cabal run
    Preprocessing executable 'holz' for holz-0.1.0.0...
    Running holz...
    Input cards as a comma-separated list in shorthand notation. (example: Ah,Th,5h)

    Your Bottom row: Ah,As,Qd
    Your Middle row: 8s,7c
    Your Top row: 4c
    Opponent's Bottom row: 7s,Qs,3s
    Opponent's Middle row: Kd,Jc
    Opponent's Top row: 8c
    Card you are about to play: 5c

    Bottom: 0.119225
    Middle: 0.158675
    Top:    -0.342215

TODO
=====================

These are the features I am currently working on.

* **GUI.** Holz currently uses a CLI, which makes it awkward to use due to the amount of data the user has to enter for each decision. I am planning on writing a GUI for it once I settle on a Haskell GUI library.

* **Profile and optimize.** I have not done any optimization on the hand evaluation algorithm. Currently it is just fast enough to be usable, but optimization should make it more accurate and responsive. In particular, Holz currently uses a single thread but can easily be made parallel.

* **Increase the number of supported players to 4.** Currently only supports two players.

* **Increase test coverage.** Test coverage is minimal at the moment (a dozen doctests only). More tests are needed, especially for the hand evaluation module which is awkward to doctest.

WONTDO
=====================

These are the features I am very unlikely to add.

* **Initial setting.** Currently you can only add cards one at a time, which means you cannot use Holz to set your starting five cards. Finding an initial setting is computationally expensive (about 200 choices as opposed to 3), so this is likely to be difficult barring major gains in optimization.

* **Pineapple.** Holz will only support standard OFC poker, not the Pineapple variant.
