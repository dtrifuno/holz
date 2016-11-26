=========
Holz
=========


Open-face Chinese poker (`OFC <en.wikipedia.org/wiki/Open-face_Chinese_poker`_) is a game where players set three rows of cards (bottom, middle and top) with the goal of making the best poker hand in each row. Each player starts with initial five cards to set and are afterwards dealt one card a time. Holz uses Monte Carlo simulation to compute the expected value of playing a card in a particular row.

Holz is still in very early stages of development. Use at your own risk.

Installation
=====================

To install just clone the repository and run cabal.::
    git clone git@github.com/dtrifuno/holz.git
    cabal build
    cabal install

Usage
=====================

Just run the executable holz and follow the prompts. Here's an example of a Holz session.::
    FIXME


TODO
=====================

These are the features I am currently working on.

* **GUI.** Holz currently uses a CLI, which makes it awkward to use due to the amount of data the user has to enter for each simulation. I am planning on writing a GUI for it once I settle on Haskell GUI library.

* **Profile and optimize.** I have not done any optimization on the hand evaluation algorithm. Currently it is just fast enough to be usable, but optimization should make it more accurate and responsive.

* **Increase the number of supported players to 4.** Currently only supports two players.

* **Increase test coverage.** Test coverage is minimal at the moment (a dozen doctests only). More tests are needed, especially for the hand evaluation module which is awkward to doctest.

WONTDO
=====================

* **Initial setting.** Currently you can only add cards one at a time, which means you cannot use Holz to set your starting five cards. Finding an initial setting is computationally expensive (about 200 choices as opposed to 3), so this is likely to be difficult.

* **Pineapple.** Holz will only support standard OFC poker, not the Pineapple variant.
