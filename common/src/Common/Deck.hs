module Common.Deck
    ( Deck
    , fibonacci
    )
where

import           Common.Card (Card (..))

type Deck = [Card]

fibonacci :: Deck
fibonacci =
    [ One
    , Two
    , Three
    , Five
    , Eight
    , Thirteen
    , Twenty
    , Forty
    , OneHundred
    , Unknown
    , Infinity
    ]

-- 1,2,3,5,8,13,20,40,100
-- 1,2,3,5,8,13,20,40,?
-- 0,1,2,4,8,16,32,64
-- 1,2,4,8,12,16,24,40,80
-- ☕,1,2,3,5,8,13,20,?
-- XS,S,M,L,XL,XXL,?
-- 1,2,5,10,20,50,100
