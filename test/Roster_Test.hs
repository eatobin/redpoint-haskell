module Roster_Test where

import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Roster
import           Test.HUnit

plr1 = Player {name = "Ringo Starr", giftHist = [GiftPair {giver = "GeoHar", givee = "JohLen"}]}
plr2 = Player {name = "John Lennon", giftHist = [GiftPair {giver = "RinSta", givee = "PauMcc"}]}
rstr1 = Map.fromList [("RinSta", plr1), ("JohLen", plr2)]
johLen = rstr1 ! "JohLen"
--
-- testMakeBorrower = (~=?)
--   br1
--   (makeBorrower "Borrower1" 1)
--
-- testGetName = (~=?)
--   "Borrower1"
--   (getName br1)
--
-- testSetName = (~=?)
--   br1
--   (setName "Borrower1" (Borrower "Jack" 1))
--
-- testGetMaxBooks = (~=?)
--   1
--   (getMaxBooks br1)
--
-- testSetMaxBooks = (~=?)
--   Borrower {name = "Borrower1", maxBooks = 11}
--   (setMaxBooks 11 br1)
--
-- testBorrowerToString = (~=?)
--   "Borrower1 (1 books)"
--   (borrowerToString br1)
--
-- borrowerTests = TestList [ testMakeBorrower, testGetName
--                        , testSetName, testGetMaxBooks
--                        , testSetMaxBooks, testBorrowerToString ]
--
-- runBorrowerTests = runTestTT $ TestList [ borrowerTests ]
