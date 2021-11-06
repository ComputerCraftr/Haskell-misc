module Lib
    ( someFunc
    ) where

import Karatsuba
    ( karatsubaMultiplyRecursive, integerDigitsBase256 )
import MonadTest ( lastThreeElementsLessThan100 )
import IOTest (getTickets, printTickets)

someFunc :: IO ()
someFunc = do
    print (karatsubaMultiplyRecursive 4728407613652922552759072682000430063038909260147253520810242655 6600837613863385147035699626875158239463295294103558148787165987)
    print (karatsubaMultiplyRecursive 70058738427051160334746874429252579119928572246444725963276835611 86519327888246410496398575518280509016134914626047673943654504547)
    print (karatsubaMultiplyRecursive 238731412424945154431470551094200359684685955096667820591767288323 503689947939754385148039592295214272825977641172541362111833935209)
    print (integerDigitsBase256 65536)
    print (lastThreeElementsLessThan100 [3242, 50, 243, 50, 70, 50, 90])
    print (lastThreeElementsLessThan100 [3242, 50, 243, 50, 700, 50, 90])
    sectionTickets <- getTickets
    printTickets sectionTickets
