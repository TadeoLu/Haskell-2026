module TestGuia5 where

import Test.HUnit
import Guia5 

correrTest = runTestTT testSumarN

testSumarN = test [ "eSumar5" ~: (sumarN 5 ([1,2,3])) ~?= ([6,7,8]),"eSumar10" ~: (sumarN 10 ([-1,2,3])) ~?= ([9,12,13]) ]

