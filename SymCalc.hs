-- Symbolic calculus library using the notation from
-- Structure and Interpretation of Classical Mechanics
-- by Gerald Sussman and Jack Wisdom

-- Code is based off both
-- http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
-- and https://github.com/hepek/Ramblings/blob/master/symb.lhs


module SymCalc
    (
    ) where

import Data.Either

infixl 4 :+:
infixl 5 :/:, :*:
infixr 6 :^:

data Expr = Atom a
          | (Expr a) :+: (Expr a)
          | (Expr a) :/: (Expr a)
          | (Expr a) :*: (Expr a)
          | (Expr a) :^: (Expr a)
          | Neg (Expr a)
          | Sin (Expr a)
          | Cos (Expr a)
          | Tan (Expr a)
          | Exp (Expr a)
          | Ln (Expr a)
          | E
          | Symbol String
          deriving (eq)

instance (Num a) => Num (Expr a) where
        (+) = (:+:)
        (-) = flip (:+:) . Neg
        (*) = (:*:)
        negate = Neg
        signum = undefined
        abs = undefined
        fromInteger a = Atom (fromInteger a)

