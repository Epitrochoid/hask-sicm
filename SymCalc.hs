{-# LANGUAGE RecordWildCards #-}

-- Symbolic calculus library using the notation from
-- Structure and Interpretation of Classical Mechanics
-- by Gerald Sussman and Jack Wisdom

-- Code is based off both
-- http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
-- and https://github.com/hepek/Ramblings/blob/master/symb.lhs


module SymCalc
    ( Expr(..)
    ) where

import Data.Either

infixl 4 :+:
infixl 5 :/:, :*:
infixr 6 :^:

data Expr a = Atom a
            | (Expr a) :+: (Expr a)
            | (Expr a) :-: (Expr a)
            | (Expr a) :/: (Expr a)
            | (Expr a) :*: (Expr a)
            | (Expr a) :^: (Expr a)
            | Neg (Expr a)
            | Sin (Expr a)
            | Cos (Expr a)
            | Tan (Expr a)
            | Exp (Expr a)
            | Ln (Expr a)
            | Inv (Expr a)
            | E
            | Const a
            | Symbol String
            deriving (Eq)

instance (Num a) => Num (Expr a) where
        (+) = (:+:)
        (-) = (:-:)
        (*) = (:*:)
        negate = Neg
        signum = undefined
        abs = undefined
        fromInteger a = Atom (fromInteger a)

instance (Floating a) => Fractional (Expr a) where
        a / b = a :/: b
        fromRational a = Atom (fromRational a)

instance (Floating a) => Floating (Expr a) where
        pi = Atom pi
        exp = Exp
        sqrt a = a :^: (1/2)
        log = Ln
        sin = Sin
        cos = Cos
        tan a = (Sin a) :/: (Cos a)
        (**) = (:^:)
        logBase a b = (Ln a) :/: (Ln b)
        asin = undefined
        atan = undefined
        acos = undefined
        sinh = undefined
        cosh = undefined
        asinh = undefined
        atanh = undefined
        acosh = undefined

instance (Show a) => Show (Expr a) where
        show (Atom a) = show a
        show (Symbol a) = filter (not . (== '"')) (show a)
        show (a :*: b) = "(" ++ show a ++ "*" ++ show b ++ ")"
        show (a :/: b) = "(" ++ show a ++ "/" ++ show b ++ ")"
        show (a :+: b) = "(" ++ show a ++ "+" ++ show b ++ ")"
        show (a :-: b) = "(" ++ show a ++ "-" ++ show b ++ ")"
        show (a :^: b) = "(" ++ show a ++ "^" ++ show b ++ ")"
        show (Neg a) = "(-" ++ show a ++ ")"
        show (Sin a) = "sin(" ++ show a ++ ")"
        show (Cos a) = "cos(" ++ show a ++ ")"
        show (Tan a) = "tan(" ++ show a ++ ")"
        show (Exp a) = "exp(" ++ show a ++ ")"
        show (Ln a) = "ln(" ++ show a ++ ")"
        show (Inv a) = "(1/" ++ show a ++ ")"
        show E = "e"
        show (Const a) = show a


