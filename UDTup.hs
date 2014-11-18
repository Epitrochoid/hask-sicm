import SymCalc

data UDTup a = Up (Expr a)
             | Up (Expr a, Expr a)
             | Up (Expr a, Expr a)
             | Down (Expr a)
             | Down (Expr a, Expr a)
             | Down (Expr a, Expr a, Expr a)


