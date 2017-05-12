def leaf(var_name):
    return { "expression_type": "literal", "var_name": var_name }

def bind(var_name, exp):
    return { "expression_type": "lambda", "bound_var": leaf(var_name), "expression": exp }

def appl(exp1, exp2):
    return { "expression_type": "application", "left": exp1, "right": exp2 }

v = leaf
l = bind
c = appl

iden = lambda x: l(x, v(x))

zero = l(s, iden(z))
one = l(s, c(s, iden(z)))
two = l(s, c(s, c(s, iden(z))))

succ = lambda n: l("x", l("y", l("z", c(v("y"), c(c(v("w"), v("y")), v("x"))))))
