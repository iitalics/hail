import hail.expr.functions as hl
from hail.expr.expressions.expression_typecheck import *
from hail.typecheck import *

@typecheck(string=expr_str)
def reverse(string):
    return hl.rbind(
        string,
        lambda s: hl.delimit(
            hl.range(hl.len(s) - 1, -1, -1).map(lambda i: s[i]),
            delimiter = ''))

@typecheck(string=expr_str, table=expr_dict(expr_str, expr_str))
def translate(string, table):
    return hl.rbind(
        string,
        lambda s: hl.rbind(
            table,
            lambda tb: hl.delimit(
                hl.range(hl.len(s)).map(lambda i: s[i]).map(lambda c: tb.get(c, c)),
                delimiter = '')))
