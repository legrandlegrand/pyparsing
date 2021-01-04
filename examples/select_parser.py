# select_parser.py
# Copyright 2010,2019 Paul McGuire
#
# a simple SELECT statement parser, taken from SQLite's SELECT statement
# definition at https://www.sqlite.org/lang_select.html
#
import sys
from pyparsing import *
#sys.setrecursionlimit(10000)

ParserElement.enablePackrat()

LPAR, RPAR, COMMA = map(Suppress, "(),")
DOT, STAR = map(Literal, ".*")
select_stmt = Forward().setName("select statement")

# keywords
keywords = {
    k: CaselessKeyword(k)
    for k in """\
    UNION ALL AND INTERSECT EXCEPT COLLATE ASC DESC ON USING NATURAL INNER CROSS LEFT OUTER JOIN AS INDEXED NOT
    SELECT DISTINCT FROM WHERE GROUP BY HAVING ORDER LIMIT OFFSET OR CAST ISNULL NOTNULL NULL IS BETWEEN ELSE END
    CASE WHEN THEN EXISTS IN LIKE GLOB REGEXP MATCH ESCAPE CURRENT_TIME CURRENT_DATE CURRENT_TIMESTAMP TRUE FALSE
    """.split()
}
vars().update(keywords)

any_keyword = MatchFirst(keywords.values())

quoted_identifier = QuotedString('"', escQuote='""')
identifier = (~any_keyword + Word(alphas, alphanums + "_")).setParseAction(
    pyparsing_common.downcaseTokens
) | quoted_identifier
collation_name = identifier.copy()
column_name = identifier.copy()
column_alias = identifier.copy()
table_name = identifier.copy()
table_alias = identifier.copy()
index_name = identifier.copy()
function_name = identifier.copy()
parameter_name = identifier.copy()
schema_name = identifier.copy()

comment = "--" + restOfLine

# expression
expr = Forward().setName("expression")

numeric_literal = pyparsing_common.number
string_literal = QuotedString("'", escQuote="''")
blob_literal = Regex(r"[xX]'[0-9A-Fa-f]+'")
literal_value = (
    numeric_literal
    | string_literal
    | blob_literal
    | TRUE
    | FALSE
    | NULL
    | CURRENT_TIME
    | CURRENT_DATE
    | CURRENT_TIMESTAMP
)
bind_parameter = Word("?", nums) | Combine(oneOf(": @ $") + parameter_name)
type_name = oneOf("TEXT REAL INTEGER BLOB NULL")

expr_term = (
    CAST + LPAR + expr + AS + type_name + RPAR
#ply ajout CASE
    | Group(CASE + Optional(expr) + WHEN + expr + Optional(THEN + expr) + Optional(ELSE + expr) + END)
#    | EXISTS + LPAR + select_stmt + RPAR
    | EXISTS + LPAR + identifier + RPAR
#ply ajout scalar    | LPAR + select_stmt + RPAR 
    | Group(
    function_name.setName("function_name")
#ply    function_name
    + LPAR
    + Optional(STAR | Optional(DISTINCT) + delimitedList(expr))
    + RPAR
    )
    | literal_value
    | bind_parameter
    | Group(
        identifier("col_sch") + DOT + identifier("col_tab") + DOT + identifier("col")
    )
    | Group(identifier("col_tab") + DOT + identifier("col"))
    | Group(identifier("col"))
)

NOT_NULL = Group(NOT + NULL)
NOT_BETWEEN = Group(NOT + BETWEEN)
NOT_IN = Group(NOT + IN)
NOT_LIKE = Group(NOT + LIKE)
NOT_MATCH = Group(NOT + MATCH)
NOT_GLOB = Group(NOT + GLOB)
NOT_REGEXP = Group(NOT + REGEXP)

UNARY, BINARY, TERNARY = 1, 2, 3
expr << infixNotation(
    expr_term,
    [
        (oneOf("- + ~") | NOT, UNARY, opAssoc.RIGHT),
        (ISNULL | NOTNULL | NOT_NULL, UNARY, opAssoc.LEFT),
        ("||", BINARY, opAssoc.LEFT),
        (oneOf("* / %"), BINARY, opAssoc.LEFT),
        (oneOf("+ -"), BINARY, opAssoc.LEFT),
        (oneOf("<< >> & |"), BINARY, opAssoc.LEFT),
        (oneOf("< <= > >="), BINARY, opAssoc.LEFT),
        (
            oneOf("= == != <>")
            | IS
            | IN
            | LIKE
            | GLOB
            | MATCH
            | REGEXP
            | NOT_IN
            | NOT_LIKE
            | NOT_GLOB
            | NOT_MATCH
            | NOT_REGEXP,
            BINARY,
            opAssoc.LEFT,
        ),
        ((BETWEEN | NOT_BETWEEN, AND), TERNARY, opAssoc.LEFT),
        (
#            (IN | NOT_IN) + LPAR + Group(select_stmt | delimitedList(expr)) + RPAR,
            (IN | NOT_IN) + LPAR + Group(identifier | delimitedList(expr)) + RPAR,
            UNARY,
            opAssoc.LEFT,
        ),
        (AND, BINARY, opAssoc.LEFT),
        (OR, BINARY, opAssoc.LEFT),
    ],
)

compound_operator = UNION + Optional(ALL) | INTERSECT | EXCEPT

ordering_term = Group(
   expr("order_key")
#ply?    expr_term("order_key")
    + Optional(COLLATE + collation_name("collate"))
    + Optional(ASC | DESC)("direction")
)

join_constraint = Group(
    Optional(ON + expr | USING + LPAR + Group(delimitedList(column_name)) + RPAR)
)

join_op = COMMA | Group(
    Optional(NATURAL) + Optional(INNER | CROSS | LEFT + OUTER | LEFT | OUTER) + JOIN
)("join_op")

join_source = Forward()
single_source = (
    Group(schema_name("schema") + DOT + table_name("table") + Optional(Optional(AS) + table_alias("table_alias")) )
    | Group( table_name("table")+ Optional(Optional(AS) + table_alias("table_alias")) )
#    + Optional(Optional(AS) + table_alias("table_alias"))
#    + Optional(INDEXED + BY + index_name("name") | NOT + INDEXED)("index")
#    | Group(LPAR + select_stmt + RPAR + Optional(Optional(AS) + table_alias("table_alias")))
    | (LPAR + join_source + RPAR)
)

join_source <<= (
    Group(single_source + OneOrMore(join_op + single_source + join_constraint))
    | single_source
)

# result_column = "*" | table_name + "." + "*" | Group(expr + Optional(Optional(AS) + column_alias))
result_column = Group(
    STAR("col")
    | table_name("col_table") + DOT + STAR("col")
    | expr("proj_col") + Optional(Optional(AS) + column_alias("alias"))
#ply    | expr("col") + Optional(Optional(AS) + column_alias("alias"))
)

select_core = (
    SELECT
    + Optional(DISTINCT | ALL)
    + Group(delimitedList(result_column))("projections")
    + Optional(FROM + join_source("from"))
    + Optional(WHERE + expr("where_expr"))
    + Optional(
        GROUP
        + BY
        + Group(delimitedList(ordering_term))("group_by_terms")
        + Optional(HAVING + expr("having_expr"))
    )
)

select_stmt << (
    select_core
    + ZeroOrMore(compound_operator + select_core)
    + Optional(ORDER + BY + Group(delimitedList(ordering_term))("order_by_terms"))
    + Optional(
        LIMIT
        + (Group(expr + OFFSET + expr) | Group(expr + COMMA + expr) | expr)("limit")
    )
)

select_stmt.ignore(comment)


def main():
    tests = """\
#        select case when a=2 then 1 else 2 end as x from T
#        select case a when 2 then 1 else 2 end as x from T
#        select case a when 2 else 2 end as x from T
#        select case a when 2 then 1 end as x from T
#        select case a when 2 then 1 end || 'bb' from T
#        select case a when 2 then 1 end from T
#        select nvl(t.a,5)-2 as x, t.c+2 y from tt t where nvl(a,5) > 2
#        select trunc(nvl(t.a,5)-2,'TT','Z') as x, t.c+2 y from tt t where nvl(a,5) > 2
#        select ta.a+1,tb.A,count(*) from tt ta,tt tb where ta.id*6=tb.id/9 and ta.z=tb.z group by  ta.a+1,tb.A
#        select count(distinct t.o) from kjhg t
#        select t.* from kjhg t
#        select distinct t.o from kjhg t
#        select sysdate, sysdate() from t,u
#        select * from hgst ta inner join jhyy tb on (ta.c1=tb.c2) where ta.c3=2
#        select * from hgst ta, jhyy tb where ta.c1=tb.c2 and  ta.c3=2

# RULES:
#  no subquery (voir dans RevJ le nommage lien)
#  table alias mandatory or alias = table_name for colum prefix
# ? ANSI join only (SQLeo can translate other joins)

#SIMPLE select (1 table / no join)
        select tab.a,tab.b from tab
        select tab.a,tab.b from TAB
        select tab.a,tab.b from tab where tab.c='AA' order by 1
        select t_alias.a,t_alias.b from tab as t_alias 
        select t_alias_wo_as.a,t_alias_wo_as.b from tab t_alias_wo_as
        select t.a,t.b, count(*) from nn.kjh t group by t.a,t.b
        select t.a from nn.kjh as t where t.b in (1,2,3)
        select t.a from nn.kjh t where t.b in (subselect_1)
        select t.a from nn.kjh t where exists (subselect_2)
        select t.a from nn.kjh t where true
        select t.a from nn.kjh t where t.b between t.x and t.y
        select nvl(t.a,5)-2 as x, t.c+2 y from tt t where nvl(t.a,5) > 2
        
# out       select sch.tab.a,sch.tab.b from sch.tab  
#JOIN
# out        select t1.a,t2.b from tab t1,tab t2 where t1.c=t2.d
# out        select t1.a+t2.b from tab t1,tab t2 where t1.c=t2.d
#ANSI JOIN
        select t1.a,t2.b from tab t1 join tab t2 on (t1.c=t2.d)
        select t1.a+t2.b from tab t1 join tab t2 on t1.c=t2.d
        select count(t1.x) from tab t1 join tab t2 using (y)
#INLINE VIEW / DERIVED TABLE
        select t.a from subselect_1 t
# ko cf convention subq        select t.a from (subselect_1) t
        select 1
# out        select * from (select 2 from dual) as tt 
# out        select * from (select 2 from dual) as tt, zz d
        """

    success, _ = select_stmt.runTests(tests)
    print("\n{}".format("OK" if success else "FAIL"))
    return 0 if success else 1


if __name__ == "__main__":
    main()
