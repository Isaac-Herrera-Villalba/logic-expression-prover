#!/usr/bin/env python3
"""
parser.py

Lee queries.txt (una fórmula por línea, en muchas notaciones)
y genera queries.pl con líneas: query(<term>).

Soporta notaciones:
 - palabras:  neg, and, or, implies, dimplies
 - símbolos:  ->, <->, <=>, ↔, ¬, ~, ∧, ∨, &, |, ->, <->
 - paréntesis: ( ... )
 - identifiers: letras/dígitos/_ (por ejemplo a, p1, my_var)

Uso:
    python3 src/parser.py ../queries.txt ../queries.pl

Si no pasas args usa ./queries.txt -> ./queries.pl (ruta relativa a la raíz del proyecto)
"""
import re
import sys
from pathlib import Path

# Token specification (longer symbols must come first)
TOKEN_SPEC = [
    ('LPAREN',     r'\('),
    ('RPAREN',     r'\)'),
    # Unicode and multi-char symbols first
    ('DIMPL',      r'(<->|<=>|↔)'),     # bicondicional
    ('IMPL',       r'->'),              # implicación ASCII (->)
    ('NOTSYM',     r'(¬|~)'),           # negación símbolo
    ('ANDSYM',     r'(&|∧)'),           # conjunction
    ('ORSYM',      r'(\||∨)'),          # disjunction
    # word operators (longest first)
    ('DIMPL_W',    r'\bdimplies\b'),
    ('IMPL_W',     r'\bimplies\b'),
    ('AND_W',      r'\band\b'),
    ('OR_W',       r'\bor\b'),
    ('NOT_W',      r'\bneg\b|\bnot\b'),
    # identifier (letters, digits, underscore, hyphen allowed inside)
    ('IDENT',      r'[A-Za-z][A-Za-z0-9_]*'),
    ('SKIP',       r'[ \t\r\n]+'),
    ('MISMATCH',   r'.'),
]

TOKEN_RE = re.compile('|'.join('(?P<%s>%s)' % pair for pair in TOKEN_SPEC))

# precedence levels (lower index = lower precedence)
# We'll implement recursive descent:
# level 0: dimplies (lowest, right-assoc)
# level 1: implies (right-assoc)
# level 2: or (left-assoc)
# level 3: and (left-assoc)
# level 4: unary (neg)
# level 5: atom / parenthesis (highest atomic)
class Token:
    def __init__(self, typ, val, pos):
        self.type = typ
        self.value = val
        self.pos = pos
    def __repr__(self):
        return f"Token({self.type},{self.value!r},{self.pos})"

def tokenize(s):
    pos = 0
    tokens = []
    for mo in TOKEN_RE.finditer(s):
        kind = mo.lastgroup
        val = mo.group()
        pos = mo.start()
        if kind == 'SKIP':
            continue
        if kind == 'MISMATCH':
            raise SyntaxError(f"Unexpected character {val!r} at pos {pos}")
        tokens.append(Token(kind, val, pos))
    tokens.append(Token('EOF', '', len(s)))
    return tokens

# Parser: recursive descent
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.i = 0
    def cur(self):
        return self.tokens[self.i]
    def eat(self, *expected):
        t = self.cur()
        if t.type in expected:
            self.i += 1
            return t
        expected_names = ','.join(expected)
        raise SyntaxError(f"Expected {expected_names} at pos {t.pos}, found {t.type}({t.value})")
    def parse(self):
        node = self.parse_dimplies()
        if self.cur().type != 'EOF':
            t = self.cur()
            raise SyntaxError(f"Extra token {t.value!r} at pos {t.pos}")
        return node

    # lowest precedence: dimplies (right associative)
    def parse_dimplies(self):
        left = self.parse_implies()
        if self.cur().type in ('DIMPL','DIMPL_W'):
            op = self.eat('DIMPL','DIMPL_W')
            right = self.parse_dimplies()
            return ('dimplies', left, right)
        return left

    # implies (right-assoc)
    def parse_implies(self):
        left = self.parse_or()
        if self.cur().type in ('IMPL','IMPL_W'):
            self.eat('IMPL','IMPL_W')
            right = self.parse_implies()
            return ('implies', left, right)
        return left

    # or (left-assoc)
    def parse_or(self):
        node = self.parse_and()
        while self.cur().type in ('OR_W','ORSYM'):
            self.eat('OR_W','ORSYM')
            rhs = self.parse_and()
            node = ('or', node, rhs)
        return node

    # and (left-assoc)
    def parse_and(self):
        node = self.parse_unary()
        while self.cur().type in ('AND_W','ANDSYM'):
            self.eat('AND_W','ANDSYM')
            rhs = self.parse_unary()
            node = ('and', node, rhs)
        return node

    # unary (neg)
    def parse_unary(self):
        if self.cur().type in ('NOT_W','NOTSYM'):
            self.eat('NOT_W','NOTSYM')
            child = self.parse_unary()
            return ('neg', child)
        return self.parse_atom()

    def parse_atom(self):
        t = self.cur()
        if t.type == 'LPAREN':
            self.eat('LPAREN')
            node = self.parse_dimplies()
            self.eat('RPAREN')
            return node
        if t.type == 'IDENT':
            name = t.value
            self.eat('IDENT')
            return ('atom', name)
        raise SyntaxError(f"Unexpected token {t.type}({t.value}) at pos {t.pos}")

# Convert AST to Prolog term string
def ast_to_prolog(node):
    typ = node[0] if isinstance(node, tuple) else None
    if node is None:
        return '[]'
    if isinstance(node, tuple):
        if node[0] == 'atom':
            name = node[1]
            # sanitize: lowercase
            atom = name.lower()
            if re.fullmatch(r'[a-z][a-z0-9_]*', atom):
                return atom
            else:
                # quote it
                return f"'{atom}'"
        elif node[0] == 'neg':
            return f"neg({ast_to_prolog(node[1])})"
        elif node[0] in ('and','or','implies','dimplies'):
            left = ast_to_prolog(node[1])
            right = ast_to_prolog(node[2])
            return f"{node[0]}({left},{right})"
        else:
            raise ValueError("Unknown node type "+str(node[0]))
    else:
        raise ValueError("Invalid AST node: "+repr(node))

def parse_line_to_prolog(line):
    tokens = tokenize(line)
    p = Parser(tokens)
    ast = p.parse()
    return ast_to_prolog(ast)

def process_file(inp_path: Path, out_path: Path, verbose=True):
    lines = inp_path.read_text(encoding='utf8').splitlines()
    out_lines = []
    errors = []
    for i, raw in enumerate(lines, start=1):
        s = raw.strip()
        if s == '' or s.startswith('%') or s.startswith('//'):
            continue
        try:
            prolog_term = parse_line_to_prolog(s)
            out_lines.append(f"query({prolog_term}).")
            if verbose:
                print(f"[OK] Line {i}: {s}  -> query({prolog_term}).")
        except Exception as e:
            errmsg = f"Line {i}: parse error: {e}"
            errors.append(errmsg)
            print(f"[ERR] {errmsg}")
    out_path.write_text("\n".join(out_lines)+"\n", encoding='utf8')
    if verbose:
        print(f"\nWrote {len(out_lines)} queries to {out_path}")
        if errors:
            print(f"Encountered {len(errors)} errors (see above).")
    return errors

# CLI
def main_cli():
    in_file = Path(sys.argv[1]) if len(sys.argv) > 1 else Path('queries.txt')
    out_file = Path(sys.argv[2]) if len(sys.argv) > 2 else Path('queries.pl')
    if not in_file.exists():
        print(f"Input {in_file} not found.")
        sys.exit(2)
    process_file(in_file, out_file)

if __name__ == '__main__':
    main_cli()

