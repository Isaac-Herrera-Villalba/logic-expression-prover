#!/usr/bin/env python3
"""
 src/parser.py
 ------------------------------------------------------------
 Descripción:

 Analizador sintáctico que lee el archivo queries.txt y genera
 tmp/queries.pl con las consultas convertidas a términos Prolog.

 Cada línea puede ser:
   - Una fórmula proposicional.
   - Un secuente del cálculo LKP (Γ ⊢ Δ).

 El módulo reconoce tanto símbolos lógicos como palabras clave
 y permite comentarios de una o varias líneas.
 ------------------------------------------------------------
"""

import re
import sys
from pathlib import Path

# Definición de los tokens aceptados
TOKEN_SPEC = [
    ('LPAREN',     r'\('),
    ('RPAREN',     r'\)'),
    ('DIMPL',      r'(<->|<=>|↔)'),
    ('IMPL',       r'->'),
    ('NOTSYM',     r'(¬|~)'),
    ('ANDSYM',     r'(&|∧)'),
    ('ORSYM',      r'(\||∨)'),
    ('DIMPL_W',    r'\bdimplies\b'),
    ('IMPL_W',     r'\bimplies\b'),
    ('AND_W',      r'\band\b'),
    ('OR_W',       r'\bor\b'),
    ('NOT_W',      r'\bneg\b|\bnot\b'),
    ('IDENT',      r'[A-Za-z][A-Za-z0-9_]*'),
    ('SKIP',       r'[ \t\r\n]+'),
    ('MISMATCH',   r'.'),
]

TOKEN_RE = re.compile('|'.join('(?P<%s>%s)' % pair for pair in TOKEN_SPEC))

# Representa un token del analizador
class Token:
    def __init__(self, typ, val, pos):
        self.type = typ
        self.value = val
        self.pos = pos
    def __repr__(self):
        return f"Token({self.type},{self.value!r},{self.pos})"

# Convierte una cadena en una lista de tokens
def tokenize(s):
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

# Analizador recursivo para construir el árbol sintáctico
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
        raise SyntaxError(f"Expected {expected}, got {t.type} at pos {t.pos}")
    def parse(self):
        node = self.parse_dimplies()
        if self.cur().type != 'EOF':
            t = self.cur()
            raise SyntaxError(f"Extra token {t.value!r} at pos {t.pos}")
        return node

    def parse_dimplies(self):
        left = self.parse_implies()
        if self.cur().type in ('DIMPL','DIMPL_W'):
            self.eat('DIMPL','DIMPL_W')
            right = self.parse_dimplies()
            return ('dimplies', left, right)
        return left

    def parse_implies(self):
        left = self.parse_or()
        if self.cur().type in ('IMPL','IMPL_W'):
            self.eat('IMPL','IMPL_W')
            right = self.parse_implies()
            return ('implies', left, right)
        return left

    def parse_or(self):
        node = self.parse_and()
        while self.cur().type in ('OR_W','ORSYM'):
            self.eat('OR_W','ORSYM')
            rhs = self.parse_and()
            node = ('or', node, rhs)
        return node

    def parse_and(self):
        node = self.parse_unary()
        while self.cur().type in ('AND_W','ANDSYM'):
            self.eat('AND_W','ANDSYM')
            rhs = self.parse_unary()
            node = ('and', node, rhs)
        return node

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
            name = t.value.lower()
            self.eat('IDENT')
            return ('atom', name)
        raise SyntaxError(f"Unexpected token {t.type}({t.value}) at pos {t.pos}")

# Convierte un árbol sintáctico a un término Prolog
def ast_to_prolog(node):
    if node[0] == 'atom':
        name = node[1]
        if re.fullmatch(r'[a-z][a-z0-9_]*', name):
            return f"atom({name})"
        return f"atom('{name}')"
    elif node[0] == 'neg':
        return f"neg({ast_to_prolog(node[1])})"
    elif node[0] in ('and','or','implies','dimplies'):
        return f"{node[0]}({ast_to_prolog(node[1])},{ast_to_prolog(node[2])})"
    else:
        raise ValueError(f"Unknown node type {node[0]}")

# Procesa texto plano y devuelve su representación Prolog
def parse_formula_text(s):
    tokens = tokenize(s)
    p = Parser(tokens)
    ast = p.parse()
    return ast_to_prolog(ast)

# Elimina comentarios de línea o bloque
def strip_comments(text):
    text_no_block = re.sub(r'/\*.*?\*/', '', text, flags=re.DOTALL)
    lines = []
    for line in text_no_block.splitlines():
        idx_percent = line.find('%')
        idx_slash = line.find('//')
        cut = None
        if idx_percent != -1 and idx_slash != -1:
            cut = min(idx_percent, idx_slash)
        elif idx_percent != -1:
            cut = idx_percent
        elif idx_slash != -1:
            cut = idx_slash
        lines.append(line[:cut] if cut is not None else line)
    return "\n".join(lines)

# Determina si una línea es fórmula o secuente y genera el hecho Prolog
def parse_sequent_or_formula(line):
    if ('|-' in line) or ('⊢' in line):
        left_str, right_str = re.split(r'\s*\|-\s*|\s*⊢\s*', line)
        gamma = [x.strip() for x in left_str.split(',') if x.strip()]
        delta = [x.strip() for x in right_str.split(',') if x.strip()]
        gamma_terms = ",".join(parse_formula_text(f) for f in gamma)
        delta_terms = ",".join(parse_formula_text(f) for f in delta)
        return f"query(sequent, seq([{gamma_terms}],[{delta_terms}]))."
    else:
        term = parse_formula_text(line)
        return f"query(formula, {term})."

# Procesa el archivo de entrada y genera tmp/queries.pl
def process_file(inp_path, out_path, verbose=True):
    raw = inp_path.read_text(encoding='utf8')
    cleaned = strip_comments(raw)
    lines = [ln.strip() for ln in cleaned.splitlines() if ln.strip()]
    seen = set()
    out_lines = []
    for i, s in enumerate(lines, start=1):
        try:
            fact = parse_sequent_or_formula(s)
            if fact not in seen:
                seen.add(fact)
                out_lines.append(fact)
            if verbose:
                print(f"[OK] Line {i}: {s} -> {fact}")
        except Exception as e:
            print(f"[ERR] Line {i}: {e}")
    out_path.write_text("\n".join(out_lines) + "\n", encoding='utf8')

# Punto de entrada principal
if __name__ == '__main__':
    inp = Path(sys.argv[1]) if len(sys.argv) > 1 else Path('queries.txt')
    outp = Path(sys.argv[2]) if len(sys.argv) > 2 else Path('tmp/queries.pl')
    inp.parent.mkdir(exist_ok=True)
    outp.parent.mkdir(exist_ok=True)
    process_file(inp, outp)

