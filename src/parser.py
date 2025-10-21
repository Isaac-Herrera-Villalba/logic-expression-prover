#!/usr/bin/env python3
"""
parser.py

Lee queries.txt (una fórmula por línea) y genera queries.pl con líneas: query(<term>).
Soporta notaciones de palabras o símbolos y comentarios estilo Prolog:
  - Comentarios de línea: % esto es un comentario
  - Comentarios de bloque: /* comentario
                           en varias líneas */
"""

import re
import sys
from pathlib import Path

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

class Token:
    def __init__(self, typ, val, pos):
        self.type = typ
        self.value = val
        self.pos = pos
    def __repr__(self):
        return f"Token({self.type},{self.value!r},{self.pos})"

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
            op = self.eat('DIMPL','DIMPL_W')
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

def ast_to_prolog(node):
    if node[0] == 'atom':
        name = node[1]
        if re.fullmatch(r'[a-z][a-z0-9_]*', name):
            return name
        return f"'{name}'"
    elif node[0] == 'neg':
        return f"neg({ast_to_prolog(node[1])})"
    elif node[0] in ('and','or','implies','dimplies'):
        return f"{node[0]}({ast_to_prolog(node[1])},{ast_to_prolog(node[2])})"
    else:
        raise ValueError(f"Unknown node type {node[0]}")

def parse_line_to_prolog(line):
    tokens = tokenize(line)
    p = Parser(tokens)
    ast = p.parse()
    return ast_to_prolog(ast)

def strip_comments(text):
    """
    Remove Prolog-style comments:
      - block comments /* ... */ (multiline)
      - line comments starting with %
      - also keep support for // style as before
    """
    # remove block comments first (/* ... */), non-greedy, DOTALL to span newlines
    text_no_block = re.sub(r'/\*.*?\*/', '', text, flags=re.DOTALL)
    # remove % comments (from % to end of line)
    lines = []
    for line in text_no_block.splitlines():
        # keep // comments removal too (existing behavior)
        # take portion before % or // (the earliest)
        # find index of '%' and '//' if present
        idx_percent = line.find('%')
        idx_slash = line.find('//')
        cut = None
        if idx_percent != -1 and idx_slash != -1:
            cut = min(idx_percent, idx_slash)
        elif idx_percent != -1:
            cut = idx_percent
        elif idx_slash != -1:
            cut = idx_slash
        if cut is not None:
            lines.append(line[:cut])
        else:
            lines.append(line)
    return "\n".join(lines)

def process_file(inp_path, out_path, verbose=True):
    raw = inp_path.read_text(encoding='utf8')
    cleaned = strip_comments(raw)
    lines = cleaned.splitlines()
    seen = set()
    out_lines = []
    for i, line in enumerate(lines, start=1):
        s = line.strip()
        if s == '':
            continue
        try:
            term = parse_line_to_prolog(s)
            if term not in seen:
                seen.add(term)
                out_lines.append(f"query({term}).")
            if verbose:
                print(f"[OK] Line {i}: {s}  -> query({term}).")
        except Exception as e:
            print(f"[ERR] Line {i}: {e}")
    out_path.write_text("\n".join(out_lines)+"\n", encoding='utf8')

if __name__ == '__main__':
    inp = Path(sys.argv[1]) if len(sys.argv)>1 else Path('queries.txt')
    outp = Path(sys.argv[2]) if len(sys.argv)>2 else Path('tmp/queries.pl')
    inp.parent.mkdir(exist_ok=True)
    outp.parent.mkdir(exist_ok=True)
    process_file(inp, outp)
###########################################

