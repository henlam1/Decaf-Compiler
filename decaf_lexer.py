import os
import sys
import ply.lex as lex

# Names         : Henry Lam    &    Sharon Guan
# Netid         : henlam            shgguan
# Student ID    : 113350850         114430436

# module: decaf_lexer.py
# This module just contains the lexing rules

# List of token names
tokens = (
   'PLUS',
   'MINUS',
   'TIMES',
   'DIVIDE',
   'LPAREN',
   'RPAREN',
   'LCURLY',
   'RCURLY',
   'AND',
   'OR',
   'EEQUALS',
   'NEQUALS',
   'EQUALS',
   'LESS',
   'GREATER',
   'LEQUALS',
   'GEQUALS',
   'NOT',
   'PLUSPLUS',
   'MINUSMINUS',
   'INT_CONST',
   'FLOAT_CONST', #Float constant to differntiate from Float declaration
   'STRING_CONST',
   'ID',
   'COMMENT',
   'PERIOD',
   'COMMA',
   'SEMICOLON'
)

reserved = {
    'boolean'   :   'BOOLEAN',
    'break'     :   'BREAK',
    'continue'  :   'CONTINUE', 
    'class'     :   'CLASS',
    'do'        :   'DO',
    'else'      :   'ELSE',
    'extends'   :   'EXTENDS',
    'false'     :   'FALSE',
    'float'     :   'FLOAT',
    'for'       :   'FOR',
    'if'        :   'IF',
    'int'       :   'INT',
    'new'       :   'NEW',
    'null'      :   'NULL',
    'private'   :   'PRIVATE',
    'public'    :   'PUBLIC',
    'return'    :   'RETURN',
    'static'    :   'STATIC',
    'super'     :   'SUPER',
    'this'      :   'THIS',
    'true'      :   'TRUE',
    'void'      :   'VOID',
    'while'     :   'WHILE'
}
reservedTokens = tuple(value for value in reserved.values())
tokens += reservedTokens

# Single Line Token Definitions
# Arithmetic
t_PLUS      = r'\+'
t_MINUS     = r'-'
t_TIMES     = r'\*'
t_DIVIDE    = r'/'
t_LPAREN    = r'\('
t_RPAREN    = r'\)'
t_LCURLY    = r'{'
t_RCURLY    = r'}'
t_PERIOD    = r'\.'
t_COMMA     = r','
t_SEMICOLON = r';'

# Boolean
t_AND       = r'&&'
t_OR        = r'\|\|'
t_EQUALS    = r'='
t_EEQUALS   = r'=='
t_NEQUALS   = r'!='
t_LESS      = r'<'
t_GREATER   = r'>'
t_LEQUALS   = r'<='
t_GEQUALS   = r'>='

# Unary
t_NOT       = r'!'
t_PLUSPLUS  = r'\+\+'
t_MINUSMINUS= r'--'

# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'

#Ignoring Single and Multi-line omments
def t_COMMENT(t):
    # Multi Line: \**\ or \** **\.
    #   ?!\/ -> Negative lookahead for /. Will not match if / is in it
    #   ^* -> Matches anything that's not *
    # Single Line: //.* 
    r'(\/\*(\*(?!\/)|[^*])*\*\/)|(//.*)'
    pass
    # No return value. Token discarded

# Function Token Definitions
def t_FLOAT_CONST(t):
    r'(0[.][0-9]+)|([0-9]+[.][0-9]+)'
    t.value = float(t.value)
    return t

def t_INT_CONST(t):     # Sort FLOAT first and then INT for longest match
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_STRING_CONST(t):
    r'["]([^"\\]|[\\.])*["]'
    t.value = str(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')    # Check for reserved words
    return t

# Define a rule so we can track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineStart = t.lexer.lexpos
    t.lexer.lineno += len(t.value)

def t_error(t):
    print("invalid something:", t.value[0], " at line ", t.lexer.lineno)
    exit()

# Build the lexer
#lexer = lex.lex()

# Get input data
# cwd = os.path.dirname(os.path.abspath(__file__))
# fileName = sys.argv[1]
# filePath = os.path.join(cwd, fileName)
