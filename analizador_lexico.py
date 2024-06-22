import ply.lex as lex
import ply.yacc as yacc
import sys

# Lista de tokens
tokens = (
    'INT', 'FLOAT', 'STRING', 'IF', 'WHILE', 'FOR', 'IN', 'RANGE', 'INPUT',
    'ID', 'NUMBER', 'DECIMAL', 'TEXT',
    'ASSIGN', 'EQ', 'GT', 'LT', 'PLUS', 'AND',
    'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE', 'SEMICOLON', 'COMMA'
)

# Definiciones de expresiones regulares para tokens simples
t_ASSIGN = r'='
t_EQ = r'=='
t_GT = r'>'
t_LT = r'<'
t_PLUS = r'\+\+'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_SEMICOLON = r';'
t_COMMA = r','

# Definiciones de palabras reservadas
reserved = {
    'INT': 'INT',
    'FLOAT': 'FLOAT',
    'STRING': 'STRING',
    'IF': 'IF',
    'WHILE': 'WHILE',
    'FOR': 'FOR',
    'IN': 'IN',
    'RANGE': 'RANGE',
    'INPUT': 'INPUT',
    'AND' : 'AND'
}

# Definiciones de tokens complejos
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')  # Verifica si es una palabra reservada
    return t

def t_DECIMAL(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_TEXT(t):
    r'\".*?\"'
    t.value = str(t.value)
    return t

# Ignorar espacios y tabulaciones
t_ignore = ' \t'

# Manejo de saltos de línea
def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count('\n')

# Manejador de errores
def t_error(t):
    print(f"Caracter ilegal '{t.value[0]}' en la línea {t.lineno}, posición {t.lexpos}")
    t.lexer.skip(1)

# Definición del analizador léxico
lexer = lex.lex()

# Definición de la precedencia y asociaciones
precedence = ()

# Diccionario para almacenar las variables y sus tipos
variables = {}

# Definición de la gramática
def p_program(p):
    '''program : statements'''
    p[0] = ('program', p[1])

def p_statements(p):
    '''statements : statement
                  | statement statements'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

def p_statement(p):
    '''statement : declaration
                 | assignment
                 | if_statement
                 | while_statement
                 | for_statement
                 | input_statement
                 | increment_statement'''
    p[0] = p[1]


def p_declaration(p):
    '''declaration : INT ID ASSIGN NUMBER SEMICOLON
                   | FLOAT ID ASSIGN DECIMAL SEMICOLON
                   | STRING ID ASSIGN TEXT SEMICOLON
                   | FLOAT ID ASSIGN NUMBER SEMICOLON'''
    if p[1] == 'INT' and isinstance(p[4], int):
        variables[p[2]] = ('int', p[4])
    elif p[1] == 'FLOAT' and (isinstance(p[4], float) or isinstance(p[4], int)):
        variables[p[2]] = ('float', float(p[4]))
    elif p[1] == 'STRING' and isinstance(p[4], str):
        variables[p[2]] = ('string', p[4])
    else:
        print(f"Error semántico en la línea {p.lineno(1)}, posición {p.lexpos(1)}: {p[2]} debe ser del tipo {p[1]}")
        sys.exit(1)
    p[0] = ('declaration', p[1], p[2], p[4])
    
def p_assignment(p):
    '''assignment : ID ASSIGN expression SEMICOLON'''
    if p[1] not in variables:
        print(f"Error semántico: Variable '{p[1]}' no ha sido declarada.")
    else:
        var_type, _ = variables[p[1]]
        if isinstance(p[3], float) and var_type != 'float':
            print(f"Error semántico: Tipo incorrecto para asignación a '{p[1]}'. Se esperaba '{var_type}'.")
        elif isinstance(p[3], int) and var_type != 'int':
            print(f"Error semántico: Tipo incorrecto para asignación a '{p[1]}'. Se esperaba '{var_type}'.")
        elif isinstance(p[3], str) and var_type != 'string':
            print(f"Error semántico: Tipo incorrecto para asignación a '{p[1]}'. Se esperaba '{var_type}'.")
        else:
            variables[p[1]] = (var_type, p[3])
    p[0] = ('assignment', p[1], p[3])

def p_if_statement(p):
    '''if_statement : IF LPAREN condition RPAREN LBRACE statements RBRACE'''
    p[0] = ('if', p[3], p[6])

def p_while_statement(p):
    '''while_statement : WHILE LPAREN condition RPAREN LBRACE statements RBRACE'''
    p[0] = ('while', p[3], p[6])

def p_for_statement(p):
    '''for_statement : FOR LPAREN ID IN RANGE LPAREN NUMBER COMMA NUMBER RPAREN RPAREN LBRACE statements RBRACE'''
    p[0] = ('for', p[3], p[7], p[9], p[13])

def p_condition(p):
    '''condition : expression EQ expression
                 | expression GT expression
                 | expression LT expression
                 | condition AND condition'''
    if p[2] == 'AND':
        p[0] = ('and', p[1], p[3])
    else:
        p[0] = ('condition', p[2], p[1], p[3])


def p_expression(p):
    '''expression : NUMBER
                  | DECIMAL
                  | ID'''
    p[0] = p[1]

def p_input_statement(p):
    '''input_statement : INPUT LPAREN TEXT RPAREN SEMICOLON'''
    if not isinstance(p[3], str):
        print(f"Error semántico en la línea {p.lineno(1)}, posición {p.lexpos(1)}: INPUT debe recibir una cadena de texto")
        sys.exit(1)
    p[0] = ('input', p[3])

def p_increment_statement(p):
    '''increment_statement : ID PLUS SEMICOLON'''
    if p[1] not in variables or variables[p[1]][0] != 'int':
        print(f"Error semántico en la línea {p.lineno(1)}, posición {p.lexpos(1)}: {p[1]} debe ser una variable entera")
        sys.exit(1)
    else:
        variables[p[1]] = ('int', variables[p[1]][1] + 1)
    p[0] = ('increment', p[1])

def p_error(p):
    if p:
        print(f"Error de sintaxis en '{p.value}' en la línea {p.lineno}, posición {p.lexpos}")
    else:
        print("Error de sintaxis en EOF - se esperaba más código o una estructura de bloque está incompleta")

parser = yacc.yacc()

# Prueba del parser
data = '''
INT EDAD = 34;
FLOAT B = 5.0;
STRING M = "ERES MAYOR";
STRING N = "ERES MENOR";
IF (EDAD == 34 AND 5 > 7) {
    WHILE (EDAD > 70) {
        INPUT("holas");
        EDAD ++;
    }
}
WHILE (100 > 56) {
    FOR (i IN RANGE(1, 400)) {
        B = 50.4;
    }
}
'''

lexer.input(data)
while True:
    tok = lexer.token()
    if not tok:
        break
    print(tok.type)
    

result = parser.parse(data)
print("soy el result we", result)