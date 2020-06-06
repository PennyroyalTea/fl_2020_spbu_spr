import ply.lex as lex
import ply.yacc as yacc
from collections import defaultdict
import sys


tokens = (
	'LEFT_BRACKET',
	'RIGHT_BRACKET',
	'COMMA',
	'STOP',
	'GOAL_SPLIT',
	'RELATION_SPLIT',
	'VAR',
	'ID',
)

t_LEFT_BRACKET = r'\('			
t_RIGHT_BRACKET = r'\)' 		
t_COMMA = r','					
t_STOP = r'\.'					
t_GOAL_SPLIT = r'\?-'			
t_RELATION_SPLIT = r':-'		
t_VAR = r'[A-Z][0-9a-zA-Z]*'	
t_ID = r'[a-z][0-9a-zA-Z]*'		



t_ignore  = ' \t\n'


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lex.lex()


tree = None


def p_program_no_relation(p):
	'''program : goal'''
	global tree
	tree = ('program', [p[1]])


def p_program(p):
	'''program : relation goal'''
	global tree
	tree = ('program', [p[1], p[2]])


def p_relation_single(p):
	'''relation : atom RELATION_SPLIT body STOP'''
	p[0] = ('relation', [p[1], p[3]])


def p_relation_multiple(p):
	'''relation : atom RELATION_SPLIT body STOP relation'''
	p[0] = ('relation', [p[1], p[3], p[5]])


def p_relation_empty(p):
	'''relation : atom STOP'''
	p[0] = ('relation', [p[1]])


def p_relation_empty_multiple(p):
	'''relation : atom STOP relation'''
	p[0] = ('relation', [p[1], p[3]])


def p_goal(p):
	'''goal : GOAL_SPLIT body STOP'''
	p[0] = ('goal', [p[2]])


def p_goal_empty(p):
	'''goal : GOAL_SPLIT STOP'''
	p[0] = ('goal', [('empty', [])])

def p_atom(p):
	'''atom : ID LEFT_BRACKET args RIGHT_BRACKET'''
	p[0] = ('atom', [('ID={}'.format(p[1]), []),p[3]])


def p_atom_empty(p):
	'''atom : ID'''
	p[0] = ('atom', [('ID={}'.format(p[1]), [])])


def p_args_var_single(p):
	'''args : VAR'''
	p[0] = ('args', [('VAR={}'.format(p[1]), [])])


def p_args_var_multiple(p):
	'''args : VAR COMMA args'''
	p[0] = ('args', [('VAR={}'.format(p[1]), []), p[3]])


def p_args_atom_single(p):
	'''args : atom'''
	p[0] = ('args', [p[1]])


def p_args_atom_multiple(p):
 	'''args : atom COMMA args'''
 	p[0] = ('args', [p[1], p[3]])


def p_body_single(p):
	'''body : atom
	'''
	p[0] = ('body', [p[1]])


def p_body_multiple(p):
	'''body : atom COMMA body'''
	p[0] = ('body', [p[1], p[3]])


syntax_error = False


def p_error(p):
	global syntax_error
	syntax_error = True
	print (f'Syntax error at {p}')


def parse(s):
	global relations, goal, atoms, syntax_error, varrs
	syntax_error = False
	yacc.parse(s)
	return tree, syntax_error


yacc.yacc()
def parse_file(file):
	with open(file, 'r') as f:
		program = f.read()
		return parse(program)