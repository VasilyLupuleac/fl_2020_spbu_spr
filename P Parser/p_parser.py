from ply import lex, yacc
from collections import defaultdict
import string
from p_ast import *


tokens = ("VAR", "IDENT", "BEGIN", "TARGET_BEGIN", "END", "LBR", "RBR", "COMMA")

t_VAR = r"[A-Z]([A-Z]|[a-z]|[0-9])*"
t_IDENT = r"[a-z]([A-Z]|[a-z]|[0-9])*"
t_BEGIN = r":-"
t_TARGET_BEGIN = r"\?-"
t_END = r"."
t_LBR = r"\("
t_RBR = r"\)"
t_COMMA = r"\,"
t_ignore = " \n"

def t_error(t):
	print("Illegal character '%s'" % t.value[0])
	t.lexer.skip(1)

lex.lex()

error = False
program = Program()

def copy_if(p, bound=3, index=3):
	return p[index].copy() if len(p) > bound else []


def p_program(p):
	"""statement : relations TARGET_BEGIN target END
				 | TARGET_BEGIN target END
				 | relations TARGET_BEGIN END
				 | TARGET_BEGIN END"""
	relations = defaultdict(list)
	global program
	if (isinstance(p[2], str) and p[2] == "?-"):
		for rel in p[1]:
			relations[rel.name].append(rel)
		program = Program(relations, copy_if(p, 4, 3))
	else:
		program = Program(relations, copy_if(p, 3, 2))


def p_relations(p):
	"""relations : relation END relations
				 | relation END"""
	p[0] = copy_if(p)
	p[0].insert(0, p[1])


def p_relation(p):
	"""relation : atom BEGIN body
			    | atom"""
	p[0] = Relation(p[1].name, p[1].args, copy_if(p))


def p_atom(p):
	"""atom : IDENT LBR args RBR
			| IDENT"""
	p[0] = Atom(p[1], copy_if(p))


def p_args_var(p):
	"""args : VAR COMMA args
			| VAR"""
	p[0] = copy_if(p)
	p[0].insert(0, Var(p[1]))


def p_args_atom(p):
	"""args : atom COMMA args
			| atom"""
	p[0] = copy_if(p)
	p[0].insert(0, p[1])
	
	
def p_body(p):
	"""body : atom COMMA body
			| atom"""
	p[0] = copy_if(p)
	p[0].insert(0, p[1])

def p_target(p):
	"""target : atom COMMA target
			  | atom"""
	p[0] = copy_if(p)
	p[0].insert(0, p[1])

def p_error(p):
	global error
	error = True
	print("Parsing failed")
	print(p)


def try_parsing(input):
	global error, program
	error = False
	program = Program()
	yacc.parse(input)
	return error, program


yacc.yacc()

if __name__ == "__main__":
	print("Please enter source file name")
	fn = input()
	try:
		f = open(fn, 'r')
		yacc.parse(f.read())
		if not error:
			print(program)
			
	except FileNotFoundError:
		print("File not found")
	













