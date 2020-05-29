from collections import defaultdict
import string

class Program:
	def __init__(self, rels=defaultdict(list), target=[]):
		self.rels = rels
		self.target = target
	
	def __str__(self):
		s = "Parsing succeeded!\n\n"
		for name, lines in self.rels.items():
			s += "Relation " + name + ":\n" + "\n".join(map(str, lines)) + "\n\n"
		s += "\nTarget:\n" + "\n".join(map(str, self.target))
		return s
		
			


class Relation:
	def __init__(self, name, args, body):
		self.name = name
		self.args = args
		self.body = body
	
	def __str__(self):
		return ("Line(name = " + self.name +
			    ", args = [" + ", ".join(map(str, self.args)) +
			    "], body = [" + ", ".join(map(str, self.body)) + "])")


class Atom:
	def __init__(self, name, args):
		self.name = name
		self.args = args
	
	def __str__(self):
		return "Atom(name = " + self.name + ", args = [" + ", ".join(map(str, self.args)) + "])"
			
		

class Var:
	def __init__(self, name):
		self.name = name
	
	def __str__(self):
		return "Var(" + self.name + ")"