#!/usr/bin/python
import random
import sys
import re 

def get_vector_type_and_size(type):
	match = re.match("([bi]?)vec([2-4])", type)
	if match:
		return { "" : "float", "b" : "bool", "i" : "int" }[match.group(1)], int(match.group(2))

def get_type_and_size(type):
	if type == "float":
		return "float", 1
	if type == "bool":
		return "bool", 1
	if type == "int":
		return "int", 1
	return get_vector_type_and_size(type)

def constant(type):
	if type == "float":
		return str(random.random())
	if type == "bool":
		return random.choice(["true", "false"])
	if type == "int":
		return str(random.randint(0, 255))
	basetype, size = get_vector_type_and_size(type)
	match = re.match("([bi]?)vec([2-4])", type)
	return type + "(" + ", ".join([constant(basetype) for i in range(size)]) + ")"

def variable(type):
	return {
		"float" : lambda: "gl_FragCoord.x",
		"vec4" : lambda: "gl_FragCoord",
	}[type]()

def primary_expr(type):
	return random.choice([constant, variable])(type)

def function_call(type):
	return random.choice({
		"float" : [
			lambda: "dot(" + expression("vec4") + ", " + expression("vec4") + ")"
		], "vec4" : [
			lambda: "vec4(" + expression("float") + ", " + expression("float") + ", " + expression("float") + ", " + expression("float") + ")"
		]
	}[type])()

def postfix_expr(type):
	return random.choice([
		expression,
		function_call
	])(type)

def expression(type):
	return random.choice([
		primary_expr,
		primary_expr,
		primary_expr,
		primary_expr,
		primary_expr,
		primary_expr,
		function_call,
		lambda type: "(" + expression(type) + ")",
		lambda type: expression(type) + " * " + expression(type),
		lambda type: expression(type) + " / " + expression(type),
		lambda type: expression(type) + " + " + expression(type),
		lambda type: expression(type) + " - " + expression(type),
	])(type)

import argparse

parser = argparse.ArgumentParser(description='Generate random ESSL shaders.')
parser.add_argument('-s', '--seed', type=int)
args = parser.parse_args()

if args.seed == None:
	args.seed = random.randint(0, sys.maxint)
random.seed(args.seed)

print "/* seed: " + str(args.seed) + " */"
print "void main() {"
print "\tgl_FragColor = " + expression("vec4") + ";"
print "}"
