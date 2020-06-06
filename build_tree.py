import sys
import re
from parser import parse_file

from operator import itemgetter
from tree_format import format_tree

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

if __name__ == '__main__':
	with open('tree.txt', 'w') as file:
		tree, syntax_error = parse_file(sys.argv[1])
		if (syntax_error) :
			print(f"{bcolors.FAIL} SYNTAX_ERROR {bcolors.ENDC}")
		else :
			print(f"{bcolors.OKGREEN} NO SYNTAX ERRORS {bcolors.ENDC}")
		print(format_tree(tree, format_node=itemgetter(0), get_children=itemgetter(1)))
