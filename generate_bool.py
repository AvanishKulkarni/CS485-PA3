import random
from generate_arith import gen_arith

# INT_MIN = -2_147_483_648
INT_MIN = -64
# INT_MAX = 2_147_483_647
INT_MAX = 64

import random

def gen_cond(variables, depth, is_boolean=False):
  if depth == 0:
    value = gen_arith(depth=0, input=variables)
    return value

  if is_boolean:
    # Generate Boolean expressions
    left = gen_cond(variables, depth - 1)
    right = gen_cond(variables, depth - 1)
    return f"({left} = {right})"  # Boolean equality

  # Generate integer expressions
  left = gen_cond(variables, depth - 1, is_boolean=False)
  right = gen_cond(variables, depth - 1, is_boolean=False)
  operator = random.choice(["<", "<=", "="])  # Valid comparison operators for integers

  # Switch to Boolean context for deeper levels
  if depth - 1 > 0:
    boolean_part = f"(not {gen_cond(variables, depth - 1, is_boolean=True)})"
    return f"(({left} {operator} {right}) = {boolean_part})"
  else:
    return f"({left} {operator} {right})"

if __name__ == "__main__":
  variables = [chr(c) for c in range(ord('a'), ord('d')+1)]
  random_expression = gen_cond(variables, depth=5)

  with open(r"test/arithmetic_random.cl", "w") as file:
    file.write("class Main inherits IO {\n")
    file.write("  main() : Object {\n")
    file.write("    let\n")
    for i in range(len(variables)-1):
      file.write(f'    {variables[i]} : Int,\n')
    file.write(f'    {variables[-1]} : Int\n')
    file.write("    in {\n")
    for var in variables:
      file.write(f'    {var} <- in_int();\n')
      
    file.write("    if (")
    file.write(random_expression)
    file.write(")\n")
    file.write("    then\n      out_int(1)\n    else\n      out_int(0)\n    fi;}\n")
    file.write("  };\n")
    file.write("};\n")
