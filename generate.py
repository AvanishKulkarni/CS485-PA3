import random

# INT_MIN = -2_147_483_648
INT_MIN = -64
# INT_MAX = 2_147_483_647
INT_MAX = 64
OPERATORS = ['<', '<=', '=', 'not']

def random_number():
  num = random.randint(INT_MIN, INT_MAX)
  return f'~{abs(num)}' if num < 0 else str(num)

def random_operator():
  return random.choice(OPERATORS)

import random

def generate_expression(variables, depth=3, is_boolean=False):
  if depth == 0:
    # Base case: create a simple negated integer
    value = random.choice(variables)
    return f"~{value}"

  if is_boolean:
    # Generate Boolean expressions
    left = generate_expression(variables, depth - 1)
    right = generate_expression(variables, depth - 1)
    return f"({left} = {right})"  # Boolean equality

  # Generate integer expressions
  left = generate_expression(variables, depth - 1, is_boolean=False)
  right = generate_expression(variables, depth - 1, is_boolean=False)
  operator = random.choice(["<"])  # Valid comparison operators for integers

  # Switch to Boolean context for deeper levels
  if depth - 1 > 0:
    boolean_part = f"(not {generate_expression(variables, depth - 1, is_boolean=True)})"
    return f"(({left} {operator} {right}) = {boolean_part})"
  else:
    return f"({left} {operator} {right})"

variables = [chr(c) for c in range(ord('a'), ord('z')+1)]  # Sample integer values
random_expression = generate_expression(variables, depth=5)

# Generate a random expression with nesting
with open(r"test/arithmetic_random.cl", "w") as file:
  file.write("class Main inherits IO {\n")
  file.write("  main() : Object {\n")
  file.write("    if (")
  file.write(random_expression)
  file.write(")\n")
  file.write("    then\n      out_int(1)\n    else\n      out_int(0)\n    fi\n")
  file.write("  };\n")
  file.write("};\n")
