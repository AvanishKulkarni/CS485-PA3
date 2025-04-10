import random

# INT_MIN = -2_147_483_648
INT_MIN = -50
# INT_MAX = 2_147_483_647
INT_MAX = 50
OPERATORS = ['+', '-', '*', '/']

def random_number():
  num = random.randint(INT_MIN, INT_MAX)
  return f'~{abs(num)}' if num < 0 else str(num)

def random_operator():
  return random.choice(OPERATORS)

def generate_expression(depth=0, max_depth=3):
  if depth >= max_depth or random.random() < 0.1:  # Base case
    return str(random_number())
  
  left = generate_expression(depth + 1, max_depth)
  right = generate_expression(depth + 1, max_depth)
  operator = random_operator()

  if operator == '/' and right == '0':
    right = str(random.randint(1, INT_MAX)) 

  return f'({left} {operator} {right})'

# Generate a random expression with nesting
random_expression = generate_expression()
with open(r"test/arithmetic_random.cl", "w") as file:
  file.write("class Main inherits IO {\n")
  file.write("  main() : Object {\n")
  file.write("    out_int(")
  file.write(random_expression)
  file.write(")\n")
  file.write("  };\n")
  file.write("};\n")
