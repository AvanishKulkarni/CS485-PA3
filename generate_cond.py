import random
from generate_bool import gen_cond
from generate_arith import gen_arith

def rand_int():
    return random.randint(1, 100)

variables = [rand_int() for _ in range(100)]

def generate_condition(depth=0, max_depth=3):
    if depth >= max_depth:
        return gen_cond(variables, depth=3)

    then_branch = generate_condition(depth + 1, max_depth)
    else_branch = generate_condition(depth + 1, max_depth)

    return f"if ({gen_cond(variables, depth=3)}) then ({then_branch}) else ({else_branch}) fi"

# Generate the nested conditional string
nested_conditions = generate_condition(max_depth=10)

with open(r"test/cond_random.cl", "w") as file:
  file.write("class Main inherits IO {\n")
  file.write("  main() : Object {\n")
  file.write("    if (")
  file.write(nested_conditions)
  file.write(")\n")
  file.write(f"    then\n      out_int({gen_arith(depth=5)})\n    else\n      out_int({gen_arith(depth=5)})\n    fi\n")
  file.write("  };\n")
  file.write("};\n")