import random
from generate_bool import gen_cond
from generate_arith import gen_arith

def rand_int():
    return random.randint(1, 100)

variables = [chr(c) for c in range(ord('a'), ord('z')+1)]

def gen_prog(depth=3):
    if depth <= 0:
        return gen_cond(variables, depth=3)

    then_branch = gen_prog(depth - 1)
    else_branch = gen_prog(depth - 1)

    return f"if ({gen_cond(variables, depth)})\n"

# Generate the nested conditional string
nested_conditions = gen_prog(depth=5)

with open(r"test/cond_random.cl", "w") as file:
    file.write("class Main inherits IO {\n")
    file.write("  main() : Object {\n")
    file.write("    let\n")
    for i in range(len(variables)-1):
        file.write(f'    {variables[i]} : Int,\n')
    file.write(f'    {variables[-1]} : Int\n')
    file.write("    in {\n")
    for var in variables:
        file.write(f'    {var} <- in_int();\n')
    file.write("    ")
    file.write(nested_conditions)
    file.write(f"    then\n      out_int(1)\n    else\n      out_int(0)\n    fi;")
    file.write("}\n")
    file.write("  };\n")
    file.write("};\n")

with open(r"test/cond_random.cl-input", "w") as file:
    for i in range(len(variables)):
        file.write(f"{random.randint(-1024, 1024)}\n")