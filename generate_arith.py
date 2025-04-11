import random

def gen_arith(depth=0, max_depth=5):
    if depth >= max_depth:
        return str(random.randint(1, 100))

    operator = random.choice(["+", "-", "*"])

    if random.random() < 0.5:
        return f"~({gen_arith(depth + 1, max_depth)})"

    left_operand = gen_arith(depth + 1, max_depth)
    right_operand = gen_arith(depth + 1, max_depth)

    return f"({left_operand} {operator} {right_operand})"

if __name__ == "__main__":
  nested_expression = gen_arith()
  print(nested_expression)
