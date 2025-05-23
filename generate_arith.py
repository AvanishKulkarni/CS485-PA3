import random

def gen_arith(depth=0, max_depth=5, input=[random.randint(1, 100) for _ in range(100)]):
    if depth >= max_depth:
        if (random.random() < 0.5):
            return f"{random.choice(input)} <- {random.choice(input)}"
        else:
            return f"{random.choice(input)}"

    operator = random.choice(["+", "-", "*",])

    if random.random() < 0.5:
        return f"~({gen_arith(depth + 1, max_depth, input=input)})"

    left_operand = gen_arith(depth + 1, max_depth, input=input)
    right_operand = gen_arith(depth + 1, max_depth, input=input)

    return f"({left_operand} {operator} {right_operand})"

if __name__ == "__main__":
  letters = [random.randint(0, 100) for c in range(1000)]
  nested_expression = gen_arith(max_depth=10, input=letters)
  print(nested_expression)
