def modify_file(input_file, output_file, prefix, suffix):
    with open(input_file, 'r') as infile, open(output_file, 'w') as outfile:
        for line in infile:
            modified_line = f"{prefix}{line.strip()}{suffix}\n"
            modified_line = modified_line.replace("%", "%%")
            outfile.write(modified_line)

# Example usage
input_filename = "input.txt"
output_filename = "output.txt"
prepend_string = "fprintf aout \"\\t"
append_string = "\\n\";"

modify_file(input_filename, output_filename, prepend_string, append_string)
