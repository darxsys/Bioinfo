import sys

def generate_reverse(path):
    """Generates reverse complements for a fasta file and outputs them
    to the stdout.
    """
    
    with open(path, "r") as f:
        for line in f:
            line = line.strip()
            # print (line)  
            if len(line) == 0:
                continue
                
            if line[0] == ">":
                line = line + "_R"
                print(line)
            else:
                buf = ""
                for char in line:
                    if char == "A":
                        buf += "T"
                    elif char == "T":
                        buf += "A"
                    elif char == "G":
                        buf += "C"
                    elif char == "C":
                        buf += "G"

                print (buf[::-1])

if __name__ == "__main__":
    if not len(sys.argv) == 2:
        raise ValueError("One argument needed - <name_of_input_file>.")

    generate_reverse(sys.argv[1])
