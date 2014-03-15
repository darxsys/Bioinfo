import sys

def main(path, percentage):
    with open(path, "r") as f:
        lines = f.readlines()
        count = int(percentage * len(lines)) + 1

        fp = 0
        print (count)

        for i in range(min(count, len(lines))):
            line = lines[i].strip().split()
            if not line[0] == "-1":
                continue

            fp += 1

    print (fp / float(count))

if __name__ == "__main__":
    if not len(sys.argv) == 3:
        raise ValueError ("Not a good number of args.")
    
    main(sys.argv[1], float(sys.argv[2]))