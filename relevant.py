import sys

OFFSET = 0

def find_num_relevant(path):
    num = 0
    pairs = set()
    with open(path, "r") as f:
        lines = f.readlines()
        for i in range(len(lines)):
            line = lines[i]
            if not line[0] == ">":
                continue

            line = line.strip().split("_")
            # print (line)

            for j in range(i+1, len(lines)):
                line_j = lines[j]
                if not line_j[0] == ">":
                    continue

                line_j = line_j.strip().split("_")
                # print (line_j[1] + " " + line[1])
                # print ("here")
                if int(line_j[2]) >= int(line[2]) + OFFSET and \
                        int(line_j[2]) <= int(line[3]) - OFFSET:

                    t = (int(line[1]), int(line_j[1]))
                    pairs.add(t)
                    num += 1

                if int(line[2]) >= int(line_j[2]) + OFFSET and \
                        int(line[2]) <= int(line_j[3]) - OFFSET:

                    t = (int(line[1]), int(line_j[1]))
                    pairs.add(t)
                    num += 1

    return num, pairs

def precision_recall(path_dataset, path_results, path_roc):
    num, pairs = find_num_relevant(path_dataset)

    num_retrieved = 0
    num_relevant = 0

    # temp = 0
    out = open(path_roc, "w")
    remember = set()

    max_score = 0

    with open(path_results, "r") as f:
        lines = f.readlines()
        lines = sorted(lines, key=lambda line: int(line.strip().split()[-1]), reverse = True)
        # max_score = int(lines[0].strip().split()[-1]

        for line in lines:
            line = line.strip().split()
            if line[0] == line[1]:
                continue

            if max_score == 0:
                max_score = int(line[-1])
                # print (max_score)

            id1 = int(line[0].split("_")[1])
            id2 = int(line[1].split("_")[1])

            # switch values
            if id2 < id1:
                id1 ^= id2
                id2 ^= id1
                id1 ^= id2

            if (id1, id2) in remember:
                continue

            remember.add((id1, id2))
            num_retrieved += 1

            if (id1, id2) in pairs:
                num_relevant += 1

            if (id1, id2) in pairs:
                # out.write("1\t" + str(1) + "\n")
                out.write("1\t" + str(float(line[-1]) / max_score) + "\n")
            else:
                # out.write("-1\t" + str(1) + "\n")
                out.write("-1\t" + str(float(line[-1]) / max_score) + "\n")

    prec = float(num_relevant) / num_retrieved
    rec = float(num_relevant) / num
    out.write("method\tprec=%.3lf,recall=%.3lf\n" % (prec, rec))

    # preproc(pairs, path_results, path_roc, prec, rec)
    # print ("Temp prec_rec: " + str(temp))
    return prec, rec

def preproc(pairs, in_file, out_file, precision, recall):
    # num, pairs = relevant.find_num_relevant(dataset)
    remember = set()

    out = open(out_file, "w")
    out.write("method\tprec=%.3lf,recall=%.3lf\n" % (precision, recall))

    # temp = 0

    with open(in_file, "r") as f:
        lines = f.readlines()
        lines = sorted(lines, key=lambda line: int(line.strip().split()[-1]), reverse = True)
        # print (lines)
        # max_e = float(lines[-1].strip().split()[-2])
        for line in lines:
            line = line.strip().split()
            if line[0] == line[1]:
                continue

            id1 = int(line[0].split("_")[1])
            id2 = int(line[1].split("_")[1])

            # switch values
            if id2 < id1:
                id1 ^= id2
                id2 ^= id1
                id1 ^= id2

            if (id1, id2) in remember:
                continue

            remember.add((id1, id2))
            # num_retrieved += 1
            # temp += 1

            if (id1, id2) in pairs:
                out.write("1\t" + str(float(line[-1])) + "\n")
            else:
                out.write("-1\t" + str(float(line[-1])) + "\n")
                # num_relevant += 1
    # print ("Temp preproc: " + str(temp))

if __name__ == "__main__":
    if not len(sys.argv) == 4:
        raise ValueError("Wrong number of arguments. Three input files needed. \
            <path_to_dataset>  <path_to_swsharp_results_in_bm8_format> <path_to_roc_output_file>.")

    # print(find_num_relevant(sys.argv[1]))
    print(precision_recall(sys.argv[1], sys.argv[2], sys.argv[3]))