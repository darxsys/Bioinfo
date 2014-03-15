#!/usr/bin/python
import sys

def main(stat_file_name, filled_gap_name, output_name):
    """This function generates a new stat file in the following manner:
    For each line of the input stat file, it checks if the gap presented
    by that line is in the filled_gap file. If it is, that means the gap
    has been wrongly filled by finis. Otherwise, the gap was correctly 
    filled. This is indicated by a flag appended to each line of the stat
    file. 0 - wrong, 1 - good. It writes the new stat file to the output_name.
    """

    stat_file = open(stat_file_name, "r")
    if stat_file == None:
        raise ValueError("Could not open statistics file.")

    filled_gap = open(filled_gap_name, "r")
    if filled_gap == None:
        raise ValueError("Could not open new filled file.")

    output = open(output_name, 'w')
    if output == None:
        raise ValueError("Could not open output file.")

    i = 1
    j = 0
    stat_lines = stat_file.readlines()
    filled_lines = filled_gap.readlines()
    header = stat_lines[0].strip()
    new_header = header + "\t 0-wrong,1-okay\n"

    output.write(new_header)

    while i < len(stat_lines) and j < len(filled_lines):
        orig_line = stat_lines[i].strip()
        # to tackle tail of the statistic file 
        try:
            num = int(filled_lines[j].split()[0])
        except:
            num = -1

        if i == num:
            new_line = orig_line + "\t0\n"
            j += 1
        else:
            new_line = orig_line + "\t1\n"

        output.write(new_line)
        i += 1

    while i < len(stat_lines):
        new_line = stat_lines[i].strip() + "\t1\n"
        output.write(new_line)
        i += 1

if __name__ == "__main__":
    if not len(sys.argv) == 4:
        print ("python script.py <gaps_stat_file> <filled_gap_status> <output_file>")
        raise ValueError("Wrong number of arguments.")
        # sys.exit(1)

    main(sys.argv[1], sys.argv[2], sys.argv[3])