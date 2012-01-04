import sys
import subprocess

def report_error(file, line, msg):
    print "%s(%d): error: %s" % (file, line, msg)

def check_status(file, line, expected, actual):
    if (expected != actual):
        report_error(file, line, "expected status = %d, received %d" % (expected, actual)) 

def check_output(file, line, expected, actual):
    if (expected != actual):
        report_error(file, line, "output doesn't match expectation:")
        sys.stdout.write(actual)

def run_test(file, line, command, input, output, status):
    s = "test"
    p = subprocess.Popen(command,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            shell=True)
    actual_output, _ = p.communicate(input)
    actual_status = p.returncode
    check_status(file, line, status, actual_status)
    check_output(file, line, output, actual_output)

class TestFile(object):
    def __init__(self, filename):
        self.file = open(filename, "r")
        self.lineno = 0
        self.line = ''
        self.read_line()
        self.eof = False

    def skip_blank_lines(self):
        while not self.eof and self.line.strip() == '':
            self.read_line()

    def read_to_marker(self, marker):
        text = ''
        while not self.eof:
            line = self.line
            self.read_line()
            if (line.strip() == marker): return text
            text += line
        print "Error at end of file."
        sys.exit(1)

    def read_line(self):
        self.lineno += 1
        self.line = self.file.readline()
        if self.line == "":
            self.eof = True

    def eof(self):
        return self.eof

def parse_test_file(filename):
    try:
        file = TestFile(filename)
    except:
        print "Error opening %s" % filename
        return None

    file.skip_blank_lines()
    while not file.eof:
        lineno = file.lineno
        command = file.read_to_marker("***")
        input = file.read_to_marker("***")
        output = file.read_to_marker("***")
        status_text = file.read_to_marker("===")
        file.skip_blank_lines()
        try:
            status = int(status_text)
        except ValueError:
            report_error(filename, lineno, "Invalid status code: %s" % status_text)
            sys.exit(1)

        run_test(filename, lineno, command, input, output, status)

def main():
    parse_test_file("test")

if __name__ == '__main__':
    main()
