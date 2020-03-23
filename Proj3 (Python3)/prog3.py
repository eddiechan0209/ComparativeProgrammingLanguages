#! /usr/bin/env python
import os.path
from subprocess import call
import sys

student = sys.argv[1]
score = 0

def progTest(tnum,points):
    testinfile = "testin"+tnum+".txt"
    testoutfile = "testout"+tnum+".txt"
    progfile = "prog"+tnum+".txt"
    if (call(["python3","tli.py",  "../../"+progfile],stdin=open("../../"+testinfile,"r+"),stdout=open(testoutfile,"w+")) == 0):
        if (not os.path.isfile(testoutfile)):
            print "No "+testoutfile+" generated"
        else:
            if (call(["diff","-b", "-B", "../../"+testoutfile, testoutfile]) == 0):
                print "correct "+testoutfile
                return points
            else: 
                print "wrong " + testoutfile
                return 0
    else:
        print "test"+tnum+" failed"
        return 0


def progTestError(tnum,points):
    testinfile = "testin"+tnum+".txt"
    testoutfile = "testout"+tnum+".txt"
    progfile = "prog"+tnum+".txt"
    if (call(["./tli",  "../../"+progfile],stdin=open("../../"+testinfile,"r+"),stderr=open(testoutfile,"w+")) != 0):
        if (not os.path.isfile(testoutfile)):
            print "No "+testoutfile+" generated"
        else:
            if (call(["diff","-b", "-B", "../../"+testoutfile, testoutfile]) == 0):
                print "correct "+testoutfile
                return points
            else: 
                print "wrong " + testoutfile
                return 0
    else:
        print "test"+tnum+" failed"
        return 0


if (not os.path.isdir(student)):
    print student,"not a directory"
    sys.exit(1)
else:
    print "processing",student
    os.chdir(student)

if (not os.path.isfile('tli.py')):
    print "No tli.py"

call(["rm","testout*.txt"])


score = score + progTest("1a",1)
score = score + progTest("1b",1)
score = score + progTest("1c",1)
score = score + progTest("1d",1)
score = score + progTest("2a",1)
score = score + progTest("2b",1)
score = score + progTest("2c",1)
score = score + progTest("3a",1)
score = score + progTest("3b",1)
score = score + progTest("4a",0.5)
score = score + progTest("4b",0.5)

print student, "score is ", score
