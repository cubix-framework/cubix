#!/usr/bin/ruby

# WARNING
# When running Java tests for multiple transformations, please offset them by a few minutes
# to avoid having multiple instances of javac compile the same file at the same time
#
# You'd think this would be benign....but no. We found instances where running the compiled version
# of a Java test (an original, un-transformed version) would actually produce the output of a different test.
# I did not investigate further because I didn't have a working decompiler installation ready, but did verify
# that this was so even though the bytecode contained the right class file name.
# 
# And so, we had a run where the identity transformation failed many tests that the Hoist/Testcov transformation passed!
# Offsetting the runs of the tests by 5 minutes resulted in a flawless execution.

JAVA_DIR     = "/Users/jkoppel/research_large/other_frameworks/java-semantics/"
# Using system javac
JAVA_TESTS   = JAVA_DIR + "tests/"

RUNPROG = ".stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/examples-multi/examples-multi"

TIMELIMIT=60

transform = ARGV[0]

OUT_DIR = "tmp_java_" + transform

Dir.mkdir OUT_DIR

def get_output(fil, input_fil)
  folder = File.dirname(fil)
  system("cd #{folder}; javac #{File.basename(fil)}")
  class_name = File.basename(fil, File.extname(fil))

  input = ""
  if input_fil != nil
    input = "< #{input_fil}"
  end
  
  res = `cd #{folder}; java #{class_name} #{input}`
  res
end

def compare_output(fil1, fil2, input_fil)
  out1 = get_output(fil1, input_fil)
  out2 = get_output(fil2, input_fil)

  if out1 != out2
    puts "Output different: #{fil1} #{fil2}"
    puts "File 1 is:\n#{out1}"
    puts "File 2 is:\n#{out2}"
    false
  else
    true
  end
end

num_tests = 0
num_passed = 0

num_loc = 0

Dir.glob(JAVA_TESTS + "*/") do |testdir|
  out_subdir = OUT_DIR + "/" + File.basename(testdir)

  Dir.mkdir out_subdir

  Dir.glob(testdir + "*.java") do |testfil|

    if File.directory? testfil
      puts "Skipping directory #{testfil}"
      next
    end

    
    if transform == "count_loc"
      num_loc += `wc -l #{testfil}`.to_i
      next
    end

    num_tests += 1

    outfil = out_subdir + "/" + File.basename(testfil)

    testnam = File.basename(testfil, File.extname(testfil))
    input_fil = testdir + testnam + ".in"
    if !(File.exists?(input_fil))
      input_fil = nil
    end
    
    if transform != "id"
      system("#{RUNPROG} java id #{testfil} > /dev/null")
      if $? != 0 then
        puts "Failed round trip or parse: #{testfil}"
        next
      end
    end
    
    puts "Transforming from #{testfil} to #{outfil}"
    res = `gtimeout #{TIMELIMIT} #{RUNPROG} java #{transform} #{testfil}`

    File.open(outfil, 'w') do |f|
      f.puts(res)
      f.puts("class TestCoverage { public static boolean[] coverage = new boolean[100000]; }")
    end

    if compare_output(testfil, outfil, input_fil)
      num_passed += 1
    end
  end
end

if transform == "count_loc"
  puts num_loc
else
  puts "Tests: #{num_tests}"
  puts "Passed: #{num_passed}"
end

# FIXME: Handle package tests (test folder 61)
