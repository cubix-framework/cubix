#!/usr/bin/ruby

TC_ARR_INIT = "TestCoverage = {coverage: []};\n"
def addTestCoverageArr(s)
  if s.index(/^\"use strict\"/m)
    s.gsub(/"use strict";?/m, "\"use strict\"; " + TC_ARR_INIT)
  else
    TC_ARR_INIT + s
  end
end

RUNPROG = ".stack-work/dist/x86_64-osx/Cabal-3.8.1.0/build/examples-multi/examples-multi"
TIMELIMIT = "45s"

JS_DIR     = "/Users/jkoppel/research_large/other_frameworks/javascript-semantics/"
PATH_TO_JS = "node"
JS_TESTS   = JS_DIR + "test262/test/suite/"
PRELUDE = JS_DIR + "prelude.js"

transform = ARGV[0]

OUT_DIR = "tmp_js_" + transform

Dir.mkdir OUT_DIR

excluded_tests = File.read(JS_DIR + "list-invalid-tests.txt").split(/\s+/)

tests = `find #{JS_TESTS}ch{08,09,10,11,12,13,14} -name '*.js'`.split(/\s+/).select {|s| not (excluded_tests.index s) }

num_tests = 0
num_passed = 0

num_loc = 0

tests.each do |testfil|
  outfil = OUT_DIR + "/" + File.basename(testfil)

  
  if transform == "count_loc"
    num_loc += `wc -l #{testfil}`.to_i
    next
  end

  test_contents = File.read(testfil)
  positive = (nil == test_contents.index("@negative"))

  # I had mysterious errors on big files using "<(" that "=(" seems to fix
  system("zsh -c \"#{PATH_TO_JS} =(cat #{PRELUDE} #{testfil})\"")

  status = $?

  num_tests += 1

  if (status == 0) == positive
    puts "Node passed: #{testfil}"
  else
    puts "Node failed: #{testfil}"
    puts "ALERT: Node failed negative test" unless positive
    next
  end
  
  if transform != "id"
    system("#{RUNPROG} javascript id #{testfil} > /dev/null")
    if $? != 0 then
      puts "Failed round trip or parse: #{testfil}"
      num_passed += 1 if !positive
      next
    end
  end
  
  puts "Transforming from #{testfil} to #{outfil}"
  res = `gtimeout #{TIMELIMIT} #{RUNPROG} javascript #{transform} #{testfil}`

  if $? != 0 then
    if positive
      puts "Failed to transform: #{testfil}"
    else
      puts "Correctly failed to transform: #{testfil}"
      num_passed += 1
    end
    next
  end

  File.open(outfil, "w") do |f|
    f.puts(addTestCoverageArr(res))
  end

  # I had mysterious errors on big files using "<(" that "=(" seems to fix
  system("zsh -c \"#{PATH_TO_JS} =(cat #{PRELUDE} #{outfil})\"")
  status = $?

  if (status == 0) == positive
    puts "Test passed: #{testfil}"
    num_passed += 1
  else 
    puts "Test failed: #{testfil}"
    puts "ALERT: Negative test failure"
  end
    
end


if transform == "count_loc"
  puts num_loc
else
  puts "Tests: #{num_tests}"
  puts "Passed: #{num_passed}"
end
