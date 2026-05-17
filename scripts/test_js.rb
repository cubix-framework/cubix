#!/usr/bin/ruby

transform = ARGV[0]

TIMELIMIT = "45s"

# Jakub 2025.06.12: Updated to match new Lua tests script, but I
#                   have not run it.
JS_TESTS = ENV["JS_TESTS"]
JS_EXCLUDE = ENV["JS_EXCLUDE"]
JS_PRELUDE = ENV["JS_PRELUDE"]
RUNPROG = `cabal list-bin multi`.strip
OUT_DIR = "tmp_js_" + transform

TC_ARR_INIT = "TestCoverage = {coverage: []};\n"
def addTestCoverageArr(s)
  if s.index(/^\"use strict\"/m)
    s.gsub(/"use strict";?/m, "\"use strict\"; " + TC_ARR_INIT)
  else
    TC_ARR_INIT + s
  end
end

# Build driver first
system("cabal build cubix-examples:multi")
if !$?.success?
  puts "cubix-examples:multi failed to build, aborting..."
  exit
end

Dir.mkdir OUT_DIR

excluded_tests = File.read(JS_EXCLUDE).split(/\s+/)

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

  # Feed via stdin so node runs as a script (this === globalThis), not
  # as a CommonJS module (this === module.exports). The ES5 test262
  # corpus uses `this.x` to mean "set on the global", which only
  # works in script mode — historical Node defaulted there, modern
  # Node defaults to module mode when invoked with a file argument.
  system("cat #{JS_PRELUDE} #{testfil} | node")

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

  system("cat #{JS_PRELUDE} #{outfil} | node")
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
