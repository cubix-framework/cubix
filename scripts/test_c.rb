#!/usr/bin/ruby

RUNPROG = ".stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/examples-multi/examples-multi"

GCC_DIR     = "/Users/jkoppel/research_large/other_frameworks/gcc/"
#PATH_TO_GCC = GCC_DIR + "../gcc_build/gcc/xgcc"
# Just using system GCC. GCC torture tests are meant to be portable
# Okay, actually they're not that portable. Using Homebrew GCC
GCC_COMMAND = "/usr/local/bin/gcc-6"
GCC_TORTURE_TESTS   = GCC_DIR + "gcc/testsuite/gcc.c-torture/execute/"

transform = ARGV[0]

OUT_DIR = "tmp_c_" + transform

Dir.mkdir OUT_DIR

num_tests = 0
num_passed = 0

num_loc = 0

Dir.glob(GCC_TORTURE_TESTS + "*.c") do |testfil|
  testnam = File.basename(testfil, File.extname(testfil))

  if transform == "count_loc"
    num_loc += `wc -l #{testfil}`.to_i
    next
  end

  outfil = OUT_DIR + "/" + testnam + ".c"

  num_tests += 1

  system("cd #{GCC_TORTURE_TESTS}; #{GCC_COMMAND} -w -o #{testnam} #{testnam}.c; ./#{testnam}")
  if $? != 0 then
    puts "Clang failed: #{testfil}"
    next
  end

  just_preprocessed = ""

  if transform != "id"
    just_preprocessed = `#{RUNPROG} c id #{testfil} > /dev/null`
    if $? != 0 then
      puts "Failed round trip or parse: #{testfil}"
      next
    end
  end
  
  puts "Transforming from #{testfil} to #{outfil}"
  res = `#{RUNPROG} c #{transform} #{testfil}`

  File.open(outfil, "w") do |f|
    f.puts("struct {int coverage[10000];} TestCoverage;")
    f.puts(res)
  end

  if $? == 0 then
    puts "Running program"
    system("cd #{OUT_DIR}; #{GCC_COMMAND} -w -o #{testnam} #{testnam}.c; ./#{testnam}")
    if $? != 0
      puts "#{testnam} transformed version failed"
    else
      num_passed +=1
    end
  else
    puts "#{testnam} failed transform"
  end
end

if transform == "count_loc"
  puts num_loc
else
  puts "Total: #{num_tests}"
  puts "Passed: #{num_passed}"
end