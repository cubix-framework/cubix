#!/usr/bin/ruby

LUA_DIR     = "/Users/jkoppel/research_large/other_frameworks/lua/"
PATH_TO_LUA = LUA_DIR + "src/lua"
LUA_TESTS   = LUA_DIR + "lua-5.3.3-tests/"

RUNPROG = ".stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/examples-multi/examples-multi"

transform = ARGV[0]

OUT_DIR = "tmp_lua_" + transform

Dir.mkdir OUT_DIR

num_loc = 0

Dir.glob(LUA_TESTS + "*.lua") do |testfil|
  outfil = OUT_DIR + "/" + File.basename(testfil)
  
  if transform == "count_loc"
    num_loc += `wc -l #{testfil}`.to_i
    next
  end
  
  system("#{RUNPROG} lua id #{testfil} > /dev/null")
  if $? != 0 then
    puts "Failed round trip or parse: #{testfil}"
    next
  end
  
  puts "Transforming from #{testfil} to #{outfil}"
  system("#{RUNPROG} lua #{transform} #{testfil} > #{outfil}")
end


if transform == "count_loc"
  puts num_loc
else
  system("cd #{OUT_DIR}; #{PATH_TO_LUA} -e\"_U=true\" all.lua")
end
