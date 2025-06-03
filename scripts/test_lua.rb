#!/usr/bin/env ruby

transform = ARGV[0]

# In order to override env values put them in the devenv.local.nix
# file in the projet root diretory.
LUA_TESTS = ENV["LUA_TESTS"]
RUNPROG = `cabal list-bin multi`.strip
OUT_DIR = "tmp_lua_" + transform

# Build driver first
system("cabal build cubix-examples:multi")
if !$?.success?
  puts "cubix-examples:multi failed to build, aborting..."
  exit
end

Dir.mkdir OUT_DIR

num_loc = 0

Dir.glob(LUA_TESTS + "/*.lua") do |testfil|
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
  system("cd #{OUT_DIR}; lua -e\"_U=true\" all.lua")
end
