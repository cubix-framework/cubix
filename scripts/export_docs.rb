#!/usr/bin/ruby

require 'psych'
require 'shellwords'

##################### Helpers ############################

# Like the colorize gem, but with no dependency
class String
  def red;            "\e[31m#{self}\e[0m" end
  def green;          "\e[32m#{self}\e[0m" end
end


######################## Safety checks ###################

if not File.exists?("stack.yaml") and not File.exists?("cubix.cabal")
  puts "Must execute #{ARGV[0]} from the Cubix root directory"
  exit! 1
end


if not ENV.member?("CUBIX_DOC_EXPORT")
  puts "Must set CUBIX_DOC_EXPORT to the location to export. Typically, this will be a subdirectory of cubix-www"
  exit! 1
end

##################### Actual script #####################

target_dir = ENV["CUBIX_DOC_EXPORT"]

puts "Will export Haddock to #{target_dir}".green
puts "ANY EXISTING FILES AT THIIS LOCATION WILL BE WIPED".red

system("stack build cubix cubix-compdata compstrat comptrans --ghc-options='-O0 -j +RTS -A256m -n2m -RTS' --haddock --no-haddock-deps")

paths = Psych.safe_load(`stack path`)
doc_path = File.join(paths["local-install-root"], "doc")

puts "Copying path #{doc_path} to #{target_dir}".green

system("rm -rf #{Shellwords.escape(target_dir)}")
system("cp -r #{Shellwords.escape(doc_path)} #{Shellwords.escape(target_dir)}")

