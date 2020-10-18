#!/bin/env ruby

hs_files = %x!fd '\.hs$' selda/src!.split
hs_files.each do |source|
  target = source.sub(/^selda\/src/, 'selda-sqlite/impl')
  puts "#{source} -> #{target}"
  %x!mkdir -p #{File.dirname target}!
  %x!ln -sri #{source} #{target}!
end
