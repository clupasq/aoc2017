# This solves part 2 of the challenge
require 'pp'
require 'set'

FILE = 'input/day14.out'

text = File.read FILE
rows = text.split ?\n

blocks = Set.new

128.times do |y|
  128.times do |x|
    blocks << [y, x] if rows[y][x] == ?1
  end
end

links = []

blocks.each do |c|
  y, x = c
  neighbours = [
    [y, x-1], # left
    [y-1, x]  # top
  ]
  neighbours.each do |n|
    links << [c, n] if blocks.include? n
  end
end

Cluster = Struct.new(:value, :elements, :parent) do
  def top
    self.parent == self ? self : parent.top
  end
end

clusters = {}
blocks.each do |b|
  c = Cluster.new(b, Set.new([b]))
  c.parent = c
  clusters[b] = c
end

p blocks.size
p links.size

links.each do |a, b|
  ca = clusters[a].top
  cb = clusters[b].top
  next if ca == cb
  set = ca.elements | cb.elements
  set.each do |x|
    cluster = clusters[x]
    cluster.parent = ca
    cluster.elements = set
  end
end

p clusters.count {|k, v| v.parent == v }

