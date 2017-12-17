
# Naive
#
Node = Struct.new(:value, :next) do
  def insert_after(value)
    new_node = Node.new value, self.next
    self.next = new_node
  end

  def rotate(r)
    r == 0 ? self : self.next.rotate(r-1)
  end
end

def spinlock(rot, count)
  n = Node.new(0)
  n.next = n
  l = 1
  count.times do |i|
    r = rot % l
    n = n.rotate(rot).insert_after(i + 1)
    l += 1
  end
  n
end



def run
  question1 = spinlock(354, 2017)
  puts question1.next.value
  question2 = spinlock(354, 500_000)
end


# from https://www.reddit.com/r/adventofcode/comments/7kc0xw/2017_day_17_solutions/drd5y54/

input = ARGV[0]&.to_i || 354

buffer = [0]
pos = 0

(1..2017).each { |n|
  pos = (pos + input) % buffer.size
  buffer.insert(pos + 1, n)
  pos += 1
}
puts buffer[buffer.index(2017) + 1]

value_after_zero = nil

pos = 0

(1..50_000_000).each { |n|
  pos = (pos + input) % n
  value_after_zero = n if pos == 0
  pos += 1
}

puts value_after_zero

