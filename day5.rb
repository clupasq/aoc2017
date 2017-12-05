def count_steps_inside list
  i = 0
  steps = 0
  while i >= 0 && i < list.size
    crt = list[i]
    list[i] = crt + 1
    i += crt
    steps += 1
  end
  steps
end

def count_steps_inside_2 list
  i = 0
  steps = 0
  while i >= 0 && i < list.size
    crt = list[i]
    list[i] = crt > 2 ? crt - 1 : crt + 1
    i += crt
    steps += 1
  end
  steps
end


puts count_steps_inside [0, 3, 0, 1, -3]
puts count_steps_inside_2 [0, 3, 0, 1, -3]

bigList = File.readlines('input/day5.in').map(&:to_i)

puts count_steps_inside bigList.dup
puts count_steps_inside_2 bigList.dup


