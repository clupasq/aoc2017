


def main
  a = 1

  b = 84 * 100 + 100000
  c = b + 17000

  while true do
    f = 1
    d = 2

    begin
      e = 2

      begin
        if (d*e) == b
          f = 0
        end
        e += 1

      end until e == b

      d += 1

    end until d == b

    puts [a, b, c, d, e, f, h]

    if f != 0
      h += 1
    end

    if b == c
      puts h
      return
    end

    b += 17
  end

end

main

