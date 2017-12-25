require 'rspec/autorun'

class TuringMachine
  def initialize
    @left = [0]
    @right = [0]
    @count_set = 0
    @pointer = 0
    @exec_count = 0
  end

  attr_reader :state, :count_set

  def execnext
    new_val, dir, new_state = @rules[@state][read]
    write(new_val)
    move(dir)
    @state = new_state
    p [@state, @pointer, debug_state] if @debug

    @exec_count += 1
    puts @exec_count if @exec_count % 100000 == 0
  end


  def set_state(s)
    @state = s
  end

  def read
    l, i = location
    l[i]
  end

  def write(val)
    l, i = location
    @count_set += 1 if l[i] == 0 && val == 1
    @count_set -= 1 if l[i] == 1 && val == 0
    l[i] = val
  end

  def move(dir)
    case dir
    when :left
      move_left
    when :right
      move_right
    end
  end

  def move_left
    @pointer -= 1
    l, i = location
    l << 0 if l.size <= i
  end

  def move_right
    @pointer += 1
    l, i = location
    l << 0 if l.size <= i
  end

  def debug_state
    @left.reverse + @right
  end

  private

  def location
    list  = @pointer < 0 ? @left : @right
    index = @pointer < 0 ? -@pointer-1 : @pointer
    [list, index]
  end
end


class SampleMachine < TuringMachine

  def initialize
    super
    @state = :A
    # @debug = true
    @rules = {
      A: {
        0 => [1, :right, :B],
        1 => [0, :left , :B]
      },
      B: {
        0 => [1, :left , :A],
        1 => [1, :right, :A]
      },
    }
  end

end


describe :SampleMachine do

  it 'can execute correctly' do
    s = SampleMachine.new
    6.times {s.execnext}
    expect(s.count_set).to eql 3
  end

end






class RealMachine < TuringMachine

  def initialize
    super
    @state = :A
    # @debug = true
    @rules = {
      A: {
        0 => [ 1, :right, :B],
        1 => [ 0, :right, :C]
      },
      B: {
        0 => [ 0, :left, :A],
        1 => [ 0, :right, :D]
      },
      C: {
        0 => [ 1, :right, :D],
        1 => [ 1, :right, :A]
      },
      D: {
        0 => [ 1, :left, :E],
        1 => [ 0, :left, :D]
      },
      E: {
        0 => [ 1, :right, :F],
        1 => [ 1, :left, :B]
      },
      F: {
        0 => [ 1, :right, :A],
        1 => [ 1, :right, :E]
      }
    }
  end

end

r = RealMachine.new
12_368_930.times { r.execnext }
puts r.count_set


