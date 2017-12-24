require 'rspec/autorun'


class World
  def initialize(str)
    @cells = parse_map str
    @dir   = :N
    @pos   = [0, 0]
    @inf_count = 0
  end

  attr_reader :cells, :inf_count

  def update
    old_state = @cells[@pos] || :Clean
    @dir = change_dir(old_state)
    new_state = trans_state(old_state)
    @cells[@pos] = new_state
    @inf_count += 1 if new_state == :Infected
    @pos = advance
  end

  private

  def advance
    y, x = @pos
    case @dir
    when :N then [y-1, x  ]
    when :E then [y  , x+1]
    when :S then [y+1, x  ]
    when :W then [y  , x-1]
    end
  end

  def trans_state(s)
    case s
    when :Clean then :Weakened
    when :Weakened then :Infected
    when :Infected then :Flagged
    when :Flagged then :Clean
    end
  end

  def change_dir(s)
    case s
    when :Clean     then left(@dir)
    when :Weakened  then @dir
    when :Infected  then right(@dir)
    when :Flagged   then left(left(@dir))
    end
  end

  def left(dir)
    case dir
    when :N then :W
    when :E then :N
    when :S then :E
    when :W then :S
    end
  end

  def right(dir)
    case dir
    when :N then :E
    when :E then :S
    when :S then :W
    when :W then :N
    end
  end



  def change_state
  end

  def parse_map(str)
    lines = str.lines
    size = lines.size
    offset = size / 2
    ({}).tap do |cells|
      lines.each.with_index do |l, y|
        l.chars.each.with_index do |c, x|
          next unless c == ?#
            cells[[y - offset, x - offset]] = :Infected
        end
      end
    end
  end
end



MAPSTR = <<EOS
..#
#..
...
EOS

describe 'Virus' do

  it 'can parse map' do
    world = World.new(MAPSTR)
    expect(world.cells).to eq ({ [-1,  1] => :Infected,
                                 [ 0, -1] => :Infected })
  end

  it 'answers q2 on test scenario' do
    world = World.new(MAPSTR)
    100.times { world.update }
    expect(world.inf_count).to eql 26
  end

  # it 'answers q2 on test scenario 2' do
  #   world = World.new(MAPSTR)
  #   10_000_000.times { world.update }
  #   expect(world.inf_count).to eql 2511944
  # end

end


def main
  text = File.read('input/day22.in')
  world = World.new text
  10_000_000.times do
    world.update
  end
  puts world.inf_count
end

main


