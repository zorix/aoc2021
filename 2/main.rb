depth = 0
x = 0
aim = 0
File.readlines('input.txt').each do |line|
  arr = line.split ' '
  val = arr[1].to_i
  case arr[0]
  when 'down'
    aim += val
  when 'up'
    aim -= val
  when 'forward'
    x += val
    depth += aim * val
  end
end

puts x, depth
puts x*depth
