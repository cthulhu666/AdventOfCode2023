DIGITS = {
  'one' => '1',
  'two' => '2',
  'three' => '3',
  'four' => '4',
  'five' => '5',
  'six' => '6',
  'seven' => '7',
  'eight' => '8',
  'nine' => '9'
}

lines = File.readlines('input.txt').map(&:strip)

def indices(str, substr, i = 0)
  i = str.index(substr, i)
  return [] if i.nil?

  [i] + indices(str, substr, i + 1)
end

def parse_line(s)
  arr = (DIGITS.keys.map { |d| [d, indices(s, d)] } + DIGITS.values.map { |d| [d, indices(s, d)] }).
    select { |_, i| i.any? }.
    reduce([]) { |acc, (s, arr)| acc + arr.map { |n| [s, n] } }.
    sort_by { |_, i| i }
  n = [DIGITS.fetch(arr[0][0], arr[0][0]), DIGITS.fetch(arr[-1][0], arr[-1][0])].join.to_i
  n
end

puts lines.map { |l| parse_line(l) }.sum

__END__

six7sixqrdfive3twonehsk = 61 # not 62!
zoneight47five5sixjxd74 = 14

