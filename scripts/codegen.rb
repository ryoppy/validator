types = []
args = []
values = []
(2..22).each { |i|
  types.push "A#{i}"
  args.push "a#{i}: Validation[A#{i}]"
  values.push "a#{i}"
  puts "def apply[#{types.join(", ")}](#{args.join(", ")}) = #{values.join(" :: ")}"
}
