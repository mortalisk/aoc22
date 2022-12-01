

biggest = 0
current = 0
for line in io.lines("input1") do
  if line == "" then
    biggest = math.max(biggest, current)
    current = 0
  else
    current = current + tonumber(line)
  end
end

print(biggest)



