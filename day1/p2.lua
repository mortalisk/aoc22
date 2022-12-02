function p(t)
  for key, val in pairs(amounts) do
    print(key, val)
  end
end


amounts = {[1] = 0}
current = 1
for line in io.lines("input1") do
  if line == "" then
    current = current + 1 
    amounts[current] = 0
  else
    amounts[current] = amounts[current] + tonumber(line)
  end
end

table.sort(amounts)
n = table.getn(amounts)

p(amounts)
print(amounts[n] + amounts[n-1] + amounts[n-2])

