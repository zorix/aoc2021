function lines_from(file)
  lines = {}
  for line in io.lines(file) do
    table.insert(lines, line)
    -- lines[#lines + 1] = line
  end
  return lines
end

function wrong_char_for_line(l)
  local s = ''
  for i = 1, #l do
    local c = l:sub(i,i)
    
    if c == '(' or c == '[' or c == '{' or c == '<' then
      s = s..c
    elseif c == ')' or c == ']' or c == '}' or c == '>' then
      local lc = s:sub(-1,-1)
      if lc == '(' and c == ')' then
        s = s:sub(1,-2)
      elseif lc == '[' and c == ']' then
        s = s:sub(1,-2)
      elseif lc == '{' and c == '}' then
        s = s:sub(1,-2)
      elseif lc == '<' and c == '>' then
        s = s:sub(1,-2)
      else
        return c
      end
    end
  end
  return nil
end

function part_a(lines)
  local t = {}
  t[')'] = 0
  t[']'] = 0
  t['}'] = 0
  t['>'] = 0
  for i = 1, #lines do
    local wc = wrong_char_for_line(lines[i])
    if wc ~= nil then
      t[wc] = t[wc] +1
    end
  end
  local sum = 0
  sum = sum + t[')'] * 3
  sum = sum + t[']'] * 57
  sum = sum + t['}'] * 1197
  sum = sum + t['>'] * 25137
  print(sum)
end

local lines = lines_from("./input.txt")

part_a(lines)

function remaining_open_for_line(l)
  local s = ''
  for i = 1, #l do
    local c = l:sub(i,i)
    
    if c == '(' or c == '[' or c == '{' or c == '<' then
      s = s..c
    elseif c == ')' or c == ']' or c == '}' or c == '>' then
      local lc = s:sub(-1,-1)
      if lc == '(' and c == ')' then
        s = s:sub(1,-2)
      elseif lc == '[' and c == ']' then
        s = s:sub(1,-2)
      elseif lc == '{' and c == '}' then
        s = s:sub(1,-2)
      elseif lc == '<' and c == '>' then
        s = s:sub(1,-2)
      else
        return nil
      end
    end
  end
  return s
end

function part_b(lines)
  local sums = {}
  for i = 1, #lines do
    local rc = remaining_open_for_line(lines[i])
    if rc ~= nil then
      local s = 0
      for j = rc:len(), 1, -1 do
        s = s * 5
        local c = rc:sub(j,j)
        if c == '(' then
          s = s + 1
        elseif c == '[' then
          s = s + 2
        elseif c == '{' then
          s = s + 3
        elseif c == '<' then
          s = s + 4
        end
      end
      table.insert(sums, s)
    end
  end

  table.sort(sums)
  print(sums[math.ceil(#sums/2)])
end

part_b(lines)
