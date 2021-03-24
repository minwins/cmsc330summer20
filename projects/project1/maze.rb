#!/usr/local/bin/ruby

# ########################################
# CMSC 330 - Project 1
# ########################################

#-----------------------------------------------------------
# the following is a parser that reads in a simpler version
# of the maze files.  Use it to get started writing the rest
# of the assignment.  You can feel free to move or modify 
# this function however you like in working on your assignment.

def read_file(file)
  line = file.gets
  if line == nil then return end

  # read 1st line, must be maze header
  sz, sx, sy, ex, ey = line.split(/\s/)
  # puts "header spec: size=#{sz}, start=(#{sx},#{sy}), end=(#{ex},#{ey})" 
  @size = 0
  @size = Integer(sz)
  @open_cells = 0

  @maze_map = {} 
  @path_map = {}
  @path_name = {}
  @path_arr = []
  @path_short = {}
  @path_short_arr = []

  @start_x = Integer(sx)  
  @start_y = Integer(sy)
  @end_x = Integer(ex)
  @end_y = Integer(ey)
  
  # read additional lines
  while line = file.gets do

    # begins with "path", must be path specification
    if line[0...4] == "path"
      p, name, x, y, ds = line.split(/\s/)
      if !name.nil? && !x.nil? && !y.nil? && !ds.nil?
        @path_map["#{name} #{x} #{y}"] = ["#{ds}"] 
        @path_name["#{name}"] = ["#{x} #{y}"]
      end      
    # otherwise must be cell specification (since maze spec must be valid)
    else
      x, y, ds, w = line.split(/\s/,4)
      if !w.nil? && !ds.nil?
        w.chomp!
        ds.chomp!
      end
      if !ds.nil?
        @maze_map["#{x} #{y}"] = ["#{ds} #{w}"]
        if ds.size == 4  
          @open_cells += 1
        end 
      end
      if ds.nil?
        @maze_map["#{x} #{y}"] = ["  "]
      end
    end
  end

  # if any cells are ommited, this will check and add the cells as no openings.
  i = 0  
  while i < @size
    j = 0
    while j < @size
      if !@maze_map.has_key?("#{i} #{j}")
        @maze_map["#{i} #{j}"] = ["  "]
      end
      j += 1  
    end
    i += 1
  end
end

def open_cells 
  return @open_cells
end

def bridge_cells
  @bridge_cells = 0 
  x = 0
  while x < @size - 1
    y = 0
    while y < @size - 1 
      value = @maze_map["#{x} #{y}"].to_s
      direction, weight = value.split(/\s/)  
        if direction.include? "d"
          value_down = @maze_map["#{x} #{y+1}"].to_s
          direction_down, weight_down = value_down.split(/\s/)
            if direction_down.include? "d"
              @bridge_cells += 1
            end
        end
        if direction.include? "r"
          value_right = @maze_map["#{x+1} #{y}"].to_s
          direction_right, weight_right = value_right.split(/\s/)        
            if direction_right.include? "r"
              @bridge_cells += 1
            end
        end
        y += 1
     end
     x += 1
  end
  return @bridge_cells 
end

def paths_cells(prints)
  if @path_map.empty?
    return "none"
  end

  @path_map.each { |k,v|
    cost = 0.0
    k = k.to_s
    v = v.to_s

    v = v.sub("[" , "")
    v = v.gsub("\"" , "")
    v = v.sub("]" , "")
  
    path = v.chars
  
    k = k.sub("[" , "")
    k = k.gsub("\"" , "") 
    k = k.sub("]" , "")
  
    name = String(k.scan(/^\S+/))
    name = name.gsub("[", "")
    name = name.gsub("]", "") 
    name = name.gsub("\"", "")

    x = Integer(k.scan(/\s(\d+)\s/).join(''))
    y = Integer(k.scan(/\s\d+\s(\d+)/).join(''))    
      
    for i in path
      if (x < @size and x >= 0) && (y < @size and y >= 0)
        direction, weights = @maze_map["#{x} #{y}"].to_s.split(/\s/,2)
          
        direction = direction.sub("[" , "")
        direction = direction.gsub("\"" , "")
        direction_arr = direction.chars
         
        weights = weights.sub("]", "")
        weights = weights.sub("\"", "")
        weights_arr = weights.split(/ /)
               
        if direction_arr.index(i) == nil
           cost = nil
           next
        end
            
        if i == "u"
          index = direction_arr.index(i)
          cost += Float(weights_arr[index])
          y -= 1
        elsif i == "d"
          index = direction_arr.index(i)
          cost += Float(weights_arr[index])
          y += 1
        elsif i == "l"
          index = direction_arr.index(i)
          cost += Float(weights_arr[index])
          x -= 1
        elsif i == "r"
          index = direction_arr.index(i) 
          cost += Float(weights_arr[index])
          x += 1
        end
      end
    end # matches with "for i in path"
    if cost != nil
       formatted = "%10.4f" % cost
       path_new = formatted + " " + name
       @path_arr.push(path_new) 
    end
  }
  if @path_arr.empty?
    return "none"
  end
  @path_arr.sort!

  name = String(@path_arr[0])
  name = name.scan(/\S+$/).to_s     
  
  name = name.gsub("[", "")
  name = name.gsub("]", "") 
  name = name.gsub("\"", "")
  cordinate = String(@path_name["#{name}"])
  
  cordinate = cordinate.gsub(/[^0-9 ]/,"")
  
  @path_short_arr << cordinate

  @path = String(@path_map["#{name} #{cordinate}"])
  @path.to_s
  @path = @path.gsub(/[\W]/,"")
  @path = @path.split(//)
   
  @short_x, @short_y = cordinate.split(/ /)
  @short_x = Integer(@short_x)
  @short_y = Integer(@short_y)  
  x = @short_x 
  y = @short_y 

  for i in @path
    if i == "u"
      y -= 1
      @path_short_arr << "#{x} #{y}"
    elsif i == "d"
      y += 1
      @path_short_arr << "#{x} #{y}"
    elsif i == "l" 
      x -= 1
      @path_short_arr << "#{x} #{y}" 
    elsif i == "r"
      x += 1
      @path_short_arr << "#{x} #{y}" 
    end
  end
   
  if prints == "0"
    return @path_arr
  end
end

def print_cells
  x, y, row, col = 0, 0, 0, 0
  str = ""
  while row < @size * 2 + 1
    col = 0
    while col < @size * 2 + 1
      if row == 0 || row == @size * 2 
        if col%2 == 0
          str << "+"
        else
          str << "-"
        end
      else
        if col == 0 || col == @size * 2
          if row%2 == 0
            str << "+"
          else
            str << "|"
          end 
        else
          value = @maze_map["#{x} #{y}"].to_s
          direction, weight = value.split(/\s/)
          if row%2 == 1 #odd row 
            if col%2 == 1 #odd col spaces in cell
              if x == @start_x && y == @start_y
                if @path_short_arr.include?("#{x} #{y}")                  
                  str << "S"
                else
                  str << "s"
                end
              elsif x == @end_x && y == @end_y
                if @path_short_arr.include?("#{x} #{y}")
                  str << "E"
               else 
                  str << "e"
                end
              elsif @path_short_arr.include?("#{x} #{y}")
                str << "*"
              else
                str << " " 
              end
            elsif col%2 == 0 #even col vertical walls
              if col != 0 && col != 1 
                x += 1
              end
              if direction.include? ("r")
                str << " "
              else
                str << "|"
              end
            end             
          elsif row%2 == 0 #even rows
            if col%2 == 0 #even col always "+" 
              str << "+"
              if col != 0 && col != 1
                x += 1
              end
            elsif col%2 == 1 # odd colums horizontal walls for no
              if direction.include? ("d")
                str << " "
              else
              str << "-"
              end
            end
          end
        end  
      end 
      col += 1  
    end
    if row%2 == 0 && row != 0
      y += 1
    end 
    str << "\n"
    row += 1
    x = 0
  end
  str.chomp!  
  return str
end

def sort_cells
  array_open = []

  zero_opening = "0"
  one_opening = "1"
  two_opening = "2"
  three_opening = "3" 
  four_opening = "4"
  
  @maze_map.each{|key, value| value = value.to_s
  x, y = key.split(/\s/)
 
  direction, weight = value.split(/\s/)
  direction = direction.sub("[" , "")
  direction = direction.sub("\"" , "")
  
  if direction.length == 0
    zero_opening << ",(#{x},#{y})"
  elsif direction.length == 1
    one_opening << ",(#{x},#{y})"
  elsif direction.length == 2
    two_opening << ",(#{x},#{y})"
  elsif direction.length == 3
    three_opening << ",(#{x},#{y})"
  elsif direction.length == 4
    four_opening << ",(#{x},#{y})"
  end
  }
  if zero_opening != "0"
    array_open << zero_opening 
  end
  if one_opening != "1" 
    array_open << one_opening 
  end
  if two_opening != "2" 
    array_open << two_opening 
  end
  if three_opening != "3" 
    array_open << three_opening 
  end
  if four_opening != "4" 
    array_open << four_opening 
  end   
  return array_open  
end

def solve_cells
  if @distance_str.include? "(#{@end_x},#{@end_y})"
    return true
  else
    return false
  end
end

def distance_cells(print)
  @distance_str = "" #"0,(#{@start_x},#{@start_y})"
  hop_str = ""
  hops = 0
  line = []
  visited = []
  x = @start_x
  y = @start_y
  line << "#{x} #{y}"
  line << "hop" 
  while !line.empty? 
    curr = line.shift
    if curr == "hop"
      @distance_str << hop_str
      #@distance_str << "\n" 
      hops += 1
      hop_str = ""      
    else
      x, y = curr.split(/\s/)
      int_x = x.to_i
      int_y = y.to_i
      if hop_str.empty?
        hop_str << "\n#{hops.to_s},(#{x},#{y})"
      else   
        hop_str << ",(#{x},#{y})"
      end
      value = @maze_map["#{curr}"].to_s
      direction, weight = value.split(/\s/)
      direction = direction.gsub(/[\W]/,"")
      direction = direction.split(//)
      if !visited.include?(curr)
        visited << curr
        direction.each { |i|
          if i == "l"   
            if !visited.include?("#{int_x - 1} #{int_y}") && !line.include?("#{int_x - 1} #{int_y}")
               line.push("#{int_x - 1} #{int_y}")    
            end                                                                
          end
          if i == "u"
            if !visited.include?("#{int_x} #{int_y - 1}") && !line.include?("#{int_x} #{int_y - 1}")
              line.push("#{int_x} #{int_y - 1}")
            end
          end
          if i == "r"
            if !visited.include?("#{int_x + 1} #{int_y}") && !line.include?("#{int_x + 1} #{int_y}")
              line.push("#{int_x + 1} #{int_y}")
            end
          end 
          if i == "d"
            if !visited.include?("#{int_x} #{int_y + 1}") && !line.include?("#{int_x} #{int_y + 1}")           
              line.push("#{int_x} #{int_y + 1}")  
            end                
          end         
        }
        line << "hop"
      end # matches if !visited.include?(curr)
    end # matches if curr == "hop"
  end # matches while !stack.empty?
  @distance_str.strip!
  if print == "1"
    return @distance_str
  end  
end

#----------------------------------
def main(command_name, file_name)
  maze_file = open(file_name)
  # perform command
  case command_name	
   when "parse"
    parse(maze_file)
  when "open"
    read_file(maze_file)
    open_cells
  when "paths"
    read_file(maze_file)
    paths_cells("0")
  when "sortcells"
    read_file(maze_file)
    sort_cells
  when "bridge"
    read_file(maze_file) 
    bridge_cells
  when "distance"
    read_file(maze_file)
    distance_cells("1")
  when "print"
    read_file(maze_file)
    paths_cells("1")
    print_cells
  when "solve"
    read_file(maze_file)
    distance_cells("0")
    solve_cells
  else
    fail "Invalid command"
  end
end