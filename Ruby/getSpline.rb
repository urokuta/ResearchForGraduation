#!/usr/bin/env ruby

org_path = File::dirname(__FILE__)
$:.unshift(org_path.untaint)
require 'spline'
require 'pp'
require "csv"


#x = [1, 2, 3, 4, 5]
#y = [2.0, 1.1, 2.4, 3.2, 5.9]
x = []
y = []
CSV.foreach("dataM120_0214.csv", "r") do |row|
    if row.instance_of?(Array)
        x.push row[0].to_i
        y.push row[1].to_f
    end
end

#pp y
#exit

#x = [0.9, 1.3, 1.9, 2.1, 2.6, 3.0, 3.9, 4.4, 4.7, 5.0, 6.0, 7.0, 8.0, 9.2]
#y = [1.3, 1.5, 1.85, 2.1, 2.6, 2.7, 2.4, 2.15, 2.05, 2.1, 2.25, 2.3, 2.25, 1.95]
curve = Spline.new(x, y)
pp curve


i = 6500
while i <= 17000
  printf("%f,%f\r\n", i, curve.interpolate(i))
  i += 100
end
