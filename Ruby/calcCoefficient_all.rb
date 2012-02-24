#!/usr/bin/env ruby

org_path = File::dirname(__FILE__)
$:.unshift(org_path.untaint)
require 'splineCoefficient'
require 'pp'
require 'csv'

#x = [0.9, 1.3, 1.9, 2.1, 2.6, 3.0, 3.9, 4.4, 4.7, 5.0, 6.0, 7.0, 8.0, 9.2]
#y = [1.3, 1.5, 1.85, 2.1, 2.6, 2.7, 2.4, 2.15, 2.05, 2.1, 2.25, 2.3, 2.25, 1.95]


for m in 1..12
    pp m
    x = []
    y = []
    CSV.foreach("dataM#{m*10}_0214.csv", "r") do |row|
        if row.instance_of?(Array)
            x.push row[0].to_i
            y.push row[1].to_f
        end
    end



    n = x.size-1
    curve = SplineCoefficient.new(n, x, y)
    result = curve.getResult()
    #result["r"].shift
    #pp result["s"]
    #exit
    CSV.open("spline_dataM#{m*10}_0214.csv", "wb") do |csv|
        csv << ["range", "y", "q", "r", "s"]
        for i in 0..x.length-1
            csv << [x[i], y[i], result["q"][i], result["r"][i], result["s"][i]]
        end
    end

#    i = 6500
#    while i <= 17000
#        printf("%f,%f\r\n", i, curve.interpolate(i))
#        i += 100
#    end


end
