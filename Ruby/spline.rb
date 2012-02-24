class Spline
  
  # 初期化．3次スプライン補間曲線を求める
  def initialize(x, y)
    @x = x
    @y = y
    @z = []
    
    n = @x.length
    h = []
    d = []
    @z[0] = @z[n-1] = 0
    
    for i in 0...n-1
      h[i  ] =  @x[i+1] - @x[i]
      d[i+1] = (@y[i+1] - @y[i]) / h[i]
    end
    
    @z[1] = d[2] - d[1] - h[0] * @z[0]
    d[1] = 2 * (@x[2] - @x[0])
    
    for i in 1...n-2
      t = h[i] / d[i]
      @z[i+1] = d[i+2] - d[i+1] - @z[i] * t
      d[i+1] = 2 * (@x[i+2] - @x[i]) - h[i] * t
    end
    
    @z[n-2] -= h[n-2] * @z[n-1]
    
    i = n - 2
    while i > 0
      @z[i] = (@z[i] - h[i] * @z[i+1]) / d[i]
      i -= 1
    end
    
  end
  
  # 補間値を返すメソッド
  def interpolate(t)
    i = 0
    j = @x.length - 1
    
    while i < j
      k = (i + j) / 2
      if (@x[k] < t)
        i = k + 1
      else
        j = k
      end
    end
    
    if i > 0
      i -= 1
    end
    
    h = @x[i+1] - @x[i]
    d = t - @x[i]
    return (((@z[i+1] - @z[i]) * d / h + @z[i] * 3) * d + ((@y[i+1] - @y[i]) / h - (@z[i] * 2 + @z[i+1]) * h)) * d + @y[i]
    
  end
  
end