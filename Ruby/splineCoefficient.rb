class SplineCoefficient
    def initialize(n, x1, y1)
        @result = getCoefficient(n, x1, y1)
    end
    def getResult()
        @result
    end
    def getCoefficient(n, x1, y1)
        h = []
        b = []
        d = []
        g = []
        u = []
        q = []
        s = []
        r = []
        x = []
        y = []

        for i1 in 0..n
            x[i1] = x1[i1]
            y[i1] = y1[i1]
        end
        # ステップ１

        for i1 in 0..n-1
            h[i1] = x[i1+1] - x[i1]
        end
        for i1 in 1..n-1
            b[i1] = 2.0 * (h[i1] + h[i1-1])
            d[i1] = 3.0 * ((y[i1+1] - y[i1]) / h[i1] - (y[i1] - y[i1-1]) / h[i1-1])
        end
        # ステップ２

        g[1] = h[1] / b[1]
        for i1 in 2..n-2
            g[i1] = h[i1] / (b[i1] - h[i1-1] * g[i1-1])
        end
        u[1] = d[1] / b[1]
        for i1 in 2..n-1
            u[i1] = (d[i1] - h[i1-1] * u[i1-1]) / (b[i1] - h[i1-1] * g[i1-1])
        end
        # ステップ３
        r[0]   = 0
        r[n]   = 0
        r[n-1] = u[n-1]
        i1 = n-2
        (n-2).times do
             r[i1] = u[i1] - g[i1] * r[i1+1]
             i1 = i1-1
        end
        #for i1 in n-2..1
        #    r[i1] = u[i1] - g[i1] * r[i1+1]
        #end
        # ステップ４
        for i1 in 0..n-1
            q[i1] = (y[i1+1] - y[i1]) / h[i1] - h[i1] * (r[i1+1] + 2.0 * r[i1]) / 3.0
            s[i1] = (r[i1+1] - r[i1]) / (3.0 * h[i1])
        end
        result = { "p"=>p, "q"=>q, "r"=>r, "s"=>s }
    end
end
