#!/usr/bin/env python
#-*- coding:utf-8 -*-
"""
Functions for calculating the Black-Scholes equation.
"""
import math

A1 = 0.319381530
A2 = -0.35653782
A3 = 1.781477937
A4 = -1.821255978
A5 = 1.330274429
RSQRT2PI = 0.3989422804
GAMMA = 0.2316419

def BS_Call(S,K,T,R,V):
    """Calculate the Black-Scholes formula for a call option.

    Arguments:
        S:the price of the underlying stock
        K:the strike price
        R:the continuously compounded risk free interest rate
        T:the time in years until the expiration of the option
        V:the volatility for the underlying stock
    """
    d1,d2 = _getD1D2(S,K,T,R,V)
    ret = S*_normalDist(d1)-K*math.exp(-R*T)*_normalDist(d2)
    return ret

def BS_Put(S,K,T,R,V):
    d1,d2 = _getD1D2(S,K,T,R,V)
    ret = -S*_normalDist(-d1)+K*math.exp(-R*T)*_normalDist(-d2)
    return ret

def BS_Call_IV(S,K,T,R,Pr,HV,maxNum=100,i=1):
    """Calculate the Implied Volatility using Newton's method.

    Arguments:
        Pr:the real price of call option
        HV:the Historical Volatility
        maxNum:the number of repeating calculation
        i:index(Do not specify this parameter)
    """
    if i > maxNum:
        return HV
    else:
        C_f = BS_Call(S, K, T, R, HV)
        vega = BS_Vega(S, K, T, R, HV)
        
        #if HV > 5:
        newV = HV - (C_f-Pr)/vega
        if newV < 0 : newV = newV * (-1)
        if newV > 5:
            print "  HV   = " + str(HV)
            print "  C_f  = " + str(C_f)
            print "  Pr   = " + str(Pr)
            print "  Vega = " + str(vega)
        #print "  newV = " + str(newV)
        #print ""
        return BS_Call_IV(S,K,T,R,Pr,newV,maxNum,i+1)

def BS_Put_IV(S,K,T,R,Pr,HV,maxNum=10,i=1):
    if i > maxNum:
        return HV
    else:
        newV = HV - (BS_Put(S,K,T,R,HV)-Pr)/BS_Vega(S,K,T,R,HV)
        return BS_Put_IV(S,K,T,R,Pr,newV,maxNum,i+1)

def BS_Call_Delta(S,K,T,R,V):
    d1,d2 = _getD1D2(S,K,T,R,V)
    return _normalDist(d1)

def BS_Put_Delta(S,K,T,R,V):
    d1,d2 = _getD1D2(S,K,T,R,V)
    return _normalDist(d1) - 1.0

def BS_Gamma(S,K,T,R,V):
    d1,d2 = _getD1D2(S,K,T,R,V)
    return _normalDist(d1)/(S*V*math.sqrt(T))

def BS_Vega(S,K,T,R,V):
    d1,d2 = _getD1D2(S,K,T,R,V)
    return S*_normalDist(d1)*math.sqrt(T)

def BS_Call_Theta(S,K,T,R,V):
    d1,d2 = _getD1D2(S,K,T,R,V)
    return -S*_normalDist(d1)*V/(2*math.sqrt(T))-R*K*math.exp(-R*T)*_normalDist(d2)

def BS_Put_Theta(S,K,T,R,V):
    d1,d2 = _getD1D2(S,K,T,R,V)
    return -S*_normalDist(d1)*V/(2*math.sqrt(T))+R*K*math.exp(-R*T)*_normalDist(-d2)

def BS_Call_Rho(S,K,T,R,V):
    d1,d2 = _getD1D2(S,K,T,R,V)
    return K*T*math.exp(-R*T)*_normalDist(d2)

def BS_Put_Rho(S,K,T,R,V):
    d1,d2 = _getD1D2(S,K,T,R,V)
    return -K*T*math.exp(-R*T)*_normalDist(-d2)

def _getD1D2(S,K,T,R,V):
    vSqrtT = V*math.sqrt(T)
    d1 = (math.log(float(S)/K)+(R+(0.5*V*V))*T)/(vSqrtT)
    d2 = d1 - vSqrtT
    return (d1,d2)

def _normalDist(x):
    if x >= 0:
        k = 1.0/(1.0 + GAMMA*x)
        cnd = RSQRT2PI*math.exp(-0.5*x*x)*(k *(A1+ k*(A2+ k*(A3 + k*(A4 + k*A5)))))
        ret = 1.0 - cnd
        return ret
    else:
        return 1.0 - _normalDist(-x)

def BS_Call_IV_init(S, K, T, R):
    S = float(S)
    K = int(K)
    T = float(T)
    R = float(R)
    init = ((math.log(S/K) + R*T)*2/T )
    isqrt = math.sqrt(init);
    if isqrt > 1 : isqrt = 0.5
    return isqrt

