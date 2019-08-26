c Initialiser for parameter common block
      subroutine initparms(odeparms)
      external odeparms
      double precision parms(5)
      common /myparms/parms
      call odeparms(5, parms)
      return
      end

c Rate of change and output variable
      subroutine derivs (neq, t, y, ydot, yout, IP)
      integer          neq, IP(*)
      double precision t, y(neq), ydot(neq), yout(*)
      double precision rI, rG, rM, AE, K
      common /myparms/rI, rG, rM, AE, K
      if(IP(1)<1) call rexit("nout should be at least 1")      
      ydot(1) = rG*y(1)*(1-y(1)/K) - rI*y(1)*y(2)   
      ydot(2) = rI*y(1)*y(2)*AE - rM*y(2)                  
      yout(1) = y(1) + y(2) 
      return
      end

