      SUBROUTINE BESJ0(X,BJ0)
C       J0 BESSEL FUNCTION THROUGH SECOND ZERO CROSSING AT ARGUMENT=5.5
      PARAMETER (J1=56)
      DIMENSION XJ0(J1),Y(J1)
      XJ0(1) = 1.0000
      XJ0(2) = 0.9975
      XJ0(3) = 0.9900
      XJ0(4) = 0.9604
      XJ0(5) = 0.9776
      XJ0(6) = 0.9385
      XJ0(7) = 0.9120
      XJ0(8) = 0.8812
      XJ0(9) = 0.8463
      XJ0(10) = 0.8075
      XJ0(11) = 0.7652
      XJ0(12) = 0.7196
      XJ0(13) = 0.6711
      XJ0(14) = 0.6201
      XJ0(15) = 0.5669
      XJ0(16) = 0.5118
      XJ0(17) = 0.4554
      XJ0(18) = 0.3980
      XJ0(19) = 0.3400
      XJ0(20) = 0.2818
      XJ0(21) = 0.2239
      XJ0(22) = 0.1666
      XJ0(23) = 0.1104
      XJ0(24) = 0.0555
      XJ0(25) = 0.0025
      XJ0(26) = -0.0484
      XJ0(27) = -0.0968
      XJ0(28) = -0.1424
      XJ0(29) = -0.1850
      XJ0(30) = -0.2243
      XJ0(31) = -0.2601
      XJ0(32) = -0.2921
      XJ0(33) = -0.3202
      XJ0(34) = -0.3443
      XJ0(35) = -0.3643
      XJ0(36) = -0.3801
      XJ0(37) = -0.3918
      XJ0(38) = -0.3992
      XJ0(39) = -0.4026
      XJ0(40) = -0.4018
      XJ0(41) = -0.3971
      XJ0(42) = -0.3887
      XJ0(43) = -0.3766
      XJ0(44) = -0.3610
      XJ0(45) = -0.3423
      XJ0(46) = -0.3205
      XJ0(47) = -0.2961
      XJ0(48) = -0.2693
      XJ0(49) = -0.2404
      XJ0(50) = -0.2097
      XJ0(51) = -0.1776
      XJ0(52) = -0.1443
      XJ0(53) = -0.1103
      XJ0(54) = -0.0758
      XJ0(55) = -0.0412
      XJ0(56) = -0.0068
      X = 5.5*X
      Y(1) = 0.1
      DO 50 I = 2,56
        Y(I) = I/10.
        IF(X.GT.Y(I)) GO TO 50
        BJ0 = XJ0(I-1) + (XJ0(I)-XJ0(I-1))*(X-Y(I-1))/0.1
        GO TO 100
 50   CONTINUE
 100  CONTINUE
      RETURN
      END
