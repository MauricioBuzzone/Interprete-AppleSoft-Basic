05 PRINT "FIBONACCI NUMBERS"
10 LET M% = 5000 : LET C% = 0
20 LET X% = 0 : LET Y% = 1
30 IF X% > M% GOTO 100
40 PRINT "F(" C% ") = " X% : C% = C% + 1
50 X% = X% + Y%
60 IF Y% > M% GOTO 100
70 PRINT "F(" C% ") = " Y% : C% = C% + 1
80 Y% = X% + Y%
90 GOTO 30
100 END