10   RUN"RC4000.BAS
20   SAVE"RC4KSYS.BAS":END
29  '
50   REM *************************************************************
51   REM              RC4000.SYS   Version 1.00i/ms
52   REM *************************************************************
53   '
54   '        By H.C. Pennington
55   '        5/01/85
56   '
57   '        Copyright (c) 1985, Hattori Corporation of America
58   '        1111 Macarthur Boulevard, Mahwah, N.J. 07430
59   '
60   '
99   '
1505 DEF SEG=&HB000:IF PEEK(0)<>218 THEN DEF SEG=&HB800
1510 '
1520 '
1525 '
1610 GOSUB 10130
1620 GOSUB 5710
2010 MU=0:CLOSE 1,2
2020 GOSUB 3340
2030 COLOR 31
2040 SA=31:MG=11:GOSUB 13430
2050 COLOR 7
2060 V3$="12345":GOSUB 3540
2061 IF IK$=CHR$(27) THEN IK$="1"
2070 ON VAL(IK$) GOTO 3760, 2305, 2640, 2940, 3140
2090 SOUND 37,1:GOTO 2060
2105 SP=20:MU=4:GOSUB 3340:SP=0
2106 LOCATE 5,50:PRINT SS$(SE);:GOSUB 13630
2108 MG=9:GOSUB 13430
2110 IK$=INKEY$:IF IK$="" THEN 2110
2115 ON INSTR(CHR$(13)+CHR$(27),IK$) GOTO 7605, 2305
2120 SOUND 37,1:GOTO 2110
2130 GOTO 2010
2200 '****************
2201 ' READ WATCH
2202 '****************
2205 SP=20:MU=5:GOSUB 3340 :SP=0
2206 LOCATE 6,50:PRINT RS$(RE);:GOSUB 13630
2207 IK$=INKEY$:IF IK$="" THEN 2207
2208 ON INSTR(CHR$(13)+CHR$(27),IK$) GOTO 8805,2305
2210 SOUND 37,1:GOTO 2110
2212 '
2300 '****************
2301 ' COMM MENU
2302 '****************
2305 MU=3:GOSUB 3340:COLOR 15
2306 LOCATE 5,61:PRINT SS$(SE);
2307 LOCATE 6,61:PRINT RS$(RE);
2308 LOCATE 7,61:PRINT CU$(CU)
2310 COLOR 7:SA=31:MG=11:GOSUB 13430
2315 COLOR 7
2320 GOSUB 3540
2325 IF IK$=CHR$(27) THEN 2010
2330 ON VAL(IK$) GOTO 2105,2205,2355,2405,2455
2331 '
2335 SOUND 37,1:GOTO 2320
2350 '****************
2351 ' SET SEND RATE
2352 '****************
2355 IF SE=0 THEN SE=1 ELSE SE=0
2360 GOTO 2305
2400 '****************
2401 ' SET REC RATE
2402 '****************
2405 IF RE=0 THEN RE=1 ELSE RE=0
2410 GOTO 2305
2450 '****************
2451 ' SET SLOT NUMBER
2452 '****************
2455 IF CU=1 THEN CU=2 ELSE CU=1
2460 GOTO 2305
2599 '
2640 OPEN"lpt1:" AS #2:WIDTH #2,132:MG=22:GOSUB 13430
2660 IK$=INKEY$:IF IK$="" THEN 2660
2670 ON INSTR(CHR$(13)+CHR$(27),IK$) GOTO 2700, 2820
2680 SOUND 37,1:GOTO 2660
2700 PRINT #2, STRING$(45-(LEN(MG$(24)+FI$)/2),32)+MG$(24)+FI$
2710 PRINT #2, STRING$(10,32)+STRING$(70,"=")
2720 LPRINT
2730 X=1
2750 IF X<=EN THEN PR$=STRING$(10,32)+LN$(X)+">"+DA$(X)                              +"<"+PR$(DA(X))+PR$(DT(X))
2760 IF (X+40)<=EN THEN PR$=PR$+STRING$(3,32)+LN$(X+40)+">"+                         DA$(X+40)+ "<"+PR$(DA(X+40))+PR$(DT(X+40))
2775 IF X>EN THEN 2810
2780 PRINT #2, PR$
2790 IK$=INKEY$:IF IK$=CHR$(27) THEN 2810
2800 X=X+1:IF X<=40 THEN 2750
2810 LPRINT STRING$(63-X,13)
2820 GOSUB 13630:CLOSE 2
2830 GOTO 2010
2940 MU=1:GOSUB 3340
2950 SA=31:MG=11:GOSUB 13430
2960 V3$="1234":GOSUB 3540
2970 IF IK$=CHR$(27) THEN 2010
2980 ON IK GOTO 11530, 10330, 3030, 3040, 2205
3000 GOTO 2960
3020 MU=0
3030 GOSUB 9070:GOTO 2950
3040 GOSUB 9330:GOTO 2950
3140 MG=14:GOSUB 13430
3150 IK$=INKEY$:IF IK$="" THEN 3150
3160 ON INSTR(YN$,IK$) GOTO 3180,3180,3190,3190,3190
3170 SOUND 37,1:GOTO 3150
3180 CLS:NEW
3190 GOSUB 13630
3200 GOTO 2010
3340 LOCATE 2,2:COLOR 15
3341 IF SP=0 THEN SP=22
3342 X$=STRING$(78-SP,32)
3345 IF F2>0 THEN GOSUB 5090
3350 MU = MU*6
3352 LSET X$=MU$(MU)+STRING$(50,32)
3355 PRINT SPC(SP)+MU$(MU);:COLOR 7
3356 LOCATE 8,2:PRINT SPC(77);
3360 FOR X = 1 TO 5
3361 LSET X$=MU$(MU+X)+STRING$(50,32)
3370 LOCATE X+2,2:PRINT SPC(SP) X$;
3380 NEXT X
3390 MU = MU * .1
3400 RETURN
3540 '
3560 IK$=INKEY$:IF IK$="" THEN 3560
3570 IF IK$=CHR$(27) THEN RETURN
3580 IF INSTR(V3$,IK$) THEN IK=VAL(IK$):RETURN
3590 SOUND 37,1
3600 GOTO 3560
3760 LOCATE 18,1:PRINT MG$(F1);
3770 GOSUB 14210
3780 CC=PEEK(PO)
3790 POKE PO,CC:POKE PO+1,7
3800 IK$=INKEY$
3810 ON LEN(IK$) GOTO 3890, 3840
3820 IF IK$="" THEN POKE PO+1,15:POKE PO,CR:GOTO 3790
3840 IK$=RIGHT$(IK$,1)
3850 ON INSTR(CM$(2),IK$) GOTO 4030, 4110, 4970, 4850, 4710, 4400, 5310,                                       5200, 4190, 4250
3870 SOUND 37,1:GOTO 3780
3890 ON INSTR(CM$(1),IK$) GOTO 4850, 4310, 5630, 6090, 4500, 6700, 5420,                                       5550, 7575, 7305, 7405, 7485, 7495
3920 ON INSTR(CM$(3),IK$) GOTO 12030,12030,12230,12430,12630,                                                  12830, 8755, 8785, 7505, 7510
3950 IF INSTR(V1$,IK$) THEN 3980 ELSE 3955
3955 IF F1 THEN 4590
3960 POKE PO,ASC(IK$)
3965 F2=1:IF F2 THEN F3=1
3970 PO=PO+2:IF PO>IL THEN PO=PZ:GOSUB 4780
3975 GOTO 3780
3980 '
3982 SOUND 37,1:GOTO 3780
4030 PO=PO-2:IF PO<PZ THEN PO=IL:GOSUB 4780
4040 CC=PEEK(PO):POKE PO,CR
4050 SOUND 2000,1:POKE PO,CC
4060 GOTO 3780
4110 PO=PO+2:IF PO>IL THEN PO=PZ:GOSUB 4780
4120 CC=PEEK(PO):POKE PO,CR
4130 SOUND 2000,1:POKE PO,CC
4140 GOTO 3780
4190 PO = PZ
4200 GOTO 3780
4250 PO = IL
4260 GOTO 3780
4310 PO=PO-2:IF PO<PZ THEN PO=IL:GOSUB 4780
4320 CC=32:POKE PO,CR
4330 IF F1 THEN 4400
4340 SOUND 1000,1:POKE PO,CC
4350 GOTO 3780
4400 FOR X = PO TO IL STEP 2
4410 POKE X,PEEK(X+2)
4420 NEXT X
4430 POKE IL,32
4440 SOUND 1000,1
4450 GOTO 3780
4500 FOR X=PO TO IL STEP 2
4510 POKE X, 32
4520 NEXT X
4530 F2 = 1
4540 GOTO 3780
4590 FOR X = IL TO PO STEP -2
4600 POKE X,PEEK(X-2)
4610 NEXT X
4620 POKE PO,ASC(IK$)
4630 PO=PO+2:IF PO>IL THEN PO=PZ:GOSUB 4780
4640 F2 = 1
4660 GOTO 3780
4710 IF F1 THEN F1=0:CR=220 ELSE F1=1:CR=219
4720 LOCATE 18,1:PRINT MG$(F1);
4730 GOTO 3780
4780 F1 = 0:CR = 220
4790 LOCATE 18,1:PRINT MG$(F1);
4800 RETURN
4850 IF EN=1 THEN 4920
4860 IF F2 >0 THEN GOSUB 5090
4870 IF F1 >0 THEN GOSUB 4780
4880 IF EN<EF THEN CN=EN
4890 LN=LN+1:IF LN>CN THEN LN=1
4900 GOSUB 5710
4910 PO=PZ
4920 GOTO 3780
4970 IF EN=1 THEN 5040
4980 IF F2 >0 THEN GOSUB 5090
4990 IF F1 >0 THEN GOSUB 4780
5000 IF EN<EF THEN CN=EN
5010 LN=LN-1:IF LN<1 THEN LN=CN
5020 GOSUB 5710
5030 PO=PZ
5040 GOTO 3780
5090 DA$(LN)=""
5100 FOR X = 0 TO 23
5110 DA$(LN)=DA$(LN)+CHR$(SCREEN(18,29+X))
5120 NEXT X
5130 IF F2=1 THEN F3=1
5140 F2 = 0
5150 PO=PZ:RETURN
5200 IF EN=1 THEN 5260
5210 IF EN <12 THEN 4850
5220 IF F2 >0 THEN GOSUB 5090
5230 IF F1 >0 THEN GOSUB 4780
5240 LN=LN+11:IF LN>EN THEN LN=LN-EN
5250 GOSUB 5710
5255 PO=PZ
5260 GOTO 3780
5310 IF EN=1 THEN  5370
5320 IF EN <12 THEN  4970
5330 IF F2 >0 THEN GOSUB 5090
5340 IF F1 >0 THEN GOSUB 4780
5350 LN=LN-11:IF LN<1 THEN LN=LN+EN
5360 GOSUB 5710
5365 PO=PZ
5370 GOTO 3780
5420 IF EN=1 THEN 5500
5430 IF F2 >0 THEN GOSUB 5090
5440 IF F1 >0 THEN GOSUB 4780
5450 GOSUB 13760
5460 IF LA>0 THEN  5480
5470 GOSUB 13840
5480 IF LA>0 THEN LN=LA ELSE GOTO 5500
5490 GOSUB 5710
5500 GOTO 3780
5550 LOCATE 18,29
5560 PRINT DA$(LN);
5570 F2 = 0
5580 GOTO 3780
5630 IF F2 >0 THEN GOSUB 5090
5640 IF F1 >0 THEN GOSUB 4780
5650 LOCATE 18,1:PRINT "                     "
5660 GOTO 2010
5710 IF EN>11 THEN 5920
5730 CL=LN-5:IF CL<0 THEN CL=LN-6
5740 FOR X=1 TO 5
5750 LOCATE 12+X,25
5760 IF CL<=0 THEN PRINT BL$:GOTO 5790
5770 PRINT LN$(CL)+" "+CHR$(179);
5780 PRINT DA$(CL)+CHR$(179)+" "+DT$(DA(CL))+DT$(DT(CL))
5790 CL = CL+1:IF CL=0 THEN CL=1
5800 NEXT X
5820 CL=LN
5830 FOR X=6 TO 11
5840 LOCATE 12+X,25
5850 IF CL>EN THEN PRINT BL$:GOTO 5880
5860 PRINT LN$(CL)+" "+CHR$(179);
5870 PRINT DA$(CL)+CHR$(179)+" "+DT$(DA(CL))+DT$(DT(CL))
5880 CL=CL+1
5890 NEXT X
5900 GOTO 6020
5920 CL=LN-5
5930 IF EN=>EF THEN CN=EF ELSE CN=EN+1
5940 IF CL<=0 THEN CL=EN+CL
5950 IF CL=0 THEN CL=1
5960 FOR X=1 TO 11
5970 LOCATE 12+X,25
5980 PRINT LN$(CL)+" "+CHR$(179);
5990 PRINT DA$(CL)+CHR$(179)+" "+DT$(DA(CL))+DT$(DT(CL))
6000 CL = CL+1:IF CL>EN THEN CL=1
6010 NEXT X
6020 GOSUB 11330
6030 IL=PZ+46:IF DA(LN)<5 AND DT(LN)>0 THEN IL=PZ+22
6040 PO=PZ:RETURN
6090 IF DA(LN)=5 THEN  6100 ELSE 6280
6100 IK=DT(LN)
6110 IF DA(LN+1)=5 THEN GOSUB 6570:GOTO 6280
6120 IF LN=EN THEN GOSUB 6570:GOTO 6280
6130 MG=12:GOSUB 13430
6150 IK$=INKEY$:IF IK$="" THEN 6150
6160 ON INSTR("YynN",IK$) GOTO 6190,6190,6400,6400
6170 GOTO 6150
6190 GOSUB 13630
6200 GOSUB 6570
6220 GOSUB 13760
6230 IF LA>0 THEN OS=LA-LN:GOTO 6300
6240 EN=LN-1:GOSUB 6510
6250 IF DA(1)<5 THEN GOSUB 6620
6260 GOTO 6340
6280 OS=1
6300 MG=13:GOSUB 13430
6310 GOSUB 6440
6320 EN=EN-OS:GOSUB 6510
6340 IF EN<2 THEN  6350 ELSE 6360
6350 IF DA(1)<5 THEN GOSUB 6620
6360 IF LN>EN THEN LN=EN
6370 IF LN<1 THEN LN=1
6380 IF EN=0 THEN EN=1:GOSUB 6620
6390 GOSUB 5710
6400 GOSUB 13630
6410 F3=1:PO=PZ:GOTO 3770
6440 FOR X=LN TO EN-OS
6450 DA$(X)=DA$(X+OS)
6460 DA(X)=DA(X+OS):DT(X)=DT(X+OS)
6470 NEXT X
6480 RETURN
6510 FOR X=EN+1 TO EF
6520 DA$(X)=DD$(0):DT(X)=0:DA(X)=4
6530 NEXT X
6540 RETURN
6570 LA(IK)=LA(IK)-1
6580 IF LA(IK)<0 THEN LA(IK)=0
6590 RETURN
6620 IF EN>2 THEN 6650
6630 LA(0)=1
6640 DA$(1)=DL$(0):DA(1)=5:DT(1)=0
6650 RETURN
6700 IF EN+1>EF THEN MG=7:GOSUB 13230:GOTO 3770
6710 MU=2:GOSUB 3340
6720 SA=31:MG=11:GOSUB 13430
6730 IF EN<EF THEN GOSUB 3540 ELSE GOTO 7240
6740 IF IK$=CHR$(27) THEN  3770
6745 IF IK$="5" THEN SOUND 37,1:GOTO 6730
6770 IK=VAL(IK$)-2:IF IK<1 THEN IK=0
6780 IF IK$="1" THEN RQ=5:GOTO 6820
6790 IF LA(IK)>0 THEN RQ=4:GOTO 6831
6792 RQ=5
6820 GOSUB 6900
6830 IF Q1=0 THEN MG=8:GOSUB 13230:GOTO 6720
6831 IF DT(LN)=IK THEN LN=LN+1:GOTO 7080
6832 IF LA(IK)=0 AND IK>0 THEN LN=1:GOTO 7080
6833 IF LA(IK)=0 AND IK=0 THEN LN=EN+1:GOTO 7080
6834 IF LA(0)>0 AND IK=0 AND RQ=4 THEN GOSUB 6990:GOTO 7080
6850 IF LA(IK)>0 THEN GOSUB 6990:GOTO 7080
6860 IF IK=0 AND RQ=5 THEN LN=EN+1:GOTO 7080
6861 LN=1
6870 GOTO 7080
6900 Q1=0
6910 FOR X=0 TO 3
6920 Q1=Q1+LA(X)
6930 NEXT X
6950 IF Q1>11 THEN Q1=0 ELSE Q1=1
6960 RETURN
6990 X=LN+1:LA=0
7000 IF DA(X)=5 AND DT(X)=IK THEN LA=X:GOTO 7050
7010 X=X+1
7020 IF X<=EN THEN  7000
7030 X=1
7040 IF DA(X)=5 AND DT(X)=IK THEN LA=X:GOTO 7050
7041 X=X+1
7042 IF X<LN+1 THEN  7040
7050 LN=LA+1
7055 RETURN
7080 MG=6:GOSUB 13430
7090 FOR X=EN+1 TO LN+1 STEP -1
7100 LSET DA$(X)=DA$(X-1)
7110 DA(X)=DA(X-1)
7120 DT(X)=DT(X-1)
7130 NEXT X
7140 IF RQ=5 THEN DA$(LN)=DL$(IK)
7150 IF RQ=4 THEN DA$(LN)=DD$(IK)
7160 DA(LN)=RQ:DT(LN)=IK
7180 EN=EN+1
7190 IF DA(LN)=5 THEN LA(IK)=LA(IK)+1
7200 GOSUB 5710
7210 MU=0:F3=1
7211 PO=PZ
7220 GOTO 6700
7240 MG=7:GOSUB 13230:GOTO 7210
7299 '
7300 ' ***************
7301 '
7302 ' ***************
7305 '
7310 '
7315 '
7320 '
7325 '
7330 '
7335 '
7340 '
7345 '
7350 '
7355 '
7360 GOTO 3780
7361 '
7405 IF LN=EN THEN 7460
7408 GOSUB 5090
7410 MG=29:GOSUB 13430
7415 IP$="":LO=47:GOSUB 13930:GOSUB 14505
7420 IF IP$="" THEN 7455
7425 IP$=LEFT$(IP$,24)
7430 MG=31:GOSUB 13430
7435 X=LN+1
7440 IF INSTR(DA$(X),IP$) THEN LN=X:GOTO 7455
7445 X=X+1:IF X>EN THEN 7450 ELSE 7440
7450 MG=30:GOSUB 13230
7455 GOSUB 13630
7456 IF X=LN THEN GOSUB 5710
7460 GOTO 3780
7485 GOSUB 5090:PO=PZ:LN=1:GOSUB 5710:GOTO 3780
7495 GOSUB 5090:PO=PZ:LN=EN:GOSUB 5710:GOTO 3780
7499 '
7500 ' ***************
7501 '
7502 ' ***************
7505 '
7506 '
7507 '
7510 '
7511 '
7512 '
7526 '
7551 '
7560 'GOTO 3780
7569 '
7575 IF DA(LN)=5 THEN  7580
7576 IF DT(LN)>0 THEN  3780
7580 IF PO<PZ+24 THEN PO=PZ+24:GOTO 7590
7585 IF PO=>PZ+24 THEN PO=PZ
7590 GOTO 3780
7605 Q1=LN
7610 MG=21:GOSUB 13430
7611 LO=54
7615 OPEN CU$(CU)+SE$(SE)+",N,8,2,RS,CS0,DS0,CD0" AS #1
7630 DR$=CHR$(2)+"L"
7635 FOR X=1 TO EN
7640 IF DA(X)=5 THEN GOSUB 8705
7645 NEXT X
7650 DR$=DR$+"@"+CHR$(0)
7655 DR$=LEFT$(DR$+STRING$(22,0),26)
7665 PRINT #1, DR$;
7685 X=1
7690 LOCATE 9,LO:PRINT X;
7692 ON DT(X)+1 GOTO 7740,7805,8005,7740
7695 X=X+1:IF X<=EN THEN  7690
7697 FOR X=EN+1 TO 81
7698 LOCATE 9,LO:PRINT X;
7699 PRINT#1, "@"+STRING$(24,76);
7700 NEXT X
7710 CLOSE 1
7715 LN=Q1:LO=0
7720 GOTO 2010
7740 IF DA(X)=5 THEN PR$="L" ELSE PR$="d"
7745 PR$=PR$+DA$(X)
7750 PRINT #1, PR$;
7755 GOTO 7695
7805 IF DA(X)=5 THEN  7740
7810 AP=0
7815 GOSUB 8505
7825 FOR J=0 TO LL
7826 LOCATE 9,LO+3:PRINT"-";
7827 AP=0
7835 MO$=MID$(DA$(BL+J),13,2)
7840 DY$=MID$(DA$(BL+J),16,2)
7845 HR$=MID$(DA$(BL+J),20,2)
7847 AP$=MID$(DA$(BL+J),19,1):IF AP$="P" AND VAL(HR$) <12 THEN AP=12
7848 IF AP$="A" AND VAL(HR$)=12 THEN HR$="00"
7850 MI$=MID$(DA$(BL+J),23,2)
7855 HR$=STR$(VAL(HR$)+AP)
7856 HR$="0"+RIGHT$(HR$,LEN(HR$)-1)
7857 HR$=RIGHT$(HR$,2)
7865 SD(J)=VAL(MO$+DY$+HR$+MI$)
7870 SN(J)=VAL(LN$(BL+J))
7871 LOCATE 9,LO+3:PRINT" ";
7875 NEXT J
7885 GOSUB 8605
7895 FOR J=0 TO LL-1
7900 PR$="d"+DA$(SN(J))
7905 PRINT #1,PR$;
7910 NEXT J
7915 X=EL
7920 GOTO 7695
8005 IF DA(X)=5 THEN  7740
8010 GOSUB 8505
8020 FOR J=0 TO LL-1
8022 AP=0
8023 LOCATE 9,LO+3:PRINT"-";
8025 AP$=MID$(DA$(BL+J),19,1)
8030 DY$=MID$(DA$(BL+J),13,1)
8035 HR$=MID$(DA$(BL+J),20,2)
8040 MI$=MID$(DA$(BL+J),23,2)
8042 IF AP$="P" AND VAL(HR$)<12 THEN AP=12
8045 HR$=STR$(VAL(HR$)+AP)
8046 HR$="0"+RIGHT$(HR$,LEN(HR$)-1)
8047 HR$=RIGHT$(HR$,2)
8055 SD(J)=VAL(DY$+HR$+MI$)
8060 SN(J)=VAL(LN$(BL+J))
8061 LOCATE 9,LO+3:PRINT" ";
8065 NEXT J
8075 GOSUB 8605
8085 FOR J=0 TO LL-1
8090 PR$="d"+DA$(SN(J))
8095 PRINT #1,PR$;
8100 NEXT J
8105 X=EL
8110 GOTO 7695
8199 '
8200 ' ***************
8201 '
8202 ' ***************
8205 '
8215 '
8220 '
8225 '
8228 '
8229 '
8230 '
8231 '
8232 '
8233 '
8240 '
8245 '
8250 '
8255 GOTO 7695
8299 '
8505 BL=X:LN=X
8510 GOSUB 13760
8515 IF LA>0 THEN EL=LA-1
8520 IF LA=0 THEN EL=EN
8525 LL=(EL-BL)+1
8530 RETURN
8605 G%=LL:N%=LL
8615 WHILE G%>1:G%=G%/2
8620 FOR F%=1 TO 1
8625 FOR C%=0 TO N%-G%-1
8630 WHILE SD(C%)> SD(G%+C%)
8631 LOCATE 9,LO+3:PRINT "*"
8635 SWAP SD(C%),SD(G%+C%)
8640 SWAP SN(C%),SN(G%+C%)
8645 F%=0
8646 LOCATE 9,LO+3:PRINT " "
8650 WEND
8655 NEXT C%
8660 NEXT F%
8665 WEND
8666 LOCATE 9,LO+3:PRINT " "
8670 RETURN
8705 Y=(X-1)*25
8710 Y=Y+(4096*DT(X))
8715 DR$=DR$+RIGHT$(MKI$(Y),1)+LEFT$(MKI$(Y),1)
8720 RETURN
8755 CT$="":CT=DT(LN)
8758 FOR X=0 TO 24
8760 CT$=CT$+CHR$(SCREEN(18,29+X))
8762 NEXT X
8764 COLOR 0,7:LOCATE 7,54:PRINT"*";
8766 COLOR 7,0:GOTO 3780
8785 IF DT(LN)=CT OR DT(LN)=0 OR DA(LN)=5 THEN GOTO 8788
8786 LOCATE 18,29:PRINT LEFT$(CT$,12);:GOTO 8790
8788 LOCATE 18,29:PRINT CT$;
8790 F2=1:GOTO 3780
8800 '---------------------------
8801 '   READ FROM WATCH
8802 '---------------------------
8805 CLOSE 1:IF F3>0 THEN MG=10:GOSUB 13430 ELSE GOTO 8809
8806 IK$=INKEY$:IF IK$="" THEN 8806
8807 ON INSTR(YN$,IK$)GOTO 8809,8809,8995,8995
8808 SOUND 37,1:GOTO 8806
8809 GOSUB 13630:MG=32:COLOR 31:GOSUB 13430:COLOR 7
8810 IF IK$=CHR$(27) THEN GOTO 8950
8811 OPEN CU$(CU)+RE$(RE)+",N,8,1,RS,CS0,DS0,CD0" AS #1
8812 IF IK$=CHR$(27) THEN GOTO 8950
8815 IF LOC(1)= 0 THEN IK$=INKEY$:GOTO 8812 ELSE A$=INPUT$(1,#1)
8820 BEEP:MG=28:GOSUB 13430
8825 FOR X = 0 TO 80
8830    DA$(X) = INPUT$(25,#1)
8835 NEXT
8840 CLOSE 1
8845 BEEP:BEEP:MG=33:GOSUB 13430
8850 '---------------------------
8851 '   DEF LABEL DATA TYPES
8852 '---------------------------
8855 LSET DU$=DA$(0)
8860 FOR X=1 TO 12
8865    LSET DE$(X)=LEFT$(DE$(X),1)+CHR$(0)
8870    DE(X)=CVI(DE$(X))
8875 NEXT X
8880 '---------------------------
8885 '   FIND THE EOF
8890 '---------------------------
8895 FOR X = 1 TO 80
8900   IF INSTR(DA$(X),"@L") THEN EN=X-1:X=85
8905 NEXT
8906 IF X=81 THEN EN=80
8910 '---------------------------
8911 '   NORMALIZE FILE
8912 '---------------------------
8915 DC=0
8918 FOR X=0 TO 3:LA(X)=0:NEXT
8920 FOR X=1 TO EN
8925    DA(X)=4
8930    IF LEFT$(DA$(X),1)="L"THEN DA(X)=5:DC=DC+1
8935    DA$(X)=RIGHT$(DA$(X),24):GOSUB 8965
8940 IF DA(X)=5 THEN LA(DT(X))=LA(DT(X))+1
8945 NEXT
8950 GOTO 8990
8960 '--------------------------
8961 ' LOAD DATA TYPE SBR
8962 '--------------------------
8965 IF DE(DC)<16 THEN DT(X)=0 : RETURN
8970 IF DE(DC)<32 THEN DT(X)=1 : RETURN
8975 IF DE(DC)<48 THEN DT(X)=2 : RETURN
8980 IF DE(DC)<64 THEN DT(X)=3 : RETURN
8985 DT(X)=4 : RETURN
8990 LN=1:F3=0:GOSUB 13630:GOSUB 5710
8995 CLOSE 1:GOTO 2305
8996 '--------------------------
9070 IF FI$="" THEN MG=27:GOSUB 13230:GOTO 9220
9071 IF F3>0 THEN MG=10:GOSUB 13430 ELSE 9075
9072 IK$=INKEY$:IF IK$="" THEN 9072
9073 ON INSTR(YN$,IK$) GOTO 9075,9075,9220,9220,9220
9074 GOTO 9072
9075 ON ERROR GOTO 9550
9080 OPEN"I",1,FI$
9090 MG=16:GOSUB 13430
9100 INPUT #1, DA$(0)
9110 EN=VAL(DA$(0))
9140 LA(0)=0:LA(1)=0:LA(2)=0:LA(3)=0
9150 FOR X = 1 TO EF
9160 INPUT#1, DA$(X):
9170 GOSUB 9930
9180 NEXT X
9185 ON ERROR GOTO 9225
9186 INPUT #1, PR$:TZ=VAL(PR$)
9190 ON ERROR GOTO 0
9200 CLOSE 1
9210 F3=0:LN=1:GOSUB 5710
9220 RETURN
9225 RESUME NEXT
9330 IF FI$="" THEN MG=27:GOSUB 13230:GOTO 9520
9335 IF F2=1 THEN GOSUB 5090
9340 ON ERROR GOTO 9590
9350 OPEN"I",1,FI$:CLOSE 1
9360 MG=26:GOSUB 13430
9370 IK$=INKEY$:IF IK$="" THEN 9370
9380 ON INSTR(YN$,IK$) GOTO 9410,9410,9490,9490,9490
9390 GOTO 9370
9410 MG=15:GOSUB 13430
9420 ON ERROR GOTO 9550
9430 OPEN"O",1,FI$
9440 PRINT #1, STR$(EN)
9450 FOR X=1 TO EF
9460 GOSUB 10030
9470 PRINT #1, PR$
9480 NEXT X
9485 PRINT #1, STR$(TZ)
9486 F3=0
9490 CLOSE 1
9500 ON ERROR GOTO 0
9510 CLOSE 1
9520 GOSUB 13630
9530 RETURN
9550 RESUME 9560
9560 MG=17:GOSUB 13230
9570 GOTO 9490
9590 RESUME 9410
9930 DA(X)=4
9940 IF LEFT$(DA$(X),1)="L" THEN DA(X)=5
9950 DT(X)=VAL(RIGHT$(DA$(X),2))
9960 IF DA(X)=5 THEN LA(DT(X))=LA(DT(X))+1
9970 LN$(X)=RIGHT$(STR$(X),2)
9980 DA$(X)=MID$(DA$(X),3,24)
9990 RETURN
10030 IF DA(X)=5 THEN PR$="L " ELSE PR$="d "
10070 PR$=PR$+DA$(X)+RIGHT$(STR$(DT(X)),2)
10080 RETURN
10130 DA(1)=5:DT(1)=0:LN$(1)=" 1":DA$(1)=DL$(0)
10140 FOR X=2 TO EF
10150 DA(X)=4:DT(X)=0
10160 LN$(X)=RIGHT$(STR$(X),2)
10170 DA$(X)=DD$(0)
10180 NEXT X
10190 EN=1:LN=1:LA(0)=1:FI$=""
10200 RETURN
10330 ON ERROR GOTO 10490
10340 MG=18:GOSUB 13430:MG=20
10350 IP$="":LO=53:GOSUB 13930:GOSUB 14505
10360 IF IP$="" THEN  10400
10370 TIME$=IP$
10390 ON ERROR GOTO 10490
10400 MG=19:GOSUB 13430:MG=20
10410 IP$="":LO=53:GOSUB 13930:GOSUB 14505
10420 IF IP$="" THEN 10460
10440 DATE$=IP$
10460 ON ERROR GOTO 0
10465 GOSUB 11330
10470 GOTO 2950
10490 RESUME 10500
10500 GOSUB 13230
10510 IF MG=20 THEN 10330 ELSE 10390
11070 FOR X = 1 TO 9
11080 LOCATE X,2
11090 PRINT SPC(78)
11100 NEXT X
11110 RETURN
11330 LOCATE 12,1
11331 '
11340 PRINT MG$(3);80-EN
11350 LOCATE 13,1
11360 PRINT MG$(4);12-(LA(0)+LA(1)+LA(2)+LA(3))
11361 '
11390 LOCATE 22,1:PRINT MG$(5);
11400 LOCATE 23,1:PRINT STRING$(20,32);
11410 LOCATE 23,1:IF FI$="" THEN PRINT MG$(2) ELSE PRINT FI$;
11415 LOCATE 25,19:PRINT"TIME: ";TIME$;:LOCATE 25,49:PRINT"DATE: ";DATE$;
11420 RETURN
11530 MG=25:GOSUB 13430:MG=20
11540 ON ERROR GOTO 11660
11550 GOSUB 13930
11560 IP$="":LO=50:GOSUB 14505
11570 IF IP$="" THEN 11590
11580 IF ASC(IP$)<65 THEN GOSUB 13230:GOTO 11530
11590 FI$=IP$
11600 GOSUB 11330
11610 GOSUB 13630
11620 ON ERROR GOTO 0
11630 GOTO 2950
11660 RESUME 11670
11670 GOSUB 13230:GOTO 11530
12030 IF DT(LN)=0 OR DT(LN)=3 OR DA(LN)=5 THEN 3780
12040 ON DT(LN) GOTO 12050, 12050, 12060
12050 LO=47:GOTO 12070
12060 LO=41
12070 AP$=CHR$(SCREEN(18,LO))
12080 IF AP$="A" THEN AP$="P" ELSE AP$="A"
12090 IF DT(LN)<3 THEN AP$=LEFT$(AP$,1)
12100 LOCATE 18,LO:PRINT AP$;
12110 F2=1:GOTO 3780
12230 IF DT(LN)=0 OR DA(LN)=5 THEN 3780
12240 ON DT(LN) GOTO 12250, 12250, 12260
12250 LO=48:GOTO 12270
12260 LO=42
12270 LOCATE 18,LO
12280 GOSUB 13120
12282 PR=PR+1
12285 IF DT(LN)=3 AND PR>23 THEN PR=0
12286 IF DT(LN)=3 THEN  12300
12290 IF PR>12 THEN PR=1
12300 GOSUB 13160
12310 F2=1:GOTO 3780
12430 IF DT(LN)=0 OR DA(LN)=5 THEN 3780
12440 ON DT(LN) GOTO 12450, 12450, 12460
12450 LO=51:GOTO 12470
12460 LO=45
12470 LOCATE 18,LO
12480 GOSUB 13120
12490 PR=PR+1:IF PR>59 THEN PR=0
12500 GOSUB 13160
12510 F2=1:GOTO 3780
12630 IF DT(LN)=1 AND DA(LN)=4 THEN 12640 ELSE 3780
12640 LO=41:LOCATE 18,LO
12660 GOSUB 13120
12670 PR=PR+1:IF PR>12 THEN PR=1
12680 GOSUB 13160
12690 IF DT(LN)=1 THEN LOCATE 18,44:PRINT"01";
12700 F2=1:GOTO 3780
12830 IF DA(LN)=5 THEN 3780
12840 IF DT(LN)=1 OR DT(LN)=2 THEN 12850 ELSE 3780
12850 ON DT(LN) GOTO 12860, 12970
12860 LO=41:GOSUB 13120
12870 MO=PR:YR=VAL(RIGHT$(DATE$,2))
12880 DY=VAL(MID$("312831303130313130313031",MO*2-1,2))
12890 IF MO< VAL(DATE$) THEN YR=YR+1
12900 MO=VAL(PR$):IF MO<>2 THEN 12920
12910 IF (YR/4)-INT(YR/4)=0 THEN DY=29
12920 LO=44:GOSUB 13120
12930 PR=PR+1:IF PR>DY THEN PR=1
12940 LOCATE 18,LO:GOSUB 13160
12950 F2=1:GOTO 3780
12970 LO=41:GOSUB 13120
12980 PR=PR+1:IF PR>7 THEN PR=0
12990 LOCATE 18,41
13000 PRINT DY$(PR);
13010 F2=1:GOTO 3780
13120 PR$=CHR$(SCREEN(18,LO))+CHR$(SCREEN(18,LO+1))
13130 PR=VAL(PR$):RETURN
13160 IF PR>9 THEN PRINT RIGHT$(STR$(PR),2);
13170 IF PR<10 THEN PRINT "0"+RIGHT$(STR$(PR),1);
13180 RETURN
13230 LOCATE 9,2:PRINT SPC(78);
13240 LOCATE 9,2:COLOR 15
13280 PRINT SPC(39-(LEN(MG$(MG))/2));
13290 PRINT MG$(MG):SOUND 100,20
13300 FOR X=1 TO 1000:NEXT X
13310 COLOR 7:GOSUB 13630
13311 ON ERROR GOTO 0
13320 RETURN
13430 IF SA=0 THEN SA=15
13440 GOSUB 13630
13450 LOCATE 9,2:COLOR SA
13460 PRINT SPC(39-(LEN(MG$(MG))/2));
13470 PRINT MG$(MG)
13480 COLOR 7:SA=0
13490 RETURN
13630 LOCATE 9,2
13640 PRINT SPC(78)
13670 RETURN
13760 X=LN+1:LA=0
13770 IF DA(X)=5 THEN LA=X:GOTO 13800
13780 X=X+1
13790 IF X<EN+1 THEN  13770
13800 RETURN
13840 X=1:LA=0
13850 IF DA(X)=5 THEN LA=X:GOTO 13880
13860 X=X+1
13870 IF X<LN+1 THEN  13850
13880 RETURN
13930 FOR X=1 TO 3:SOUND X*400,4:NEXT
13940 RETURN
14210 FOR Z=0 TO 3
14215 LOCATE Z+2,2
14220 FOR X=0 TO 9 STEP 2
14225 COLOR 15,0:PRINT HL$(X+(10*Z));:COLOR 0,7:PRINT HL$(X+((10*Z)+1));
14230 NEXT X
14235 NEXT Z
14245 COLOR 7,0:LOCATE 6,2:PRINT SPC(78);:LOCATE 7,4
14250 COLOR 0,7:PRINT HL$(40)+HL$(41)
14260 COLOR 15,0
14265 LOCATE 7, 3:PRINT" 1";
14270 LOCATE 7,10:PRINT" 2";
14275 LOCATE 7,17:PRINT"  3";
14280 LOCATE 7,25:PRINT" 4";
14285 LOCATE 7,32:PRINT"  5";
14290 LOCATE 7,40:PRINT" 6";
14295 LOCATE 7,47:PRINT"  7";
14300 LOCATE 7,55:PRINT" 8";
14305 LOCATE 7,62:PRINT"  9";
14310 LOCATE 7,70:PRINT" 0";
14315 IF LEN(CT$)>0 THEN COLOR 0,7:LOCATE 7,54:PRINT"*";
14318 COLOR 7,0
14320 LOCATE 8,2:PRINT SPC(78);
14325 LOCATE 9,2:PRINT SPC(78);
14335 RETURN
14505 X=0
14525 LOCATE 9,LO:PRINT IP$+"_ ";
14530 IK$=INKEY$:IF IK$="" THEN 14530
14550 ON INSTR(V4$,IK$) GOTO 14615, 14640, 14660
14560 IF INSTR(V2$,IK$)<1 THEN 14585
14565 '
14568 SOUND 37,1:GOTO 14530
14585 X=X+1:IF X>24 THEN X=24:SOUND 37,1:GOTO 14530
14590 IP$=IP$+IK$
14595 GOTO 14525
14615 X=X-1:IF X<0 THEN X=0:IP$="":GOTO 14525
14620 IP$=LEFT$(IP$,X):GOTO 14525
14640 RETURN
14660 IP$="":GOTO 14640
20000 GOTO 3870

