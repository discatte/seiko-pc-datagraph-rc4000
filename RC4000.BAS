10    GOTO 1020
20    SAVE"A:RC4000.BAS" : END
29  '
50   REM ******************************************************************
51   REM               RC4000.BAS    Version 1.00i/ms
52   REM ******************************************************************
53   '
54   '        By H.C. Pennington
55   '        5/01/85
56   '
57   '        Copyright (c) 1985, Hattori Corporation of America
58   '        1111 Macarthur Boulevard, Mahwah, N.J. 07430
59   '
60   '
999  '
1020 CLS
1025 DIM DA$(81), LN$(81), MG$(35)
1026 DIM MU$(40),HL$(41)
1027 DIM DT(85), DA(85), SD(85), SN(85) 
1029 DIM DE(12),DE$(12)
1030 PRINT CHR$(254):DEF SEG=&HB000:IF PEEK(0)<>254 THEN X=1:DEF SEG=&HB800
1031 '
1033 LA(3) = 0
1035 BLOAD "GRAPHICA.PIX",0
1036 LOCATE 11,35:PRINT LEFT$(DATE$,5);
1037 LOCATE 13,38:PRINT LEFT$(TIME$,5);
1038 LOCATE 12,43:PRINT RIGHT$(TIME$,2);
1039 IK$=INKEY$:IF IK$="" THEN 1037
1040 LOCATE 21,50:COLOR 31:PRINT "Initilizing program ...     ";
1041 COLOR 7
1043 KEY OFF
1045 CR = 220
1046 CC = 0
1047 PZ = 2776
1048 PO = PZ
1049 IL = PZ + 46
1050 EN = 80
1051 AP=0
1052 AP$=""
1053 BL=0
1054 C%=0
1055 F%=0
1056 G%=0
1057 N%=0
1058 X=0:J=0:Y=0
1059 LO=1
1060 LL=0
1061 MG=0
1062 MU=0
1063 PR=0
1065 EL=0
1076 EF = 80
1077 MU = 0
1078 LN = 1
1080 CL = 0
1095 F1 = 0
1100 F2 = 0
1105 F3 = 0
1125 PR$=""
1130 IP$=""
1150 IK = 0
1155 LA=0
1166 CN=0
1168 DD=0
1170 DY=0
1172 DY$=""
1174 HH=0
1176 HR=0
1178 HR$=""
1180 OS=0
1182 MI$=""
1184 MM=0
1186 MO=0
1188 MO$=""
1190 Q1=0
1192 RQ=0
1194 SA=0
1196 SP=0
1198 SS=0
1200 YR=0
1202 YY=0
1204 DR$=""
1205 CT$=""
1206 CT=0
1210 SE=1:RE=1:CU=1
1285 V1$="!@^()_{};`~\|,<>"
1286 V2$=V1$+".\/"
1287 V4$=CHR$(8)+CHR$(13)+CHR$(27)
1289 YN$="YyNn"+CHR$(27)
1310 RESTORE 1330
1315 FOR X=1 TO 13
1320 READ Y : CM$(1)=CM$(1)+CHR$(Y)
1325 NEXT X
1330 DATA 13, 08, 27
1335 DATA 04, 05, 09, 10
1340 DATA 18, 20, 6, 19, 2, 14
1350 RESTORE 1370
1355 FOR X=1 TO 10
1360 READ Y : CM$(2)=CM$(2)+CHR$(Y)
1365 NEXT X
1370 DATA 75, 77, 72, 80
1375 DATA 82,83
1380 DATA 73,81
1385 DATA 71,79
1395 FOR X=0 TO 9
1400 KEY X+1, MID$("[]{}<>()|^",X+1,1)
1405 CM$(3)=CM$(3)+MID$("[]{}<>()|^",X+1,1)
1410 NEXT X
1450 '--------------------------
1451 ' SET WATCH INPUT DIRECTORY
1452 '--------------------------
1455 OPEN"R",3,"RC4000.BAS"
1460 FIELD#3,25 AS DU$
1465 FIELD#3,1 AS DE$( 0),2 AS DE$( 1),2 AS DE$( 2),2 AS DE$( 3),2 AS DE$( 4),               2 AS DE$( 5),2 AS DE$( 6),2 AS DE$( 7),2 AS DE$( 8),2 AS DE$( 9),               2 AS DE$(10),2 AS DE$(11),2 AS DE$(12)
1500 REM
1505 GOSUB 15820
1506 GOSUB 15030
1510 CHAIN "RC4000.SYS",1505,ALL
15030 COLOR 15:CLS
15040 LOCATE 1,1:PRINT CHR$(218)+STRING$(78,196)+CHR$(191)
15050 FOR X = 2 TO 10
15060 LOCATE X,1
15061 PRINT CHR$(179)+STRING$(78, 32)+CHR$(179)
15070 NEXT X
15080 LOCATE 11,1
15081 PRINT CHR$(192)+STRING$(28,196)+" Press  <Esc> to exit "+                       STRING$(28,196)+CHR$(217)
15090 COLOR 7
15110 LOCATE 12,1
15111 PRINT SPACE$(27)+CHR$(218)+STRING$(24,196)+CHR$(191)
15120 FOR X = 13 TO 23
15130 LOCATE X,1
15131 PRINT SPACE$(27)+CHR$(179)+STRING$(24, 32)+CHR$(179)
15140 NEXT X
15150 LOCATE 24,1
15151 PRINT SPACE$(27)+CHR$(192)+STRING$(24,196)+CHR$(217);
15160 RETURN
15820 MG$(0)  = "          Typing Mode"
15825 MG$(1)  = "          Insert Mode"
15830 MG$(2)  = "- None -"
15835 MG$(3)  = " Lines Free:"
15840 MG$(4)  = "Labels Free:"
15845 MG$(5)  = "Active File:"
15850 MG$(6)  = "Inserting label/data"
15855 MG$(7)  = "Watch full"
15860 MG$(8)  = "Already have 12 labels - Unable to insert"
15865 MG$(9)  = "                                 "
15870 MG$(10) = "Current file not saved - Continue (Y/N)?"
15875 MG$(11) = "Press a number key."
15880 MG$(12) = "Data below label will be lost - Continue (Y/N)?"
15885 MG$(13) = "Deleting label/data"
15890 MG$(14) = "Are you sure (Y/N)?"
15895 MG$(15) = "Saving data file"
15900 MG$(16) = "Loading data file"
15905 MG$(17) = "I/O error - Correct and retry"
15910 MG$(18) = "Enter time (HH:MM:SS):"
15915 MG$(19) = "Enter date (MM-DD-YY):"
15920 MG$(20) = "     Invalid input - Re-enter     "
15925 MG$(21) = "Sending data to RC-4000..."
15930 MG$(22) = "Ready printer - Press <Enter> when ready"
15935 MG$(23) = "Press <Esc> to exit"
15940 MG$(24) = "SEIKO PCDatagraph Watch Data File: "
15945 MG$(25) = "Enter file name: "
15950 MG$(26) = "File already exists - Use it anyway (Y/N)?"
15955 MG$(27) = "Can't LOAD/SAVE - No file name"
15960 MG$(28) = "Receiving data from RC-4000..."
15965 MG$(29) = "Search for: "
15970 MG$(30) = "Can't find target"
15975 MG$(31) = "Searching ..."
15980 MG$(32) = "Receive ready - Press <LOCK> button on RC-4000"
15985 MG$(33) = "Please wait ..."
16000 BL$     = "   ³      --- --- ---       ³                          "
16010 DL$( 0) = "- MEMO -----------------"
16020 DL$( 1) = "- SCHEDULE ----- ALARM -"
16030 DL$( 2) = "- WEEKLY ------- ALARM -"
16040 DL$( 3) = "                        "
16050 DD$( 0) = " - - - - -              "
16060 DD$( 1) = " - - - - -  01/01 A12:00"
16070 DD$( 2) = " - - - - -  0 SUN A12:00"
16080 DD$( 3) = "                        "
16540 RESTORE 16620
16550 FOR X = 0 TO 35
16560 READ MU$(X)
16570 NEXT X
16620 DATA "           Main Menu                                  "
16630 DATA "     1.  Edit Create Watch Data      "
16640 DATA "     2.  Communications Menu         "
16650 DATA "     3.  Print Watch Data            "
16660 DATA "     4.  System Menu                 "
16670 DATA "     5.  Quit Program                "
16760 DATA "           System Menu               "
16770 DATA "     1.  Name File                   "
16780 DATA "     2.  Change Date/Time            "
16790 DATA "     3.  Load File                   "
16800 DATA "     4.  Save File                   "
16810 DATA "                                     "
16900 DATA "          Insert Menu (^I)                            "
16910 DATA "1.  Add Memo Label                   "
16920 DATA "2.  Add Memo Data                    "
16930 DATA "3.  Add Schedule Alarm Data/Label    "
16940 DATA "4.  Add Weekly Alarm Data/Label      "
16950 DATA "                                     "
16960 DATA "       Communications Menu           "
16961 DATA "1.  Send Data to RC-4000             "
16963 DATA "2.  Receive Data from RC-4000        "
16964 DATA "3.  Set Send Speed (Hi-Lo)    Speed: "
16965 DATA "4.  Set Receive Speed (Hi-Lo) Speed: "
16966 DATA "5.  Set Comm Slot (1-2)        Slot: "
17040 DATA "        Send Data to RC-4000         "
17050 DATA "     Connect RC-4000 to computer     "
17060 DATA "                                     "
17070 DATA "  1. Set RC-4000 to DATA IN (H)      "
17080 DATA "  2. Press <Enter> to send           "
17090 DATA "                                     "
17100 DATA "      Receive Data from RC-4000      "
17102 DATA "Connect RC-4000 & RC-4200 to computer"
17104 DATA "                                     "
17106 DATA "  1. Turn on RC-4200 & set to RC OUT "
17108 DATA "  2. Set RC-4000 to DATA IN (H)      "
17110 DATA "  3. Press <Enter> key on computer   "
17180 DT$(0)= "Memo        "
17190 DT$(1)= "Sched. Alarm"
17200 DT$(2)= "Weekly Alarm"
17210 DT$(3)= "Memo        "
17220 DT$(5)= "Label: "
17230 DT$(4)= " Data: "
17240 PR$(0)= "ME"
17250 PR$(1)= "SA"
17260 PR$(2)= "WA"
17270 PR$(3)= "WT"
17280 PR$(5)= "L: "
17290 PR$(4)= "D: "
17292 SE$(0)="300"
17293 SE$(1)="2400"
17294 RE$(0)="300"
17295 RE$(1)="2400"
17296 SS$(0)="(L)"
17297 SS$(1)="(H)"
17298 RS$(0)="(L)"
17299 RS$(1)="(H)"
17300 CU$(1)="COM1:"
17301 CU$(2)="COM2:"
17310 DY$(0)= "0 SUN "
17320 DY$(1)= "1 MON "
17330 DY$(2)= "2 TUE "
17340 DY$(3)= "3 WED "
17350 DY$(4)= "4 THU "
17360 DY$(5)= "5 FRI "
17370 DY$(6)= "6 SAT "
17380 DY$(7)= "7 DAY"
17425 HL$(0)= "  Home":HL$(1)="-Beg Line"
17430 HL$(2)= "  End":HL$(3)="-End Line "
17435 HL$(4)= "  Ins":HL$(5)="-Ins Char "
17440 HL$(6)= "  Del":HL$(7)="-Del Char "
17445 HL$(8)= "  Lt/Rt":HL$(9)="-Lft/Rgt"
17455 HL$(10)= "  Up":HL$(11)="-Up 1 Line "
17460 HL$(12)= "  Dn":HL$(13)="-Dwn 1 Line"
17465 HL$(14)= "  CR":HL$(15)="-Dwn 1 Line"
17470 HL$(16)= "  PgUp":HL$(17)="-Up Page "
17475 HL$(18)= "  PgDn":HL$(19)="-Dwn Page"
17485 HL$(20)= "  ^I":HL$(21)="-ns Line   "
17490 HL$(22)= "  ^D":HL$(23)="-el Line   "
17495 HL$(24)= "  ^E":HL$(25)="-rase Line "
17500 HL$(26)= "  ^J":HL$(27)="-mp to Lbl "
17505 HL$(28)= "  ^R":HL$(29)="-estre Line"
17515 HL$(30)= "    ":HL$(31)="           "
17520 HL$(32)= "  ^S":HL$(33)="-earch Data"
17525 HL$(34)= "  ^B":HL$(35)="-eg of Data"
17530 HL$(36)= "  ^N":HL$(37)="-End File  "
17535 HL$(38)= "  ^T":HL$(39)="-ab Center "
17545 HL$(40)= " AM-PM  AM-PM   Hours  Mins    Month  Days   "
17550 HL$(41)= " Cut    Paste               "
17800 RETURN
