Imports System
Imports System.Math
Imports Autodesk.AutoCAD.Runtime
Imports Autodesk.AutoCAD.ApplicationServices
Imports Autodesk.AutoCAD.DatabaseServices
Imports Autodesk.AutoCAD.Geometry
Imports Autodesk.AutoCAD.EditorInput

Namespace RFLToolsApplication

    Public Class QuickTrain
        Private Shared Tol As Double = 0.0000000001
        Private Shared TolFine As Double = 0.0000000000000001

        Public AlignList As New AlignList
        Public ProfList As New ProfList
        Public SuperList As New SuperList

        '(defun POINTATSTATION (STA / P S Z)
        ' (setq P (XY (list STA 0.0)))
        ' (if (= nil P)
        '  (eval nil)
        '  (progn
        '   (if (or (= nil PVILIST) (= nil (setq Z (ELEVATION STA))))
        '    (setq Z 0.0)
        '   )
        '   (if (or (= nil SUPERLIST) (= nil (setq S (SUPER STA))))
        '    (setq S (list 0.0 0.0))
        '   )
        '   (list (car P) (cadr P) (+ Z (/ (+ (car S) (cadr S)) 2000.0)))
        '  )
        ' )
        ')
        Public Function PointAtStation(ByVal Sta As Double, ByRef POut As DataTypes.Point3d) As Boolean
            Dim P, POut2D As DataTypes.Point2d
            P.X = Sta
            P.Y = 0.0
            If AlignList.XY(P, POut2D) Then
                Dim Z As Double
                Dim S As DataTypes.Point2d
                If Not ProfList.Elevation(Sta, Z) Then
                    Z = 0.0
                End If
                If Not SuperList.Super(Sta, S) Then
                    S.X = 0.0
                    S.Y = 0.0
                End If
                POut.X = POut2D.X
                POut.Y = POut2D.Y
                POut.Z = Z + (S.X + S.Y) / 2000.0
                Return True
            Else
                Return False
            End If
        End Function

        '(defun FINDFRONTSTA (STA WB / *error* P P2 PM STA1 STA2 STAM STAE TOL)
        ' (setq TOL 0.00000005)
        ' (defun *error* (msg)
        '  (eval nil)
        ' )
        ' (if (= nil (setq P (POINTATSTATION STA)))
        '  (eval nil)
        '  (progn
        '   (setq STAE (+ (caar ALIGNLIST) (GETALIGNLENGTH)))
        '   (setq STA2 (+ STA (* 1.5 WB)))
        '   (if (> STA2 STAE) (setq STA2 STAE))
        '   (setq P2 (POINTATSTATION STA2))
        '   (if (or (= nil P2) (< (distance P P2) WB))
        '    (eval nil)
        '    (progn
        '     (setq STA1 (+ STA (* 0.5 WB)))
        '     (setq P1 (POINTATSTATION STA1))
        '     (setq STAM (/ (+ STA1 STA2) 2.0))
        '     (setq PM (POINTATSTATION STAM))
        '     (while (> (abs (- WB (distance P PM))) TOL)
        '      (if (< (distance P PM) WB)
        '       (setq P1 PM STA1 STAM)
        '       (setq P2 PM STA2 STAM)
        '      )
        '      (setq STAM (/ (+ STA1 STA2) 2.0))
        '      (setq PM (POINTATSTATION STAM))
        '     )
        '     (eval STAM)
        '    )
        '   )
        '  )
        ' )
        ')
        Public Function FindFrontStation(ByVal Sta As Double, ByVal WB As Double, ByRef StaOut As Double) As Boolean
            Dim P As DataTypes.Point3d
            If PointAtStation(Sta, P) Then
                Dim StaE, StaM, Sta1, Sta2 As Double
                Dim PM, P1, P2 As DataTypes.Point3d

                StaE = AlignList.StaStart + AlignList.Length
                Sta2 = Sta + 1.5 * WB
                If Sta2 > StaE Then Sta2 = StaE
                If PointAtStation(Sta2, P2) Then
                    If P.DistanceTo(P2) >= WB Then
                        Sta1 = Sta + 0.5 * WB
                        PointAtStation(Sta1, P1)
                        StaM = (Sta1 + Sta2) / 2.0
                        PointAtStation(StaM, PM)
                        While Abs(WB - P.DistanceTo(PM)) > Tol
                            If P.DistanceTo(PM) < WB Then
                                P1 = PM
                                Sta1 = StaM
                            Else
                                P2 = PM
                                Sta2 = StaM
                            End If
                            StaM = (Sta1 + Sta2) / 2.0
                            PointAtStation(StaM, PM)
                        End While
                        StaOut = StaM
                        Return True
                    Else
                        Return False
                    End If
                Else
                    Return False
                End If
            Else
                Return False
            End If
        End Function

        '(defun FINDBACKSTA (STA WB / *error* P P2 PM STA1 STA2 STAM STAS TOL)
        ' (setq TOL 0.00000005)
        ' (defun *error* (msg)
        '  (eval nil)
        ' )
        ' (if (= nil (setq P (POINTATSTATION STA)))
        '  (eval nil)
        '  (progn
        '   (setq STAS (caar ALIGNLIST))
        '   (setq STA2 (- STA (* 1.5 WB)))
        '   (if (< STA2 STAS) (setq STA2 STAS))
        '   (setq P2 (POINTATSTATION STA2))
        '   (if (or (= nil P2) (< (distance P P2) WB))
        '    (eval nil)
        '    (progn
        '     (setq STA1 (- STA (* 0.5 WB)))
        '     (setq P1 (POINTATSTATION STA1))
        '     (setq STAM (/ (+ STA1 STA2) 2.0))
        '     (setq PM (POINTATSTATION STAM))
        '     (while (> (abs (- WB (distance P PM))) TOL)
        '      (if (< (distance P PM) WB)
        '       (setq P1 PM STA1 STAM)
        '       (setq P2 PM STA2 STAM)
        '      )
        '      (setq STAM (/ (+ STA1 STA2) 2.0))
        '      (setq PM (POINTATSTATION STAM))
        '     )
        '     (eval STAM)
        '    )
        '   )
        '  )
        ' )
        ')
        Public Function FindBackStation(ByVal Sta As Double, ByVal WB As Double, ByRef StaOut As Double) As Boolean
            Dim P As DataTypes.Point3d
            If PointAtStation(Sta, P) Then
                Dim StaS, StaM, Sta1, Sta2 As Double
                Dim PM, P1, P2 As DataTypes.Point3d

                StaS = AlignList.StaStart
                Sta2 = Sta - 1.5 * WB
                If Sta2 < StaS Then Sta2 = StaS
                If PointAtStation(Sta2, P2) Then
                    If P.DistanceTo(P2) >= WB Then
                        Sta1 = Sta - 0.5 * WB
                        PointAtStation(Sta1, P1)
                        StaM = (Sta1 + Sta2) / 2.0
                        PointAtStation(StaM, PM)
                        While Abs(WB - P.DistanceTo(PM)) > Tol
                            If P.DistanceTo(PM) < WB Then
                                P1 = PM
                                Sta1 = StaM
                            Else
                                P2 = PM
                                Sta2 = StaM
                            End If
                            StaM = (Sta1 + Sta2) / 2.0
                            PointAtStation(StaM, PM)
                        End While
                        StaOut = StaM
                        Return True
                    Else
                        Return False
                    End If
                Else
                    Return False
                End If
            Else
                Return False
            End If
        End Function

        '(defun GETTRUCKPOUT (STA WB GAUGE DX DY DZ / A P R RX RY RZ STA2)
        ' (if (= nil (setq P (POINTATSTATION STA)))
        '  (eval nil)
        '  (if (= nil (setq STA2 (FINDFRONTSTA STA WB)))
        '   (eval nil)
        '   (if (= nil (setq A (GETTRUCKANGS STA STA2 GAUGE)))
        '    (eval nil)
        '    (progn
        '     (setq RX (car A))
        '     (setq RY (cadr A))
        '     (setq RZ (caddr A))
        '     (setq A (+ RX (angle (list 0.0 0.0) (list DY DZ))))
        '     (setq R (sqrt (+ (expt DY 2) (expt DZ 2))))
        '     (setq DY (* R (cos A)))
        '     (setq DZ (* R (sin A)))
        '     (setq A (+ RY (angle (list 0.0 0.0) (list DZ DX))))
        '     (setq R (sqrt (+ (expt DZ 2) (expt DX 2))))
        '     (setq DZ (* R (cos A)))
        '     (setq DX (* R (sin A)))
        '     (setq A (+ RZ (angle (list 0.0 0.0) (list DX DY))))
        '     (setq R (sqrt (+ (expt DX 2) (expt DY 2))))
        '     (setq DX (* R (cos A)))
        '     (setq DY (* R (sin A)))
        '     (list (+ (car P) DX) (+ (cadr P) DY) (+ (caddr P) DZ))
        '    )
        '   )
        '  )
        ' )
        ')
        Public Function GetTruckPOut(ByVal Sta As Double, ByVal WB As Double, ByVal Gauge As Double, ByVal DX As Double, _
                                     ByVal DY As Double, ByVal DZ As Double, ByRef POut As DataTypes.Point3d) As Boolean
            Dim A, P As DataTypes.Point3d
            Dim Sta2 As Double

            If PointAtStation(Sta, P) Then
                If FindFrontStation(Sta, WB, Sta2) Then
                    If GetTruckAngs(Sta, Sta2, Gauge, A) Then
                        Dim Ang, R, RX, RY, RZ As Double
                        Dim P2D1, P2D2 As DataTypes.Point2d
                        RX = A.X
                        RY = A.Y
                        RZ = A.Z
                        P2D1.X = 0.0
                        P2D1.Y = 0.0
                        P2D2.X = DY
                        P2D2.Y = DZ
                        Ang = RX + P2D1.AngleTo(P2D2)
                        R = Sqrt((DY * DY) + (DZ * DZ))
                        DY = R * Cos(Ang)
                        DZ = R * Sin(Ang)
                        P2D2.X = DZ
                        P2D2.Y = DX
                        Ang = RY + P2D1.AngleTo(P2D2)
                        R = Sqrt((DZ * DZ) + (DX * DX))
                        DZ = R * Cos(Ang)
                        DX = R * Sin(Ang)
                        P2D2.X = DX
                        P2D2.Y = DY
                        Ang = RZ + P2D1.AngleTo(P2D2)
                        R = Sqrt((DX * DX) + (DY * DY))
                        DX = R * Cos(Ang)
                        DY = R * Sin(Ang)
                        POut.X = P.X + DX
                        POut.Y = P.Y + DY
                        POut.Z = P.Z + DZ
                        Return True
                    Else
                        Return False
                    End If
                Else
                    Return False
                End If
            Else
                Return False
            End If
        End Function

        '(defun FINDFRONTTRUCK (P WB WBT DX DZ GAUGE / *error* P1 P2 PM RX RY RZ STA STA1 STA2 STAM STAE TOL)
        ' (setq TOL 0.00001)
        ' (defun *error* (msg)
        '  (eval nil)
        ' )
        ' (if (= nil (setq STA (car (STAOFF P))))
        '  (eval nil)
        '  (progn
        '   (setq STAE (+ (caar ALIGNLIST) (GETALIGNLENGTH)))
        '   (setq STA2 (+ STA (* 1.5 (max WB WBT))))
        '   (if (> STA2 STAE) (setq STA2 STAE))
        '   (setq P2 (GETTRUCKPOUT STA2 WBT GAUGE DX DY DZ))
        '   (if (or (= nil P2) (< (distance P P2) WB))
        '    (eval nil)
        '    (progn
        '     (setq STA1 (+ STA (* 0.5 WB)))
        '     (setq P1 (GETTRUCKPOUT STA1 WBT GAUGE DX DY DZ))
        '     (setq STAM (/ (+ STA1 STA2) 2.0))
        '     (setq PM (GETTRUCKPOUT STAM WBT GAUGE DX DY DZ))
        '     (while (> (abs (- WB (distance P PM))) TOL)
        '      (if (< (distance P PM) WB)
        '       (setq P1 PM STA1 STAM)
        '       (setq P2 PM STA2 STAM)
        '      )
        '      (setq STAM (/ (+ STA1 STA2) 2.0))
        '      (setq PM (GETTRUCKPOUT STAM WBT GAUGE DX DY DZ))
        '     )
        '     (eval STAM)
        '    )
        '   )
        '  )
        ' )
        ')
        Public Function FindFrontTruck(ByVal P As DataTypes.Point3d, ByVal WB As Double, ByVal WBT As Double, _
                                       ByVal DX As Double, ByVal DZ As Double, ByVal Gauge As Double, ByRef StaOut As Double) As Boolean
            Dim Sta As Double
            Dim P2D, POut2D As DataTypes.Point2d

            P2D.X = P.X
            P2D.Y = P.Y
            If AlignList.StaOff(P2D, POut2D) Then
                Dim DY, StaE, StaM, Sta1, Sta2 As Double
                Dim PM, P1, P2 As DataTypes.Point3d

                DY = 0.0
                Sta = POut2D.X
                StaE = AlignList.StaStart + AlignList.Length
                Sta2 = Sta + 1.5 * Max(WB, WBT)
                If Sta2 > StaE Then Sta2 = StaE
                If GetTruckPOut(Sta2, WBT, Gauge, DX, DY, DZ, P2) Then
                    If P.DistanceTo(P2) >= WB Then
                        Sta1 = Sta + 0.5 * WB
                        GetTruckPOut(Sta1, WBT, Gauge, DX, DY, DZ, P1)
                        StaM = (Sta1 + Sta2) / 2.0
                        GetTruckPOut(StaM, WBT, Gauge, DX, DY, DZ, PM)
                        While Abs(WB - P.DistanceTo(PM)) > Tol
                            If P.DistanceTo(PM) < WB Then
                                P1 = PM
                                Sta1 = StaM
                            Else
                                P2 = PM
                                Sta2 = StaM
                            End If
                            StaM = (Sta1 + Sta2) / 2.0
                            GetTruckPOut(StaM, WBT, Gauge, DX, DY, DZ, PM)
                        End While
                        StaOut = StaM
                        Return True
                    Else
                        Return False
                    End If
                Else
                    Return False
                End If
            Else
                Return False
            End If
        End Function

        '(defun GETTRUCKANGS (STA1 STA2 GAUGE / ASIN ACOS P1 P2 RX RY RZ S S1 S2 STA2 TOL WB)
        ' (defun ACOS (X)
        '  (* 2.0 (atan (/ (sqrt (- 1.0 (expt X 2))) (+ 1.0 X))))
        ' )
        ' (defun ASIN (X)
        '  (* 2.0 (atan (/ X (+ 1.0 (sqrt (- 1.0 (expt X 2)))))))
        ' )
        ' (if (or (= nil (setq P1 (POINTATSTATION STA1)))
        '         (= nil (setq P2 (POINTATSTATION STA2))))
        '  (eval nil)
        '  (progn
        '   (if (= nil (setq S1 (SUPER STA1)))
        '    (setq S1 (list 0.0 0.0))
        '   )
        '   (if (= nil (setq S2 (SUPER STA2)))
        '    (setq S2 (list 0.0 0.0))
        '   )
        '   (setq S (/ (+ (* -1.0 (car S1)) (cadr S1) (* -1.0 (car S2)) (cadr S2)) 4.0))
        '   (setq RX (* -1.0 (ASIN (/ S (/ GAUGE 2.0)))))
        '   (setq RY (* -1.0 (atan (/ (- (caddr P2) (caddr P1)) (distance (list (car P1) (cadr P1)) (list (car P2) (cadr P2)))))))
        '   (setq RZ (angle (list (car P1) (cadr P1)) (list (car P2) (cadr P2))))
        '   (list RX RY RZ)
        '  )
        ' )
        ')
        Public Function GetTruckAngs(ByVal Sta1 As Double, ByVal Sta2 As Double, ByVal Gauge As Double, ByRef AOut As DataTypes.Point3d) As Boolean
            Dim P1, P2 As DataTypes.Point3d
            Dim S1, S2 As DataTypes.Point2d
            Dim S As Double

            If PointAtStation(Sta1, P1) Or PointAtStation(Sta2, P2) Then
                If Not SuperList.Super(Sta1, S1) Then
                    S1.X = 0.0
                    S1.Y = 0.0
                End If
                If Not SuperList.Super(Sta2, S2) Then
                    S2.X = 0.0
                    S2.Y = 0.0
                End If
                S = ((-1.0 * S1.X) + S1.Y + (-1.0 * S2.X) + S2.Y) / 4.0

                AOut.X = -1.0 * Asin(S / (Gauge / 2.0))
                AOut.Y = -1.0 * Atan2((P2.Z - P1.Z), (P1.DistanceTo2D(P2)))
                AOut.Z = P1.AngleTo2D(P2)
                Return True
            Else
                Return False
            End If
        End Function

        '(defun GETCARANGS (P1 P2 GAUGE / ASIN ACOS RX RY RZ S S1 S2 STA1 STA2 TOL WB)
        ' (defun ACOS (X)
        '  (* 2.0 (atan (/ (sqrt (- 1.0 (expt X 2))) (+ 1.0 X))))
        ' )
        ' (defun ASIN (X)
        '  (* 2.0 (atan (/ X (+ 1.0 (sqrt (- 1.0 (expt X 2)))))))
        ' )
        ' (if (or (= nil (setq STA1 (car (STAOFF P1))))
        '         (= nil (setq STA2 (car (STAOFF P2)))))
        '  (eval nil)
        '  (progn
        '   (if (= nil (setq S1 (SUPER STA1)))
        '    (setq S1 (list 0.0 0.0))
        '   )
        '   (if (= nil (setq S2 (SUPER STA2)))
        '    (setq S2 (list 0.0 0.0))
        '   )
        '   (setq S (/ (+ (* -1.0 (car S1)) (cadr S1) (* -1.0 (car S2)) (cadr S2)) 4.0))
        '   (setq RX (* -1.0 (ASIN (/ S (/ GAUGE 2.0)))))
        '   (setq RY (* -1.0 (atan (/ (- (caddr P2) (caddr P1)) (distance (list (car P1) (cadr P1)) (list (car P2) (cadr P2)))))))
        '   (setq RZ (angle (list (car P1) (cadr P1)) (list (car P2) (cadr P2))))
        '   (list RX RY RZ)
        '  )
        ' )
        ')
        Public Function GetCarAngs(ByVal P1 As DataTypes.Point3d, ByVal P2 As DataTypes.Point3d, ByVal Gauge As Double, ByRef AOut As DataTypes.Point3d) As Boolean
            Dim POut1, POut2, P12D, P22D, S1, S2 As DataTypes.Point2d
            Dim S, Sta1, Sta2 As Double

            P12D.X = P1.X
            P12D.Y = P1.Y
            P22D.X = P2.X
            P22D.Y = P2.Y
            If AlignList.StaOff(P12D, POut1) Or AlignList.StaOff(P22D, POut2) Then
                Sta1 = POut1.X
                Sta2 = POut2.X
                If Not SuperList.Super(Sta1, S1) Then
                    S1.X = 0.0
                    S1.Y = 0.0
                End If
                If Not SuperList.Super(Sta2, S2) Then
                    S2.X = 0.0
                    S2.Y = 0.0
                End If
                S = ((-1.0 * S1.X) + S1.Y + (-1.0 * S2.X) + S2.Y) / 4.0

                AOut.X = -1.0 * Asin(S / (Gauge / 2.0))
                AOut.Y = -1.0 * Atan2((P2.Z - P1.Z), (P1.DistanceTo2D(P2)))
                AOut.Z = P1.AngleTo2D(P2)
                Return True
            Else
                Return False
            End If
        End Function

    End Class

End Namespace
