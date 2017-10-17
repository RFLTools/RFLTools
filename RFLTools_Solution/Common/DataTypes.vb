Imports System
Imports System.Math
Imports Autodesk.AutoCAD.Runtime
Imports Autodesk.AutoCAD.ApplicationServices
Imports Autodesk.AutoCAD.DatabaseServices
Imports Autodesk.AutoCAD.Geometry
Imports Autodesk.AutoCAD.EditorInput

Namespace RFLToolsApplication
    Public Class AlignList
        Private Shared Tol As Double = 0.0000000001
        Private Shared TolFine As Double = 0.00000000000001

        Public Nodes() As DataTypes.AlignmentNode

        Sub New()
            GetAlign()
        End Sub

        Sub New(ByVal AlignListName As String)
            GetAlign(AlignListName)
        End Sub

        Public Function StaStart() As Double
            If (Nodes Is Nothing) Then
                Return Double.NaN
            Else
                Return Nodes(0).StaStart
            End If
        End Function

        Public Function SightDist(ByVal Sta As Double, ByVal Dist As Double, ByRef StaOut As Double) As Boolean
            If StaStart() = Double.NaN Then
                Return False
            Else
                If (Sta < StaStart()) Or (Sta > StaStart() + Length()) Then
                    Return False
                ElseIf (Abs(Dist) < Tol) Then
                    StaOut = Sta
                    Return True
                ElseIf (Dist > 0.0) Then
                    Dim Sta1, Sta2 As Double
                    Dim P, P1, P2, POut, PTmp As DataTypes.Point2d
                    PTmp.X = Sta
                    PTmp.Y = 0.0
                    If (XY(PTmp, P) = False) Then
                        Return False
                    Else
                        Sta1 = Sta
                        P1.X = P.X
                        P1.Y = P.Y
                        Sta2 = StaStart() + Length()
                        Nodes(Nodes.Length - 1).GetP2(P2)
                        If P.DistanceTo(P2) < Dist Then
                            Return False
                        Else
                            While ((Sta2 - Sta1) > Tol)
                                StaOut = (Sta1 + Sta2) / 2.0
                                PTmp.X = StaOut
                                PTmp.Y = 0.0
                                XY(PTmp, POut)
                                If (P.DistanceTo(POut) > Dist) Then
                                    Sta2 = StaOut
                                    P2.X = POut.X
                                    P2.Y = POut.Y
                                Else
                                    Sta1 = StaOut
                                    P1.X = POut.X
                                    P1.Y = POut.Y
                                End If
                            End While
                            Return True
                        End If
                    End If
                Else
                    Dim Sta1, Sta2 As Double
                    Dim P, P1, P2, POut, PTmp As DataTypes.Point2d
                    PTmp.X = Sta
                    PTmp.Y = 0.0
                    If (XY(PTmp, P) = False) Then
                        Return False
                    Else
                        Dist = Dist * -1.0
                        Sta1 = Sta
                        P1.X = P.X
                        P1.Y = P.Y
                        Sta2 = StaStart()
                        Nodes(0).GetP1(P2)
                        If P.DistanceTo(P2) < Dist Then
                            Return False
                        Else
                            While ((Sta1 - Sta2) > Tol)
                                StaOut = (Sta1 + Sta2) / 2.0
                                PTmp.X = StaOut
                                PTmp.Y = 0.0
                                XY(PTmp, POut)
                                If (P.DistanceTo(POut) > Dist) Then
                                    Sta2 = StaOut
                                    P2.X = POut.X
                                    P2.Y = POut.Y
                                Else
                                    Sta1 = StaOut
                                    P1.X = POut.X
                                    P1.Y = POut.Y
                                End If
                            End While
                            Return True
                        End If
                    End If
                End If
            End If
        End Function

        Public Function StaOff(ByVal P As DataTypes.Point2d, ByRef StaOffResult As DataTypes.Point2d) As Boolean
            If Not (Nodes Is Nothing) Then
                Dim Node, Node0 As DataTypes.AlignmentNode
                Dim Sta, StaBest, Offset, OffsetBest As Double
                Dim Found As Boolean = False
                Dim StaOffNode As DataTypes.Point2d
                Dim FirstNode As Boolean = True
                Dim P11, P12, P21, P22 As DataTypes.Point2d

                For Each Node In Nodes
                    If FirstNode Then
                        FirstNode = False
                    Else
                        Node0.GetP1(P11)
                        Node0.GetP2(P12)
                        Node.GetP1(P21)
                        Node.GetP2(P22)
                        If ((P.DistanceTo(P12) < P.DistanceTo(P11)) And (P.DistanceTo(P21) < P.DistanceTo(P22))) Then
                            Sta = Node.StaStart
                            Offset = P.DistanceTo(P21)
                            If (Sin(P21.AngleTo(P) - Node.AngIn) > 0.0) Then Offset = Offset * -1.0
                            If Not Found Then
                                Found = True
                                StaBest = Sta
                                OffsetBest = Offset
                            ElseIf Abs(Offset) < Abs(OffsetBest) Then
                                StaBest = Sta
                                OffsetBest = Offset
                            End If
                        End If
                    End If
                    If Node.StaOff(P, StaOffNode) Then
                        If Not Found Then
                            Found = True
                            StaBest = StaOffNode.X
                            OffsetBest = StaOffNode.Y
                        ElseIf Abs(StaOffNode.Y) < Abs(OffsetBest) Then
                            StaBest = StaOffNode.X
                            OffsetBest = StaOffNode.Y
                        End If
                    End If
                    Node0 = Node
                Next
                If Found Then
                    StaOffResult.X = StaBest
                    StaOffResult.Y = OffsetBest
                    Return True
                Else
                    Return False
                End If
            Else
                Return False
            End If
        End Function

        Public Function XY(ByVal P As DataTypes.Point2d, ByRef XYResult As DataTypes.Point2d) As Boolean
            If Not (Nodes Is Nothing) Then
                Dim Node As DataTypes.AlignmentNode
                Dim Found As Boolean = False
                Dim C As Integer = 0

                Node = Nodes(C)
                If P.X >= Node.StaStart Then
                    While C < Nodes.Length And Not Found
                        Node = Nodes(C)
                        If P.X <= Node.StaStart + Node.Length Then
                            Found = Node.XY(P, XYResult)
                        End If
                        C = C + 1
                    End While
                End If
                If Found Then
                    Return True
                Else
                    Return False
                End If
            Else
                Return False
            End If
        End Function

        Public Function Length() As Double
            If (Nodes Is Nothing) Then
                Return Nothing
            Else
                Dim Node As DataTypes.AlignmentNode
                Length = 0.0
                For Each Node In Nodes
                    Length = Length + Node.Length
                Next
                Return Length
            End If
        End Function

        Public Sub GetAlign()
            Nodes = GetAlignList("AlignList")
        End Sub

        Public Sub GetAlign(ByVal AlignListName As String)
            Nodes = GetAlignList(AlignListName)
        End Sub

        Public Function GetAlignList(ByVal AlignListName As String) As DataTypes.AlignmentNode()
            Dim Alignlist As ResultBuffer = GetPutSym.GetSymString(AlignListName)

            If (Alignlist Is Nothing) Then
                Return Nothing
            Else
                Dim NodeCount As Integer
                Dim C As Integer
                Dim NodeError As Boolean = False
                Dim AlignListArray As TypedValue() = Alignlist.AsArray

                If (AlignListArray(C).TypeCode <> LispDataType.ListBegin) Or _
                   (AlignListArray(AlignListArray.Length - 1).TypeCode <> LispDataType.ListEnd) Then
                    NodeError = True
                Else
                    C = 1
                    NodeCount = 0
                    While C < AlignListArray.Length - 1
                        If AlignListArray(C + 4).TypeCode = LispDataType.ListBegin Then
                            'Spiral
                            NodeCount = NodeCount + 1
                            C = C + 11
                        Else
                            'Line / Arc
                            NodeCount = NodeCount + 1
                            C = C + 6
                        End If
                    End While
                End If

                If NodeError = True Then
                    Return Nothing
                Else
                    Dim AlignListResult(NodeCount - 1) As DataTypes.AlignmentNode
                    Dim StaStart, Bulge As Double
                    Dim P1, P2 As DataTypes.Point2d
                    Dim P As Point2d

                    C = 1
                    NodeCount = 0
                    While C < AlignListArray.Length - 1 And NodeError = False
                        If AlignListArray(C).TypeCode = LispDataType.ListBegin Then
                            If AlignListArray(C + 1).TypeCode = LispDataType.Double Then
                                If AlignListArray(C + 2).TypeCode = LispDataType.Point2d Then
                                    If AlignListArray(C + 3).TypeCode = LispDataType.Point2d Then
                                        StaStart = AlignListArray(C + 1).Value
                                        P = AlignListArray(C + 2).Value
                                        P1.X = P.X
                                        P1.Y = P.Y
                                        P = AlignListArray(C + 3).Value
                                        P2.X = P.X
                                        P2.Y = P.Y
                                    Else
                                        NodeError = True
                                    End If
                                Else
                                    NodeError = True
                                End If
                            Else
                                NodeError = True
                            End If
                        Else
                            NodeError = True
                        End If
                        If AlignListArray(C + 4).TypeCode = LispDataType.ListBegin Then
                            If AlignListArray(C + 5).TypeCode = LispDataType.Point2d Then
                                If AlignListArray(C + 6).TypeCode = LispDataType.Point2d Then
                                    If AlignListArray(C + 7).TypeCode = LispDataType.Point2d Then
                                        If AlignListArray(C + 8).TypeCode = LispDataType.Double Then
                                            If AlignListArray(C + 9).TypeCode = LispDataType.ListEnd Then
                                                If AlignListArray(C + 10).TypeCode = LispDataType.ListEnd Then
                                                    'Spiral
                                                    AlignListResult(NodeCount).AlignNodeType = DataTypes.AlignmentNodeType.Spiral
                                                    AlignListResult(NodeCount).AlignNodeSpiral.StaStart = StaStart
                                                    AlignListResult(NodeCount).AlignNodeSpiral.P1 = P1
                                                    AlignListResult(NodeCount).AlignNodeSpiral.P2 = P2
                                                    P = AlignListArray(C + 5).Value
                                                    AlignListResult(NodeCount).AlignNodeSpiral.PLT.X = P.X
                                                    AlignListResult(NodeCount).AlignNodeSpiral.PLT.Y = P.Y
                                                    P = AlignListArray(C + 6).Value
                                                    AlignListResult(NodeCount).AlignNodeSpiral.PLTST.X = P.X
                                                    AlignListResult(NodeCount).AlignNodeSpiral.PLTST.Y = P.Y
                                                    P = AlignListArray(C + 7).Value
                                                    AlignListResult(NodeCount).AlignNodeSpiral.PST.X = P.X
                                                    AlignListResult(NodeCount).AlignNodeSpiral.PST.Y = P.Y
                                                    AlignListResult(NodeCount).AlignNodeSpiral.Lo = AlignListArray(C + 8).Value
                                                    NodeCount = NodeCount + 1
                                                    C = C + 11
                                                Else
                                                    NodeError = True
                                                End If
                                            Else
                                                NodeError = True
                                            End If
                                        Else
                                            NodeError = True
                                        End If
                                    Else
                                        NodeError = True
                                    End If
                                Else
                                    NodeError = True
                                End If
                            Else
                                NodeError = True
                            End If
                        ElseIf AlignListArray(C + 4).TypeCode = LispDataType.Double Then
                            If AlignListArray(C + 5).TypeCode = LispDataType.ListEnd Then
                                Bulge = AlignListArray(C + 4).Value
                                If Abs(Bulge) < TolFine Then
                                    'Line
                                    AlignListResult(NodeCount).AlignNodeType = DataTypes.AlignmentNodeType.Tangent
                                    AlignListResult(NodeCount).AlignNodeTangent.StaStart = StaStart
                                    AlignListResult(NodeCount).AlignNodeTangent.P1 = P1
                                    AlignListResult(NodeCount).AlignNodeTangent.P2 = P2
                                Else
                                    'Arc
                                    AlignListResult(NodeCount).AlignNodeType = DataTypes.AlignmentNodeType.Arc
                                    AlignListResult(NodeCount).AlignNodeArc.StaStart = StaStart
                                    AlignListResult(NodeCount).AlignNodeArc.P1 = P1
                                    AlignListResult(NodeCount).AlignNodeArc.P2 = P2
                                    AlignListResult(NodeCount).AlignNodeArc.Bulge = Bulge
                                End If
                                NodeCount = NodeCount + 1
                                C = C + 6
                            Else
                                NodeError = True
                            End If
                        Else
                            NodeError = True
                        End If
                    End While

                    If NodeError = True Then
                        Return Nothing
                    Else
                        Return AlignListResult
                    End If
                End If
            End If
        End Function

    End Class

    Public Class ProfList
        Private Shared Tol As Double = 0.0000000001
        Private Shared TolFine As Double = 0.00000000000001

        Public Nodes() As DataTypes.ProfileNode

        Sub New()
            GetProf()
        End Sub

        Sub New(ByVal ProfListName As String)
            GetProf(ProfListName)
        End Sub

        Public Sub GetProf()
            Nodes = GetProfList("PVIList")
        End Sub

        Public Sub GetProf(ByVal ProfListName As String)
            Nodes = GetProfList(ProfListName)
        End Sub

        Public Function GetProfList(ByVal ProfListName As String) As DataTypes.ProfileNode()
            Dim Proflist As ResultBuffer = GetPutSym.GetSymString(ProfListName)

            If (Proflist Is Nothing) Then
                Return Nothing
            Else
                Dim NodeCount As Integer
                Dim C As Integer
                Dim NodeError As Boolean = False
                Dim ProfListArray As TypedValue() = Proflist.AsArray

                If (ProfListArray(C).TypeCode <> LispDataType.ListBegin) Or _
                   (ProfListArray(ProfListArray.Length - 1).TypeCode <> LispDataType.ListEnd) Then
                    NodeError = True
                Else
                    NodeCount = (ProfListArray.Length - 2) / 6
                End If

                If NodeError = True Then
                    Return Nothing
                Else
                    Dim ProfListResult(NodeCount - 1) As DataTypes.ProfileNode

                    C = 1
                    NodeCount = 0
                    While C < ProfListArray.Length - 1 And NodeError = False
                        If ProfListArray(C).TypeCode = LispDataType.ListBegin Then
                            If ProfListArray(C + 1).TypeCode = LispDataType.Double Then
                                If ProfListArray(C + 2).TypeCode = LispDataType.Double Then
                                    If ProfListArray(C + 3).TypeCode = LispDataType.Text Then
                                        If ProfListArray(C + 4).TypeCode = LispDataType.Double Then
                                            If ProfListArray(C + 5).TypeCode = LispDataType.ListEnd Then
                                                ProfListResult(NodeCount).Station = ProfListArray(C + 1).Value
                                                ProfListResult(NodeCount).Elevation = ProfListArray(C + 2).Value
                                                ProfListResult(NodeCount).Value = ProfListArray(C + 4).Value
                                                If ProfListArray(C + 3).Value = "L" Then
                                                    ProfListResult(NodeCount).ProfileNodeType = DataTypes.ProfileNodeType.Parabolic
                                                Else
                                                    ProfListResult(NodeCount).ProfileNodeType = DataTypes.ProfileNodeType.Circular
                                                End If
                                                NodeCount = NodeCount + 1
                                                C = C + 6
                                            Else
                                                NodeError = True
                                            End If
                                        Else
                                            NodeError = True
                                        End If
                                    Else
                                        NodeError = True
                                    End If
                                Else
                                    NodeError = True
                                End If
                            Else
                                NodeError = True
                            End If
                        Else
                            NodeError = True
                        End If
                    End While

                    If NodeError = True Then
                        Return Nothing
                    Else
                        For NodeCount = 0 To ProfListResult.Length - 1
                            If NodeCount = 0 Then
                                ProfListResult(NodeCount).G1 = Double.NaN
                                If NodeCount = ProfListResult.Length - 1 Then
                                    ProfListResult(NodeCount).G2 = Double.NaN
                                Else
                                    ProfListResult(NodeCount).G2 = (ProfListResult(NodeCount + 1).Elevation - _
                                                                    ProfListResult(NodeCount).Elevation) / _
                                                                   (ProfListResult(NodeCount + 1).Station - _
                                                                    ProfListResult(NodeCount).Station)
                                End If
                            ElseIf NodeCount = ProfListResult.Length - 1 Then
                                ProfListResult(NodeCount).G1 = (ProfListResult(NodeCount).Elevation - _
                                                                ProfListResult(NodeCount - 1).Elevation) / _
                                                               (ProfListResult(NodeCount).Station - _
                                                                ProfListResult(NodeCount - 1).Station)
                                ProfListResult(NodeCount).G2 = Double.NaN
                            Else
                                ProfListResult(NodeCount).G1 = (ProfListResult(NodeCount).Elevation - _
                                                                ProfListResult(NodeCount - 1).Elevation) / _
                                                               (ProfListResult(NodeCount).Station - _
                                                                ProfListResult(NodeCount - 1).Station)
                                ProfListResult(NodeCount).G2 = (ProfListResult(NodeCount + 1).Elevation - _
                                                                ProfListResult(NodeCount).Elevation) / _
                                                               (ProfListResult(NodeCount + 1).Station - _
                                                                ProfListResult(NodeCount).Station)
                            End If
                        Next
                        Return ProfListResult
                    End If
                End If
            End If
        End Function

        Public Function Elevation(ByVal Sta As Double, ByRef ElevationResult As Double) As Boolean
            If Not (Nodes Is Nothing) Then
                If Nodes.Length > 1 Then
                    Dim C As Integer = 1

                    If Sta >= Nodes(0).Station And Sta <= Nodes(Nodes.Length - 1).Station Then
                        While Sta > Nodes(C).Station + (Nodes(C).Value / 2.0)
                            C = C + 1
                        End While

                        Dim D As Double

                        ElevationResult = Nodes(C - 1).Elevation + Nodes(C).G1 * (Sta - Nodes(C - 1).Station)
                        D = Sta - (Nodes(C).Station - (Nodes(C).Value / 2.0))
                        If D > TolFine And Nodes(C).Value > TolFine Then
                            ElevationResult = ElevationResult +
                                              ((D * D * (Nodes(C).G2 - Nodes(C).G1)) /
                                               (2.0 * Nodes(C).Value))
                        End If
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

        Public Function Slope(ByVal Sta As Double, ByRef SlopeResult As Double) As Boolean
            If Not (Nodes Is Nothing) Then
                If Nodes.Length > 1 Then
                    Dim C As Integer = 1

                    If Sta >= Nodes(0).Station And Sta <= Nodes(Nodes.Length - 1).Station Then
                        While Sta > Nodes(C).Station + (Nodes(C).Value / 2.0)
                            C = C + 1
                        End While

                        Dim D As Double

                        SlopeResult = Nodes(C).G1
                        D = Sta - (Nodes(C).Station - (Nodes(C).Value / 2.0))
                        If D > TolFine And Nodes(C).Value > TolFine Then
                            SlopeResult = SlopeResult + (D / Nodes(C).Value) * (Nodes(C).G2 - Nodes(C).G1)
                        End If
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

    End Class

    Public Class SuperList
        Private Shared Tol As Double = 0.0000000001
        Private Shared TolFine As Double = 0.00000000000001

        Public Nodes() As DataTypes.SuperNode

        Sub New()
            GetSuper()
        End Sub

        Sub New(ByVal SuperListName As String)
            GetSuper(SuperListName)
        End Sub

        Public Sub GetSuper()
            Nodes = GetSuperList("SuperList")
        End Sub

        Public Sub GetSuper(ByVal SuperListName As String)
            Nodes = GetSuperList(SuperListName)
        End Sub

        Public Function GetSuperList(ByVal SuperListName As String) As DataTypes.SuperNode()
            Dim SuperList As ResultBuffer = GetPutSym.GetSymString(SuperListName)

            If (SuperList Is Nothing) Then
                Return Nothing
            Else
                Dim NodeCount As Integer
                Dim C As Integer
                Dim NodeError As Boolean = False
                Dim SuperListArray As TypedValue() = SuperList.AsArray

                If (SuperListArray(C).TypeCode <> LispDataType.ListBegin) Or _
                   (SuperListArray(SuperListArray.Length - 1).TypeCode <> LispDataType.ListEnd) Then
                    NodeError = True
                Else
                    NodeCount = SuperListArray.Length - 2
                End If

                If NodeError = True Then
                    Return Nothing
                Else
                    Dim SuperListResult(NodeCount - 1) As DataTypes.SuperNode
                    Dim P As Point3d

                    C = 1
                    NodeCount = 0
                    While C < SuperListArray.Length - 1 And NodeError = False
                        If SuperListArray(C).TypeCode = LispDataType.Point3d Then
                            P = SuperListArray(C).Value
                            SuperListResult(NodeCount).Station = P.X
                            SuperListResult(NodeCount).Left = P.Y
                            SuperListResult(NodeCount).Right = P.Z
                            NodeCount = NodeCount + 1
                            C = C + 1
                        Else
                            NodeError = True
                        End If
                    End While

                    If NodeError = True Then
                        Return Nothing
                    Else
                        Return SuperListResult
                    End If
                End If
            End If

        End Function

        Public Function Super(ByVal Sta As Double, ByRef SuperResult As DataTypes.Point2d) As Boolean
            If Not (Nodes Is Nothing) Then
                If Sta < Nodes(0).Station Then
                    SuperResult.X = Nodes(0).Left
                    SuperResult.Y = Nodes(0).Right
                    Return True
                ElseIf Sta > Nodes(Nodes.Length - 1).Station Then
                    SuperResult.X = Nodes(Nodes.Length - 1).Left
                    SuperResult.Y = Nodes(Nodes.Length - 1).Right
                    Return True
                Else
                    Dim C As Integer = 0

                    While Sta > Nodes(C).Station
                        C = C + 1
                    End While

                    SuperResult.X = Nodes(C - 1).Left +
                                    ((Sta - Nodes(C - 1).Station) / (Nodes(C).Station - Nodes(C - 1).Station)) *
                                    (Nodes(C).Left - Nodes(C - 1).Left)
                    SuperResult.Y = Nodes(C - 1).Right +
                                    ((Sta - Nodes(C - 1).Station) / (Nodes(C).Station - Nodes(C - 1).Station)) *
                                    (Nodes(C).Right - Nodes(C - 1).Right)

                    Return True
                End If
            Else
                Return False
            End If

        End Function
    End Class

    Public Class DataTypes

        Private Shared Tol As Double = 0.0000000001
        Private Shared TolFine As Double = 0.00000000000001

        Public Structure Point2d
            Public X As Double
            Public Y As Double

            Public Function DistanceTo(ByVal P As Point2d) As Double
                DistanceTo = Sqrt(Pow((P.X - X), 2) + Pow((P.Y - Y), 2))
            End Function

            Public Function AngleTo(ByVal P As Point2d) As Double
                AngleTo = Atan2(P.Y - Y, P.X - X)
            End Function

        End Structure

        Public Structure Point3d
            Public X As Double
            Public Y As Double
            Public Z As Double

            Public Function DistanceTo(ByVal P As Point3d) As Double
                DistanceTo = Sqrt(Pow((P.X - X), 2) + Pow((P.Y - Y), 2) + Pow((P.Z - Z), 2))
            End Function

            Public Function DistanceTo2D(ByVal P As Point3d) As Double
                DistanceTo2D = Sqrt(Pow((P.X - X), 2) + Pow((P.Y - Y), 2))
            End Function

            Public Function AngleTo2D(ByVal P As Point3d) As Double
                AngleTo2D = Atan2(P.Y - Y, P.X - X)
            End Function

        End Structure

        Public Structure Tangent
            Public StaStart As Double
            Public P1 As Point2d
            Public P2 As Point2d

            Public Function Length() As Double
                Return P1.DistanceTo(P2)
            End Function

            Public Function StaOff(ByVal P As Point2d, ByRef StaOffResult As Point2d) As Boolean
                Dim Ang, D, D1, D11, D2, D22 As Double

                D = Length()
                D1 = P1.DistanceTo(P)
                D2 = P2.DistanceTo(P)
                D11 = ((D * D) + ((D1 * D1) - (D2 * D2))) / (2.0 * D)
                D22 = D - D11
                'If (D11 <= (D + Tol)) And (D22 <= (D + Tol)) Then
                If (D11 <= D) And (D22 <= D) Then
                    StaOffResult.X = StaStart + D11
                    StaOffResult.Y = Sqrt(Abs((D1 * D1) - (D11 * D11)))
                    Ang = P1.AngleTo(P2) - P1.AngleTo(P)
                    While Ang < 0.0
                        Ang = Ang + 2.0 * PI
                    End While
                    If Ang > (PI / 2.0) Then
                        StaOffResult.Y = StaOffResult.Y * -1.0
                    End If
                    Return True
                Else
                    Return False
                End If
            End Function

            Public Function XY(ByVal P As DataTypes.Point2d, ByRef XYResult As DataTypes.Point2d) As Boolean
                Dim Sta As Double = P.X - StaStart
                Dim Offset As Double = P.Y
                Dim Ang As Double

                If Sta <= Length() Then
                    Ang = P1.AngleTo(P2)
                    XYResult.X = P1.X + Sta * Cos(Ang) + Offset * Sin(Ang)
                    XYResult.Y = P1.Y + Sta * Sin(Ang) - Offset * Cos(Ang)
                    Return True
                Else
                    Return False
                End If
            End Function

            Public Function GetP1(ByRef PResult As DataTypes.Point2d) As Boolean
                PResult.X = P1.X
                PResult.Y = P1.Y
                Return True
            End Function

            Public Function GetP2(ByRef PResult As DataTypes.Point2d) As Boolean
                PResult.X = P2.X
                PResult.Y = P2.Y
                Return True
            End Function

            Public Function AngIn() As Double
                Return P1.AngleTo(P2)
            End Function

            Public Function AngOut() As Double
                Return P1.AngleTo(P2)
            End Function

        End Structure

        Public Structure Arc
            Public StaStart As Double
            Public P1 As Point2d
            Public P2 As Point2d
            Public Bulge As Double

            Public Function Length() As Double
                Dim ATotal, Chord, R As Double
                ATotal = 4.0 * Atan(Abs(Bulge))
                Chord = P1.DistanceTo(P2)
                R = Chord / (2.0 * Sin(ATotal / 2.0))
                Return R * ATotal
            End Function

            Private Function Radius() As Double
                Dim ATotal, Chord As Double

                ATotal = 4.0 * Atan(Abs(Bulge))
                Chord = P1.DistanceTo(P2)
                Return Chord / (2.0 * Sin(ATotal / 2.0))
            End Function

            Private Function Center() As Point2d
                Dim Ang, ATotal, Chord, R, X, Y As Double

                ATotal = 4.0 * Atan(Abs(Bulge))
                Chord = P1.DistanceTo(P2)
                Ang = P1.AngleTo(P2)
                R = Radius()
                X = Chord / 2.0
                Y = Sqrt((R * R) - (X * X)) * Sign(Bulge) * Sign(Abs(Bulge) - 1.0)
                Center.X = P1.X + X * Cos(Ang) + Y * Sin(Ang)
                Center.Y = P1.Y + X * Sin(Ang) - Y * Cos(Ang)
                Return Center
            End Function

            Public Function StaOff(ByVal P As Point2d, ByRef StaOffResult As Point2d) As Boolean
                Dim PC As Point2d
                Dim Ang1, Ang2, R As Double

                PC = Center()
                If Bulge < 0.0 Then
                    Ang1 = PC.AngleTo(P1) - PC.AngleTo(P)
                    Ang2 = PC.AngleTo(P1) - PC.AngleTo(P2)
                Else
                    Ang1 = PC.AngleTo(P) - PC.AngleTo(P1)
                    Ang2 = PC.AngleTo(P2) - PC.AngleTo(P1)
                End If
                While Ang1 < 0.0
                    Ang1 = Ang1 + 2.0 * PI
                End While
                While Ang2 < 0.0
                    Ang2 = Ang2 + 2.0 * PI
                End While
                'If Ang1 <= (Ang2 + TolFine) Then
                If Ang1 <= Ang2 Then
                    R = Radius()
                    StaOffResult.X = StaStart + R * Ang1
                    StaOffResult.Y = Sign(Bulge) * (PC.DistanceTo(P) - R)
                    Return True
                Else
                    Return False
                End If
                Return False
            End Function

            Public Function XY(ByVal P As DataTypes.Point2d, ByRef XYResult As DataTypes.Point2d) As Boolean
                Dim Sta As Double = P.X - StaStart

                If Sta <= Length() Then
                    Dim Offset As Double = P.Y
                    Dim PC As Point2d = Center()
                    Dim R As Double = Radius()
                    Dim P2 As Point2d
                    Dim Ang As Double

                    Ang = PC.AngleTo(P1) + Sign(Bulge) * (Sta / R)
                    P2.X = PC.X + R * Cos(Ang)
                    P2.Y = PC.Y + R * Sin(Ang)

                    If Bulge < 0.0 Then
                        Ang = P2.AngleTo(PC)
                    Else
                        Ang = PC.AngleTo(P2)
                    End If

                    XYResult.X = P2.X + Offset * Cos(Ang)
                    XYResult.Y = P2.Y + Offset * Sin(Ang)

                    Return True
                Else
                    Return False
                End If
            End Function

            Public Function GetP1(ByRef PResult As DataTypes.Point2d) As Boolean
                PResult.X = P1.X
                PResult.Y = P1.Y
                Return True
            End Function

            Public Function GetP2(ByRef PResult As DataTypes.Point2d) As Boolean
                PResult.X = P2.X
                PResult.Y = P2.Y
                Return True
            End Function

            Public Function AngIn() As Double
                AngIn = P1.AngleTo(Center()) + System.Math.PI
                If (Bulge > 0.0) Then AngIn = AngIn * -1.0
            End Function

            Public Function AngOut() As Double
                AngOut = P2.AngleTo(Center()) + System.Math.PI
                If (Bulge > 0.0) Then AngOut = AngOut * -1.0
            End Function

        End Structure

        Public Structure Spiral
            Public StaStart As Double
            Public P1 As Point2d
            Public P2 As Point2d
            Public PLT As Point2d
            Public PLTST As Point2d
            Public PST As Point2d
            Public Lo As Double

            Public Function SpiralLs() As Double
                SpiralLs = 2.0 * Theta() * R()
            End Function

            Public Function Length() As Double
                Length = SpiralLs() - Lo
            End Function

            Public Function Theta() As Double
                Theta = Abs(PST.AngleTo(PLTST) - PLTST.AngleTo(PLT))
                If Theta < 0.0 Then Theta = Theta + 2.0 * PI
                If Theta > PI Then Theta = 2.0 * PI - Theta
            End Function

            Public Function R() As Double
                Return (PLTST.DistanceTo(PST) * Sin(Theta())) / SpiralCommands.SpiralFYR(Theta())
            End Function

            Private Function OddEven(ByVal N As Integer) As Integer
                Dim Remainder As Integer

                Remainder = N Mod 2
                If Remainder = 1 Then
                    Return -1
                Else
                    Return 1
                End If
            End Function

            Private Function SpiralFYR() As Double
                Return SpiralFYR(Theta())
            End Function

            Private Function SpiralFYR(ByVal Theta As Double) As Double
                Dim Ar2, Denominator, Numerator, Sum, Sum2 As Double
                Dim N As Integer

                Sum = -1.0
                Sum2 = 0.0
                Ar2 = 2.0 * Theta
                N = 1
                While (Abs(Sum - Sum2) > Tol)
                    Sum = Sum2
                    Numerator = OddEven(N + 1) * Pow(Ar2, ((2.0 * N) - 1.0))
                    Denominator = Pow(2.0, ((2.0 * N) - 1.0)) * ((4.0 * N) - 1.0) * SpiralFact((2.0 * N) - 1.0)
                    Sum2 = Sum2 + (Numerator / Denominator)
                    N = N + 1
                End While
                Sum = Sum * Ar2
                Return Sum
            End Function

            Private Function SpiralFXR() As Double
                Return SpiralFXR(Theta())
            End Function

            Private Function SpiralFXR(ByVal Theta As Double) As Double
                Dim Ar2, Denominator, Numerator, Sum, Sum2 As Double
                Dim N As Integer

                Sum = -1.0
                Sum2 = 0.0
                Ar2 = 2.0 * Theta
                N = 1
                While (Abs(Sum - Sum2) > Tol)
                    Sum = Sum2
                    If Theta > TolFine Then
                        Numerator = OddEven(N + 1) * Pow(Ar2, (2.0 * (N - 1)))
                    Else
                        Numerator = 0.0
                    End If
                    Denominator = Pow(2.0, (2.0 * (N - 1.0))) * ((4.0 * N) - 3.0) * SpiralFact(2.0 * (N - 1.0))
                    Sum2 = Sum2 + (Numerator / Denominator)
                    N = N + 1
                End While
                Sum = Sum * Ar2
                Return Sum
            End Function

            Private Function SpiralFact(ByVal N As Integer) As Double
                Dim F As Double
                F = 1.0
                While (N > 0)
                    F = F * N
                    N = N - 1
                End While
                SpiralFact = F
            End Function

            Private Function SpiralFact(ByVal N As Double) As Double
                Dim F As Double
                F = 1.0
                While (N > 0.0)
                    F = F * N
                    N = N - 1.0
                End While
                SpiralFact = F
            End Function

            Private Function SpiralP() As Double
                Return SpiralP(R(), SpiralLs())
            End Function

            Private Function SpiralP(ByVal R As Double, ByVal LS As Double) As Double
                Dim Theta As Double

                Theta = LS / (2.0 * R)
                SpiralP = R * (SpiralFYR(Theta) - (1.0 - Cos(Theta)))
            End Function

            Private Function SpiralPR() As Double
                Return SpiralPR(Theta())
            End Function

            Private Function SpiralPR(ByVal Theta As Double) As Double
                Return SpiralFYR(Theta) - (1.0 - Cos(Theta))
            End Function

            Private Function SpiralK() As Double
                Return SpiralK(R(), SpiralLs())
            End Function

            Private Function SpiralK(ByVal R As Double, ByVal LS As Double) As Double
                Dim Theta As Double

                Theta = LS / (2.0 * R)
                Return R * (SpiralFXR(Theta) - Sin(Theta))
            End Function

            Private Function SpiralKR() As Double
                Return SpiralKR(Theta())
            End Function

            Private Function SpiralKR(ByVal Theta As Double) As Double
                SpiralKR = SpiralFXR(Theta) - Sin(Theta)
            End Function

            Private Function StaOff_Fctn(ByVal Val As Double, ByVal Px As Double, ByVal Py As Double, ByVal A2 As Double, ByVal SpiralDirection As Double) As Double
                Dim R As Double

                If Abs(Val) < TolFine Then
                    R = 0.0
                Else
                    R = Sqrt(A2 / (2.0 * Val))
                End If

                If Abs(Val) < TolFine Then
                    Return Px
                Else
                    Return ((Px - (R * SpiralFXR(Val))) * Cos(Val)) + (SpiralDirection * (Py - (SpiralDirection * R * SpiralFYR(Val))) * Sin(Val))
                End If
            End Function

            Public Function Staoff(ByVal P As Point2d, ByRef StaOffResult As Point2d) As Boolean
                Dim A2, Alpha, F0, F1, F2, OffsetDirection, Px, Py, R0, SpiralDirection, Theta0, Theta1, Theta2 As Double
                Dim SP0, SP1 As Point2d

                If Sin(PLTST.AngleTo(PST) - PLT.AngleTo(PLTST)) > 0.0 Then
                    SpiralDirection = 1.0
                Else
                    SpiralDirection = -1.0
                End If
                Alpha = PLT.AngleTo(PLTST)
                Px = ((P.Y - PLT.Y) * Sin(Alpha)) + ((P.X - PLT.X) * Cos(Alpha))
                Py = ((P.Y - PLT.Y) * Cos(Alpha)) - ((P.X - PLT.X) * Sin(Alpha))
                A2 = 2.0 * R() * R() * Theta()
                Theta1 = (Lo * Lo) / (2.0 * A2)
                Theta2 = Theta()
                F1 = StaOff_Fctn(Theta1, Px, Py, A2, SpiralDirection)
                F2 = StaOff_Fctn(Theta2, Px, Py, A2, SpiralDirection)
                If (F1 * F2) > TolFine Then
                    Return False
                Else
                    If P.DistanceTo(PST) < TolFine Then
                        Theta0 = Theta()
                    ElseIf P.DistanceTo(PLT) < TolFine Then
                        Theta0 = 0.0
                    Else
                        Theta0 = (Theta1 + Theta2) / 2.0
                        F0 = StaOff_Fctn(Theta0, Px, Py, A2, SpiralDirection)
                        While (Abs(Theta1 - Theta2) > TolFine)
                            If (F0 * F2) > 0.0 Then
                                Theta2 = Theta0
                                F2 = F0
                            Else
                                Theta1 = Theta0
                                F1 = F0
                            End If
                            Theta0 = (Theta1 + Theta2) / 2.0
                            F0 = StaOff_Fctn(Theta0, Px, Py, A2, SpiralDirection)
                        End While
                    End If
                    If Abs(Theta0) < TolFine Then
                        R0 = 0.0
                    Else
                        R0 = Sqrt(A2 / (2.0 * Theta0))
                    End If
                    If Abs(R0) < TolFine Then
                        StaOffResult.X = 0.0
                    Else
                        StaOffResult.X = A2 / R0
                    End If
                    SP0.X = R0 * SpiralFXR(Theta0)
                    SP0.Y = SpiralDirection * R0 * SpiralFYR(Theta0)
                    SP1.X = Px
                    SP1.Y = Py
                    If Sin(SP0.AngleTo(SP1)) > 0.0 Then
                        OffsetDirection = -1.0
                    Else
                        OffsetDirection = 1.0
                    End If
                    StaOffResult.Y = OffsetDirection * SP0.DistanceTo(SP1)
                    If P2.DistanceTo(PST) < P1.DistanceTo(PST) Then
                        StaOffResult.X = StaOffResult.X + StaStart - Lo
                    Else
                        StaOffResult.X = SpiralLs() + StaStart - StaOffResult.X
                        StaOffResult.Y = StaOffResult.Y * -1.0
                    End If
                    Return True
                End If
            End Function

            Public Function XY(ByVal P As DataTypes.Point2d, ByRef XYResult As DataTypes.Point2d) As Boolean
                Dim Sta As Double = P.X - StaStart

                If Sta <= Length() Then
                    Dim Offset As Double = P.Y
                    Dim Ang As Double = PLT.AngleTo(PLTST)
                    Dim LS As Double = SpiralLs()
                    Dim Ang2, SpiralDirection, Theta0, R0, X0, Y0 As Double
                    Dim PS As Point2d

                    If Sin(PLTST.AngleTo(PST) - PLT.AngleTo(PLTST)) > 0.0 Then
                        SpiralDirection = 1.0
                    Else
                        SpiralDirection = -1.0
                    End If

                    If P2.DistanceTo(PST) < P1.DistanceTo(PST) Then
                        Sta = Sta + Lo
                    Else
                        Sta = LS - Sta
                        Offset = Offset * -1.0
                    End If
                    If Sta < TolFine Then
                        PS = PLT
                        Theta0 = 0.0
                    Else
                        Theta0 = Theta() * Pow((Sta / LS), 2.0)
                        If Sta < TolFine Then
                            R0 = 0.0
                            X0 = 0.0
                            Y0 = 0.0
                        Else
                            R0 = R() * (LS / Sta)
                            X0 = R0 * SpiralFXR(Theta0)
                            Y0 = SpiralDirection * R0 * SpiralFYR(Theta0)
                        End If
                        PS.X = PLT.X + (X0 * Cos(Ang)) - (Y0 * Sin(Ang))
                        PS.Y = PLT.Y + (X0 * Sin(Ang)) + (Y0 * Cos(Ang))
                    End If
                    Ang2 = Ang + (SpiralDirection * Theta0) - (PI / 2.0)

                    Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
                    Dim ed As Editor = doc.Editor

                    XYResult.X = PS.X + Offset * Cos(Ang2)
                    XYResult.Y = PS.Y + Offset * Sin(Ang2)
                    Return True
                Else
                    Return False
                End If
            End Function

            Public Function GetP1(ByRef PResult As DataTypes.Point2d) As Boolean
                PResult.X = P1.X
                PResult.Y = P1.Y
                Return True
            End Function

            Public Function GetP2(ByRef PResult As DataTypes.Point2d) As Boolean
                PResult.X = P2.X
                PResult.Y = P2.Y
                Return True
            End Function

            Public Function AngIn() As Double
                Return P1.AngleTo(PLTST)
            End Function

            Public Function AngOut() As Double
                Return PLTST.AngleTo(P2)
            End Function

        End Structure

        Public Enum AlignmentNodeType
            Tangent
            Arc
            Spiral
        End Enum

        Public Structure AlignmentNode
            Public AlignNodeType As AlignmentNodeType
            Public AlignNodeTangent As Tangent
            Public AlignNodeArc As Arc
            Public AlignNodeSpiral As Spiral

            Public Function StaStart() As Double
                If AlignNodeType = AlignmentNodeType.Tangent Then
                    Return AlignNodeTangent.StaStart
                ElseIf AlignNodeType = AlignmentNodeType.Arc Then
                    Return AlignNodeArc.StaStart
                ElseIf AlignNodeType = AlignmentNodeType.Spiral Then
                    Return AlignNodeSpiral.StaStart
                Else
                    Return Double.NaN
                End If
            End Function

            Public Function Length() As Double
                If AlignNodeType = AlignmentNodeType.Tangent Then
                    Return AlignNodeTangent.Length
                ElseIf AlignNodeType = AlignmentNodeType.Arc Then
                    Return AlignNodeArc.Length
                ElseIf AlignNodeType = AlignmentNodeType.Spiral Then
                    Return AlignNodeSpiral.Length
                Else
                    Return Double.NaN
                End If
            End Function

            Public Function StaOff(ByVal P As Point2d, ByRef StaOffResult As Point2d) As Boolean
                Select Case AlignNodeType
                    Case AlignmentNodeType.Tangent
                        StaOff = AlignNodeTangent.StaOff(P, StaOffResult)
                    Case AlignmentNodeType.Arc
                        StaOff = AlignNodeArc.StaOff(P, StaOffResult)
                    Case AlignmentNodeType.Spiral
                        StaOff = AlignNodeSpiral.Staoff(P, StaOffResult)
                    Case Else
                        StaOff = False
                End Select
                Return StaOff
            End Function

            Public Function XY(ByVal P As Point2d, ByRef XYResult As Point2d) As Boolean
                Select Case AlignNodeType
                    Case AlignmentNodeType.Tangent
                        XY = AlignNodeTangent.XY(P, XYResult)
                    Case AlignmentNodeType.Arc
                        XY = AlignNodeArc.XY(P, XYResult)
                    Case AlignmentNodeType.Spiral
                        XY = AlignNodeSpiral.XY(P, XYResult)
                    Case Else
                        XY = False
                End Select
                Return XY
            End Function

            Public Function GetP1(ByRef PResult As Point2d) As Boolean
                Select Case AlignNodeType
                    Case AlignmentNodeType.Tangent
                        GetP1 = AlignNodeTangent.GetP1(PResult)
                    Case AlignmentNodeType.Arc
                        GetP1 = AlignNodeArc.GetP1(PResult)
                    Case AlignmentNodeType.Spiral
                        GetP1 = AlignNodeSpiral.GetP1(PResult)
                    Case Else
                        GetP1 = False
                End Select
                Return GetP1
            End Function

            Public Function GetP2(ByRef PResult As Point2d) As Boolean
                Select Case AlignNodeType
                    Case AlignmentNodeType.Tangent
                        GetP2 = AlignNodeTangent.GetP2(PResult)
                    Case AlignmentNodeType.Arc
                        GetP2 = AlignNodeArc.GetP2(PResult)
                    Case AlignmentNodeType.Spiral
                        GetP2 = AlignNodeSpiral.GetP2(PResult)
                    Case Else
                        GetP2 = False
                End Select
                Return GetP2
            End Function

            Public Function AngIn() As Double
                If AlignNodeType = AlignmentNodeType.Tangent Then
                    Return AlignNodeTangent.AngIn
                ElseIf AlignNodeType = AlignmentNodeType.Arc Then
                    Return AlignNodeArc.AngIn
                ElseIf AlignNodeType = AlignmentNodeType.Spiral Then
                    Return AlignNodeSpiral.AngIn
                Else
                    Return Double.NaN
                End If
            End Function

            Public Function AngOut() As Double
                If AlignNodeType = AlignmentNodeType.Tangent Then
                    Return AlignNodeTangent.AngOut
                ElseIf AlignNodeType = AlignmentNodeType.Arc Then
                    Return AlignNodeArc.AngOut
                ElseIf AlignNodeType = AlignmentNodeType.Spiral Then
                    Return AlignNodeSpiral.AngOut
                Else
                    Return Double.NaN
                End If
            End Function

        End Structure

        Public Enum ProfileNodeType
            Parabolic
            Circular
        End Enum

        Public Structure ProfileNode
            Public ProfileNodeType As ProfileNodeType
            Public Station, Elevation, Value, G1, G2 As Double
        End Structure

        Public Structure SuperNode
            Public Station, Left, Right As Double
        End Structure

    End Class
End Namespace