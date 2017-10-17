' (C) Copyright 2011 by Robert Livingston (RFLTools.com)
'
Imports System
Imports Autodesk.AutoCAD.Runtime
Imports Autodesk.AutoCAD.ApplicationServices
Imports Autodesk.AutoCAD.DatabaseServices
Imports Autodesk.AutoCAD.Geometry
Imports Autodesk.AutoCAD.EditorInput

' This line is not mandatory, but improves loading performances
<Assembly: CommandClass(GetType(RFLToolsApplication.MyCommands))>

Namespace RFLToolsApplication

    ' This class is instantiated by AutoCAD for each document when
    ' a command is called by the user the first time in the context
    ' of a given document. In other words, non static data in this class
    ' is implicitly per-document!
    Public Class MyCommands
        <LispFunction("RFL:SpiralFYR", "SpiralFYRLocal")>
        Public Function SpiralFYR(ByVal Args As ResultBuffer)

            If Args Is Nothing Then
                Return Args
            Else
                Dim InputArgs As Array = Args.AsArray
                If InputArgs.Length = 1 Then
                    Return SpiralCommands.SpiralFYR(CType(InputArgs.GetValue(0), TypedValue).Value)
                Else
                    Return 0
                End If
            End If
        End Function

        <LispFunction("RFL:SpiralFXR", "SpiralFXRLocal")>
        Public Function SpiralFXR(ByVal Args As ResultBuffer)

            If Args Is Nothing Then
                Return Args
            Else
                Dim InputArgs As Array = Args.AsArray
                If InputArgs.Length = 1 Then
                    Return SpiralCommands.SpiralFXR(CType(InputArgs.GetValue(0), TypedValue).Value)
                Else
                    Return 0
                End If
            End If
        End Function

        <LispFunction("RFL:SpiralP", "SpiralPLocal")>
        Public Function SpiralP(ByVal Args As ResultBuffer)

            If Args Is Nothing Then
                Return Args
            Else
                Dim InputArgs As Array = Args.AsArray
                If InputArgs.Length = 2 Then
                    Return SpiralCommands.SpiralP(CType(InputArgs.GetValue(0), TypedValue).Value, CType(InputArgs.GetValue(1), TypedValue).Value)
                Else
                    Return 0
                End If
            End If
        End Function

        <LispFunction("RFL:SpiralPR", "SpiralPRLocal")>
        Public Function SpiralPR(ByVal Args As ResultBuffer)

            If Args Is Nothing Then
                Return Args
            Else
                Dim InputArgs As Array = Args.AsArray
                If InputArgs.Length = 1 Then
                    Return SpiralCommands.SpiralPR(CType(InputArgs.GetValue(0), TypedValue).Value)
                Else
                    Return 0
                End If
            End If
        End Function

        <LispFunction("RFL:SpiralK", "SpiralKLocal")>
        Public Function SpiralK(ByVal Args As ResultBuffer)

            If Args Is Nothing Then
                Return Args
            Else
                Dim InputArgs As Array = Args.AsArray
                If InputArgs.Length = 2 Then
                    Return SpiralCommands.SpiralK(CType(InputArgs.GetValue(0), TypedValue).Value, CType(InputArgs.GetValue(1), TypedValue).Value)
                Else
                    Return 0
                End If
            End If
        End Function

        <LispFunction("RFL:SpiralKR", "SpiralKLocal")>
        Public Function SpiralKR(ByVal Args As ResultBuffer)

            If Args Is Nothing Then
                Return Args
            Else
                Dim InputArgs As Array = Args.AsArray
                If InputArgs.Length = 1 Then
                    Return SpiralCommands.SpiralKR(CType(InputArgs.GetValue(0), TypedValue).Value)
                Else
                    Return 0
                End If
            End If
        End Function

        <LispFunction("RFL:StaOff", "StaOff2Local")>
        Public Function StaOff(ByVal Args As ResultBuffer)

            If Args Is Nothing Then
                Return Nothing
            Else
                Dim InputArgs As TypedValue() = Args.AsArray()
                Dim AcadP2D As Point2d
                Dim AcadP3D As Point3d
                Dim P, PResult As DataTypes.Point2d

                Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
                Dim ed As Editor = doc.Editor

                If InputArgs.Length = 1 Then
                    If InputArgs(0).TypeCode = LispDataType.Point2d Or InputArgs(0).TypeCode = LispDataType.Point3d Then
                        Dim AlignList As New AlignList("RFL:AlignList")

                        If InputArgs(0).TypeCode = LispDataType.Point2d Then
                            AcadP2D = InputArgs(0).Value
                            P.X = AcadP2D.X
                            P.Y = AcadP2D.Y
                        Else
                            AcadP3D = InputArgs(0).Value
                            P.X = AcadP3D.X
                            P.Y = AcadP3D.Y
                        End If

                        If AlignList.StaOff(P, PResult) Then
                            Return New ResultBuffer _
                                       (New TypedValue(LispDataType.ListBegin),
                                        New TypedValue(CInt(LispDataType.Double), PResult.X),
                                        New TypedValue(CInt(LispDataType.Double), PResult.Y),
                                        New TypedValue(LispDataType.ListEnd))
                        Else
                            Return Nothing
                        End If
                    Else
                        ed.WriteMessage("Not 2D or 3D point" & vbLf)
                        Return Nothing
                    End If
                Else
                    ed.WriteMessage("Wrong number of arguments" & vbLf)
                    Return Nothing
                End If
            End If
        End Function

        <LispFunction("RFL:XY", "XYLocal")>
        Public Function XY(ByVal Args As ResultBuffer)

            If Args Is Nothing Then
                Return Nothing
            Else
                Dim InputArgs As TypedValue() = Args.AsArray()
                Dim AcadP As Point2d
                Dim P, PResult As DataTypes.Point2d

                Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
                Dim ed As Editor = doc.Editor

                If InputArgs.Length = 1 Then
                    If InputArgs(0).TypeCode = LispDataType.Point2d Then
                        Dim AlignList As New AlignList("RFL:AlignList")

                        AcadP = InputArgs(0).Value
                        P.X = AcadP.X
                        P.Y = AcadP.Y

                        If AlignList.XY(P, PResult) Then
                            Return New ResultBuffer _
                                       (New TypedValue(LispDataType.ListBegin),
                                        New TypedValue(CInt(LispDataType.Double), PResult.X),
                                        New TypedValue(CInt(LispDataType.Double), PResult.Y),
                                        New TypedValue(LispDataType.ListEnd))
                        Else
                            Return Nothing
                        End If
                    Else
                        ed.WriteMessage("Not 2D point" & vbLf)
                        Return Nothing
                    End If
                Else
                    ed.WriteMessage("Wrong number of arguments" & vbLf)
                    Return Nothing
                End If
            End If
        End Function

        <LispFunction("RFL:GetAlignLength", "GetAlignLengthLocal")>
        Public Function GetAlignLength(ByVal Args As ResultBuffer)
            Dim AlignList As New AlignList("RFL:AlignList")

            If (AlignList Is Nothing) Then
                Return Nothing
            Else
                Return AlignList.Length
            End If
        End Function

        <LispFunction("RFL:SightDist", "SightDistLocal")>
        Public Function SightDist(ByVal Args As ResultBuffer)
            Dim AlignList As New AlignList("RFL:AlignList")

            If (AlignList Is Nothing) Then
                Return Nothing
            Else
                If Args Is Nothing Then
                    Return Nothing
                Else
                    Dim InputArgs As TypedValue() = Args.AsArray()
                    Dim Sta, Dist, StaOut As Double

                    Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
                    Dim ed As Editor = doc.Editor

                    If InputArgs.Length = 2 Then
                        If (InputArgs(0).TypeCode = LispDataType.Double Or InputArgs(0).TypeCode = LispDataType.Int16 Or InputArgs(0).TypeCode = LispDataType.Int32) And
                           (InputArgs(1).TypeCode = LispDataType.Double Or InputArgs(1).TypeCode = LispDataType.Int16 Or InputArgs(1).TypeCode = LispDataType.Int32) Then

                            Sta = Convert.ToDouble(InputArgs(0).Value)
                            Dist = Convert.ToDouble(InputArgs(1).Value)

                            If AlignList.SightDist(Sta, Dist, StaOut) Then
                                Return StaOut
                            Else
                                Return Nothing
                            End If

                        End If
                        ed.WriteMessage("Not real value" & vbLf)
                        Return Nothing
                    Else
                        ed.WriteMessage("Wrong number of arguments" & vbLf)
                        Return Nothing
                    End If
                End If
            End If
        End Function

        <LispFunction("RFL:Elevation", "ElevationLocal")>
        Public Function Elevation(ByVal Args As ResultBuffer)

            If Args Is Nothing Then
                Return Nothing
            Else
                Dim InputArgs As TypedValue() = Args.AsArray()
                Dim Sta, ElevResult As Double

                Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
                Dim ed As Editor = doc.Editor

                If InputArgs.Length = 1 Then
                    If InputArgs(0).TypeCode = LispDataType.Double Or
                       InputArgs(0).TypeCode = LispDataType.Int16 Or
                       InputArgs(0).TypeCode = LispDataType.Int32 Then
                        Dim ProfList As New ProfList("RFL:PVIList")

                        Sta = Convert.ToDouble(InputArgs(0).Value)

                        If ProfList.Elevation(Sta, ElevResult) Then
                            Return ElevResult
                        Else
                            Return Nothing
                        End If
                    Else
                        ed.WriteMessage("Not real value" & vbLf)
                        Return Nothing
                    End If
                Else
                    ed.WriteMessage("Wrong number of arguments" & vbLf)
                    Return Nothing
                End If
            End If
        End Function

        <LispFunction("RFL:Slope", "SlopeLocal")>
        Public Function Slope(ByVal Args As ResultBuffer)

            If Args Is Nothing Then
                Return Nothing
            Else
                Dim InputArgs As TypedValue() = Args.AsArray()
                Dim Sta, SlopeResult As Double

                Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
                Dim ed As Editor = doc.Editor

                If InputArgs.Length = 1 Then
                    If InputArgs(0).TypeCode = LispDataType.Double Or
                       InputArgs(0).TypeCode = LispDataType.Int16 Or
                       InputArgs(0).TypeCode = LispDataType.Int32 Then
                        Dim ProfList As New ProfList("RFL:PVIList")

                        Sta = Convert.ToDouble(InputArgs(0).Value)

                        If ProfList.Slope(Sta, SlopeResult) Then
                            Return SlopeResult
                        Else
                            Return Nothing
                        End If
                    Else
                        ed.WriteMessage("Not real value" & vbLf)
                        Return Nothing
                    End If
                Else
                    ed.WriteMessage("Wrong number of arguments" & vbLf)
                    Return Nothing
                End If
            End If
        End Function

        <LispFunction("RFL:Super", "SuperLocal")>
        Public Function Super(ByVal Args As ResultBuffer)

            If Args Is Nothing Then
                Return Nothing
            Else
                Dim InputArgs As TypedValue() = Args.AsArray()
                Dim Sta As Double
                Dim SuperResult As DataTypes.Point2d

                Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
                Dim ed As Editor = doc.Editor

                If InputArgs.Length = 1 Then
                    If InputArgs(0).TypeCode = LispDataType.Double Or
                       InputArgs(0).TypeCode = LispDataType.Int16 Or
                       InputArgs(0).TypeCode = LispDataType.Int32 Then
                        Dim SuperList As New SuperList("RFL:SuperList")

                        Sta = Convert.ToDouble(InputArgs(0).Value)

                        If SuperList.Super(Sta, SuperResult) Then
                            Return New ResultBuffer _
                                       (New TypedValue(LispDataType.ListBegin),
                                        New TypedValue(CInt(LispDataType.Double), SuperResult.X),
                                        New TypedValue(CInt(LispDataType.Double), SuperResult.Y),
                                        New TypedValue(LispDataType.ListEnd))
                        Else
                            Return Nothing
                        End If
                    Else
                        ed.WriteMessage("Not real value" & vbLf)
                        Return Nothing
                    End If
                Else
                    ed.WriteMessage("Wrong number of arguments" & vbLf)
                    Return Nothing
                End If
            End If
        End Function

        <LispFunction("ArgDump", "ArgDumpLocal")>
        Public Function ArgDump(ByVal Args As ResultBuffer)

            If Args Is Nothing Then
                Return Args
            Else
                Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
                Dim ed As Editor = doc.Editor
                Dim InputArgs As TypedValue() = Args.AsArray()

                ed.WriteMessage("Starting summary :" & vbCrLf)
                For Each rb As TypedValue In InputArgs
                    Select Case rb.TypeCode
                        Case LispDataType.Angle
                            ed.WriteMessage("Angle " & vbCrLf)
                        Case LispDataType.DottedPair
                            ed.WriteMessage("DottedPair " & vbCrLf)
                        Case LispDataType.Double
                            ed.WriteMessage("Double " & vbCrLf)
                        Case LispDataType.Int16
                            ed.WriteMessage("Int16 " & vbCrLf)
                        Case LispDataType.Int32
                            ed.WriteMessage("Int32 " & vbCrLf)
                        Case LispDataType.ListBegin
                            ed.WriteMessage("ListBegin " & vbCrLf)
                        Case LispDataType.ListEnd
                            ed.WriteMessage("ListEnd " & vbCrLf)
                        Case LispDataType.Nil
                            ed.WriteMessage("Nil " & vbCrLf)
                        Case LispDataType.None
                            ed.WriteMessage("None " & vbCrLf)
                        Case LispDataType.ObjectId
                            ed.WriteMessage("ObjectId " & vbCrLf)
                        Case LispDataType.Orientation
                            ed.WriteMessage("Orientation " & vbCrLf)
                        Case LispDataType.Point2d
                            ed.WriteMessage("Point2d " & vbCrLf)
                        Case LispDataType.Point3d
                            ed.WriteMessage("Point3d " & vbCrLf)
                        Case LispDataType.SelectionSet
                            ed.WriteMessage("SelectioSet " & vbCrLf)
                        Case LispDataType.T_atom
                            ed.WriteMessage("T_atom " & vbCrLf)
                        Case LispDataType.Text
                            ed.WriteMessage("Text " & vbCrLf)
                        Case LispDataType.Void
                            ed.WriteMessage("Void " & vbCrLf)
                    End Select
                Next
                Return Nothing

            End If
        End Function

        <LispFunction("RFLT_PutSymbol", "PutSymbolLocal")>
        Public Shared Function PutSym(ByVal myLispArgs As ResultBuffer) As ResultBuffer
            Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
            Dim ed As Editor = doc.Editor
            If myLispArgs Is Nothing Then
                ed.WriteMessage(vbLf & "no arguments!" & vbLf)
            Else
                Dim args As TypedValue() = myLispArgs.AsArray()
                If args.Length = 1 Then
                    ed.WriteMessage(vbLf & "too few arguments!" & vbLf)
                ElseIf args(0).TypeCode <> LispDataType.Text Then
                    ed.WriteMessage(vbLf & "argument is not a string!" & vbLf)
                Else
                    Dim SymName As String = args(0).Value
                    Dim ResBuf As ResultBuffer = New ResultBuffer
                    Dim stat As Integer
                    For nCint As Integer = 1 To (args.Length - 1)
                        ResBuf.Add(New TypedValue(args(nCint).TypeCode, args(nCint).Value))
                    Next
                    stat = RFLTools.RFLToolsApplication.GetPutSym.acedPutSym(args(0).Value, ResBuf.UnmanagedObject)
                    If stat = PromptStatus.OK Then
                        Return ResBuf
                    Else
                        ed.WriteMessage("error putting symbol!" & vbLf)
                    End If
                End If
            End If
            Return Nothing
        End Function

        <LispFunction("RFLT_GetSymbol", "GetSymbolLocal")>
        Public Shared Function GetSym(ByVal myLispArgs As ResultBuffer) As ResultBuffer
            Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
            Dim ed As Editor = doc.Editor
            If myLispArgs Is Nothing Then
                ed.WriteMessage(vbLf & "no arguments!" & vbLf)
            Else
                Dim args As TypedValue() = myLispArgs.AsArray()
                If args.Length > 1 Then
                    ed.WriteMessage(vbLf & "too many arguments!" & vbLf)
                ElseIf args(0).TypeCode <> LispDataType.Text Then
                    ed.WriteMessage(vbLf & "argument is not a string!" & vbLf)
                Else
                    Dim ResBuf As ResultBuffer = RFLTools.RFLToolsApplication.GetPutSym.GetSymString(args(0).Value)
                    Return ResBuf
                End If
            End If
            Return Nothing
        End Function

        <LispFunction("RFLT_Invoke", "InvokeLocal")>
        Public Shared Function Invoke(ByVal myLispArgs As ResultBuffer) As ResultBuffer
            Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
            Dim ed As Editor = doc.Editor
            If myLispArgs Is Nothing Then
                ed.WriteMessage(vbLf & "no arguments!" & vbLf)
            Else
                Dim args As TypedValue() = myLispArgs.AsArray()
                If args(0).TypeCode <> LispDataType.Text Then
                    ed.WriteMessage(vbLf & "argument is not a string!" & vbLf)
                Else
                    Dim rb As IntPtr = IntPtr.Zero
                    Dim ResBuf As ResultBuffer = New ResultBuffer
                    Dim stat As Integer
                    stat = RFLTools.RFLToolsApplication.GetPutSym.acedInvoke(myLispArgs.UnmanagedObject, rb)
                    If stat = CType(PromptStatus.OK, Integer) AndAlso Not (rb = IntPtr.Zero) Then
                        Return CType(DisposableWrapper.Create(GetType(ResultBuffer), rb, True), ResultBuffer)
                    Else
                        ed.WriteMessage("error invoking routine!" & vbLf)
                    End If
                End If
            End If
            Return Nothing
        End Function

    End Class

End Namespace