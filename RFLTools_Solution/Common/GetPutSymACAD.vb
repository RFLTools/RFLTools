'System
Imports System.Runtime.InteropServices 'for DllImport()

'Autodesk
Imports Autodesk.AutoCAD.Runtime
Imports Autodesk.AutoCAD.DatabaseServices
Imports Autodesk.AutoCAD.EditorInput
Imports Autodesk.AutoCAD.ApplicationServices

Namespace RFLToolsApplication

    Public Class GetPutSym
        ' Note acCore.dll replaces acad.exe from versions previous to 2013 in all DllImports

        ' Use P/Invoke for acedGetSym
        <DllImport("acCore.dll", CharSet:=CharSet.Unicode, _
               CallingConvention:=CallingConvention.Cdecl, EntryPoint:="acedGetSym")> _
        Shared Function acedGetSym(ByVal args As String, <Out()> ByRef result As IntPtr) As Integer
        End Function

        ' Use P/Invoke for acedPutSym
        <DllImport("acCore.dll", CharSet:=CharSet.Unicode, CallingConvention:=CallingConvention.Cdecl, EntryPoint:="acedPutSym")> _
        Shared Function acedPutSym(ByVal args As String, ByVal result As IntPtr) As Integer
        End Function

        'use P/Invoke for acedInvoke
        <DllImport("acCore.dll", CallingConvention:=CallingConvention.Cdecl, EntryPoint:="acedInvoke")> _
        Shared Function acedInvoke(ByVal args As IntPtr, ByRef result As IntPtr) As Integer
        End Function

        Public Shared Function GetSymString(ByVal varname As String) As ResultBuffer ', ByRef stat As Integer) As ResultBuffer
            ' IntPtr is an integer (used as a pointer for .NET) initialize to zero
            Dim rb As IntPtr = IntPtr.Zero
            ' use the function that was p/Invoked, pass in the name that was 
            ' provided by the user and the IntPtr
            Dim stat As Integer = 0
            stat = acedGetSym(varname, rb)

            ' If stat is ok and the IntPtr is not zero create a ResultBuffer and Return it
            If stat = CType(PromptStatus.OK, Integer) AndAlso Not (rb = IntPtr.Zero) Then
                Return CType(DisposableWrapper.Create(GetType(ResultBuffer), rb, True), ResultBuffer)
            End If

            ' Getting the symbol failed if we reach this return statement
            Return Nothing
        End Function

    End Class

End Namespace