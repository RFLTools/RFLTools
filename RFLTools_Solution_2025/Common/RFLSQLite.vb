Imports System
Imports System.Data.SQLite

Imports Autodesk.AutoCAD.Runtime
Imports Autodesk.AutoCAD.ApplicationServices
Imports Autodesk.AutoCAD.DatabaseServices
Imports Autodesk.AutoCAD.Geometry
Imports Autodesk.AutoCAD.EditorInput

Namespace RFLToolsApplication
    Public Class RFLSQLite

        Private Function Column(InLine As String, Col As Integer, Delim As String) As String
            Dim StringArray() As String = Strings.Split(InLine, Delim)
            If Col < 1 Or Col > StringArray.Length Then
                Column = ""
            Else
                Column = StringArray(Col - 1)
            End If
        End Function

        Public Function CreateDataBase(ByVal FullPath As String) As Boolean
            Try
                Dim Result As Boolean = False
                Dim ConnectionString As String = String.Format("Data Source = {0}", FullPath)
                If Not DuplicateFile(FullPath) Then
                    Dim CreateTable As String = "CREATE TABLE `points` (
	                                            `id` INTEGER PRIMARY KEY AUTOINCREMENT,
	                                            `x` REAL,
	                                            `y` REAL,
	                                            `z` REAL
                                                );"
                    Using SqlConn As New SQLiteConnection(ConnectionString)
                        Dim Cmd As New SQLiteCommand(CreateTable, SqlConn)
                        SqlConn.Open()
                        Cmd.ExecuteNonQuery()
                        SqlConn.Close()
                    End Using
                    Return True
                Else
                    Return False
                End If

            Catch ex As System.Exception
                Return False
            End Try
        End Function

        Public Function AddDataBasePoints(ByVal DbFullPath As String, ByVal PointsFullPath As String, ByVal NoLines As Int16) As Boolean
            Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
            Dim ed As Editor = doc.Editor
            Dim SqlString, InLine As String
            Dim StartTime, StopTime As TimeSpan


            Try
                Dim Result As Boolean = False
                Dim ConnectionString As String = String.Format("Data Source = {0}", DbFullPath)
                Dim SQLConn As New SQLiteConnection(ConnectionString)
                Dim SQLTrans As SQLiteTransaction
                Dim Cmd As New SQLiteCommand

                If DuplicateFile(DbFullPath) And DuplicateFile(PointsFullPath) Then
                    Dim C As Int32 = 0
                    Dim X, Y, Z As String
                    Dim objFileReader As New System.IO.StreamReader(PointsFullPath)

                    StartTime = Now.TimeOfDay

                    Do While objFileReader.Peek() <> -1 And NoLines > 0
                        InLine = objFileReader.ReadLine()
                        NoLines = NoLines - 1
                    Loop

                    'SQLConn = New SQLiteConnection(ConnectionString)
                    SQLConn.Open()

                    'Cmd = SQLConn.CreateCommand()

                    SQLTrans = SQLConn.BeginTransaction()
                    Cmd.Transaction = SQLTrans
                    Cmd.CommandTimeout = 99999

                    Do While objFileReader.Peek() <> -1
                        InLine = objFileReader.ReadLine()
                        X = Column(InLine, 1, " ")
                        Y = Column(InLine, 2, " ")
                        Z = Column(InLine, 3, " ")
                        If X <> "" And Y <> "" And Z <> "" Then
                            Cmd.CommandText = "INSERT INTO points (x, y, z) VALUES (" & X & ", " & Y & ", " & Z & ");"
                            Cmd.ExecuteNonQuery()
                            If C Mod 100000 = 0 Then
                                StopTime = Now.TimeOfDay - StartTime
                                ed.WriteMessage(String.Format("Point {0} : {1:00}:{2:00}:{3:00}", C, StopTime.Hours, StopTime.Minutes, StopTime.Seconds) & vbLf)
                            End If
                            C = C + 1
                        End If
                    Loop

                    SQLTrans.Commit()

                    SQLConn.Close()

                    StopTime = Now.TimeOfDay - StartTime
                    ed.WriteMessage(String.Format("Point {0} : {1:00}:{2:00}:{3:00}", C, StopTime.Hours, StopTime.Minutes, StopTime.Seconds) & vbLf)

                    Return True
                Else
                    Return False
                End If

            Catch ex As System.Exception
                ed.WriteMessage(ex.ToString & vbLf)
                Return False
            End Try
        End Function

        Private Function DuplicateFile(FullPath As String) As Boolean
            Return System.IO.File.Exists(FullPath)
        End Function

        Public Function GetPlistDb(ByRef P1 As DataTypes.Point3d, ByRef P2 As DataTypes.Point3d, ByRef P3 As DataTypes.Point3d, ByRef P4 As DataTypes.Point3d, ByVal DSta As Double, ByVal Swath As Double, ByVal DbFullPath As String, ByRef PListRb As ResultBuffer) As Boolean
            Dim doc As Document = Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument
            Dim ed As Editor = doc.Editor
            Dim SqlString As String
            Dim StartTime, StopTime As DateTime
            Dim ElapsedTime As TimeSpan
            Dim D1 As Double = P1.DistanceTo2D(P2)
            Dim D2 As Double = P3.DistanceTo2D(P4)

            Try
                SqlString = "SELECT * FROM points WHERE " &
                            "abs(" &
                            (P2.Y - P1.Y).ToString("F8") &
                            " * X - " &
                            (P2.X - P1.X).ToString("F8") &
                            " * Y + " &
                            ((P2.X * P1.Y) - (P2.Y * P1.X)).ToString("F8") &
                            " ) / " &
                            D1.ToString("F8") &
                            " <= " &
                            DSta.ToString("F8") &
                            " AND " &
                            "abs(" &
                            (P4.Y - P3.Y).ToString("F8") &
                            " * X - " &
                            (P4.X - P3.X).ToString("F8") &
                            " * Y + " &
                            ((P4.X * P3.Y) - (P4.Y * P3.X)).ToString("F8") &
                            " ) / " &
                            D2.ToString("F8") &
                            " <= " &
                            (Swath / 2.0).ToString("F8") &
                            ";"

                If DuplicateFile(DbFullPath) Then
                    Dim Result As Boolean = False
                    Dim ConnectionString As String = String.Format("Data Source = {0}", DbFullPath)
                    Dim SQLConn As New SQLiteConnection(ConnectionString)
                    Dim DataReader As SQLiteDataReader
                    Dim Cmd As SQLiteCommand = SQLConn.CreateCommand()
                    Dim X, Y, Z As Double
                    StartTime = Now

                    SQLConn.Open()
                    Cmd.CommandText = SqlString
                    DataReader = Cmd.ExecuteReader()

                    PListRb.Add(New TypedValue(LispDataType.ListBegin))

                    While (DataReader.Read())
                        X = DataReader.GetDouble(1)
                        Y = DataReader.GetDouble(2)
                        Z = DataReader.GetDouble(3)
                        PListRb.Add(New TypedValue(LispDataType.ListBegin))
                        PListRb.Add(New TypedValue(CInt(LispDataType.Double), X))
                        PListRb.Add(New TypedValue(CInt(LispDataType.Double), Y))
                        PListRb.Add(New TypedValue(CInt(LispDataType.Double), Z))
                        PListRb.Add(New TypedValue(LispDataType.ListEnd))
                    End While

                    PListRb.Add(New TypedValue(LispDataType.ListEnd))

                    StopTime = Now
                    ElapsedTime = StopTime.Subtract(StartTime)

                    ed.WriteMessage(String.Format("Search time = " & ElapsedTime.TotalSeconds.ToString("0.0000") & " seconds" & vbLf))
                End If
            Catch ex As System.Exception
                ed.WriteMessage(ex.ToString & vbLf)
                Return False
            End Try
            Return True
        End Function

    End Class
End Namespace