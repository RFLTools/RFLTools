Imports System
Imports System.Math

Namespace RFLToolsApplication
    Public Class SpiralCommands
        Private Shared Tol As Double = 0.0000000001
        Private Shared TolFine As Double = 0.0000000000000001

        Public Shared Function OddEven(ByVal N As Integer) As Integer
            Dim Remainder As Integer

            Remainder = N Mod 2
            If Remainder = 1 Then
                OddEven = -1
            Else
                OddEven = 1
            End If
        End Function

        Public Shared Function SpiralFYR(ByVal Theta As Double) As Double
            Dim Ar2, Denominator, Numerator, Sum, Sum2 As Double
            Dim N As Integer

            Sum = -1.0
            Sum2 = 0.0
            Ar2 = 2.0 * Theta
            N = 1
            'While (Abs(Sum - Sum2) > Tol)
            While ((Sum <> Sum2) And (N < 1000))
                Sum = Sum2
                Numerator = OddEven(N + 1) * Pow(Ar2, ((2.0 * N) - 1.0))
                Denominator = Pow(2.0, ((2.0 * N) - 1.0)) * ((4.0 * N) - 1.0) * SpiralFact((2.0 * N) - 1.0)
                Sum2 = Sum2 + (Numerator / Denominator)
                N = N + 1
            End While
            Sum = Sum * Ar2
            SpiralFYR = Sum
        End Function

        Public Shared Function SpiralFXR(ByVal Theta As Double) As Double
            Dim Ar2, Denominator, Numerator, Sum, Sum2 As Double
            Dim N As Integer

            Sum = -1.0
            Sum2 = 0.0
            Ar2 = 2.0 * Theta
            N = 1
            'While (Abs(Sum - Sum2) > Tol)
            While ((Sum <> Sum2) And (N < 1000))
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
            SpiralFXR = Sum
        End Function

        Private Shared Function SpiralFact(ByVal N As Integer) As Double
            Dim F As Double
            F = 1.0
            While (N > 0)
                F = F * N
                N = N - 1
            End While
            SpiralFact = F
        End Function

        Private Shared Function SpiralFact(ByVal N As Double) As Double
            Dim F As Double
            F = 1.0
            While (N > 0.0)
                F = F * N
                N = N - 1.0
            End While
            SpiralFact = F
        End Function

        Public Shared Function SpiralP(ByVal R As Double, ByVal LS As Double) As Double
            Dim Theta As Double

            Theta = LS / (2.0 * R)
            SpiralP = R * (SpiralFYR(Theta) - (1.0 - Cos(Theta)))
        End Function

        Public Shared Function SpiralPR(ByVal Theta As Double) As Double
            SpiralPR = SpiralFYR(Theta) - (1.0 - Cos(Theta))
        End Function

        Public Shared Function SpiralK(ByVal R As Double, ByVal LS As Double) As Double
            Dim Theta As Double

            Theta = LS / (2.0 * R)
            SpiralK = R * (SpiralFXR(Theta) - Sin(Theta))
        End Function

        Public Shared Function SpiralKR(ByVal Theta As Double) As Double
            SpiralKR = SpiralFXR(Theta) - Sin(Theta)
        End Function

    End Class

End Namespace
