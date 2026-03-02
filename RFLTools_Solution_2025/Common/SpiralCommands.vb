Imports System
Imports System.Math

Namespace RFLToolsApplication
    Public Class SpiralCommands
        Private Shared Tol As Double = 0.0000000001
        Private Shared TolFine As Double = 0.0000000000000001

        ' Factorial cache to avoid recalculating factorials in loops
        ' Pre-computed factorials up to 170! (max for Double type)
        Private Shared ReadOnly FactorialCache As New Dictionary(Of Integer, Double)()
        Private Shared ReadOnly CacheLock As New Object()

        ' Maximum factorial value that fits in a Double (170! ≈ 7.257e306)
        Private Const MaxFactorialIndex As Integer = 170

        ''' <summary>
        ''' Returns an alternating sign based on whether N is odd or even.
        ''' Used in Taylor series expansions for Fresnel integrals to generate alternating signs.
        ''' </summary>
        ''' <param name="N">The integer index (typically from series iteration)</param>
        ''' <returns>Returns -1 if N is odd, +1 if N is even</returns>
        ''' <remarks>
        ''' This function generates the pattern: ..., -1, 1, -1, 1, -1, 1, ...
        ''' Equivalent to (-1)^N for the alternating sign pattern in series.
        ''' </remarks>
        Public Shared Function OddEven(ByVal N As Integer) As Integer
            ' Use inline conditional for cleaner, more efficient code
            Return If((N And 1) = 1, -1, 1)
        End Function

        ''' <summary>
        ''' Calculates the Y-component of the Fresnel integral (Euler spiral/clothoid) using Taylor series.
        ''' This function computes: Y(θ) = ∫₀^θ sin(t²) dt
        ''' Used for transition curve calculations in highway and railroad design.
        ''' </summary>
        ''' <param name="Theta">The spiral parameter angle in radians</param>
        ''' <returns>The Y-coordinate offset value, or NaN for invalid inputs</returns>
        ''' <remarks>
        ''' Convergence uses absolute tolerance (Tol = 1e-10) with maximum 1000 iterations.
        ''' Returns NaN for negative Theta, 0 for Theta ≈ 0.
        ''' Series: Σ((-1)^(n+1) * (2θ)^(2n-1)) / (2^(2n-1) * (4n-1) * (2n-1)!)
        ''' </remarks>
        Public Shared Function SpiralFYR(ByVal Theta As Double) As Double
            ' Input validation (non-breaking, backward compatible)
            If Theta < 0.0 Then
                Return Double.NaN
            End If

            If Math.Abs(Theta) < TolFine Then
                Return 0.0
            End If

            ' Initialize variables with descriptive names
            Dim TwoTheta As Double = 2.0 * Theta
            Dim CurrentSum As Double = 0.0
            Dim PreviousSum As Double = -1.0  ' Initialize to ensure first iteration runs
            Dim N As Integer = 1

            ' Incremental power tracking for performance (avoids repeated Pow() calls)
            Dim TwoThetaPower As Double = TwoTheta  ' Starts at TwoTheta^1 = TwoTheta^(2*1-1)
            Dim TwoPower As Double = 2.0           ' Starts at 2^1

            ' Taylor series expansion with absolute tolerance convergence
            While (Math.Abs(PreviousSum - CurrentSum) > Tol) AndAlso (N < 1000)
                PreviousSum = CurrentSum

                ' Calculate term components
                Dim Exponent As Integer = CInt((2.0 * N) - 1.0)
                Dim Sign As Double = OddEven(N + 1)

                ' Use incremental powers for efficiency
                Dim Numerator As Double = Sign * TwoThetaPower
                Dim Denominator As Double = TwoPower * ((4.0 * N) - 1.0) * SpiralFact(Exponent)

                ' Check for potential overflow/underflow
                If Double.IsInfinity(Denominator) OrElse Denominator = 0.0 Then
                    Exit While  ' Return best estimate
                End If

                CurrentSum = CurrentSum + (Numerator / Denominator)

                ' Update incremental powers for next iteration
                ' TwoTheta^(2n-1) -> TwoTheta^(2(n+1)-1) = TwoTheta^(2n+1) = TwoTheta^(2n-1) * TwoTheta^2
                TwoThetaPower = TwoThetaPower * TwoTheta * TwoTheta
                ' 2^(2n-1) -> 2^(2(n+1)-1) = 2^(2n+1) = 2^(2n-1) * 4
                TwoPower = TwoPower * 4.0

                N = N + 1
            End While

            ' Apply final scaling and return result
            Return CurrentSum * TwoTheta
        End Function

        ''' <summary>
        ''' Calculates the X-component of the Fresnel integral (Euler spiral/clothoid) using Taylor series.
        ''' This function computes: X(θ) = ∫₀^θ cos(t²) dt
        ''' Used for transition curve calculations in highway and railroad design.
        ''' </summary>
        ''' <param name="Theta">The spiral parameter angle in radians</param>
        ''' <returns>The X-coordinate offset value, or NaN for invalid inputs</returns>
        ''' <remarks>
        ''' Convergence uses absolute tolerance (Tol = 1e-10) with maximum 1000 iterations.
        ''' Returns NaN for negative Theta, 0 for Theta ≈ 0.
        ''' Series: Σ((-1)^(n+1) * (2θ)^(2(n-1))) / (2^(2(n-1)) * (4n-3) * (2(n-1))!)
        ''' </remarks>
        Public Shared Function SpiralFXR(ByVal Theta As Double) As Double
            ' Input validation (non-breaking, backward compatible)
            If Theta < 0.0 Then
                Return Double.NaN
            End If

            If Math.Abs(Theta) < TolFine Then
                Return 0.0
            End If

            ' Initialize variables with descriptive names
            Dim TwoTheta As Double = 2.0 * Theta
            Dim CurrentSum As Double = 0.0
            Dim PreviousSum As Double = -1.0  ' Initialize to ensure first iteration runs
            Dim N As Integer = 1

            ' Incremental power tracking for performance (avoids repeated Pow() calls)
            ' Note: For FXR, the exponent is 2(N-1), so we start at TwoTheta^0 = 1.0
            Dim TwoThetaPower As Double = 1.0  ' Starts at TwoTheta^0 = TwoTheta^(2*(1-1))
            Dim TwoPower As Double = 1.0      ' Starts at 2^0

            ' Taylor series expansion with absolute tolerance convergence
            While (Math.Abs(PreviousSum - CurrentSum) > Tol) AndAlso (N < 1000)
                PreviousSum = CurrentSum

                ' Calculate term components
                Dim Exponent As Integer = CInt(2.0 * (N - 1.0))
                Dim Sign As Double = OddEven(N + 1)

                ' Use incremental powers for efficiency
                Dim Numerator As Double = Sign * TwoThetaPower
                Dim Denominator As Double = TwoPower * ((4.0 * N) - 3.0) * SpiralFact(Exponent)

                ' Check for potential overflow/underflow
                If Double.IsInfinity(Denominator) OrElse Denominator = 0.0 Then
                    Exit While  ' Return best estimate
                End If

                CurrentSum = CurrentSum + (Numerator / Denominator)

                ' Update incremental powers for next iteration
                ' TwoTheta^(2(n-1)) -> TwoTheta^(2n) = TwoTheta^(2(n-1)) * TwoTheta^2
                TwoThetaPower = TwoThetaPower * TwoTheta * TwoTheta
                ' 2^(2(n-1)) -> 2^(2n) = 2^(2(n-1)) * 4
                TwoPower = TwoPower * 4.0

                N = N + 1
            End While

            ' Apply final scaling and return result
            Return CurrentSum * TwoTheta
        End Function

        ''' <summary>
        ''' Calculates factorial of an integer value with caching for performance.
        ''' Uses memoization to avoid recalculating factorials in iterative loops.
        ''' </summary>
        ''' <param name="N">The integer value to calculate factorial for (must be >= 0)</param>
        ''' <returns>The factorial value as a Double</returns>
        ''' <remarks>
        ''' Maximum supported value is 170 due to Double precision limits.
        ''' Values beyond 170! will cause overflow (Double.PositiveInfinity).
        ''' Thread-safe implementation using lock on cache access.
        ''' </remarks>
        Private Shared Function SpiralFact(ByVal N As Integer) As Double
            ' Validate input
            If N < 0 Then
                Throw New ArgumentException("Factorial is not defined for negative numbers.", NameOf(N))
            End If

            ' Handle base cases
            If N = 0 OrElse N = 1 Then
                Return 1.0
            End If

            ' Check for overflow potential
            If N > MaxFactorialIndex Then
                Throw New ArgumentOutOfRangeException(NameOf(N),
                    String.Format("Factorial of {0} exceeds Double precision limit (max: {1})", N, MaxFactorialIndex))
            End If

            ' Check cache first (thread-safe)
            Dim cachedValue As Double
            If FactorialCache.TryGetValue(N, cachedValue) Then
                Return cachedValue
            End If

            ' Calculate factorial iteratively
            Dim F As Double = 1.0
            Dim i As Integer = 2
            While (i <= N)
                F = F * i

                ' Early overflow detection
                If Double.IsInfinity(F) Then
                    Throw New OverflowException(String.Format("Factorial calculation overflowed at {0}!", i))
                End If

                i = i + 1
            End While

            ' Cache the result (thread-safe)
            SyncLock CacheLock
                If Not FactorialCache.ContainsKey(N) Then
                    FactorialCache(N) = F
                End If
            End SyncLock

            Return F
        End Function

        ''' <summary>
        ''' Calculates factorial for a Double value.
        ''' Note: This only works correctly for integer values stored as Double.
        ''' For true non-integer factorials, use the Gamma function: Gamma(N+1) = N!
        ''' </summary>
        ''' <param name="N">The double value (should be an integer value)</param>
        ''' <returns>The factorial value as a Double</returns>
        ''' <remarks>
        ''' This overload delegates to the Integer version for better performance.
        ''' Non-integer values will be truncated, which may not be mathematically correct.
        ''' Consider using Math.Gamma(N+1) for true non-integer factorial calculations.
        ''' </remarks>
        Private Shared Function SpiralFact(ByVal N As Double) As Double
            ' Validate input
            If N < 0.0 Then
                Throw New ArgumentException("Factorial is not defined for negative numbers.", NameOf(N))
            End If

            ' Handle base cases
            If N < TolFine Then ' Effectively zero
                Return 1.0
            End If

            ' Check if N is effectively an integer
            Dim NInt As Integer = CInt(Math.Round(N))
            If Math.Abs(N - CDbl(NInt)) < TolFine Then
                ' Use the more efficient integer version with caching
                Return SpiralFact(NInt)
            End If

            ' For non-integer values, calculate iteratively
            ' Note: This decrements by 1.0, so it only works for integer-valued doubles
            ' For true non-integer factorials, Gamma(N+1) should be used instead
            Dim F As Double = 1.0
            Dim NTemp As Double = N
            While (NTemp > 1.0)
                F = F * NTemp

                ' Early overflow detection
                If Double.IsInfinity(F) Then
                    Throw New OverflowException(String.Format("Factorial calculation overflowed at {0}!", NTemp))
                End If

                NTemp = NTemp - 1.0
            End While

            Return F
        End Function

        ''' <summary>
        ''' Calculates the P-offset (perpendicular offset) of a spiral curve from the tangent.
        ''' This is the lateral shift at the end of the spiral transition curve.
        ''' Used in highway and railroad horizontal curve design (AASHTO standards).
        ''' </summary>
        ''' <param name="R">The radius of the circular curve (must be > 0)</param>
        ''' <param name="LS">The length of the spiral (must be >= 0)</param>
        ''' <returns>The P-offset value in the same units as R and LS, or NaN for invalid inputs</returns>
        ''' <remarks>
        ''' Formula: P = R × (Y(θ) - (1 - cos(θ))) where θ = LS / (2R)
        ''' The P-offset represents the perpendicular distance from the tangent to the circular curve.
        ''' Returns NaN if R <= 0 or LS < 0 (backward compatible error handling).
        ''' </remarks>
        Public Shared Function SpiralP(ByVal R As Double, ByVal LS As Double) As Double
            ' Input validation (non-breaking, backward compatible)
            If R <= 0.0 OrElse LS < 0.0 Then
                Return Double.NaN
            End If

            ' Handle zero-length spiral (no offset)
            If Math.Abs(LS) < TolFine Then
                Return 0.0
            End If

            ' Calculate spiral parameter angle
            Dim Theta As Double = LS / (2.0 * R)

            ' Calculate P-offset using Fresnel integral Y-component
            Return R * (SpiralFYR(Theta) - (1.0 - Cos(Theta)))
        End Function

        ''' <summary>
        ''' Calculates the P-offset ratio (P/R) for a spiral curve given the spiral parameter angle.
        ''' This is equivalent to SpiralP with R=1, representing the normalized P-offset.
        ''' </summary>
        ''' <param name="Theta">The spiral parameter angle in radians (θ = LS / (2R))</param>
        ''' <returns>The normalized P-offset ratio (dimensionless), or NaN for invalid inputs</returns>
        ''' <remarks>
        ''' Formula: P/R = Y(θ) - (1 - cos(θ))
        ''' To get actual P-offset: P = R × SpiralPR(θ)
        ''' Delegates input validation to SpiralFYR function.
        ''' </remarks>
        Public Shared Function SpiralPR(ByVal Theta As Double) As Double
            ' Calculate normalized P-offset (delegates validation to SpiralFYR)
            Return SpiralFYR(Theta) - (1.0 - Cos(Theta))
        End Function

        ''' <summary>
        ''' Calculates the K-offset (tangential offset) of a spiral curve along the tangent.
        ''' This is the longitudinal shift at the end of the spiral transition curve.
        ''' Used in highway and railroad horizontal curve design (AASHTO standards).
        ''' </summary>
        ''' <param name="R">The radius of the circular curve (must be > 0)</param>
        ''' <param name="LS">The length of the spiral (must be >= 0)</param>
        ''' <returns>The K-offset value in the same units as R and LS, or NaN for invalid inputs</returns>
        ''' <remarks>
        ''' Formula: K = R × (X(θ) - sin(θ)) where θ = LS / (2R)
        ''' The K-offset represents the tangential distance along the tangent line.
        ''' Returns NaN if R <= 0 or LS < 0 (backward compatible error handling).
        ''' </remarks>
        Public Shared Function SpiralK(ByVal R As Double, ByVal LS As Double) As Double
            ' Input validation (non-breaking, backward compatible)
            If R <= 0.0 OrElse LS < 0.0 Then
                Return Double.NaN
            End If

            ' Handle zero-length spiral (no offset)
            If Math.Abs(LS) < TolFine Then
                Return 0.0
            End If

            ' Calculate spiral parameter angle
            Dim Theta As Double = LS / (2.0 * R)

            ' Calculate K-offset using Fresnel integral X-component
            Return R * (SpiralFXR(Theta) - Sin(Theta))
        End Function

        ''' <summary>
        ''' Calculates the K-offset ratio (K/R) for a spiral curve given the spiral parameter angle.
        ''' This is equivalent to SpiralK with R=1, representing the normalized K-offset.
        ''' </summary>
        ''' <param name="Theta">The spiral parameter angle in radians (θ = LS / (2R))</param>
        ''' <returns>The normalized K-offset ratio (dimensionless), or NaN for invalid inputs</returns>
        ''' <remarks>
        ''' Formula: K/R = X(θ) - sin(θ)
        ''' To get actual K-offset: K = R × SpiralKR(θ)
        ''' Delegates input validation to SpiralFXR function.
        ''' </remarks>
        Public Shared Function SpiralKR(ByVal Theta As Double) As Double
            ' Calculate normalized K-offset (delegates validation to SpiralFXR)
            Return SpiralFXR(Theta) - Sin(Theta)
        End Function

    End Class

End Namespace
