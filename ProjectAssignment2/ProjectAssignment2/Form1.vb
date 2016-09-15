Imports System.Math
Imports System
Imports System.Collections
Public Class Form1

    'Class Variables
    Dim txtPopSize As Integer = 200 'this specifies the population size that is this population has 200 chromosomes
    Dim ValidFolding As Integer
    Public population(200) As genotype         ' assume this is 1st population or, pop1
    Public newpopulation(200) As genotype      ' assume this is new  population or, pop2
    Dim txtProteinStructure As String = "hphpphhphpphphhpphph" 'this is the input protein sequnce given in the assignment here you can give any protein sequnce from the assignment input file.
    Dim txtProteinLength As Integer 'stores the length for the above input protein sequence
    Dim HPModel As Integer() 'this array is used to store all the hydrophobic positions(i.e indexes of hydropohobic positions) from the input protein sequence
    Dim HPCount As Integer 'stores the number of hydrophobic occurrences present in the input protein sequence
    Dim eliteRate As Decimal = 0.05 'specifies eliterate
    Dim crossOverRate As Decimal = 0.8 'specifies crossover rate
    Dim mutationRate As Decimal = 0.5 'specifies mutationrate
    Dim CurrentPosNewPopulation As Integer 'keep track of the index in the new population
    Dim mutationPositionInNewPopulation As Integer 'this variable specifies index in the new population(i.e once you select the chromosome then perform mutation you have to put in the new population so this variable specify index in the new population)
    Dim totalFitness As Integer = 0
    Dim Generation As Integer = 0 'keeps the track of the generation


    'Genotype which represent coordinates for each chromosome in the population and it's corresponding fitness
    Public Class genotype
        Implements IComparable
        Public Fitness As Integer
        Public X(64) As Integer
        Public Y(64) As Integer
        'this function is used for sorting when you implement IComparable interface you will get this abstract method and here I provided defintion for this abstract method to sort chromosomes based on their fitness in ascending order
        Public Function CompareTo(ByVal gene As Object) As Integer _
        Implements IComparable.CompareTo
            If CType(gene, genotype).Fitness < Me.Fitness Then
                Return 1
            ElseIf CType(gene, genotype).Fitness = Me.Fitness Then
                Return 0
            ElseIf CType(gene, genotype).Fitness > Me.Fitness Then
                Return -1
            End If
            Return Nothing 'I am placing this to avoid warning 
        End Function
    End Class

    Function Initialization()
        Dim i As Integer
        For i = 1 To txtPopSize
            ValidFolding = 0
            RandomOrientation(i)

            While (ValidFolding = 0)
                RandomOrientation(i)
            End While
            population(i).Fitness = ComputeFitness(i)
            totalFitness = totalFitness + population(i).Fitness
        Next i
        Return Nothing 'I am placing this to avoid warning 
    End Function

    'this method iterates through every character in the input protein sequence and if it find the hydrophoibc it stores it correspoing index into an array.
    Function GetHydroPhobicPositions()
        Dim hIndex As Integer = 1
        txtProteinLength = txtProteinStructure.Length
        HPModel = New Integer(txtProteinLength) {}
        HPCount = 0
        Dim hOccurence As Char() = txtProteinStructure.ToCharArray()
        For index = 1 To txtProteinLength
            If (hOccurence(index - 1) = "h") Then
                HPModel(hIndex) = index
                hIndex = hIndex + 1
                HPCount = HPCount + 1
            End If
        Next index
        Return Nothing 'I am placing this to avoid warning 
    End Function

    'this logic is understood from your paper "Fast Computation of the Fitness Function for Protein Folding Prediction in a 2D Hydrophobic-Hydrophilic Model"
    Function ComputeFitness(n As Long) As Integer
        Dim isSequential As Integer
        Dim Fitness As Integer = 0
        Dim latticeDistance As Integer
        For i = 1 To HPCount - 1
            For j = i + 1 To HPCount
                isSequential = (Abs(HPModel(i) - HPModel(j))) '/*Not Sequential */
                If (isSequential <> 1) Then
                    latticeDistance = Abs(population(n).X(HPModel(i)) - population(n).X(HPModel(j))) + Abs(population(n).Y(HPModel(i)) - population(n).Y(HPModel(j)))
                    If (latticeDistance = 1) Then
                        Fitness = Fitness - 1
                    End If
                End If
            Next j
        Next i
        Return Fitness
    End Function

    Function RandomOrientation(m As Long)

        Dim PreviousDirection, PresentDirection, i, temp1, temp2, temp3, X, Y, j, Flag, Step2 As Integer
        Dim a(4), Ax(4), Ay(4) As Integer

        population(m) = New genotype()
        ValidFolding = 1
        population(m).X(1) = 0
        population(m).Y(1) = 0
        population(m).X(2) = 1
        population(m).Y(2) = 0
        PreviousDirection = 1

        For i = 3 To txtProteinLength

            Select Case PreviousDirection
                Case 1
                    a(1) = 1
                    Ax(1) = 1
                    Ay(1) = 0
                    a(2) = 3
                    Ax(2) = 0
                    Ay(2) = 1
                    a(3) = 4
                    Ax(3) = 0
                    Ay(3) = -1
                Case 2
                    a(1) = 2
                    Ax(1) = -1
                    Ay(1) = 0
                    a(2) = 3
                    Ax(2) = 0
                    Ay(2) = 1
                    a(3) = 4
                    Ax(3) = 0
                    Ay(3) = -1
                Case 3
                    a(1) = 1
                    Ax(1) = 1
                    Ay(1) = 0
                    a(2) = 2
                    Ax(2) = -1
                    Ay(2) = 0
                    a(3) = 3
                    Ax(3) = 0
                    Ay(3) = 1
                Case 4
                    a(1) = 1
                    Ax(1) = 1
                    Ay(1) = 0
                    a(2) = 2
                    Ax(2) = -1
                    Ay(2) = 0
                    a(3) = 4
                    Ax(3) = 0
                    Ay(3) = -1
            End Select
            Randomize()
            temp1 = Int(3 * Rnd() + 1)
            PresentDirection = temp1
            temp2 = 0
            temp3 = 0
            X = population(m).X(i - 1) + Ax(temp1)
            Y = population(m).Y(i - 1) + Ay(temp1)
            Flag = 0

            For j = 1 To i - 1
                If (X = population(m).X(j) And Y = population(m).Y(j)) Then
                    Flag = 1
                    GoTo MyJump1
                End If
            Next j

MyJump1:
            If (Flag = 1) Then
                Flag = 0
                Step2 = 6 - temp1
                Select Case Step2
                    Case 3
                        Randomize()
                        If Int(Rnd() * 2 + 1) = 1 Then
                            temp2 = 1
                        Else
                            temp2 = 2
                        End If
                    Case 4
                        Randomize()
                        If Int(Rnd() * 2 + 1) = 1 Then
                            temp2 = 1
                        Else
                            temp2 = 3
                        End If
                    Case 5
                        Randomize()
                        If Int(Rnd() * 2 + 1) = 1 Then
                            temp2 = 2
                        Else
                            temp2 = 3
                        End If
                End Select

                PresentDirection = temp2
                temp3 = 6 - (temp1 + temp2)
                X = population(m).X(i - 1) + Ax(temp2)
                Y = population(m).Y(i - 1) + Ay(temp2)

                For j = 1 To i - 1
                    If (X = population(m).X(j) And Y = population(m).Y(j)) Then
                        Flag = 1
                        GoTo MyJump2
                    End If
                Next j
MyJump2:
                If (Flag = 1) Then
                    Flag = 0
                    PresentDirection = temp3
                    X = population(m).X(i - 1) + Ax(temp3)
                    Y = population(m).Y(i - 1) + Ay(temp3)
                    For j = 1 To i - 1
                        If (X = population(m).X(j) And Y = population(m).Y(j)) Then
                            Flag = 1
                            ValidFolding = 0
                            'GoTo MyJump3

                        End If
                    Next j
                End If
            End If
            PreviousDirection = a(PresentDirection)
            population(m).X(i) = population(m).X(i - 1) + Ax(PresentDirection)
            population(m).Y(i) = population(m).Y(i - 1) + Ay(PresentDirection)
        Next i
MyJump3:
        Return Nothing 'I am placing this to avoid warning 
    End Function

    'this is my algorithm entry point where the following genetic algorithm steps are followed in the sequence.
    Private Sub Form1_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load

        'Finding the HydroPhobic Positions in the given Protein Structrure
        GetHydroPhobicPositions()

        'Generating the Population 
        Initialization()

        'Sorting the Population based on the Fitness of each gene
        Array.Sort(population)

        'Build Elite Calculation
        CalculateElitePopulation()

        'CrossOver Population
        BuildCrossOverPopulation()

        'Fill Remaining Population
        FillRemainingNewPopulation()

        'Mutation
        PerformMutation()

        'ComupteNextGeneration
        ComupteNextGeneration()
    End Sub

    'this method takes first 0.05% of chromosomes(best fitness ones afters sorting based on their fitness) from the old population and places it into the new population
    Private Sub CalculateElitePopulation()
        newpopulation = New genotype(txtPopSize) {}
        Dim elitePopulation As Integer = eliteRate * txtPopSize
        'this is predifined method of array class which accept following paramenters in order (1)source array (2) source index (3) destination array (4) desitnation index (5) length
        Array.ConstrainedCopy(population, 1, newpopulation, 1, elitePopulation)
    End Sub

    'this method is used for performing crossover on the chromosomes in the  population
    Private Sub BuildCrossOverPopulation()
        ' this specifies the start index in the new population  for the chromosome to be placed (after performing crossover) in my case the start index is 11 because first
        '10 chromosome are occupied by elite ones as my eliteRate is 0.05%
        Dim crossOverStartIndex As Integer = eliteRate * txtPopSize + 1
        'this variable stores the last index of the chromsome from the crossover population  so in my case crossoverate is 0.8 when it is multiplied with population it gives 160 
        'as my first 10 chrosome belong to elite popluation the indexs from 11 to 170 in the new population belongs to the crossover population
        Dim crossOverPopulationCount As Integer = crossOverRate * txtPopSize + crossOverStartIndex - 1
        'this is arbiratry point in the selected chromsome for performing crossover
        Dim crossOverPoint As Integer
        Dim i, j As Integer
        Dim maxEndPoint As Integer = txtProteinLength - 3
        'this loop i am running 160 times as my crossover rate is 0.8% when multiplied with population it gives 160
        For sIndex = crossOverStartIndex To crossOverPopulationCount
            CurrentPosNewPopulation = sIndex
            'For performing crossover two chromsomes are required and we consider them as ith and jth chromosome and both of these chromosomes 
            'are selected by using Roulettewheel selection
            i = GetChromosomeIndvidualsUsingRoulettewheelSelection()
            'if the above method returns 0 we again call the above method because our population starts from 1st index as 0th index we have nothing
            While i = 0
                i = GetChromosomeIndvidualsUsingRoulettewheelSelection()
            End While
            'if the above method returns 0 we again call the above method because our population starts from 1st index as 0th index we have nothing
            j = GetChromosomeIndvidualsUsingRoulettewheelSelection()
            While j = 0
                j = GetChromosomeIndvidualsUsingRoulettewheelSelection()
            End While
            newpopulation(CurrentPosNewPopulation) = New genotype()
            Randomize()
            'crossover point we are selecting between 2 and 19 as these are valid points if it is 20 lenght protein sequnece
            crossOverPoint = (maxEndPoint * Rnd() + 2)
            Dim Success As Integer = CrossOver(i, j, crossOverPoint)
            'here if the success has  the value 1 then that means crossover is performed successfully and if it is having value 0 
            'that means crossover is failure there fore we are selecting again ith and jth chromosome
            While Success = 0
                i = GetChromosomeIndvidualsUsingRoulettewheelSelection()
                While i = 0
                    i = GetChromosomeIndvidualsUsingRoulettewheelSelection()
                End While
                j = GetChromosomeIndvidualsUsingRoulettewheelSelection()
                While j = 0
                    j = GetChromosomeIndvidualsUsingRoulettewheelSelection()
                End While
                Randomize()
                crossOverPoint = (maxEndPoint * Rnd() + 2)
                Success = CrossOver(i, j, crossOverPoint)
            End While
        Next sIndex
    End Sub

    'this method selects chromosome for crossover using  the roulettewheel selection logic
    Private Function GetChromosomeIndvidualsUsingRoulettewheelSelection() As Integer
        Randomize()
        'rnd is multiplied with the total fitness and stored in rndVar variable
        Dim rndVar As Integer = Rnd() * Math.Abs(totalFitness)
        Dim index As Integer
        For index = 1 To txtPopSize
            rndVar = rndVar - Math.Abs(population(index).Fitness)
            If (rndVar < 0) Then
                Return index - 1
            End If
        Next
        Return Nothing 'I am placing this to avoid warning 
    End Function

    Function CrossOver(i As Long, j As Long, n As Integer) As Long
        Dim PrevDirection, k, z, p As Long
        Dim temp1, temp2, temp3, Collision, dx, dy, Step2 As Long
        Dim id As Long
        Dim a(4), Ax(4), Ay(4) As Integer

        id = CurrentPosNewPopulation

        '/* Detect Previous Direction */
        If (population(i).X(n) = population(i).X(n - 1)) Then
            p = population(i).Y(n - 1) - population(i).Y(n)
            If (p = 1) Then
                PrevDirection = 3
            Else
                PrevDirection = 4
            End If

        Else
            p = population(i).X(n - 1) - population(i).X(n)
            If (p = 1) Then
                PrevDirection = 1
            Else
                PrevDirection = 2
            End If
        End If


        Select Case PrevDirection
            Case 1
                Ax(1) = -1
                Ay(1) = 0
                Ax(2) = 0
                Ay(2) = 1
                Ax(3) = 0
                Ay(3) = -1
            Case 2
                Ax(1) = 1
                Ay(1) = 0
                Ax(2) = 0
                Ay(2) = 1
                Ax(3) = 0
                Ay(3) = -1
            Case 3
                Ax(1) = 1
                Ay(1) = 0
                Ax(2) = -1
                Ay(2) = 0
                Ax(3) = 0
                Ay(3) = -1

            Case 4
                Ax(1) = 1
                Ay(1) = 0
                Ax(2) = -1
                Ay(2) = 0
                Ax(3) = 0
                Ay(3) = 1
        End Select

        Randomize()


        temp1 = Int(Rnd() * 3 + 1)

        newpopulation(id).X(n + 1) = population(i).X(n) + Ax(temp1)
        newpopulation(id).Y(n + 1) = population(i).Y(n) + Ay(temp1)
        Collision = 0

        dx = newpopulation(id).X(n + 1) - population(j).X(n + 1)
        dy = newpopulation(id).Y(n + 1) - population(j).Y(n + 1)

        For k = n + 1 To txtProteinLength
            newpopulation(id).X(k) = population(j).X(k) + dx

            newpopulation(id).Y(k) = population(j).Y(k) + dy

            For z = 1 To n
                If ((newpopulation(id).X(k) = population(i).X(z)) And (newpopulation(id).Y(k) = population(i).Y(z))) Then
                    Collision = 1
                    ' CrossoverInternalFailCount = CrossoverInternalFailCount + 1
                    'CrossoverCollisionCount = CrossoverCollisionCount + 1
                    GoTo MyOut1
                End If
            Next z
        Next k

MyOut1:
        If (Collision = 1) Then         '/* ======> Second try ==== */
            Collision = 0
            Step2 = 6 - temp1
            Select Case Step2
                Case 3
                    Randomize()
                    If Int(Rnd() * 2 + 1) = 1 Then
                        temp2 = 1
                    Else
                        temp2 = 2
                    End If

                Case 4
                    Randomize()
                    If Int(Rnd() * 2 + 1) = 1 Then
                        temp2 = 1
                    Else
                        temp2 = 3
                    End If

                Case 5
                    Randomize()
                    If Int(Rnd() * 2 + 1) = 1 Then
                        temp2 = 2
                    Else
                        temp2 = 3
                    End If
            End Select

            temp3 = 6 - (temp1 + temp2)
            newpopulation(id).X(n + 1) = population(i).X(n) + Ax(temp2)
            newpopulation(id).Y(n + 1) = population(i).Y(n) + Ay(temp2)
            dx = newpopulation(id).X(n + 1) - population(j).X(n + 1)
            dy = newpopulation(id).Y(n + 1) - population(j).Y(n + 1)

            For k = n + 1 To txtProteinLength

                newpopulation(id).X(k) = population(j).X(k) + dx
                newpopulation(id).Y(k) = population(j).Y(k) + dy

                For z = 1 To n
                    If ((newpopulation(id).X(k) = population(i).X(z)) And (newpopulation(id).Y(k) = population(i).Y(z))) Then
                        Collision = 1
                        'CrossoverCollisionCount = CrossoverCollisionCount + 1
                        'CrossoverInternalFailCount = CrossoverInternalFailCount + 1
                        GoTo MyOut2
                    End If
                Next z
            Next k

MyOut2:
            If (Collision = 1) Then
                Collision = 0
                newpopulation(id).X(n + 1) = population(i).X(n) + Ax(temp3)
                newpopulation(id).Y(n + 1) = population(i).Y(n) + Ay(temp3)
                dx = newpopulation(id).X(n + 1) - population(j).X(n + 1)
                dy = newpopulation(id).Y(n + 1) - population(j).Y(n + 1)
                For k = n + 1 To txtProteinLength
                    newpopulation(id).X(k) = population(j).X(k) + dx
                    newpopulation(id).Y(k) = population(j).Y(k) + dy
                    For z = 1 To n
                        If ((newpopulation(id).X(k) = population(i).X(z)) And (newpopulation(id).Y(k) = population(i).Y(z))) Then
                            Collision = 1
                            'CrossoverInternalFailCount = CrossoverInternalFailCount + 1
                            'CrossoverCollisionCount = CrossoverCollisionCount + 1
                            GoTo MyOut3
                        End If
                    Next z
                Next k
            End If '/* 3rd try if ends */
        End If '/* 2nd try if ends */

MyOut3:
        If Collision = 0 Then
            '   CrossoverSuccessCount = CrossoverSuccessCount + 1
            For k = 1 To n
                newpopulation(id).X(k) = population(i).X(k)
                newpopulation(id).Y(k) = population(i).Y(k)
            Next k

            CrossOver = 1

        Else
            'CrossOver = 0

            'CrossoverFailCount = CrossoverFailCount + 1

            'CrossOver = CalculateThePaths(i, n, id) ' if successful should return 1 else 0
            'If CrossOver = 0 Then
            '    ' CrossoverFailCountafterDFS = CrossoverFailCountafterDFS + 1
            'End If
        End If
        Return CrossOver
    End Function

    'Now we have 170(10 belongs to elite + 160 belongs to crossovers) out of 200 chromosomes in the new popluaiton reamaing 30 we are filling by just copying from the old population
    Private Sub FillRemainingNewPopulation()
        Try
            'this is dynamically calculated from the following below variables
            'in my case remainingNewPopulationStartIndex= 171 because 
            'eliteRate * txtPopSize gives me 10 
            'crossOverRate * txtPopSize gives me 160
            'so we have 170 chromosomes filled in new populatin and we have 30 more to fill this is done by this method
            Dim remainingNewPopulationStartIndex As Integer = eliteRate * txtPopSize + crossOverRate * txtPopSize + 1
            Array.ConstrainedCopy(population, remainingNewPopulationStartIndex, newpopulation, remainingNewPopulationStartIndex, txtPopSize - remainingNewPopulationStartIndex + 1)
        Catch ex As Exception
            MessageBox.Show(ex.Message)
        End Try

    End Sub


    Private Sub PerformMutation()
        'this gives the mutation population in my case mutation rate is 0.1 * 200 which gives me 20 population
        Dim mutationPopulation As Integer = mutationRate * txtPopSize
        Randomize()
        'this variable randomly picks the chromosome from the 200 new populaiton to be mutated
        Dim geneToBeMutated As Integer = 199 * Rnd() + 1
        Randomize()
        'this is arbitary point in the chromosome where muatation is peformed and it is between 2 and 19 if it is 20 length protein sequence
        Dim maxEndPoint As Integer = txtProteinLength - 3
        Dim mutationPoint As Integer = maxEndPoint * Rnd() + 2
        Try
            Randomize()
            'this variable tells about where to put the chromosome in the new population after mutated in my case it is between 11 and 200 and I am not disturbing elite population
            mutationPositionInNewPopulation = 189 * Rnd() + 11
            For index = 1 To mutationPopulation
                mutationPositionInNewPopulation = mutationPositionInNewPopulation
                Dim MutationStatus As Integer = Mutation(geneToBeMutated, mutationPoint)
                While MutationStatus = 0
                    geneToBeMutated = 199 * Rnd() + 1
                    mutationPoint = maxEndPoint * Rnd() + 2
                    MutationStatus = Mutation(geneToBeMutated, mutationPoint)
                End While
            Next
        Catch ex As Exception
            MessageBox.Show(geneToBeMutated + "geneToBeMutated" + mutationPoint + "mutationPoint" + mutationPositionInNewPopulation)
            'MessageBox.Show(ex.Message)
        End Try

    End Sub

    Function Mutation(i As Long, n As Integer) As Long
        Dim id As Long
        Dim a As Long
        Dim b As Long
        Dim A_Limit As Long
        Dim choice As Long
        Dim Collision As Long
        Dim k As Long
        Dim z As Long
        Dim p As Long
        Dim Ary(3) As Integer

        id = mutationPositionInNewPopulation

        ' possible rotations 90ß,180ß,270ß
        '           index       1   2    3
        '


        Ary(1) = 1
        Ary(2) = 2
        Ary(3) = 3
        A_Limit = 3

        a = population(i).X(n)          '/* (a, b) rotating point */
        b = population(i).Y(n)

        Do
            Collision = 0
            If (A_Limit > 1) Then
                Randomize()
                choice = Int(A_Limit * Rnd() + 1)
            Else
                choice = A_Limit
            End If


            p = Ary(choice)
            For k = choice To A_Limit - 1
                Ary(k) = Ary(k + 1)
            Next k

            A_Limit = A_Limit - 1

            For k = n + 1 To txtProteinLength
                Select Case p

                    Case 1
                        newpopulation(id).X(k) = a + b - population(i).Y(k)       '/* X' = (a+b)-Y  */
                        newpopulation(id).Y(k) = population(i).X(k) + b - a       '/* Y' = (X+b)-a  */
                    Case 2
                        newpopulation(id).X(k) = 2 * a - population(i).X(k)       '/* X' = (2a - X) */
                        newpopulation(id).Y(k) = 2 * b - population(i).Y(k)       '/* Y' = (2b-Y)   */
                    Case 3
                        newpopulation(id).X(k) = population(i).Y(k) + a - b       '/* X' =  Y+a-b   */
                        newpopulation(id).Y(k) = a + b - population(i).X(k)       '/* Y' =  (a+b)-X */
                End Select

                For z = 1 To n

                    If ((newpopulation(id).X(k) = population(i).X(z)) And (newpopulation(id).Y(k) = population(i).Y(z))) Then
                        Collision = 1
                        'MutationInternalFailCount = MutationInternalFailCount + 1
                        'MutationCollisionCount = MutationCollisionCount + 1
                        GoTo MyJump
                    End If
                Next z
            Next k

            If (Collision = 0) Then
                A_Limit = 0
            End If
MyJump:
        Loop Until A_Limit = 0

        If (Collision = 0) Then

            For k = 1 To n
                newpopulation(id).X(k) = population(i).X(k)
                newpopulation(id).Y(k) = population(i).Y(k)
            Next k


            Mutation = 1
        Else
            'MutationFailCount = MutationFailCount + 1
            Mutation = 0
        End If

    End Function


    Private Sub ComupteNextGeneration()
        totalFitness = 0
        Array.ConstrainedCopy(newpopulation, 1, population, 1, txtPopSize)
        For index = 1 To txtPopSize
            population(index).Fitness = 0
            population(index).Fitness = ComputeFitness(index)
            totalFitness = totalFitness + population(index).Fitness
        Next

        'Sorting the Population based on the Fitness of each gene
        Array.Sort(population)

        CalculateElitePopulation()

        'CrossOver Population
        BuildCrossOverPopulation()

        'Fill Remaining Population
        FillRemainingNewPopulation()

        'Mutation
        PerformMutation()

        Generation = Generation + 1
        'Here once I reach 2000 Generations I am exiting from the application and I am using Microsoft Chart Controls to display the structure.
        If Generation = 100 Then
            Label3.Text = Label3.Text & " " & population(1).Fitness
            Chart1.Series("Protein Structure").BorderWidth = 5
            Chart1.ChartAreas(0).AxisX.Interval = 1
            Chart1.ChartAreas(0).AxisY.Interval = 1
            For index = 1 To txtProteinStructure.Length
                Chart1.Series("Protein Structure").Points.AddXY(population(1).X(index), population(1).Y(index))
            Next
            For c = 0 To txtProteinLength
                'HydropPhobic Positions I am represeting in Green COlor
                If HPModel.Contains(c + 1) Then
                    Chart1.Series("Protein Structure").Points(c).MarkerStyle = DataVisualization.Charting.MarkerStyle.Circle
                    Chart1.Series("Protein Structure").Points(c).MarkerSize = 10
                    Chart1.Series("Protein Structure").Points(c).MarkerColor = Color.Green
                Else
                    'HydropPhobic Positions I am represeting in Red COlor
                    Chart1.Series("Protein Structure").Points(c).MarkerStyle = DataVisualization.Charting.MarkerStyle.Circle
                    Chart1.Series("Protein Structure").Points(c).MarkerSize = 10
                    Chart1.Series("Protein Structure").Points(c).MarkerColor = Color.Red
                End If
            Next
            Return
        End If

        'ComupteNextGeneration
        ComupteNextGeneration()
    End Sub
End Class
