Public Class Skyscraper
    Dim n As Integer = 4
    Dim cells(n, n) As Button
    Dim board(n, n) As Integer
    Dim R(n, n), C(n, n), start As Boolean
    Dim countRowLeft(n), countRowRight(n), countColTop(n), countColBottom(n) As Button
    Dim countRowL(n), countRowR(n), countColT(n), countColB(n) As Integer
    Dim emptiesCnt As Integer

    Dim success As Boolean
    Private Function RandomizeBoardState(steps As Integer, n As Integer, board(,) As Integer, R(,) As Boolean, C(,) As Boolean)
        If steps = n ^ 2 Then
            success = True
        Else
            Dim choices As List(Of Integer) = New List(Of Integer)
            Dim move As Integer = 0
            For i = 1 To n
                choices.Add(i)
            Next
            success = False
            Do While move < n And Not success
                Dim rn As Random = New Random()
                Dim index As Integer = rn.Next(choices.Count), num As Integer = choices(index)
                Dim row As Integer = steps \ n, col As Integer = steps Mod n
                choices.RemoveAt(index)
                If R(row, num - 1) And C(col, num - 1) Then
                    board(row, col) = num
                    R(row, num - 1) = False
                    C(col, num - 1) = False
                    If Not RandomizeBoardState(steps + 1, n, board, R, C) Then
                        board(row, col) = 0
                        R(row, num - 1) = True
                        C(col, num - 1) = True
                    End If
                End If
                move += 1
            Loop
        End If
        Return success
    End Function

    Private Function CountRow(n As Integer, board(,) As Integer, row As Integer, dir As Boolean)
        Dim cntRow As Integer = 0, maxVal As Integer = -1
        If dir Then
            For j = 0 To n - 1
                If board(row, j) > maxVal Then
                    maxVal = board(row, j)
                    cntRow += 1
                End If
            Next
        Else
            For j = n - 1 To 0 Step -1
                If board(row, j) > maxVal Then
                    maxVal = board(row, j)
                    cntRow += 1
                End If
            Next
        End If
        Return cntRow
    End Function

    Private Function CountCol(n As Integer, board(,) As Integer, col As Integer, dir As Boolean)
        Dim cntCol As Integer = 0, maxVal As Integer = -1
        If dir Then
            For i = 0 To n - 1
                If board(i, col) > maxVal Then
                    maxVal = board(i, col)
                    cntCol += 1
                End If
            Next
        Else
            For i = n - 1 To 0 Step -1
                If board(i, col) > maxVal Then
                    maxVal = board(i, col)
                    cntCol += 1
                End If
            Next
        End If
        Return cntCol
    End Function

    Private Sub FillCounts(n As Integer, board(,) As Integer, cells(,) As Button, R(,) As Boolean, C(,) As Boolean)
        For i = 0 To n - 1
            For j = 0 To n - 1
                cells(i, j).Text = ""
                cells(i, j).BackColor = Color.Silver
                board(i, j) = 0
                R(i, j) = True
                C(i, j) = True
            Next
        Next
        RandomizeBoardState(0, n, board, R, C)
        For i = 0 To n - 1
            countRowL(i) = CountRow(n, board, i, True)
            countRowR(i) = CountRow(n, board, i, False)
            countColT(i) = CountCol(n, board, i, True)
            countColB(i) = CountCol(n, board, i, False)
            countRowLeft(i).Text = countRowL(i).ToString()
            countRowRight(i).Text = countRowR(i).ToString()
            countColTop(i).Text = countColT(i).ToString()
            countColBottom(i).Text = countColB(i).ToString()
        Next
        For i = 0 To n - 1
            For j = 0 To n - 1
                board(i, j) = 0
            Next
        Next
    End Sub

    Private Sub Skyscraper_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        For i = 0 To n - 1
            For j = 0 To n - 1
                cells(i, j) = DirectCast(Controls.Find("cell" & i & j, False).First, Button)
                cells(i, j).BackColor = Color.Silver
                cells(i, j).Text = ""
                board(i, j) = 0
            Next
            countRowLeft(i) = DirectCast(Controls.Find("countRow" & i & "Left", False).First, Button)
            countRowRight(i) = DirectCast(Controls.Find("countRow" & i & "Right", False).First, Button)
            countColTop(i) = DirectCast(Controls.Find("countColumn" & i & "Top", False).First, Button)
            countColBottom(i) = DirectCast(Controls.Find("countColumn" & i & "Bottom", False).First, Button)
            countRowLeft(i).BackColor = Color.White
            countRowRight(i).BackColor = Color.White
            countColTop(i).BackColor = Color.White
            countColBottom(i).BackColor = Color.White
        Next
        start = False
        emptiesCnt = n ^ 2
        FillCounts(n, board, cells, R, C)
        startBtn.Enabled = True
        randomBtn.Enabled = True
    End Sub

    Private Function IsSolved(n As Integer, board(,) As Integer, emptiesCnt As Integer)
        If emptiesCnt <> 0 Then
            Return False
        End If
        For i = 0 To n - 1
            Dim vis(n) As Boolean
            For j = 0 To n - 1
                vis(j) = False
            Next
            For j = 0 To n - 1
                If vis(board(i, j) - 1) Then
                    Return False
                End If
                vis(board(i, j) - 1) = True
            Next
        Next
        For j = 0 To n - 1
            Dim vis(n) As Boolean
            For i = 0 To n - 1
                vis(i) = False
            Next
            For i = 0 To n - 1
                If vis(board(i, j) - 1) Then
                    Return False
                End If
                vis(board(i, j) - 1) = True
            Next
        Next
        For i = 0 To n - 1
            Dim cntRowL As Integer = CountRow(n, board, i, True)
            Dim cntRowR As Integer = CountRow(n, board, i, False)
            Dim cntColT As Integer = CountCol(n, board, i, True)
            Dim cntColB As Integer = CountCol(n, board, i, False)
            If cntRowL <> countRowL(i) Then
                Return False
            End If
            If cntRowR <> countRowR(i) Then
                Return False
            End If
            If cntColT <> countColT(i) Then
                Return False
            End If
            If cntColB <> countColB(i) Then
                Return False
            End If
        Next
        Return True
    End Function

    Private Sub button_Click(sender As Object, e As EventArgs) Handles cell33.Click, cell32.Click, cell31.Click, cell30.Click, cell23.Click, cell22.Click, cell21.Click, cell20.Click, cell13.Click, cell12.Click, cell11.Click, cell10.Click, cell03.Click, cell02.Click, cell01.Click, cell00.Click
        If start Then
            Dim cellName As String = CType(sender, Button).Name
            Dim cellNum As Integer = CType(cellName.Substring(4), Integer)
            Dim row As Integer = cellNum \ 10, col As Integer = cellNum Mod 10
            For i = 0 To n - 1
                For j = 0 To n - 1
                    cells(i, j).BackColor = Color.Silver
                Next
                countRowLeft(i).BackColor = Color.White
                countRowRight(i).BackColor = Color.White
                countColTop(i).BackColor = Color.White
                countColBottom(i).BackColor = Color.White
            Next
            board(row, col) += 1
            If board(row, col) = n + 1 Then
                cells(row, col).Text = ""
                board(row, col) = 0
                emptiesCnt += 1
            Else
                cells(row, col).Text = board(row, col).ToString()
                If board(row, col) = 1 Then
                    emptiesCnt -= 1
                End If
            End If
            For i = 0 To n - 1
                For j = 0 To n - 1
                    If board(i, j) <> 0 Then
                        Dim isDuplicate As Boolean = False
                        For k = 0 To n - 1
                            If k <> i And board(k, j) = board(i, j) Then
                                cells(k, j).BackColor = Color.Red
                                isDuplicate = True
                            End If
                            If k <> j And board(i, k) = board(i, j) Then
                                cells(i, k).BackColor = Color.Red
                                isDuplicate = True
                            End If
                        Next
                        If isDuplicate Then
                            cells(i, j).BackColor = Color.Red
                        End If
                    End If
                Next
            Next
            If emptiesCnt = 0 Then
                For i = 0 To n - 1
                    Dim cntRowL As Integer = CountRow(n, board, i, True)
                    Dim cntRowR As Integer = CountRow(n, board, i, False)
                    Dim cntColT As Integer = CountCol(n, board, i, True)
                    Dim cntColB As Integer = CountCol(n, board, i, False)
                    If cntRowL <> countRowL(i) Then
                        countRowLeft(i).BackColor = Color.Red
                    End If
                    If cntRowR <> countRowR(i) Then
                        countRowRight(i).BackColor = Color.Red
                    End If
                    If cntColT <> countColT(i) Then
                        countColTop(i).BackColor = Color.Red
                    End If
                    If cntColB <> countColB(i) Then
                        countColBottom(i).BackColor = Color.Red
                    End If
                Next
            End If
            If IsSolved(n, board, emptiesCnt) Then
                start = False
                MessageBox.Show("Solved!")
            End If
        End If
    End Sub

    Private Sub startBtn_Click(sender As Object, e As EventArgs) Handles startBtn.Click
        start = True
        startBtn.Enabled = False
    End Sub

    Private Sub restartBtn_Click(sender As Object, e As EventArgs) Handles restartBtn.Click
        For i = 0 To n - 1
            For j = 0 To n - 1
                cells(i, j).BackColor = Color.Silver
                cells(i, j).Text = ""
                board(i, j) = 0
            Next
            countRowLeft(i).BackColor = Color.White
            countRowRight(i).BackColor = Color.White
            countColTop(i).BackColor = Color.White
            countColBottom(i).BackColor = Color.White
        Next
        start = False
        emptiesCnt = n ^ 2
        startBtn.Enabled = True
    End Sub

    Private Sub randomBtn_Click(sender As Object, e As EventArgs) Handles randomBtn.Click
        If Not start Then
            FillCounts(n, board, cells, R, C)
        End If
    End Sub
End Class
