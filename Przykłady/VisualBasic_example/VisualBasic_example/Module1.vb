Imports System.Math
Imports VisualBasic_example.TabliceEtc

Module Module1

    Sub Main() 'Brak komentarzy blokowych. Tylko linijkowe.
        Console.WriteLine("Wybierz: ")
        Dim read As Integer = CType(Console.ReadLine(), Integer)
        '--------------------------------------------------------------------
        'Switch case w C
        Select Case read
            Case 1
                Console.WriteLine("First.")

                Dim variable As String 'Deklaracja zmiennych z Dim na początku
                Dim x, y As Integer
                x = 32
                y = 10

                variable = "String: ""Hello"" " 'znak specjalny nie \" tylko ""
                variable += "Here is a new" & vbCrLf & "Line "
                variable += Console.ReadLine()

                Console.WriteLine(variable + (x + y).ToString) ' + lub &
                Console.WriteLine("Double: " & (x / y).ToString)
                Console.WriteLine("Integer: " & CType(x / y, Integer)) ' w C (int)floor(number)

                x = Pow(x, 2) 'przykład w C pow

                'Instrukcje warunkowe
                If x = y Then       ' ==
                ElseIf x <> y Then  ' !=
                End If

                If x And y Then
                End If

                If Not x > y Then
                    'Cos
                Else
                    'Cos
                End If

                Console.Read()
            Case 2
                '---------------------------------------------------------------
                'Pętla for
                For i = 1 To 10
                    Console.WriteLine("He he")
                Next

                For i = 1 To 50 Step 5 'Step -5 w dół pętla
                    Console.WriteLine("He he co 5")
                Next
                Console.Read()

            Case Is < 10
                '----------------------------------------------------------------
                'Pętla while
                While True
                End While

                While read < 10
                    read += 1

                    If read > 15 Then
                        Exit While
                    End If
                End While

                Console.Read()

            Case 10 To 15
                '----------------------------------------------------------------
                'Do Until
                Dim myNumber As Integer = 0

                Do Until myNumber = 5
                    Console.WriteLine("Liczba mniejsza od 6")
                    myNumber = Console.ReadLine()

                    If myNumber = 5 Then
                        Console.WriteLine("Tak to 5")
                    Else
                        Console.WriteLine("to nie ta liczba")
                    End If
                Loop
                Console.Read()

                'DoWhile
                Do While myNumber < 50
                    myNumber += 1
                    Console.WriteLine(myNumber)
                Loop
                Console.Read()

                'Wersja odwrotna
                Do
                    myNumber -= 1
                    Console.WriteLine(myNumber)
                Loop Until myNumber = 10

                Do
                    myNumber += 1
                    Console.WriteLine(myNumber)
                Loop While myNumber < 10

                

            Case Else
                Dim obiekt_struktury As TabliceEtc = New TabliceEtc()
                obiekt_struktury.void()
        End Select




    End Sub

End Module
