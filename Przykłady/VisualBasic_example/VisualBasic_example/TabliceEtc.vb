Imports Microsoft.VisualBasic

Public Structure TabliceEtc
    Public TablicaCiagow As Array 'przykład tablicy

    Public Sub void() 'Void 
        'Dim myGames As String() = {"GTA 5", "BF4", "GRID2"} - deklaracja i definicja w jednej linijce
        TablicaCiagow = New String(5) {}

        TablicaCiagow(0) = "GTA 4"
        TablicaCiagow(1) = "Battlefield 3"
        TablicaCiagow(2) = "SWAT 4"
        TablicaCiagow(3) = "Arma 2"
        TablicaCiagow(4) = "RollerCoaster Tycoon 3"
        TablicaCiagow(5) = "GRID"
        Console.WriteLine("Funkcja. " & TablicaCiagow(5))

        'For Each
        For Each GName As String In TablicaCiagow
            Console.WriteLine(GName)
        Next
        Console.ReadKey()
    End Sub
    Public Function funkcja() As Integer 'funkcja zwracająca wartość Integer
        Return 1
    End Function
End Structure
