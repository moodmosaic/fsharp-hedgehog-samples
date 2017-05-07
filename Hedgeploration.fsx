open System

#r "packages/Hedgehog/lib/net452/Hedgehog.dll"
open Hedgehog

#load "paket-files/ploeh/dependency-rejection-samples/FSharp/BookingApi/MaîtreD.fs"
open Ploeh.Samples.MaîtreD

#load "paket-files/ploeh/KataTennis/KataTennis/Tennis.fs"
open Ploeh.Katas.Tennis

module Gen =
    let reservation =
        gen {
            let! bookingDate = Gen.dateTime |> Gen.map DateTimeOffset
            let! positiveQty = Gen.int (Range.linear 1 100)
            let! trueOrFalse = Gen.bool

            return { Date = bookingDate
                     Quantity = positiveQty
                     IsAccepted = trueOrFalse }
        }

    let pointData =
        let point =
            Gen.item [Love; Fifteen; Thirty]
        gen {
            let! playerOnePoint = point
            let! playerTwoPoint = point

            return { PlayerOnePoint = playerOnePoint
                     PlayerTwoPoint = playerTwoPoint }
        }

// The properties below have been ported from
// https://github.com/ploeh/dependency-rejection-samples/blob/22230733a36f0a425deaafbee14a8922f6df83e6/FSharp/BookingApi/Ma%C3%AEtreDTests.fs

let ``tryAccept behaves correctly when it can accept`` =
    property {
        let! reservation    = Gen.reservation
        let! reservations   = Gen.list (Range.linear 0 100) Gen.reservation
        let! excessCapacity = Gen.int  (Range.linear 0 100)
        let  capacity       = excessCapacity
                              + (reservations |> List.sumBy (fun x -> x.Quantity))
                              + reservation.Quantity

        let actual = tryAccept capacity reservations reservation

        return Some { reservation with IsAccepted = true } = actual
    }

let ``tryAccept behaves correctly when it can't accept`` =
    property {
        let! reservation     = Gen.reservation
        let! reservations    = Gen.list (Range.linear 0 100) Gen.reservation
        let! lackingCapacity = Gen.int  (Range.linear 1 100)
        let  capacity        = (reservations |> List.sumBy (fun x -> x.Quantity))
                               - lackingCapacity

        let actual = tryAccept capacity reservations reservation

        return None = actual
    }

Property.print ``tryAccept behaves correctly when it can accept``
Property.print ``tryAccept behaves correctly when it can't accept``

// The properties below have been ported from
// https://github.com/ploeh/KataTennis/blob/724b4cb397436e75cb0ce2e401edb6dc41b8b369/KataTennis/TennisProperties.fs

let ``Given player: <30 when player wins then score is correct`` =
    property {
        let! current     = Gen.pointData
        let! winner      = Gen.item [PlayerOne; PlayerTwo]
        let! playerPoint = Gen.item [Love; Fifteen]
        let  current     = pointTo winner playerPoint current

        let actual = scoreWhenPoints current winner

        let expectedPlayerPoint =
            current
            |> pointFor winner
            |> incrementPoint
        let expected =
            expectedPlayerPoint
            |> Option.map (fun p -> current |> pointTo winner p |> Points)
        return expected = Some actual
    }

Property.print ``Given player: <30 when player wins then score is correct``
