open System

#load "paket-files/fsprojects/FSharpx.Collections/src/FSharpx.Collections/Collections.fs"
      "paket-files/fsprojects/FSharpx.Collections/src/FSharpx.Collections/LazyList.fsi"
      "paket-files/fsprojects/FSharpx.Collections/src/FSharpx.Collections/LazyList.fs"
      "paket-files/hedgehogqa/dotnet-hedgehog/Hedgehog/Numeric.fs"
      "paket-files/hedgehogqa/dotnet-hedgehog/Hedgehog/Seed.fs"
      "paket-files/hedgehogqa/dotnet-hedgehog/Hedgehog/Tree.fs"
      "paket-files/hedgehogqa/dotnet-hedgehog/Hedgehog/Random.fs"
      "paket-files/hedgehogqa/dotnet-hedgehog/Hedgehog/Shrink.fs"
      "paket-files/hedgehogqa/dotnet-hedgehog/Hedgehog/Gen.fs"
      "paket-files/hedgehogqa/dotnet-hedgehog/Hedgehog/Property.fs"
open Hedgehog

#load "paket-files/ploeh/dependency-rejection-samples/FSharp/BookingApi/MaîtreD.fs"
open Ploeh.Samples.MaîtreD

module Gen =
    let reservation =
        gen {
            let! bookingDate = Gen.dateTime |> Gen.map DateTimeOffset
            let! positiveQty = Gen.range 1 99
            let! trueOrFalse = Gen.bool

            return { Date = bookingDate
                     Quantity = positiveQty
                     IsAccepted = trueOrFalse }
        }

// The properties below have been ported from
// https://github.com/ploeh/dependency-rejection-samples/blob/22230733a36f0a425deaafbee14a8922f6df83e6/FSharp/BookingApi/Ma%C3%AEtreDTests.fs

let ``tryAccept behaves correctly when it can accept`` =
    property {
        let! reservation    = Gen.reservation
        let! reservations   = Gen.list Gen.reservation
        let! excessCapacity = Gen.filter (fun x -> x >= 0) Gen.int
        let  capacity       = excessCapacity
                              + (reservations |> List.sumBy (fun x -> x.Quantity))
                              + reservation.Quantity

        let actual = tryAccept capacity reservations reservation

        return Some { reservation with IsAccepted = true } = actual
    }

let ``tryAccept behaves correctly when it can't accept`` =
    property {
        let! reservation     = Gen.reservation
        let! reservations    = Gen.list Gen.reservation
        let! lackingCapacity = Gen.filter (fun x -> x > 0) Gen.int
        let  capacity        = (reservations |> List.sumBy (fun x -> x.Quantity))
                               - lackingCapacity

        let actual = tryAccept capacity reservations reservation

        return None = actual
    }

Property.print ``tryAccept behaves correctly when it can accept``
Property.print ``tryAccept behaves correctly when it can't accept``
