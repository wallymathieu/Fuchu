﻿namespace Fuchu

open System
                
module Seq = 
    let (|Empty|Cons|) l = 
        if Seq.isEmpty l
            then Empty
            else Cons(Seq.head l, Seq.skip 1 l)

    let (|One|_|) l = 
        match Seq.toList l with
        | [x] -> Some x
        | _ -> None

    let (|Two|_|) l = 
        match Seq.toList l with
        | [x;y] -> Some(x,y)
        | _ -> None

module String =
    let internal nullBool2 f a b =
        if a = null && a = null then
            true
        elif a = null || b = null then
            false
        else
            f b a

    let internal nullOption2 f a b =
        nullBool2 f a b |> function true -> Some() | false -> None

    let (|StartsWith|_|) =
        nullOption2 (fun (s: string) -> s.StartsWith)

    let (|Contains|_|) =
        nullOption2 (fun (s: string) -> s.Contains)

[<AutoOpen>]
module TestHelpers = 
    open Fuchu
    open Fuchu.Impl

    let evalSilent = eval TestPrinters.Default Seq.map

    let inline assertTestFails test =
        let test = TestCase test
        match evalSilent test with
        | [{ TestRunResult.Result = TestResult.Failed _ }] -> ()
        | x -> failtestf "Should have failed, but was %A" x

    open FsCheck

    let genLimitedTimeSpan = 
        lazy (
            let max = TimeSpan.FromDays(1.)
            let genTimeSpan = Gen.choose (0, int max.Ticks) |> Gen.map (int64 >> TimeSpan)
            let shrink (t: TimeSpan) = 
                if t.Days <> 0 then
                    seq { yield TimeSpan(0, t.Hours, t.Minutes, t.Seconds, t.Milliseconds) }
                elif t.Hours <> 0 then
                    seq { yield TimeSpan(0, 0, t.Minutes, t.Seconds, t.Milliseconds) }
                elif t.Minutes <> 0 then
                    seq { yield TimeSpan(0, 0, 0, t.Seconds, t.Milliseconds) }
                elif t.Seconds <> 0 then
                    seq { yield TimeSpan(0, 0, 0, 0, t.Milliseconds) }
                elif t.Milliseconds <> 0 then
                    seq { yield TimeSpan.Zero }
                else
                    Seq.empty
            Arb.fromGenShrink (genTimeSpan, shrink)
        )

    let genTestResultCounts = 
        lazy (
            gen {
                let! passed = Arb.generate<int>
                let! ignored = Arb.generate<int>
                let! failed = Arb.generate<int>
                let! errored = Arb.generate<int>
                let! time = genLimitedTimeSpan.Value.Generator
                return {
                    TestResultCounts.Passed = passed
                    Ignored = ignored
                    Failed = failed
                    Errored = errored
                    Time = time
                }
            }
        )

    let shrinkTestResultCounts (c: TestResultCounts) : TestResultCounts seq = 
        seq {
            for passed in Arb.shrink c.Passed do
            for ignored in Arb.shrink c.Ignored do
            for failed in Arb.shrink c.Failed do
            for errored in Arb.shrink c.Errored do
            for time in Arb.shrink c.Time ->
            {
                TestResultCounts.Passed = passed
                Ignored = ignored
                Failed = failed
                Errored = errored
                Time = time
            }
        }

    let arbTestResultCounts = 
        lazy (
            Arb.fromGenShrink(genTestResultCounts.Value, shrinkTestResultCounts)
        )

    let twoTestResultCounts = 
        lazy (
            Gen.two arbTestResultCounts.Value.Generator |> Arb.fromGen
        )
