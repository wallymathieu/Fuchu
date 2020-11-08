namespace Fuchu

open System
module Strings=
  let contains (substr:string) (t: string) = t.Contains substr

module Tests =
    open Fuchu
    open Fuchu.Impl
    open System.Threading
    open System.IO
    open System.Globalization

    let (==?) actual expected = Assert.Equal("", expected, actual)

    let tests = 
        TestList [
            testCase "basic" <| fun _ -> Assert.Equal("2+2", 4, 2+2)

            test "using computation expression" {
                Assert.Equal("2+2", 4, 2+2)
            }

            testList "sumTestResults" [
                let sumTestResultsTests = 
                    [
                        { TestRunResult.Name = ""; Result = Passed }
                        { TestRunResult.Name = ""; Result = TestResult.Error (ArgumentException()) }
                        { TestRunResult.Name = ""; Result = Failed "" }
                        { TestRunResult.Name = ""; Result = Passed }
                        { TestRunResult.Name = ""; Result = Failed "" }
                        { TestRunResult.Name = ""; Result = Passed }
                    ]
                let r = lazy sumTestResults sumTestResultsTests
                yield testCase "Passed" <| fun _ -> 
                    r.Value.Passed ==? 3
                yield testCase "Failed" <| fun _ ->
                    r.Value.Failed ==? 2
                yield testCase "Exception" <| fun _ ->
                    r.Value.Errored ==? 1
            ]

            testList "TestResultCounts" [
                testCase "ToString" <| fun _ ->
                    let c1 = { Passed = 1; Ignored = 5; Failed = 2; Errored = 3; }
                    c1.ToString() ==? "6 tests run: 1 passed, 5 ignored, 2 failed, 3 errored\n"
            ]

            testList "Exception handling" [

                testCaseAsync "Fuchu ignore" <| fun _ -> async {
                    let test () = skiptest "b"
                    let test = TestCase test
                    match! evalSilent test with
                    | [{ Result = Ignored "b" }] -> ()
                    | x -> failtestf "Expected result = Ignored, got\n %A" x }
            ]

            testList "Test filter" [
                let tests = 
                    TestList [
                        testCase "a" ignore
                        testCase "b" ignore
                        testList "c" [
                            testCase "d" ignore
                            testCase "e" ignore
                        ]
                    ]
                yield testCase "with one testcase" <| fun _ -> 
                    let t = Test.filter ((=) "a") tests |> Test.toTestCodeList |> Seq.toList
                    t.Length ==? 1 // same as assertEqual "" 1 t.Length
                yield testCase "with nested testcase" <| fun _ ->
                    let t = Test.filter (Strings.contains "d") tests |> Test.toTestCodeList |> Seq.toList
                    t.Length ==? 1
                yield testCase "with one testlist" <| fun _ ->
                    let t = Test.filter (Strings.contains "c") tests |> Test.toTestCodeList |> Seq.toList
                    t.Length ==? 2
                yield testCase "with no results" <| fun _ ->
                    let t = Test.filter ((=) "z") tests |> Test.toTestCodeList |> Seq.toList
                    t.Length ==? 0
            ]

            testList "Timeout" [
                testCaseAsync "fail" <| fun _ -> async{
                    let test = TestCaseAsync(Test.timeout 10 (fun _ -> Async.Sleep 100))
                    let! evald = evalSilent test
                    let result = sumTestResults evald
                    return result.Failed ==? 1 }
                testCaseAsync "pass" <| fun _ -> async{
                    let test = TestCaseAsync(Test.timeout 1000 (fun _-> async.Return() ))
                    let! evald = evalSilent test
                    let result = sumTestResults evald
                    return result.Passed ==? 1 }
            ]

            testList "parse args" [
                testCase "default" <|
                    fun _ ->
                        let opts = parseArgs [||]
                        opts.Parallel ==? false

                testCase "parallel" <|
                    fun _ ->
                        let opts = parseArgs [|"/m"|]
                        opts.Parallel ==? true
            ]

            testList "assertions" [
                testList "NotEqual" [
                    testCase "pass" <| fun _ ->
                        Assert.NotEqual("should be different", "", "monkey")

                    testCaseAsync "fail" <| fun _ ->
                        let test () = Assert.NotEqual("should fail", "", "")
                        assertTestFails test
                ]

                testList "string contain" [
                    testCase "pass" <| fun _ ->
                        Assert.StringContains("", "hello", "hello world")

                    testCaseAsync "fail" <| fun _ ->
                        let test () = Assert.StringContains("", "a", "hello world")
                        assertTestFails test
                ]
            ]

            testList "computation expression" [
                let testNormal a =
                    testCase "" <| fun _ ->
                        if a < 0
                            then failtest "negative"
                        if a > 5
                            then failwith "over 5"
                let testCompExp a = 
                    test "" {
                        if a < 0
                            then failtest "negative"
                        if a > 5
                            then failwith "over 5"
                    }
                for c in [-5; 1; 6] ->
                    testCaseAsync (sprintf "compare comp.exp. and normal with value %d" c) <| fun _ -> async{
                        let! normal = evalSilent <| testNormal c
                        let! compexp = evalSilent <| testCompExp c
                        let normalTag = TestResult.tag normal.[0].Result
                        let compexpTag = TestResult.tag compexp.[0].Result
                        Assert.Equal("result", normalTag, compexpTag) }
            ]
        ]
