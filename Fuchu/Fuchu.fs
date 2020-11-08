namespace Fuchu

open System
#if !FABLE_COMPILER
open System.Linq
open System.Threading.Tasks
open System.Reflection
#endif
open System.Runtime.CompilerServices

/// Actual test function
type TestCode = unit -> unit
type TestCodeAsync = unit -> Async<unit>

/// Test tree
type Test = 
    | TestCase of TestCode
    | TestCaseAsync of TestCodeAsync
    | TestList of Test seq
    | TestLabel of string * Test

type FuchuException(msg) = inherit Exception(msg)
type AssertException(msg) = inherit FuchuException(msg)
type IgnoreException(msg) = inherit FuchuException(msg)

#if !FABLE_COMPILER
/// Marks a top-level test for scanning
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property ||| AttributeTargets.Field)>]
type TestsAttribute() = inherit Attribute()
#endif

module Helpers =
    let inline ignore2 _ = ignore
    let inline ignore3 _ = ignore2

    let bracket setup teardown f () =
        let v = setup()
        try
            f v
        finally
            teardown v

    /// Print to Console and Trace
    let tprintf fmt = 
        Printf.kprintf (fun s -> 
        #if !FABLE_COMPILER
                            System.Diagnostics.Trace.Write s
        #endif
                            Console.Write s) fmt

    open System.Text.RegularExpressions
    let rx = lazy Regex(" at (.*) in (.*):line (\d+)", RegexOptions.Compiled ||| RegexOptions.Multiline)
    let stackTraceToString s = rx.Value.Replace(s, "$2($3,1): $1")
    let exnToString (e: Exception) = stackTraceToString (e.ToString())

    module Seq =
        let cons x xs = seq { yield x; yield! xs }
    #if !FABLE_COMPILER
    type Type with
        static member TryGetType t = 
            try
                Type.GetType(t, true) |> Some
            with _ -> None

    type MemberInfo with
        member m.HasAttributePred (pred: Type -> bool) =
            m.GetCustomAttributes true
            |> Seq.filter (fun a -> pred(a.GetType()))
            |> Seq.length |> (<) 0

        member m.HasAttributeType (attr: Type) =
            m.HasAttributePred ((=) attr)

        member m.HasAttribute (attr: string) =
            m.HasAttributePred (fun (t: Type) -> t.FullName = attr)

        member m.GetAttributes (attr: string) : Attribute seq =
            m.GetCustomAttributes true
            |> Seq.filter (fun a -> a.GetType().FullName = attr)
            |> Seq.cast
    #endif
    module Async=
        /// From: https://github.com/fsprojects/FSharpPlus
        let sequence (t: list<Async<_>>) : Async<list<_>> =
                let rec loop acc = function
                | []    -> async.Return (List.rev acc)
                | x::xs -> async.Bind (x, fun x -> loop (x::acc) xs)
                loop [] t
        
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Test =
    open Helpers

    /// Flattens a tree of tests
    let toTestCodeList =
        let rec loop parentName testList =
            function
            | TestLabel (name, test) -> 
                let fullName = 
                    if String.IsNullOrEmpty parentName
                        then name
                        else parentName + "/" + name
                loop fullName testList test
            | TestCase test -> Seq.cons (parentName, Choice<_,_>.Choice1Of2 test) testList
            | TestCaseAsync test -> Seq.cons (parentName, Choice<_,_>.Choice2Of2 test) testList
            | TestList tests -> Seq.collect (loop parentName testList) tests
        loop null Seq.empty

    /// Recursively maps all TestCodes in a Test
    let rec wrap f fAsync =
        function
        | TestCase test -> TestCase (f test)
        | TestCaseAsync test -> TestCaseAsync (fAsync test)
        | TestList testList -> TestList (Seq.map (wrap f fAsync) testList)
        | TestLabel (label, test) -> TestLabel (label, (wrap f fAsync) test)
        
    /// Recursively replaces TestCodes in a Test
    let rec replaceTestCode f fAsync =
        function
        | TestLabel (label, TestCase test) -> f label test
        | TestCase test -> f null test
        | TestCaseAsync test -> fAsync null test
        | TestList testList -> TestList (Seq.map (replaceTestCode f fAsync) testList)
        | TestLabel (label, test) -> TestLabel (label, replaceTestCode f fAsync test)

    /// Filter tests by name
    let filter pred =
        toTestCodeList
        >> Seq.filter (fst >> pred)
        >> Seq.map (fun (name, test) -> TestLabel (name, match test with Choice1Of2 t-> TestCase t | Choice2Of2 t-> TestCaseAsync t))
        >> TestList

    /// Applies a timeout to a test
    let timeout (timeout:int) (test: TestCodeAsync) : TestCodeAsync =
        let testFunc = Func<_,_> test
        #if !FABLE_COMPILER

        fun () -> async{
            try
                let asyncTestFunc = Task.Run(fun () -> testFunc.Invoke())
                if asyncTestFunc.Wait(timeout) |> not then
                    let ts = TimeSpan.FromMilliseconds (float timeout)
                    raise <| AssertException(sprintf "Timeout (%A)" ts)
            with :? TimeoutException ->
                let ts = TimeSpan.FromMilliseconds (float timeout)
                raise <| AssertException(sprintf "Timeout2 (%A)" ts) }
        #else
        fun () -> async {
                let! child = Async.StartChild( async {  testFunc.Invoke() }, timeout )
                return! child }
        #endif

module Impl =
    open Helpers

    type TestResult = 
        | Passed
        | Ignored of string
        | Failed of string
        | Error of exn
        override x.ToString() = 
            match x with
            | Passed -> "Passed"
            | Ignored reason -> "Ignored: " + reason
            | Failed error -> "Failed: " + error
            | Error e -> "Exception: " + exnToString e
        static member tag = 
            function
            | Passed -> 0
            | Ignored _ -> 1
            | Failed _ -> 2
            | Error _ -> 3
        static member isPassed =
            function
            | Passed -> true
            | _ -> false
        static member isIgnored =
            function
            | Ignored _ -> true
            | _ -> false
        static member isFailed =
            function
            | Failed _ -> true
            | _ -> false
        static member isException =
            function
            | Error _ -> true
            | _ -> false

    [<StructuredFormatDisplay("{Description}")>]
    type TestResultCounts = {
        Passed: int
        Ignored: int
        Failed: int
        Errored: int
        #if !FABLE_COMPILER
        Time: TimeSpan
        #endif
    } with 
        override x.ToString() =
            #if !FABLE_COMPILER
            sprintf "%d tests run: %d passed, %d ignored, %d failed, %d errored (%A)\n"
                (x.Errored + x.Failed + x.Passed)
                x.Passed x.Ignored x.Failed x.Errored x.Time
            #else
            sprintf "%d tests run: %d passed, %d ignored, %d failed, %d errored\n"
                (x.Errored + x.Failed + x.Passed)
                x.Passed x.Ignored x.Failed x.Errored
            #endif
        member x.Description = x.ToString()
        static member (+) (c1: TestResultCounts, c2: TestResultCounts) = 
            { Passed = c1.Passed + c2.Passed
              Ignored = c1.Ignored + c2.Ignored
              Failed = c1.Failed + c2.Failed
              Errored = c1.Errored + c2.Errored
              #if !FABLE_COMPILER
              Time = c1.Time + c2.Time
              #endif
            }
        static member errorCode (c: TestResultCounts) =
            (if c.Failed > 0 then 1 else 0) ||| (if c.Errored > 0 then 2 else 0)

    [<StructuredFormatDisplay("{Description}")>]
    type TestRunResult = {
        Name: string
        Result: TestResult
        #if !FABLE_COMPILER
        Time: TimeSpan
        #endif
    } with 
        override x.ToString() = 
            #if !FABLE_COMPILER
            sprintf "%s: %s (%A)" x.Name (x.Result.ToString()) x.Time
            #else
            sprintf "%s: %s" x.Name (x.Result.ToString())
            #endif
        member x.Description = x.ToString()
        static member isPassed (r: TestRunResult) = TestResult.isPassed r.Result
        static member isIgnored (r: TestRunResult) = TestResult.isIgnored r.Result
        static member isFailed (r: TestRunResult) = TestResult.isFailed r.Result
        static member isException (r: TestRunResult) = TestResult.isException r.Result
        static member isFailedOrException r = TestRunResult.isFailed r || TestRunResult.isException r

    let sumTestResults (results: #seq<TestRunResult>) =
        let counts = 
            results 
            |> Seq.map (fun r -> r.Result)
            |> Seq.countBy TestResult.tag
            |> dict
        let get result = 
            match counts.TryGetValue (TestResult.tag result) with
            | true, v -> v
            | _ -> 0

        { Passed = get TestResult.Passed
          Ignored = get (TestResult.Ignored "")
          Failed = get (TestResult.Failed "")
          Errored = get (TestResult.Error null)
          #if !FABLE_COMPILER
          Time = results |> Seq.map (fun r -> r.Time) |> Seq.fold (+) TimeSpan.Zero
          #endif
        }

    /// Hooks to print report through test run
    type TestPrinters = {
        BeforeRun: string -> unit
        #if !FABLE_COMPILER
        Passed: string -> TimeSpan -> unit
        #else
        Passed: string -> unit
        #endif
        Ignored: string -> string -> unit
        #if !FABLE_COMPILER
        Failed: string -> string -> TimeSpan -> unit
        Exception: string -> exn -> TimeSpan -> unit
        #else
        Failed: string -> string -> unit
        Exception: string -> exn -> unit
        #endif
    } with
        static member Default = {
            BeforeRun = ignore
            Ignored = ignore2
            #if !FABLE_COMPILER
            Passed = ignore2
            Failed = ignore3
            Exception = ignore3
            #else
            Passed = ignore
            Failed = ignore2
            Exception = ignore2
            #endif
            }

    /// Runs a list of tests, with parameterized printers (progress indicators) and traversal.
    /// Returns list of results.
    let evalTestList =
        #if !FABLE_COMPILER
        let failExceptions = [
            typeof<AssertException>.AssemblyQualifiedName
            "NUnit.Framework.AssertionException, NUnit.Framework"
            "NUnit.Framework.AssertionException, nunit.framework"
            "Gallio.Framework.Assertions.AssertionFailureException, Gallio"
            "Gallio.Framework.Assertions.AssertionException, Gallio"
            "Xunit.Sdk.AssertException, Xunit"
        ]
        let ignoreExceptions = [
            "NUnit.Framework.IgnoreException, NUnit.Framework"
            "NUnit.Framework.IgnoreException, nunit.framework"
            typeof<IgnoreException>.AssemblyQualifiedName
        ]
        let failExceptionTypes = lazy List.choose Type.TryGetType failExceptions
        let ignoreExceptionTypes = lazy List.choose Type.TryGetType ignoreExceptions
        let (|ExceptionInList|_|) (l: Type list) (e: #exn) = 
            let et = e.GetType()
            if l |> List.exists (fun x -> x.IsAssignableFrom et)
                then Some()
                else None
        #endif

        fun (printers: TestPrinters) map ->
            #if !FABLE_COMPILER
            let execOne (name: string, test:Choice<TestCode,TestCodeAsync>) = async{
                printers.BeforeRun name
                let w = System.Diagnostics.Stopwatch.StartNew()
                try
                    match test with
                    | Choice1Of2 test->test()
                    | Choice2Of2 test->do! test()
                    w.Stop()
                    printers.Passed name w.Elapsed
                    return { Name = name; Result = Passed; Time = w.Elapsed }
                with
                    | :? AggregateException as e->
                        w.Stop()
                        let exn = e.Flatten().InnerExceptions |> Seq.head
                        return handleException printers name w exn
                    | e ->
                        w.Stop()
                        return handleException printers name w e
            }
            #else
            let execOne (name: string, test:Choice<TestCode,TestCodeAsync>) = async{ 
                printers.BeforeRun name
                try
                    match test with
                    | Choice1Of2 test->test()
                    | Choice2Of2 test->do! test()
                    printers.Passed name
                    return { Name = name; Result = Passed }
                with e ->
                    match e with
                    | _ ->
                        printers.Exception name e
                        return { Name = name; Result = TestResult.Error e }
            }
            #endif
            map execOne

    /// Runs a tree of tests, with parameterized printers (progress indicators) and traversal.
    /// Returns list of results.
    let eval (printer: TestPrinters) map tests =
        Test.toTestCodeList tests 
        |> evalTestList printer map
        |> Seq.toList

    #if !FABLE_COMPILER
    let printFailed = tprintf "%s: Failed: %s (%A)\n"
    let printException name ex = tprintf "%s: Exception: %s (%A)\n" name (exnToString ex)
    #else
    let printFailed = tprintf "%s: Failed: %s\n"
    let printException name ex = tprintf "%s: Exception: %s\n" name (exnToString ex)
    #endif
    /// Evaluates tests sequentially
    let evalSeq = 
        let printer = 
            { TestPrinters.Default with 
                Failed = printFailed
                Exception = printException }

        fun t-> Async.sequence (eval printer Seq.map t |> List.ofSeq)
#if !FABLE_COMPILER
    /// Evaluates tests in parallel
    let evalPar =
        let funLock =
            let locker = obj()
            lock locker
        let inline funLock3 f a b c = funLock (fun () -> f a b c)
        let printFailed = funLock3 printFailed 
        let printException = funLock3 printException
        let printer = 
            { TestPrinters.Default with 
                Failed = printFailed
                Exception = printException }
        fun t-> Async.Parallel (eval printer Seq.map t) 
#endif

    /// Runs tests, returns error code
    let runEval eval (tests: Test) = async{
        #if !FABLE_COMPILER
        let w = System.Diagnostics.Stopwatch.StartNew()
        let! results = eval tests
        w.Stop()
        let summary = { sumTestResults results with Time = w.Elapsed }
        tprintf "%s" (summary.ToString())
        return TestResultCounts.errorCode summary
        #else
        let! results = eval tests
        let summary = sumTestResults results
        tprintf "%s" (summary.ToString())
        return TestResultCounts.errorCode summary
        #endif
        }
#if !FABLE_COMPILER
    let testFromMember (m: MemberInfo): Test option =
        [m]
        |> List.filter (fun m -> m.HasAttributeType typeof<TestsAttribute>)
        |> List.choose (fun m ->
                            match box m with
                            | :? FieldInfo as m ->
                                if m.FieldType = typeof<Test>
                                    then Some(unbox (m.GetValue(null)))
                                    else None
                            | :? MethodInfo as m -> 
                                if m.ReturnType = typeof<Test>
                                    then Some(unbox (m.Invoke(null, null)))
                                    else None
                            | :? PropertyInfo as m -> 
                                if m.PropertyType = typeof<Test>
                                    then Some(unbox (m.GetValue(null, null)))
                                    else None
                            | _ -> None)
        |> List.tryFind (fun _ -> true)

    let listToTestListOption = 
        function
        | [] -> None
        | x -> Some (TestList x)
        
    let testFromType =
        let asMembers x = Seq.map (fun m -> m :> MemberInfo) x
        let bindingFlags = BindingFlags.Public ||| BindingFlags.Static
        fun (t: Type) ->
            [ t.GetMethods bindingFlags |> asMembers
              t.GetProperties bindingFlags |> asMembers
              t.GetFields bindingFlags |> asMembers ]
            |> Seq.collect id
            |> Seq.choose testFromMember
            |> Seq.toList
            |> listToTestListOption

    /// Scan filtered tests marked with TestsAttribute from an assembly
    let testFromAssemblyWithFilter typeFilter (a: Assembly) =
        a.GetExportedTypes()
        |> Seq.filter typeFilter
        |> Seq.choose testFromType
        |> Seq.toList
        |> listToTestListOption

    /// Scan tests marked with TestsAttribute from an assembly
    let testFromAssembly = testFromAssemblyWithFilter (fun _ -> true)

    /// Scan tests marked with TestsAttribute from entry assembly
    let testFromThisAssembly () = testFromAssembly (Assembly.GetEntryAssembly())
#endif

[<AutoOpen; Extension>]
module Tests =
    open Impl
    open Helpers

    /// Fail this test
    let inline failtest msg = raise <| AssertException msg
    /// Fail this test
    let inline failtestf fmt = Printf.ksprintf (fun msg -> raise <| AssertException msg) fmt

    /// Skip this test
    let inline skiptest msg = raise <| IgnoreException msg
    /// Skip this test
    let inline skiptestf fmt = Printf.ksprintf (fun msg -> raise <| IgnoreException msg) fmt

    /// Builds a list/group of tests
    let inline testList name tests = TestLabel(name, TestList tests)

    /// Builds a test case
    let inline testCase name test = TestLabel(name, TestCase test)

    /// Builds an async test case
    let inline testCaseAsync name test = TestLabel(name, TestCaseAsync test)

    /// Applies a function to a list of values to build test cases
    let inline testFixture setup = 
         Seq.map (fun (name, partialTest) ->
                        testCase name (setup partialTest))

    /// Applies a value to a list of partial tests
    let inline testParam param =
         Seq.map (fun (name, partialTest) ->
                        testCase name (partialTest param))

    type TestCaseBuilder(name) = 
        member x.TryFinally(f, compensation) = 
            try
                f()
            finally
                compensation()
        member x.TryWith(f, catchHandler) = 
            try
                f()
            with e -> catchHandler e
        member x.Using(disposable: #IDisposable, f) =
            try
                f disposable
            finally
                match disposable with
                | null -> () 
                | disp -> disp.Dispose()
        member x.For(sequence, f) = 
            for i in sequence do f i
        member x.Combine(f1, f2) = f2(); f1
        member x.Zero() = ()
        member x.Delay f = f
        member x.Run f = testCase name f

    let inline test name = TestCaseBuilder name

    [<Obsolete("Use testList instead")>]
    let inline (=>>) name tests = testList name tests
    [<Obsolete("Use testCase instead")>]
    let inline (=>) name test = testCase name test
    [<Obsolete("Use testFixture instead")>]
    let inline (+>) f = testFixture f
    [<Obsolete("Use a pair instead")>]
    let inline (==>) name test = name,test

    /// Runs tests
    [<Extension; CompiledName("Run")>]
    let run tests = runEval evalSeq tests |> Async.RunSynchronously

    #if !FABLE_COMPILER
    /// Runs tests in parallel
    [<Extension; CompiledName("RunParallel")>]
    let runParallel tests = runEval evalPar tests
    #endif

    // Runner options
    type RunOptions = { Parallel: bool }

    /// Parses command-line arguments
    let parseArgs =
        let defaultOptions = { RunOptions.Parallel = false }
        let opts = [ "/m", fun o -> { o with RunOptions.Parallel = true } ]
        fun (args: string[]) ->
            (defaultOptions, args) 
            ||> Seq.fold (fun opt arg -> 
                            (opt, opts) ||> Seq.fold (fun o (a,f) -> if a = arg then f o else o))

    /// Runs tests with supplied options. Returns 0 if all tests passed, otherwise 1
    [<CompiledNameAttribute("DefaultMainWithOptions")>]
    let defaultMainWithOptions tests (options: RunOptions) = 
        #if !FABLE_COMPILER
        let run = if options.Parallel then runParallel else run
        #endif
        run tests
    
    /// Runs tests with supplied command-line options. Returns 0 if all tests passed, otherwise 1
    [<CompiledNameAttribute("DefaultMain")>]
    let defaultMain tests = parseArgs >> defaultMainWithOptions tests

    #if !FABLE_COMPILER
    /// Runs tests in this assembly with supplied command-line options. Returns 0 if all tests passed, otherwise 1
    [<CompiledNameAttribute("DefaultMainThisAssembly")>]
    let defaultMainThisAssembly args =
        let tests =
            match testFromAssembly (Assembly.GetEntryAssembly()) with
            | Some t -> t
            | None -> TestList []
        defaultMain tests args

    /// Runs tests in this assembly with supplied command-line options.
    /// You may also pass a filter that selected a subset of tests to run.
    /// Returns 0 if all tests passed, otherwise 1
    [<CompiledNameAttribute("DefaultMainThisAssembly")>]
    let defaultMainThisAssemblyFilter args filter =
        let tests =
            match testFromAssembly (Assembly.GetEntryAssembly()) with
            | Some t -> filter t
            | None -> TestList []
        defaultMain tests args
    #endif
// Functions for C#/VB.NET :
#if !FABLE_COMPILER
[<Extension>]
type TestExtensions =
    /// Pattern matching over a Test
    [<Extension>]
    static member Match(test, testCase: Func<_,_>, testList: Func<_,_>, testLabel: Func<_,_,_>, testCaseAsync: Func<_,_>) =
        match test with
        | TestCase c -> testCase.Invoke (Action c)
        | TestCaseAsync c -> failwith "Not implemented!" //TODO
        | TestList l -> testList.Invoke l
        | TestLabel (label, t) -> testLabel.Invoke(label,t)

    /// Groups tests
    [<Extension>]
    static member List (tests, name) = 
        testList name tests

    /// Creates a group of tests
    [<Extension>]
    static member List tests = 
        TestList tests

    // Run a list of tests
    [<Extension>]
    static member Run tests = TestList tests |> run

    (* TODO: Port
    /// Maps all TestCodes in a Test
    [<Extension>]
    static member Wrap (test, f: Func<Action,Action>) = 
        test |> Test.wrap (fun t -> f.Invoke(Action t).Invoke)

    /// Recursively replaces test bodies in a test
    [<Extension>]
    static member ReplaceTestCode(test, f: Func<string, Action, Test>) =
        test |> Test.replaceTestCode (fun n t -> f.Invoke(n, Action t))
    *)
    (* TODO: Port
    /// Applies a timeout to a test
    [<Extension>]
    static member Timeout(test: Action, timeout) = 
        Action(Test.timeout timeout test.Invoke)  *)
    /// Filter tests by name
    [<Extension>]
    static member Where(test, pred: Func<_,_>) = 
        Test.filter pred.Invoke test
        
type Test with

    /// Test unit
    static member Case (f: Action) = 
        TestCase f.Invoke

    /// Test unit
    static member Case (label, f: Action) = 
        testCase label f.Invoke

    /// Parameterized test
    static member Case (label: string, f: Action<_>) = 
        label, f

    /// Creates a group of tests
    static member List (name, [<ParamArray>] tests: Test[]) = 
        testList name tests

    /// Creates a group of tests
    static member List ([<ParamArray>] tests) =
        tests |> Seq.map Test.Case |> TestList

    /// Builds a list of parameterized tests
    static member List (name, setup: Func<_,_>, [<ParamArray>] tests) =
        let tests = tests |> Array.map (fun (name, test) -> Test.Case(name, setup.Invoke test))
        Test.List(name, tests)

    /// Builds a setup/teardown function to apply to parameterized tests
    static member Fixture (setup: Func<_>, teardown: Action<_>) =
        if setup = null then nullArg "setup"
        if teardown = null then nullArg "teardown"
        let f (test: Action<_>) = 
            if test = null then nullArg "test"
            let r = Helpers.bracket setup.Invoke teardown.Invoke test.Invoke
            Action r
        Func<_,_> f

    /// Skip this test
    static member Skip(reason: string, [<ParamArray>] args: obj[]) =
        skiptest (String.Format(reason, args)) |> ignore

    /// Fail this test
    static member Fail(reason: string, [<ParamArray>] args: obj[]) =
        failtest (String.Format(reason, args)) |> ignore
#endif

type Assert =

    static member NotEqual(msg, negative_case, actual) =
        if negative_case = actual
            then failtestf "%s\nExpected: %A\nnot to equal Actual: %A" msg negative_case actual

    static member Equal(msg, expected, actual) =
        if expected <> actual 
            then failtestf "%s\nExpected: %A\nActual: %A" msg expected actual

    static member None(msg, value) =
        match value with
        | Some x -> failtestf "%s\nExpected None, Actual: Some (%A)" msg x
        | _ -> ()

    static member NotNull<'a when 'a : null>(msg, actual: 'a) =
        match box actual with
        | null -> failtestf "%s\nShould not have been null" msg
        | _ -> ()

    #if !FABLE_COMPILER
    static member Raise(msg, ex: Type, f) =
        try
            f()
            failtestf "%s\nExpected exception '%s' but no exception was raised" msg ex.FullName
        with e ->
            if e.GetType() <> ex
                then failtestf "%s\nExpected exception '%s' but raised:\n%A" msg ex.FullName e
    #endif

    static member StringContains(msg, expectedSubString:string, actual: string) =
        if not (actual.Contains expectedSubString)
            then failtestf "%s\nExpected string containing: %s\nActual: %s" msg expectedSubString actual
        
