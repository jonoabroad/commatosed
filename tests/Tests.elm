module Tests exposing (suite)

import Csv exposing (parse)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "The CSV module"
        [ describe "CSV.parse"
            [ test "Convert a string of comma-separated values into a `Csv` structure." <|
                \_ ->
                    let
                        output =
                            Csv.parse "id,value\n51,one\n50,two"

                        expect =
                            { headers = [ "id", "value" ]
                            , records =
                                [ [ "51", "one" ]
                                , [ "50", "two" ]
                                ]
                            }
                    in
                    Expect.equal output expect
            , test "Convert a string of comma-separated values into a `Csv` structure with trailing newline" <|
                \_ ->
                    let
                        output =
                            Csv.parse "id,value\n51,one\n50,two\n"

                        expect =
                            { headers = [ "id", "value" ]
                            , records =
                                [ [ "51", "one" ]
                                , [ "50", "two" ]
                                ]
                            }
                    in
                    Expect.equal output expect
            , test "Parses quotes in values " <|
                \_ ->
                    let
                        output =
                            Csv.parse "value\nHere is a quote:\"\"\nAnother one:\\\"\n"

                        expect =
                            { headers = [ "value" ]
                            , records =
                                [ [ "Here is a quote:\"" ]
                                , [ "Another one:\"" ]
                                ]
                            }
                    in
                    Expect.equal output expect
            , test "Double quotes can be escaped with a backslash or a second quote" <|
                \_ ->
                    let
                        output =
                            Csv.parse "value\nHere is a quote:\"\"\nAnother one:\\\""

                        expect =
                            { headers = [ "value" ]
                            , records =
                                [ [ "Here is a quote:\"" ]
                                , [ "Another one:\"" ]
                                ]
                            }
                    in
                    Expect.equal output expect
            , test "Values that contain the character ',' can be quoted " <|
                \_ ->
                    let
                        output =
                            Csv.parse "id,value\n\"1,2,3\",\"one,two,three\"\n"

                        expect =
                            { headers = [ "id", "value" ]
                            , records =
                                [ [ "1,2,3", "one,two,three" ]
                                ]
                            }
                    in
                    Expect.equal output expect
            , test "Values within quotes can contain new lines " <|
                \_ ->
                    let
                        output =
                            Csv.parse """value
"Here is a multiline 
value"
single line value"""

                        expect =
                            { headers = [ "value" ]
                            , records =
                                [ [ "Here is a multiline \nvalue" ]
                                , [ "single line value" ]
                                ]
                            }
                    in
                    Expect.equal output expect
            , test "Values within quotes can contain new lines, again " <|
                \_ ->
                    let
                        output =
                            Csv.parse "value\n\"Here is a multiline \nvalue - 1,2,3\"\nsingle line value"

                        expect =
                            { headers = [ "value" ]
                            , records =
                                [ [ "Here is a multiline \nvalue - 1,2,3" ]
                                , [ "single line value" ]
                                ]
                            }
                    in
                    Expect.equal output expect
            , test "Can handle whitespace " <|
                \_ ->
                    let
                        output =
                            Csv.parse "h1,h2,h3\nfull, ,\t\n,full,\n,,full\n"

                        expect =
                            { headers = [ "h1", "h2", "h3" ]
                            , records =
                                [ [ "full", " ", "\t" ]
                                , [ "", "full", "" ]
                                , [ "", "", "full" ]
                                ]
                            }
                    in
                    Expect.equal output expect
            , test "Can handle empty values " <|
                \_ ->
                    let
                        output =
                            Csv.parse "\n\n\n"

                        expect =
                            { headers = []
                            , records = []
                            }
                    in
                    Expect.equal output expect
            , test "minimum viable csv " <|
                \_ ->
                    let
                        output =
                            Csv.parse ",\n,\n,"

                        expect =
                            { headers = [ "", "" ]
                            , records = [ [ "", "" ], [ "", "" ] ]
                            }
                    in
                    Expect.equal output expect
            , test "minimum viable csv, trailing \n" <|
                \_ ->
                    let
                        output =
                            Csv.parse ",\n,\n,\n"

                        expect =
                            { headers = [ "", "" ]
                            , records = [ [ "", "" ], [ "", "" ] ]
                            }
                    in
                    Expect.equal output expect
            , test "empty string" <|
                \_ ->
                    let
                        output =
                            Csv.parse ""

                        expect =
                            { headers = []
                            , records = []
                            }
                    in
                    Expect.equal output expect
            , test "header only" <|
                \_ ->
                    let
                        output =
                            Csv.parse "Header"

                        expect =
                            { headers = [ "Header" ]
                            , records = []
                            }
                    in
                    Expect.equal output expect
            ]
        , describe "CSV.parseWith"
            [ test "Convert a string of values separated by a _separator_ into a `Csv` structure." <|
                \_ ->
                    let
                        output =
                            Csv.parseWith ';' "id;value\n1;one\n2;two\n"

                        expect =
                            { headers = [ "id", "value" ]
                            , records =
                                [ [ "1", "one" ]
                                , [ "2", "two" ]
                                ]
                            }
                    in
                    Expect.equal output expect
            , test "Convert a string of values separated by a non-standard _separator_ into a `Csv` structure." <|
                \_ ->
                    let
                        output =
                            Csv.parseWith '☃' "id☃value\n1☃one\n2☃two"

                        expect =
                            { headers = [ "id", "value" ]
                            , records =
                                [ [ "1", "one" ]
                                , [ "2", "two" ]
                                ]
                            }
                    in
                    Expect.equal output expect
            ]
        ]
