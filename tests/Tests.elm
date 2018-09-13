module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Csv  exposing (parse)

suite : Test
suite =
    describe "The CSV module"
        [ describe "CSV.parse"
            [ 
              test "Convert a string of comma-separated values into a `Csv` structure." <|
                \_ ->
                    let
                         output = Csv.parse "id,value\n51,one\n50,two\n"
                         expect =      {
                            headers = ["id", "value"],
                            records = [
                                          ["51", "one"],
                                          ["50", "two"]
                                      ]
                           }
                    in
                        Expect.equal output expect
            , test "correctly parses quotes in values " <|
                \_ ->
                    let
                         output = Csv.parse "value\nHere is a quote:\"\"\nAnother one:\\\"\n"
                         expect = {
                            headers = ["value"],
                            records = [
                               ["Here is a quote:\"\""],
                               ["Another one:\\\""]
                              ]
                          }
                    in
                        Expect.equal output expect
            , test "Values that contain the character ',' can be quoted " <|
                \_ ->
                    let
                         output = Csv.parse "id,value\n\"1,2,3\",\"one,two,three\"\n"
                         expect = {
                           headers = ["id", "value"],
                           records = [
                                         ["1,2,3", "one,two,three"]
                                     ]
                          }
                    in
                        Expect.equal output expect

            , test "Double quotes can be escaped with a backslash or a second quote" <|
                \_ ->
                    let
                         output = Csv.parse "value\n,Here is a quote:\"\"\nAnother one:\\\"\n"
                         expect = {
                           headers = ["value"],
                           records = [
                                         ["Here is a quote:\""],
                                         ["Another one:\""]
                                     ]
                          }
                    in
                        Expect.equal output expect





    






    


            ],
            describe "CSV.parseWith"
            [ 
              test "Convert a string of values separated by a _separator_ into a `Csv` structure." <|
                \_ ->
                    let
                         output = Csv.parseWith ';' "id;value\n1;one\n2;two\n"
                         expect =      
                          {
                           headers = ["id", "value"],
                           records = [
                                         ["1", "one"],
                                         ["2", "two"]
                                     ]
                          } 
                    in
                        Expect.equal output expect
            , test "correctly parses quotes in values " <|
                \_ ->
                    let
                         output = Csv.parse "value\nHere is a quote:\"\"\nAnother one:\\\"\n"
                         expect = {
                            headers = ["value"],
                            records = [
                               ["Here is a quote:\"\""],
                               ["Another one:\\\""]
                              ]
                          }
                    in
                        Expect.equal output expect
            ],
            describe "CSV.split"
            [ 
              test "Convert a string of comma-separated values into a list of lists." <|
                \_ ->
                    let
                         output = Csv.split  "id,value\n1,one\n2,two\n"
                         expect = [   ["id", "value"],
                                         ["1", "one"],
                                         ["2", "two"]
                                     ]
                    in
                        Expect.equal output expect
            ],
            describe "CSV.splitWith"
            [ 
              test "Convert a string of values separated by a _separator_ into a `Csv` structure." <|
                \_ ->
                    let
                         output = Csv.splitWith ';' "id;value\n1;one\n2;two"
                         expect =  [   ["id", "value"],
                                         ["1", "one"],
                                         ["2", "two"]
                                     ]
                    in
                        Expect.equal output expect
            ]
        ]