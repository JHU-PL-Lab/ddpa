%{
  open Test_expectation_types;;
%}

%token <string> IDENTIFIER
%token AT COLON SEMICOLON COMMA EQUAL
%token QUERY ANALYSES RESULTS START END
%token EVALUATE STUCK WELL_FORMED ILL_FORMED NO_INCONSISTENCIES INCONSISTENCIES_AT
%token OPEN_PAREN CLOSE_PAREN

%type <Test_expectation_types>.expectations

%start expectations

%%

expectation_file:
  | analysis_expectation expectation_list { Expectations ((Some $1), $2) }
  | expectation_list { Expectations (None, $1) }

analysis_expectation:
  | QUERY COLON query_list SEMICOLON ANALYSES COLON analysis_list SEMICOLON RESULTS COLON result_list SEMICOLON { Some (Analysis_Expectation ($3, $7, $11)) }

expectation_list:
  | expectation SEMICOLON expectation_list { $1::$3 }
  | expectation { [$1] }

expectation:
  | EVALUATE { Expect_evaluate }
  | STUCK { Expect_stuck }
  | WELL_FORMED { Expect_well_formed }
  | ILL_FORMED { Expect_ill_formed }
  | NO_INCONSISTENCIES { Expect_analysis_no_inconsistencies }
  | INCONSISTENCIES_AT lookup_var { Expect_analysis_inconsistency_at($2) }

result_list:
  | result COMMA result_list { $1::$3 }
  | result { [$1] }

query_list:
  | query COMMA query_list { $1::$3 }
  | query { [$1] }

analysis_list:
  | analysis COMMA analysis_list { $1::$3 }
  | analysis { [$1] }

result:
  | analysis OPEN_PAREN query CLOSE_PAREN EQUAL output { Result ($1, $3, $6) }

query:
  | lookup_var AT graph_position AT context { Query($1, (Some $3), (Some $5)) }
  | lookup_var AT graph_position { Query($1, (Some $3), None) }
  | lookup_var { Query($1, None, None) }

analysis:
  | NATURAL DDPA { DDPA($1) }
  | NATURAL PLUME { PLUME($1) }
  | SPLUME { SPLUME }
  | OSPLUME { OSPLUME }

context:
  | lookup_var COMMA lookup_var_list { $1::$3 }

graph_position:
  | IDENTIFIER { ProgramPoint($1) }
  | START { START }
  | END { END }

expected_output:
  | OUTPUT { $1 }

lookup_var_list:
  | lookup_var { [$1] }
  | lookup_var COMMA lookup_var_list { $1::$3 }

lookup_var:
  | IDENTIFIER { Var $1}
