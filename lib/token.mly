/* Literals and Identifier */
%token <int>    TK_INT_LITERAL      "<int>"
%token <bool>   TK_BOOL_LITERAL     "<bool>"
%token <string> TK_ID               "<id>"
%token <string> TK_TID              "<tid>"

/* Keywords */
%token TK_LET           "let"
%token TK_IN            "in"
%token TK_IF            "if"
%token TK_THEN          "then"
%token TK_ELSE          "else"
%token TK_END           "end"

/* Symbols */
%token TK_DOT           "."
%token TK_COLON         ":"
%token TK_L_PAREN       "("
%token TK_R_PAREN       ")"
%token TK_R_ARROW       "->"
%token TK_ADD           "+"
%token TK_MUL           "*"
%token TK_ASGN          "="
%token TK_LT            "<"
%token TK_BACKSLASH     "\\"

%token TK_EOF           "<eof>"

%%