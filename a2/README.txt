=======================================================================
Optional: Explain any difficulties you had with this assignment or any
parts that are incomplete.  Suggestions on how this assignment could
be improved to help students learn more are welcome:





=======================================================================
For each of the following questions,

    (A) give the answer

    (B) give the shell command (or commands) you used to answer the
    question and in a sentence, explain why your shell command gives you
    helpful data.  (FYI: using fewer commands and piping is more elegant
        than using more commands and will be given more credit).

    Use the data given in the files G.txt, PG.txt, PG-13.txt, R.txt for
    the set of top-grossing films in each rating category (G, PG, PG-13
        and R).  Use the data in the file alltime.txt for the alltime
    inflation-adjusted returns.

    1.  Adjusting for inflation, what is the top-grossing film of all time
    (use the alltime.txt data)?

    (A) Gone with the Wind|MGM|1582.0094|1939

    (B) ./boxoffice -sort-gross < data/alltime.txt | ./boxoffice -take 1

    2. What is the 50th ranked R film by gross on the list?

    (A) The Silence of the Lambs|Orion|130.742922|1991

    (B) ./boxoffice -sort-gross < data/R.txt | ./boxoffice -take 50 | ./boxoffice -drop 49

    3. Suppose you had a chance to make 1 film with a top director and the
    director was so good you were guaranteed that whatever film you made
    would be in the top 5 grossing films in its ratings category (G, PG,
        PG-13, R) --- and equally likely to be ranked 1, 2, 3, 4, or 5.  What
    rating (G, PG, PG-13, R) would you choose to give your film if you
    wanted to make the most money?

    (A) PG-13

    (B) ./boxoffice -sort-gross < data/PG-13.txt | ./boxoffice -take 5 | ./boxoffice -average

    4. Taking inflation in to account, would you have preferred to make
    money off of blockbusters in the 70s or in the 80s?

    (A) 70s

    (B) ./boxoffice -decade 70 < data/alltime.txt | ./boxoffice -average

    5. Taking inflation in to account, which studio made the most
    money off of blockbusters in the 60s?

    (A) Dis.|2675.42

    (B) ./boxoffice -decade 60 < data/alltime.txt | ./boxoffice -by-studio | ./boxoffice -sort-studio | ./boxoffice -take 1

    ===============================================

    Karma (Optional!): Generalize the script in some way.  However, do not
                       do so in a way that changes your answers to previous questions.  We
                       will judge your answers to other parts of the assignment independently
                       of whether or not you have implemented this Karma question.  You made
                       add functionality (and extra options to the script) but you may not
                       change existing functionality or options.

                       Note that changing the data that gets parsed is pretty easy.  Here is
                       the function parse_movie.  If you want to change the separator
                       in between data items (from a "|" to a "," for example), you just need
                       to change the regular expression used in the parse_movie function to
                       split the string line in to pieces.  For example, change this:

                       let parse_movie () : movie option =
                                            let line = read_line() in
  match Str.split (Str.regexp "[|\n\r\t]") line with
                                            | t::s::m::y::_ -> Some (t, s, float_of_string m, int_of_string y)
                                            | _ -> None
                                            ;;

to:

let my_csv_parser () : my_type option =
                       let line = read_line() in
  match Str.split (Str.regexp "[,\n\r\t]") line with      (* this line changed | to , *)
                       | t::s::m::y::_ -> Some (t, s, float_of_string m, int_of_string y)
                       | _ -> None
                       ;;

                       Of course, if the number of items on a line changes, just change the pattern here:

                       | t::s::m::y::_ -> ...

                       to something else.  (See, for example, the difference between parse_movie and
                           parse_studio.)

                       Once again, if you are playing around, don't change the intended functionality of the
                       script for the other parts of the assignment -- just add functionality.
