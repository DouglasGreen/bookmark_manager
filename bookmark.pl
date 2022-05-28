/** <bookmark> A bookmark manager.

@author Douglas S. Green
@license GPL
*/
module(bookmark,
    [
        cats/1,
        delete_url/1,
        delete_urls_in_cat/1,
        normal_url/2,
        open_random_url/0,
        open_random_url/1,
        open_url/1,
        open_url/2,
        open_url_cat,
        open_url_by_search/1,
        open_urls_in_cat/1,
        print_all_url_cats/0,
        print_cats/0,
        print_url_hits/1,
        print_url_search/1,
        print_urls_in_cat/1,
        print_urls_in_cat/2,
        remove_dupes/0,
        rename_cat/2,
        save_file/0,
        search_word_urls/1,
        set_url_cat/2,
        set_cat_by_search/1,
        set_cat_by_search/2,
        url_cat/2,
        url_domain/2
    ]).

:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(url).

:- dynamic last_url/1.

cats(Cats) :-
    findall(Cat, url(_, _, _, _, Cat, _, _), CatDupes),
    sort(CatDupes, Cats).

delete_url(URL) :-
    must_be(string, URL),
    url(URL, _, _, _, _, _, _),
    retractall(url(URL, _, _, _, _, _, _)).

delete_urls_in_cat(Cat) :-
    must_be(atom, Cat),
    setof(URL0, url_cat(URL0, Cat), URLs),
    member(URL, URLs),
    format("Delete URL? ~w\n", [URL]),
    open_url(URL),
    read_user_char(Low),
    (
        Low = 'y', delete_url(URL);
        Low \= 'y'
    ),
    fail.

normal_url(URL, NormalURL) :-
    uri_normalized(URL, URI),
    uri_components(URI, uri_components(Scheme, Authority, OldPath, Search, Fragment)),
    (
        OldPath = '/',
        NewPath = '',
        !;
        NewPath = OldPath
    ),
    uri_components(NormalURLAtom, uri_components(Scheme, Authority, NewPath, Search, Fragment)),
    atom_string(NormalURLAtom, NormalURL).

open_url(URL) :-
    open_url(URL, _).

open_url(URL, FinalURL) :-
    must_be(string, URL),
    catch(
        http_open(URL, In, [status_code(Code), final_url(FinalURLAtom)]),
        Error,
        (
            term_string(Error, ErrorString),
            writeln(ErrorString),
            update_url(URL, URL, -1, ErrorString, "no"),
            last_url_seen(URL),
            www_open_url(URL),
            !,
            fail
        )
    ),
    call_cleanup(
        load_html(In, DOM, []),
        close(In)
    ),
    (
        xpath(DOM, //title, element(title, _, [TitleAtom])),
        !;
        TitleAtom = 'no'
    ),
    (
        xpath(DOM, //meta(@name=description), element(meta, DescAttribs, _)),
        get_attrib(content, DescAttribs, DescriptionAtom),
        !;
        DescriptionAtom = 'no'
    ),
    normalize_space(string(Title), TitleAtom),
    normalize_space(string(Description), DescriptionAtom),
    atom_string(FinalURLAtom, FinalURL),
    (
        url_cat(FinalURL, Cat), !;
        Cat = no
    ),
    format("URL: ~w\nCategory: ~w\nTitle: ~w\n Description: ~w\n", [FinalURL, Cat, Title, Description]),
    update_url(URL, FinalURL, Code, Title, Description),
    last_url_seen(FinalURL),
    www_open_url(FinalURL).

open_url_cat(URL, Cat) :-
    open_url(URL, FinalURL),
    set_url_cat(FinalURL, Cat).

open_random_url :-
    findall(URL, url(URL, _, _, _, _, _, _), URLs),
    random_permutation(URLs, Shuffled),
    member(URL, Shuffled),
    format("Trying ~w...\n", [URL]),
    open_url(URL).

open_random_url(Cat) :-
    must_be(atom, Cat),
    findall(URL, url(URL, _, _, _, Cat, _, _), URLs),
    random_permutation(URLs, Shuffled),
    member(URL, Shuffled),
    format("Trying ~w...\n", [URL]),
    open_url(URL).

open_url_by_search(Word) :-
    must_be(string, Word),
    search_word_urls(Word, URLs),
    length(URLs, Length),
    (
        Length > 1, writeln("Multiple matches:"), print_url_search(Word), !;
        Length = 0, writeln("No matches"), !;
        URLs = [URL], open_url(URL)
    ).

open_urls_in_cat(Cat) :-
    must_be(atom, Cat),
    setof(URL0, url_cat(URL0, Cat), URLs),
    random_permutation(URLs, Shuffled),
    member(URL, Shuffled),
    format("Open URL? ~w\n", [URL]),
    read_user_char(Low),
    (
        Low = 'y', open_url(URL);
        Low \= 'y'
    ),
    fail.

print_all_url_cats :-
    cats(Cats),
    member(Cat, Cats),
    format("\n~w:\n", [Cat]),
    print_urls_in_cat(Cat),
    nl,
    fail.

print_cats :-
    cats(Cats),
    member(Cat, Cats),
    setof(URL, url_cat(URL, Cat), URLs),
    length(URLs, Length),
    format("~w (~d URLs)\n", [Cat, Length]),
    fail.

print_urls_in_cat(Cat) :-
    must_be(atom, Cat),
    setof(URL, url_cat(URL, Cat), URLs),
    member(URL, URLs),
    url(URL, Count, _, _, _, Title, _),
    format("~w (Hits=~d, Title=~w)\n", [URL, Count, Title]),
    fail.

print_urls_in_cat(Cat, Limit) :-
    must_be(atom, Cat),
    must_be(integer, Limit),
    setof(URL, url_cat(URL, Cat), URLs),
    length(URLs, URLCount),
    ActualLimit is min(Limit, URLCount),
    length(List, ActualLimit),
    append(List, _, URLs),
    member(URL, List),
    writeln(URL),
    fail.


print_url_hits(Floor) :-
    must_be(integer, Floor),
    url(URL, Count, _, date(Y, M, D), _, _, _),
    Count >= Floor,
    format("~w - ~d hits as of ~d-~d-~d\n", [URL, Count, Y, M, D]),
    fail.

print_url_search(Word) :-
    must_be(string, Word),
    search_word_urls(Word, URLs),
    sort(URLs, Sorted),
    member(URL, Sorted),
    url_cat(URL, Cat),
    format("~w (~w)\n", [URL, Cat]),
    fail.

remove_dupes :-
    url(URL, Count1, Code1, Date1, Cat1, Title1, Description1),
    url(URL, Count2, _, Date2, Cat2, _, _),
    [Count1, Date1, Cat1] \= [Count2, Date2, Cat2],
    writeln(URL),
    delete_url(URL),
    NewCount is Count1 + Count2,
    assertz(url(URL, NewCount, Code1, Date1, Cat1, Title1, Description1)),
    fail.

remove_dupes.

rename_cat(OldName, NewName) :-
    must_be(atom, OldName),
    must_be(atom, NewName),
    retract(url(URL, Count, Code, Date, OldName, Title, Description)),
    assertz(url(URL, Count, Code, Date, NewName, Title, Description)),
    fail.

rename_cat(_, _) :-
    true.

save_file :-
    tell('url.pl'),
    writeln(":- module(url, [url/7])."),
    nl,
    writeln("% url(URL, Count, Code, Date, Cat, Title, Description)."),
    nl,
    listing(url),
    told.

search_word_urls(Word, URLs) :-
    must_be(string, Word),
    must_be(var, URLs),
    setof(URL, Description^Title^Cat^Word^search_word(Word, URL, Cat, Title, Description), URLs).

set_cat_by_search(Cat) :-
    must_be(atom, Cat),
    atom_string(Cat, Term),
    set_cat_by_search(Cat, Term).

set_cat_by_search(Cat, Term) :-
    must_be(atom, Cat),
    must_be(string, Term),
    writeln("Type Y to set, L to look up, or N to skip."),
    search_word_urls(Term, URLs),
    member(URL, URLs),
    url_cat(URL, no),
    format("~w?\n", [URL]),
    set_cat_by_search_check(URL, Cat),
    fail.

set_url_cat(URL, Cat) :-
    must_be(string, URL),
    must_be(atom, Cat),
    retract(url(URL, Count, Code, Date, _, Title, Description)),
    assertz(url(URL, Count, Code, Date, Cat, Title, Description)).

url_cat(URL, Cat) :-
    url(URL, _, _, _, Cat, _, _).

url_domain(URL, Domain) :-
    url(URL, _, _, _, _, _, _),
    uri_components(URL, uri_components(_, DomainAtom, _, _, _)),
    atom_string(DomainAtom, Domain).

get_attrib(_, [], no) :-
    !.

get_attrib(Name, [Name=Value|_], Value) :-
    !.

get_attrib(Name, [_|Attribs], Value) :-
    get_attrib(Name, Attribs, Value).

last_url_seen(URL) :-
    retractall(last_url(_)),
    assertz(last_url(URL)).

read_user_char(Low) :-
    get_single_char(Code),
    char_code(Char, Code),
    downcase_atom(Char, Low).

search_word(Word, URL, Cat, Title, Description) :-
    must_be(string, Word),
    url(URL, _, _, _, Cat, Title, Description),
    string_lower(URL, LowURL),
    string_lower(Title, LowTitle),
    string_lower(Description, LowDescription),
    string_lower(Word, LowWord),
    atomics_to_string([LowURL, Cat, LowTitle, LowDescription], ' | ', LowString),
    sub_string(LowString, _, _, _, LowWord).

set_cat_by_search_check(URL, Cat) :-
    read_user_char(Low),
    (
        % Yes, set the cat.
        Low = 'y',
        set_url_cat(URL, Cat),
        !;
        % Not sure, so look up the URL and ask again.
        Low = 'l',
        open_url(URL),
        set_cat_by_search_check(URL, Cat),
        !;
        true
    ).

update_url(URL, FinalURL, Code, Title, Description) :-
    must_be(string, URL),
    must_be(string, FinalURL),
    must_be(integer, Code),
    must_be(string, Title),
    must_be(string, Description),
    (
        url(URL, OldCount, _, _, Cat, _, _),
        NewCount is OldCount + 1,
        !;
        NewCount is 1,
        Cat = no
    ),
    date(Date),
    normal_url(FinalURL, NormalURL),
    !,
    (
        % Delete variations with or without trailing slash.
        member(OldURL, [URL, NormalURL, FinalURL]),
        delete_url(OldURL),
        string_concat(OldURL, "/", OldURLWithSlash),
        delete_url(OldURLWithSlash),
        fail,
        !;
        true
    ),
    assertz(url(NormalURL, NewCount, Code, Date, Cat, Title, Description)).
