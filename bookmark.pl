/** <bookmark> A bookmark manager.

@author Douglas S. Green
@license GPL
*/
module(bookmark,
    [
        categories/1,
        delete_url/1,
        lookup_random_url/0,
        lookup_random_url/1,
        lookup_url/1,
        lookup_url_by_search/1,
        lookup_urls_in_category/1,
        normal_url/2,
        print_all_url_categories/0,
        print_categories/0,
        print_url_hits/1,
        print_url_search/1,
        print_urls_in_category/1,
        remove_dupes/0,
        rename_category/2,
        save_file/0,
        search_word_urls/1,
        set_url_category/2,
        set_category_by_search/1,
        set_category_by_search/2,
        url_category/2,
        url_domain/2
    ]).

:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(url).

:- dynamic last_url/1.

categories(Categories) :-
    findall(Category, url(_, _, _, _, Category, _, _), CatDupes),
    sort(CatDupes, Categories).

delete_url(URL) :-
    must_be(string, URL),
    \+ var(URL),
    retractall(url(URL, _, _, _, _, _, _)).

lookup_url(URL) :-
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
        Title = "no"
    ),
    (
        xpath(DOM, //meta(@name=description), element(meta, DescAttribs, _)),
        get_attrib(content, DescAttribs, DescriptionAtom),
        !;
        Description = "no"
    ),
    atom_string(TitleAtom, Title),
    atom_string(DescriptionAtom, Description),
    atom_string(FinalURLAtom, FinalURL),
    writeln(FinalURL),
    writeln(Title),
    writeln(Description),
    update_url(URL, FinalURL, Code, Title, Description),
    last_url_seen(FinalURL),
    www_open_url(FinalURL).

lookup_random_url :-
    findall(URL, url(URL, _, _, _, _, _, _), URLs),
    random_permutation(URLs, Shuffled),
    member(URL, Shuffled),
    format("Trying ~w...\n", [URL]),
    lookup_url(URL).

lookup_random_url(Category) :-
    must_be(atom, Category),
    findall(URL, url(URL, _, _, _, Category, _, _), URLs),
    random_permutation(URLs, Shuffled),
    member(URL, Shuffled),
    format("Trying ~w...\n", [URL]),
    lookup_url(URL).

lookup_url_by_search(Word) :-
    must_be(string, Word),
    search_word_urls(Word, URLs),
    length(URLs, Length),
    (
        Length > 1, writeln("Multiple matches:"), print_url_search(Word), !;
        Length = 0, writeln("No matches"), !;
        URLs = [URL], lookup_url(URL)
    ).

lookup_urls_in_category(Category) :-
    must_be(atom, Category),
    url_category(URL, Category),
    lookup_url(URL),
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

print_all_url_categories :-
    categories(Categories),
    member(Category, Categories),
    format("\n~w:\n", [Category]),
    print_urls_in_category(Category),
    nl,
    fail.

print_categories :-
    categories(Categories),
    member(Category, Categories),
    writeln(Category),
    fail.

print_urls_in_category(Category) :-
    must_be(atom, Category),
    setof(URL, url_category(URL, Category), URLs),
    member(URL, URLs),
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
    url_category(URL, Category),
    format("~w (~w)\n", [URL, Category]),
    fail.

remove_dupes :-
    url(URL, Count1, Code1, Date1, Category1, Title1, Description1),
    url(URL, Count2, _, Date2, Category2, _, _),
    [Count1, Date1, Category1] \= [Count2, Date2, Category2],
    writeln(URL),
    delete_url(URL),
    NewCount is Count1 + Count2,
    assertz(url(URL, NewCount, Code1, Date1, Category1, Title1, Description1)),
    fail.

remove_dupes.

rename_category(OldName, NewName) :-
    must_be(atom, OldName),
    must_be(atom, NewName),
    retract(url(URL, Count, Code, Date, OldName, Title, Description)),
    assertz(url(URL, Count, Code, Date, NewName, Title, Description)),
    fail.

rename_category(_, _) :-
    true.

save_file :-
    tell('url.pl'),
    writeln(":- module(url, [url/7])."),
    nl,
    writeln("% url(URL, Count, Code, Date, Category, Title, Description)."),
    nl,
    listing(url),
    told.

search_word_urls(Word, URLs) :-
    must_be(string, Word),
    must_be(var, URLs),
    setof(URL, Description^Title^Category^Word^search_word(Word, URL, Category, Title, Description), URLs).

set_category_by_search(Category) :-
    must_be(atom, Category),
    atom_string(Category, Term),
    set_category_by_search(Category, Term).

set_category_by_search(Category, Term) :-
    must_be(atom, Category),
    must_be(string, Term),
    writeln("Type Y to set, L to look up, or N to skip."),
    search_word_urls(Term, URLs),
    member(URL, URLs),
    url_category(URL, no),
    format("~w?\n", [URL]),
    set_category_by_search_check(URL, Category),
    fail.

set_url_category(URL, Category) :-
    must_be(string, URL),
    must_be(atom, Category),
    retract(url(URL, Count, Code, Date, _, Title, Description)),
    assertz(url(URL, Count, Code, Date, Category, Title, Description)).

url_category(URL, Category) :-
    url(URL, _, _, _, Category, _, _).

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

search_word(Word, URL, Category, Title, Description) :-
    must_be(string, Word),
    url(URL, _, _, _, Category, Title, Description),
    string_lower(URL, LowURL),
    string_lower(Title, LowTitle),
    string_lower(Description, LowDescription),
    string_lower(Word, LowWord),
    atomics_to_string([LowURL, Category, LowTitle, LowDescription], ' | ', LowString),
    sub_string(LowString, _, _, _, LowWord).

set_category_by_search_check(URL, Category) :-
    read_user_char(Low),
    (
        % Yes, set the category.
        Low = 'y',
        set_url_category(URL, Category),
        !;
        % Not sure, so look up the URL and ask again.
        Low = 'l',
        lookup_url(URL),
        set_category_by_search_check(URL, Category),
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
        url(URL, OldCount, _, _, Category, _, _),
        NewCount is OldCount + 1,
        !;
        NewCount is 1,
        Category = no
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
    assertz(url(NormalURL, NewCount, Code, Date, Category, Title, Description)).
