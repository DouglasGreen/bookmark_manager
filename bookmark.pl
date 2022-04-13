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
        lookup_url/2,
        lookup_url_by_word/1,
        print_categories/0,
        print_category/1,
        print_url_categories/0,
        print_url_search/1,
        save_file/0,
        search_word_urls/1,
        set_category_by_search/1,
        set_category_by_search/2,
        url_category/2
    ]).

:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(url).

:- dynamic last_url/1.

categories(Categories) :-
    findall(Category, url(_, _, _, _, Category, _, _), CatDupes),
    sort(CatDupes, Categories).

delete_url(URL) :-
    \+ var(URL),
    retractall(url(URL, _, _, _, _, _, _)).

lookup_url(URL) :-
        catch(
            http_open(URL, In, [status_code(Code), final_url(FinalURL)]),
            Error,
            (
                term_string(Error, ErrorString),
                writeln(ErrorString),
                update_url(URL, URL, -1, ErrorString, no),
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
            Title = no
        ),
        (
            xpath(DOM, //meta(@name=description), element(meta, DescAttribs, _)),
            get_attrib(content, DescAttribs, DescriptionAtom),
            !;
            Description = no
        ),
        atom_string(TitleAtom, Title),
        atom_string(DescriptionAtom, Description),
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
    findall(URL, url(URL, _, _, _, Category, _, _), URLs),
    random_permutation(URLs, Shuffled),
    member(URL, Shuffled),
    format("Trying ~w...\n", [URL]),
    lookup_url(URL).

lookup_url_by_word(Word) :-
    search_word_urls(Word, URLs),
    length(URLs, Length),
    (
        Length > 1, writeln("Multiple matches:"), print_url_search(Word), !;
        Length = 0, writeln("No matches"), !;
        URLs = [URL], lookup_url(URL)
    ).

print_categories :-
    categories(Categories),
    member(Category, Categories),
    writeln(Category),
    fail.

print_category(Category) :-
    setof(URL, url_category(URL, Category), URLs),
    member(URL, URLs),
    writeln(URL),
    fail.

print_url_categories :-
    categories(Categories),
    member(Category, Categories),
    format("\n~w:\n", [Category]),
    print_category(Category),
    nl,
    fail.

print_url_search(Word) :-
    search_word_urls(Word, URLs),
    sort(URLs, Sorted),
    member(URL, Sorted),
    writeln(URL),
    fail.

save_file :-
    tell('url.pl'),
    writeln(":- module(url, [url/7])."),
    nl,
    writeln("% url(URL, Count, Code, Date, Category, Title, Description)."),
    nl,
    listing(url),
    told.

search_word_urls(Word, URLs) :-
    setof(URL, Description^Title^Category^Word^search_word(Word, URL, Category, Title, Description), URLs).

set_category(URL, Category) :-
    retract(url(URL, Count, Code, Date, _, Title, Description)),
    assertz(url(URL, Count, Code, Date, Category, Title, Description)).

set_category_by_search(Category) :-
    set_category_by_search(Category, Category).

set_category_by_search(Category, Term) :-
    search_word_urls(Term, URLs),
    member(URL, URLs),
    url_category(URL, no),
    writeln(URL),
    set_category(URL, Category),
    fail.

url_category(URL, Category) :-
    url(URL, _, _, _, Category, _, _).

get_attrib(_, [], no) :-
    !.

get_attrib(Name, [Name=Value|_], Value) :-
    !.

get_attrib(Name, [_|Attribs], Value) :-
    get_attrib(Name, Attribs, Value).

get_domain(URL, Domain) :-
    uri_components(URL, uri_components(Scheme, Authority, _, _, _)),
    atomic_list_concat([Scheme, '://', Authority], Domain).

last_url_seen(URL) :-
    retractall(last_url(_)),
    assertz(last_url(URL)).

search_word(Word, URL, Category, Title, Description) :-
    url(URL, _, _, _, Category, Title, Description),
    string_lower(URL, LowURL),
    string_lower(Title, LowTitle),
    string_lower(Description, LowDescription),
    string_lower(Word, LowWord),
    atomics_to_string([LowURL, Category, LowTitle, LowDescription], ' | ', LowString),
    sub_string(LowString, _, _, _, LowWord).

update_url(URL, FinalURL, Code, Title, Description) :-
    \+ var(URL),
    (
        url(URL, OldCount, _, _, Category, _, _),
        NewCount is OldCount + 1,
        !;
        NewCount is 1,
        Category = no
    ),
    date(Date),
    (
        % Delete variations with or without trailing slash.
        delete_url(URL),
        string_concat(URL, "/", URL1),
        delete_url(URL1),
        string_concat(URL2, "/", URL),
        delete_url(URL2),
        !;
        true
    ),
    assertz(url(FinalURL, NewCount, Code, Date, Category, Title, Description)).
