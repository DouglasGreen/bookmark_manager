/** <bookmark> A bookmark manager.

@author Douglas S. Green
@license GPL
*/
module(bookmark,
    [
        lookup/1,
        random_lookup/0,
        save/0
    ]).

:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(url).

:- dynamic last_url/1.

delete_url(URL) :-
    \+ var(URL),
    retractall(url(URL, _, _, _, _, _)).

lookup(URL) :-
        catch(
            http_open(URL, In, [status_code(Code), final_url(FinalURL)]),
            Error,
            (
                term_string(Error, ErrorString),
                writeln(ErrorString),
                update(URL, URL, -1, ErrorString, no),
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
        update(URL, FinalURL, Code, Title, Description),
        last_url_seen(FinalURL),
        www_open_url(FinalURL).

random_lookup :-
    findall(URL, url(URL, _, _, _, _, _), URLs),
    random_permutation(URLs, Shuffled),
    member(URL, Shuffled),
    format("Trying ~w...\n", [URL]),
    lookup(URL).

save :-
    tell('url.pl'),
    writeln(":- module(url, [url/6])."),
    nl,
    writeln("% url(URL, Count, Code, Date, Title, Description)."),
    nl,
    listing(url),
    told.

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

update(URL, FinalURL, Code, Title, Description) :-
    \+ var(URL),
    (
        url(URL, OldCount, _, _, _, _),
        NewCount is OldCount + 1,
        !;
        NewCount is 1
    ),
    date(Date),
    (
        retractall(url(URL, _, _, _, _, _)),
        !;
        true
    ),
    assertz(url(FinalURL, NewCount, Code, Date, Title, Description)).
