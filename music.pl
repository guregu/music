:- module(music, [
	semitone/2,
	enharmonic/2,
	interval/2,
	scale_intervals/2,
	scale/3,
	scale_semitones/3,
	pitch_string/2,
	pitch_accidental/2,
	op(201, yf, ♭),
	op(201, yf, ♯),
	op(201, yf, ♮),
	op(201, yf, 𝄫),
	op(201, yf, 𝄪)
]).

:- set_prolog_flag(double_quotes, chars).

:- op(201, yf, ♭). % flat
:- op(201, yf, ♯). % sharp
:- op(201, yf, ♮). % natural
:- op(201, yf, 𝄫). % double flat
:- op(201, yf, 𝄪). % double sharp

degree_step(a, b).
degree_step(b, c).
degree_step(c, d).
degree_step(d, e).
degree_step(e, f).
degree_step(f, g).
degree_step(g, a).

semitone(pitch(a, natural), 0).
semitone(pitch(a, sharp), 1).
semitone(pitch(b, flat), 1).
semitone(pitch(b, natural), 2).
semitone(pitch(c, flat), 2).
semitone(pitch(b, sharp), 3).
semitone(pitch(c, natural), 3).
semitone(pitch(c, sharp), 4).
semitone(pitch(d, flat), 4).
semitone(pitch(d, natural), 5).
semitone(pitch(d, sharp), 6).
semitone(pitch(e, flat), 6).
semitone(pitch(e, natural), 7).
semitone(pitch(f, flat), 7).
semitone(pitch(e, sharp), 8).
semitone(pitch(f, natural), 8).
semitone(pitch(f, sharp), 9).
semitone(pitch(g, flat), 9).
semitone(pitch(g, natural), 10).
semitone(pitch(g, sharp), 11).
semitone(pitch(a, flat), 11).

semitone(pitch(X, double_sharp), N) :-
	semitone(pitch(X, natural), N0),
	plus(N0, 2, N).
semitone(pitch(X, double_flat), N) :-
	semitone(pitch(X, natural), N0),
	N is (N0-2) mod 12.

semitone(X, N) :-
	integer(N),
	\+between(0, 11, N),
	Semi is N mod 12,
	semitone(X, Semi).

enharmonic(N1, N2) :-
	semitone(N1, S),
	semitone(N2, S).

interval(unison, 0).
interval(half, 1).
interval(second(minor), 1).
interval(whole, 2).
interval(second(major), 2).
interval(third(minor), 3).
interval(third(major), 4).
interval(fourth(diminished), 4).
interval(fourth(perfect), 5).
interval(fourth(augmented), 6).
interval(fifth(diminished), 6).
interval(fifth(perfect), 7).
interval(sixth(diminished), 7).
interval(fifth(augmented), 8).
interval(sixth(minor), 8).
interval(sixth(major), 9).
interval(seventh(diminished), 9).
interval(sixth(augmented), 10).
interval(seventh(minor), 10).
interval(seventh(major), 11).
interval(octave(diminished), 11).
interval(octave, 12).
interval(seventh(augmented), 12).

step(Interval, S0, S) :-
	interval(Interval, Difference),
	plus(S0, Difference, S).

% https://en.wikipedia.org/wiki/Diatonic_scale#Modes
scale_intervals(major, [whole, whole, half, whole, whole, whole, half]).
scale_intervals(minor, [whole, half, whole, whole, half, whole, whole]).
scale_intervals(ionian, X) :- scale_intervals(major, X).
scale_intervals(aeolian, X) :- scale_intervals(minor, X).
scale_intervals(dorian, [whole, half, whole, whole, whole, half, whole]).
scale_intervals(phrygian, [half, whole, whole, whole, half, whole, whole]).
scale_intervals(lydian, [whole, whole, whole, half, whole, whole, half]).
scale_intervals(mixolydian, [whole, whole, half, whole, whole, half, whole]).
scale_intervals(locrian, [half, whole, whole, half, whole, whole, whole]).

scale_semitones(Root, Mode, [Root|Semis]) :-
	scale_intervals(Mode, Steps),
	scale_step(Root, Steps, Semis).

scale_step(From, [Step|Ss], [To|Ts]) :-
	step(Step, From, To),
	scale_step(To, Ss, Ts).
scale_step(_, [], []).

scale(Pitch, Mode, Notes) :-
	semitone(Pitch, Semi),
	scale_semitones(Semi, Mode, Ss),
	label_scale(Pitch, Ss, Notes).

label_scale(pitch(Degree, Accidental), [Semi|Rest], [pitch(Degree, Accidental)|Ns]) :-
	semitone(pitch(Degree, Accidental), Semi),
	degree_step(Degree, Next),
	label_scale(pitch(Next, _), Rest, Ns).
label_scale(_, [], []).

pitch_string(pitch(X, natural), [Y]) :- atom_upper(X, Y).
pitch_string(pitch(X, flat), [Y, ♭]) :- atom_upper(X, Y).
pitch_string(pitch(X, sharp), [Y, ♯]) :- atom_upper(X, Y).
pitch_string(pitch(X, double_flat), [Y, 𝄫]) :- atom_upper(X, Y).
pitch_string(pitch(X, double_sharp), [Y, 𝄪]) :- atom_upper(X, Y).

pitch_accidental(pitch(X, natural), X♮).
pitch_accidental(pitch(X, flat), X♭).
pitch_accidental(pitch(X, sharp), X♯).
pitch_accidental(pitch(X, double_flat), X𝄫).
pitch_accidental(pitch(X, double_sharp), X𝄪).

print_tones(Tones) :-
	maplist(semitone, Tones, Pitches),
	maplist(pitch_string, Pitches, Strings),
	write(Strings).

print_pitches(Pitches) :-
	maplist(pitch_string, Pitches, Strings),
	write(Strings), nl, !.

print_scale(Root, Mode) :-
	scale(Root, Mode, Scale),
	maplist(pitch_string, Scale, Strings),
	write(Strings).

:- if(current_prolog_flag(dialect, swi)).

atom_upper(X, Y) :- upcase_atom(X, Y).

:- endif.
