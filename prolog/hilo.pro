% Simple high/low guessing game to test user input, looping and counters
% Ensure you put in the values as 3.
hilo :- random(1, 10, X), hilo(X).
hilo(X) :- writeln('Guess a number between 1 and 10: '), read(Y), hilo(X, Y, 1).
hilo(X,Z) :- writeln('Guess a number between 1 and 10: '), read(Y), Z1 is Z+1, hilo(X, Y, Z1).
hilo(X,Y,Z) :- Y > X, writeln('Too high!'), hilo(X,Z).
hilo(X,Y,Z) :- Y < X, writeln('Too low!'), hilo(X,Z).
hilo(X,Y,Z) :- Y == X, writeln('You win!'), nl, format('It took you ~w guesses', [Z]).
