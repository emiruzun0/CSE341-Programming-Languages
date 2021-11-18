%facts..
when(102, 10).					%Time of the course X is Y
when(108, 12).
when(341, 14).
when(455, 16).
when(452, 17).
where(102, z23).				%Place of the course X is Y
where(108, z11).
where(341, z06).
where(455, 207).
where(452, 207).
enroll(a,102).					%Student X is enrolled in course Y
enroll(a,108).
enroll(b,102).
enroll(c,108).
enroll(d,341).
enroll(e,455).

%rules..

schedule(X, R, T) :- when(C,T), enroll(X, C), where(C, R).	%Student X must be in course C, Class C time is T, Class C must be in room R
usage(R, T) :- where(C,R), when(C,T).						%Class C must be in room R and also it must be at time T
conflict(X,Y):- (when(X,T1), when(Y,T2), T1==T2); (where(X,P1), where(Y,P2), P1==P2).		%Course X and Y must be in same time or must be in same room
meet(X,Y):- enroll(X,C1), enroll(Y,C2), C1==C2,!.			% Student X and Y enroll same course C