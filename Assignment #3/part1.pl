%knowledge base

flight(istanbul,rize).
flight(istanbul,van).
flight(istanbul,ankara).
flight(istanbul,antalya).
flight(istanbul,izmir).
flight(istanbul,gaziantep).
flight(rize,van).
flight(rize,istanbul).
flight(van,ankara).
flight(van,rize).
flight(van,istanbul).
flight(ankara,konya).
flight(ankara,van).
flight(ankara,istanbul).
flight(gaziantep,istanbul).
flight(gaziantep,antalya).
flight(konya,antalya).
flight(konya,ankara).
flight(antalya,konya).
flight(antalya,gaziantep).
flight(antalya,istanbul).
flight(izmir,istanbul).
flight(izmir,ısparta).
flight(ısparta,izmir).
flight(ısparta,burdur).
flight(burdur,ısparta).
flight(edirne,edremit).
flight(edremit,edirne).
flight(edremit,erzincan).
flight(erzincan,edremit).

%rules
route(X, Z) :- route(X, Z, []).			%send the second method.
route(X, Z, MarkedCities) :- flight(X, Y), \+ member(Y, MarkedCities)		%There are 3 rules. These are, flight between X and Z must be, Z must not be the member of list,
						, (Z = Y; route(Z, Y, [X | MarkedCities])).			%Z can be Y or route Z and Y can be (X added the list)