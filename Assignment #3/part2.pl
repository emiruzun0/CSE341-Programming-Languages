% knowledge base
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

%facts..
distance(istanbul,rize,967.79).
distance(istanbul,van,1262.37).
distance(istanbul,ankara,351.50).
distance(istanbul,antalya,482.75).
distance(istanbul,izmir,328.80).
distance(istanbul,gaziantep,847.42).
distance(rize,van,373.01).
distance(rize,istanbul,967.79).
distance(van,ankara,920.31).
distance(van,rize,373.01).
distance(van,istanbul,1262.37).
distance(ankara,konya,227.34).
distance(ankara,van,920.31).
distance(ankara,istanbul,351.50).
distance(gaziantep,istanbul,847.42).
distance(gaziantep,antalya,592.33).
distance(konya,antalya,192.28).
distance(konya,ankara,227.34).
distance(antalya,konya,192.28).
distance(antalya,gaziantep,592.33).
distance(antalya,istanbul,482.75).
distance(izmir,istanbul,382.80).
distance(izmir,ısparta,308.55).
distance(ısparta,izmir,308.55).
distance(ısparta,burdur,24.60).
distance(burdur,ısparta,24.60).
distance(edirne,edremit,235.33).
distance(edremit,edirne,235.33).
distance(edremit,erzincan,1066.26).
distance(erzincan,edremit,1066.26).


%rules..
sroute(X,Y,D) :- flight(X,Y),                                   %There are 2 input for sroute.
                 distance(X,Y,D).                               %First one is flight X and Y must be and the other one is distance X and Y must be in file.
sroute(X,Z,D) :- flight(X,Y),                                   %Between X and Z, X-Y and Y-Z flight must be and also distance for these cities must be in there.
		 flight(Y,Z),                                   %Total distance is sum of them 2 distances. 
		 distance(X,Y,D1),
		 distance(Y,Z,D2),
		 D is D1 + D2. 
sroute(X,T,D) :- flight(X,Y),                                   %For X and T cities, the route is X-Y-Z-T. So the flights must be in file and also
                 flight(Y,Z),                                   % these distances must be in there.
                 flight(Z,T),                                   %Total distance is sum of these 3 distances.
                 distance(X,Y,D1),
                 distance(Y,Z,D2),
                 distance(Z,T,D3),
                 D is D1 + D2 + D3.
