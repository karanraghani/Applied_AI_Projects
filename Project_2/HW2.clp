(clear)
(reset)
(import nrc.fuzzy.*)
(import nrc.fuzzy.jess.*)

; Building on previous use case where we estimated the price of the player based on some 
; of the key attributes, we will now add a layer of fuzzy logic that will help in determining rating of the player 
; to a more accurate score. Based on that we can evaluate the price better and tell if player could be good fit or not
; for the team

; Template for a footballer
(deftemplate footballer 
    (slot name)
    (slot age (type INTEGER))
    (slot pace (type INTEGER))
    (slot position (allowed-values DEF MID FOR GK))
    (slot league (type INTEGER))
    (slot appearances (type INTEGER))
    (slot squad-status (type INTEGER))
    (slot division (type INTEGER)))

;template for a goalkeeper 
(deftemplate goalkeeper
    (slot clean-sheets)
    (slot reflex (type INTEGER))
    (slot pace (type INTEGER))
    (slot handling (type INTEGER))
    (slot kicking (type INTEGER)))

; template for a defender
(deftemplate defender
    (slot interception (type INTEGER))
    (slot pace (type INTEGER))
    (slot tackle (type INTEGER))
    (slot clearance (type INTEGER))
    (slot marking (type INTEGER)))

;template for a midfielder 
(deftemplate midfielder
    (slot pace (type INTEGER))
    (slot assists (type INTEGER))
    (slot dribble (type INTEGER))
    (slot passing(type INTEGER)))

; template for a forward
(deftemplate forward
    (slot goals (type INTEGER))
    (slot pace (type INTEGER))
    (slot shooting (type INTEGER)))


; Storing all the properties of the player in global variable
(defglobal
    ?*name* = nil
    ;?*age* = nil; converted into 4 categories young, learning, peak, mature and old
    ?*position* = nil
    ?*league* = nil
    ?*appearances* = nil ; converted to high mid low
    ?*squad-status* = nil ; converted into fuzzy value
    ?*division* = nil
    ?*clean-sheets* = nil
    ?*handling* = nil
    ?*reflex* = nil
    ?*kicking* = nil
    ?*interception* = nil
    ?*tackle* = nil
    ?*clearance* = nil
    ?*marking* = nil
    ?*pace* = nil ; converting pace into fuzzy value
    ?*assists* = nil
    ?*dribble* = nil
    ?*passing* = nil
    ?*goals* = nil
    ?*shooting* = nil
	?*rating-final* = nil
    ?*estimated-price* = 0
    ?*count* = 0
    )

(defglobal ?*appearance* = (new nrc.fuzzy.FuzzyVariable "appearance" 0.0 999 "matches"))
(defglobal ?*age* = (new nrc.fuzzy.FuzzyVariable "age" 0 99 "years"))
(defglobal ?*cost* = (new nrc.fuzzy.FuzzyVariable "cost" 0 200 "millions"))
(defglobal ?*status* = (new nrc.fuzzy.FuzzyVariable "squad-status" 0 10 "score"))
(defglobal ?*speed* = (new nrc.fuzzy.FuzzyVariable "speed" 0 100 "score"))



(deffunction enter-defender-props()
    (printout t "Enter interception score: " crlf)
    (bind ?*interception* (read))
    (printout t "Enter tackle score: " crlf)
    (bind ?*tackle* (read))
    (printout t "Enter clearance score: " crlf)
    (bind ?*clearance* (read))
    (printout t "Enter marking score: " crlf)
    (bind ?*marking* (read))
    
    (assert (defender (clearance ?*clearance*)(interception ?*interception*)(marking ?*marking*)
            (tackle ?*tackle*)))
    )

(deffunction enter-midfielder-props()
    ;(printout t "Enter pace score: " crlf)
    ;(bind ?*pace* (read))
    (printout t "Enter assists score: " crlf)
    (bind ?*assists* (read))
    (printout t "Enter dribble score: " crlf)
    (bind ?*dribble* (read))
    (printout t "Enter passing score: " crlf)
    (bind ?*passing* (read))
    
    (assert (midfielder (assists ?*assists*)(dribble ?*dribble*)(passing ?*passing*)))
    )

(deffunction enter-forward-props()
    ;(printout t "Enter pace score: " crlf)
    ;(bind ?*pace* (read))
    (printout t "Enter goals score: " crlf)
    (bind ?*goals* (read))
    (printout t "Enter shooting score: " crlf)
    (bind ?*shooting* (read))
    
    (assert (forward (goals ?*goals*)(shooting ?*shooting*)))
    )

(deffunction enter-goalkeeper-props()
    (printout t "Enter clean-sheets score: " crlf)
    (bind ?*clean-sheets* (read))
    (printout t "Enter reflex score: " crlf)
    (bind ?*reflex* (read))
    (printout t "Enter handling score: " crlf)
    (bind ?*handling* (read))
    (printout t "Enter kicking score: " crlf)
    (bind ?*kicking* (read))
    
    (assert (goalkeeper (clean-sheets ?*clean-sheets*)(handling ?*handling*)
            (kicking ?*kicking*)(reflex ?*reflex*)))
    )


; adding all the terms required for asserting the fuzzy concepts
(defrule init
    (declare (salience 100))
    =>
    ;all the terms are added here
    (load-package nrc.fuzzy.jess.FuzzyFunctions)
    
    (?*appearance* addTerm "low" (new ZFuzzySet 0 50))
    (?*appearance* addTerm "medium" (new TrapezoidFuzzySet 51 55 100 110))
    (?*appearance* addTerm "high" (new SFuzzySet 111 300))
    
    (?*age* addTerm "young" (new ZFuzzySet 16 20))
    (?*age* addTerm "learning-age" (new TrapezoidFuzzySet 18 20 25 27))
    (?*age* addTerm "peak-age" (new TrapezoidFuzzySet 23 25 30 32))
    (?*age* addTerm "mature" (new TrapezoidFuzzySet 28 30 35 37))
    (?*age* addTerm "old" (new SFuzzySet 35 40))
        
    (?*cost* addTerm "cheap" (new ZFuzzySet 0 20))
    (?*cost* addTerm "low" (new TrapezoidFuzzySet 18 20 30 32))
    (?*cost* addTerm "medium" (new TrapezoidFuzzySet 28 30 60 62))
    (?*cost* addTerm "high" (new TrapezoidFuzzySet 58 60 100 102))
    (?*cost* addTerm "very-high" (new SFuzzySet 100 200))
    
    (?*status* addTerm "backup" (new ZFuzzySet 0 2))
    (?*status* addTerm "subs" (new TriangleFuzzySet 3 4 5))
    (?*status* addTerm "starting-team" (new TriangleFuzzySet 6 7 8))
    (?*status* addTerm "indespensible" (new SFuzzySet 9 10))
    
    (?*speed* addTerm "low" (new ZFuzzySet 0 40))
    (?*speed* addTerm "medium" (new TrapezoidFuzzySet 41 50 80 85))
    (?*speed* addTerm "high" (new SFuzzySet 86 100))
    
    (assert(initialization-done))
    )

; Taking input from the user
; 1. Entering name, age and position first

(defrule enter-details
    (initialization-done)
    =>
    (printout t "Enter the footballer's Name: " crlf)
    (bind ?*name* (read))
    (printout t "Enter the footballer's Age: " crlf)
    (bind ?age (read))
    (printout t "Enter the footballer's Position: DEF MID FOR GK " crlf)
    (bind ?*position* (read))
    (printout t "Does the footballer belong to same(1) or different league(0)? " crlf)
    (bind ?*league* (read))
    (printout t "In which division does the team he play for belong?" crlf)
    (bind ?*division* (read))
    (printout t "Enter the footballer's appearances: " crlf)
    (bind ?*appearances* (read))
    (printout t "Is the player important or a backup in his current team, Enter a score from 1(backup) to 10(very imp)" crlf)
    (bind ?*squad-status* (read))
    (printout t "Enter pace score: " crlf)
    (bind ?*pace* (read))
    
    (bind ?player (assert(footballer (name ?*name*)(age ?age)(pace ?*pace*)(position ?*position*)(league ?*league*)
                (division ?*division*)(appearances ?*appearances*)(squad-status ?*squad-status*))))
    
    (if (= (str-compare ?*position* "DEF") 0) then
        (enter-defender-props))
    (if (= (str-compare ?*position* "MID") 0) then
        (enter-midfielder-props))
    (if (= (str-compare ?*position* "FOR") 0) then
        (enter-forward-props))
    (if (= (str-compare ?*position* "GK") 0) then
        (enter-goalkeeper-props))
    
    (assert (theAppearance (new nrc.fuzzy.FuzzyValue ?*appearance* (new SingletonFuzzySet ?*appearances*))))
    (assert (theAge (new nrc.fuzzy.FuzzyValue ?*age* (new SingletonFuzzySet ?age))))
    (assert (theStatus (new nrc.fuzzy.FuzzyValue ?*status* (new SingletonFuzzySet ?*squad-status*))))
    (assert (thePace (new nrc.fuzzy.FuzzyValue ?*speed* (new SingletonFuzzySet ?*pace*))))
    )


; Calculating the rating for a Forward
(deffunction forward-rating()
    (bind ?goalspm (/ ?*goals* ?*appearances*))
        (if (> ?goalspm .45)then 
            (bind ?goalspm 1)
        else(bind ?goalspm 0))
        (if (> ?*pace* 80)then 
            (bind ?pace-rating 1)
        else (bind ?pace-rating 0))
        (if (> ?*shooting* 85)then 
            (bind ?shooting-rating 1)
        else(bind ?shooting-rating 0))
        (bind ?rating (+(* ?goalspm 0.5) (* ?pace-rating 0.25) (* ?shooting-rating 0.25)))
        (return ?rating)
)

; Calculating the rating for a Midfielder
(deffunction midfielder-rating()
    (bind ?assistspm (/ ?*assists* ?*appearances*))
        (if (> ?assistspm .35)then 
            (bind ?assistspm 1)
        else (bind ?assistspm 0))
        (if (> ?*pace* 75)then 
            (bind ?pace-rating 1)
        else (bind ?pace-rating 0))
        (if (> ?*dribble* 75)then 
            (bind ?dribble-rating 1)
        else (bind ?dribble-rating 0))
    	(if (> ?*passing* 80)then 
            (bind ?passing-rating 1)
        else (bind ?passing-rating 0))
        (bind ?rating (+(* ?assistspm 0.40) (* ?pace-rating 0.10) (* ?dribble-rating 0.20)(* ?passing-rating 0.30)))
        (return ?rating)
)

; Calculating the rating for a Defender
(deffunction defender-rating()
        (if (> ?*interception* 85)then 
            (bind ?interception-rating 1)
        else (bind ?interception-rating 0))
        (if (> ?*tackle* 80)then 
            (bind ?tackle-rating 1)
        else (bind ?tackle-rating 0))
        (if (> ?*clearance* 85)then 
            (bind ?clearance-rating 1)
        else (bind ?clearance-rating 0))
    	(if (> ?*marking* 80)then 
            (bind ?marking-rating 1)
        else (bind ?marking-rating 0))
        (bind ?rating (+(* ?interception-rating 0.30) (* ?tackle-rating 0.20) (* ?clearance-rating 0.10)(* ?marking-rating 0.40)))
        (return ?rating)
)

; Calculating the rating for a GK
(deffunction gk-rating()
        (if (> ?*clean-sheets* 75)then 
            (bind ?clean-sheets-rating 1)
        else (bind ?clean-sheets-rating 0))
        (if (> ?*handling* 75)then 
            (bind ?handling-rating 1)
        else (bind ?handling-rating 0))
        (if (> ?*reflex* 80)then 
            (bind ?reflex-rating 1)
        else (bind ?reflex-rating 0))
    	(if (> ?*kicking* 80)then 
            (bind ?kicking-rating 1)
        else (bind ?kicking-rating 0))
        (bind ?rating (+(* ?clean-sheets-rating 0.20) (* ?reflex-rating 0.40) (* ?handling-rating 0.15)(* ?kicking-rating 0.15)))
        (return ?rating)
)


; Forward Rules!!

(defrule forward-cat-1
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "young"))
    ;(footballer{age > 15}) (footballer{age <= 20}) 
 =>
    (if (= (str-compare ?*position* "FOR") 0) then
    	(bind ?rating1 (forward-rating))
        (bind ?rating (* ?rating1 20)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .8)))	
        ;(if (< ?*appearances* 60)then		;appearances low, less value
         ;   (bind ?rating (* ?rating .8)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .7)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1.25)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        ;(printout t "Footballer is young, estimated vale " ?rating "million Euros " ?*estimated-price* crlf)
    )    
)

(defrule forward-cat-2
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "learning-age"))
    ;(footballer{age > 20}) (footballer{age <= 25}) 
 =>
    (if (= (str-compare ?*position* "FOR") 0) then
    	(bind ?rating1 (forward-rating))
        (bind ?rating (* ?rating1 80)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .8)))	
        ;(if (< ?*appearances* 60)then		;appearances low, less value
         ;   (bind ?rating (* ?rating .5)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .5)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
          ;  (bind ?rating (* ?rating 1.25)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        ;(printout t "Footballer is in learning age, estimated vale " ?rating "million Euros " crlf)
    )    
)

(defrule forward-cat-3
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "peak-age"))
    ;(footballer{age > 25}) (footballer{age <= 30}) 
 =>
    (if (= (str-compare ?*position* "FOR") 0) then
    	(bind ?rating1 (forward-rating))
        (bind ?rating (* ?rating1 75)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .9)))	
        ;(if (< ?*appearances* 100)then		;appearances low, less value
         ;   (bind ?rating (* ?rating .8)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .7)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1.25)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        ;(printout t "Footballer is at Peak age, estimated vale " ?rating "million Euros " crlf)
    )    
)

(defrule forward-cat-4
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "mature"))
    ;(footballer{age > 30}) (footballer{age <= 35}) 
 =>
    (if (= (str-compare ?*position* "FOR") 0) then
    	(bind ?rating1 (forward-rating))
        (bind ?rating (* ?rating1 50)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .9)))	
        ;(if (< ?*appearances* 120)then		;appearances low, less value
         ;   (bind ?rating (* ?rating .6)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .5)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1.25)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        ;(printout t "Footballer has Matured, estimated vale " ?rating "million Euros " crlf)
    )    
)

(defrule forward-cat-5
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "old"))
    ;(footballer{age > 35}) 
 =>
    (if (= (str-compare ?*position* "FOR") 0) then
    	(bind ?rating1 (forward-rating))
        (bind ?rating (* ?rating1 30)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .8)))	
        ;(if (< ?*appearances* 160)then		;appearances low, less value
         ;   (bind ?rating (* ?rating .5)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .5)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1.25)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        ;(printout t "Footballer has great experience and strong mentality, estimated vale " ?rating "million Euros " crlf)
    )    
)

; Midfielder Rules
(defrule midfielder-cat-1
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "young"))
    ;(footballer{age > 15}) (footballer{age <= 20}) 
 =>
    (if (= (str-compare ?*position* "MID") 0) then
    	(bind ?rating1 (midfielder-rating))
        (bind ?rating (* ?rating1 20)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .8)))	
        ;(if (< ?*appearances* 60)then		;appearances low, less value
         ;   (bind ?rating (* ?rating .8)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .7)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1.5)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        ;(printout t "midfielder is young, estimated vale " ?rating "million Euros " crlf)
    )    
)

(defrule midfielder-cat-2
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "learning-age"))
    ;(footballer{age > 20}) (footballer{age <= 25}) 
 =>
    (if (= (str-compare ?*position* "MID") 0) then
    	(bind ?rating1 (midfielder-rating))
        (bind ?rating (* ?rating1 70)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .8)))	
        ;(if (< ?*appearances* 60)then		;appearances low, less value
         ;   (bind ?rating (* ?rating .6)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .5)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1.50)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        ;(printout t "midfielder is in learning age, estimated vale " ?rating "million Euros " crlf)
       
    )    
)

(defrule midfielder-cat-3
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "peak-age"))
    ;(footballer{age > 25}) (footballer{age <= 30}) 
 =>
    (if (= (str-compare ?*position* "MID") 0) then
    	(bind ?rating1 (midfielder-rating))
        (bind ?rating (* ?rating1 80)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .9)))	
        ;(if (< ?*appearances* 100)then		;appearances low, less value
         ;   (bind ?rating (* ?rating .7)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .5)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1.50)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        (printout t "midfielder is at Peak age, rating:"?rating1" estimated vale " ?rating "million Euros " crlf)
    )    
)

(defrule midfielder-cat-4
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "mature"))
    ;(footballer{age > 30}) (footballer{age <= 35}) 
 =>
    (if (= (str-compare ?*position* "MID") 0) then
    	(bind ?rating1 (midfielder-rating))
        (bind ?rating (* ?rating1 60)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .9)))	
        ;(if (< ?*appearances* 120)then		;appearances low, less value
         ;   (bind ?rating (* ?rating .6)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .5)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1.25)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        (printout t "midfielder has Matured, rating:"?rating1" estimated vale " ?rating "million Euros " crlf)
    )    
)

(defrule midfielder-cat-5
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "old"))
    ;(footballer{age > 35}) 
 =>
    (if (= (str-compare ?*position* "MID") 0) then
    	(bind ?rating1 (midfielder-rating))
        (bind ?rating (* ?rating1 35)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .8)))	
        ;(if (< ?*appearances* 160)then		;appearances low, less value
         ;   (bind ?rating (* ?rating .5)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .25)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        (printout t "midfielder has great experience and strong mentality, rating:"?rating1" estimated vale " ?rating "million Euros " crlf)
    )    
)



; Defender Rules
(defrule defender-cat-1
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "young"))
    ;(footballer{age > 15}) (footballer{age <= 20}) 
 =>
    (if (= (str-compare ?*position* "DEF") 0) then
    	(bind ?rating1 (defender-rating))
        (bind ?rating (* ?rating1 20)) 		; base rate, young defenders are not values as high as forwards
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .8)))	
        ;(if (< ?*appearances* 60)then		;appearances low, less value for defenders specially with low experience
         ;   (bind ?rating (* ?rating .4)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .7)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1.5)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        
        ;(printout t "defender is young, estimated vale " ?rating "million Euros " crlf)
    )    
)

(defrule defender-cat-2
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "learning-age"))
    ;(footballer{age > 20}) (footballer{age <= 25}) 
 =>
    (if (= (str-compare ?*position* "DEF") 0) then
    	(bind ?rating1 (defender-rating))
        (bind ?rating (* ?rating1 60)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .8)))	
        ;(if (< ?*appearances* 60)then		;appearances low, less value
         ;   (bind ?rating (* ?rating .6)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .5)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1.25)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        
        ;(printout t "defender is in learning age, estimated vale " ?rating "million Euros " crlf)
    )    
)

(defrule defender-cat-3
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "peak-age"))
    ;(footballer{age > 25}) (footballer{age <= 30}) 
 =>
    (if (= (str-compare ?*position* "DEF") 0) then
    	(bind ?rating1 (defender-rating))
        (bind ?rating (* ?rating1 60)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .9)))	
        ;(if (< ?*appearances* 100)then		;appearances low, less value
         ;   (bind ?rating (* ?rating .6)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .5)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1.50)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        
        ;(printout t "defender is at Peak age, estimated vale " ?rating "million Euros " crlf)
    )    
)

(defrule defender-cat-4
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "mature"))
    ;(footballer{age > 30}) (footballer{age <= 35}) 
 =>
    (if (= (str-compare ?*position* "DEF") 0) then
    	(bind ?rating1 (defender-rating))
        (bind ?rating (* ?rating1 50)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .8)))	
        ;(if (< ?*appearances* 200)then		;appearances low, less value
         ;   (bind ?rating (* ?rating .4)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .3)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1.1)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        
        ;(printout t "defender has Matured, estimated vale " ?rating "million Euros " crlf)
    )    
)

(defrule defender-cat-5
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "old"))
    ;(footballer{age > 35}) 
 =>
    (if (= (str-compare ?*position* "DEF") 0) then
    	(bind ?rating1 (defender-rating))
        (bind ?rating (* ?rating1 30)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .8)))	
        ;(if (< ?*appearances* 160)then		;appearances low, less value
         ;   (bind ?rating (* ?rating .5)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .25)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        
        ;(printout t "defender has great experience and strong mentality, estimated vale " ?rating "million Euros " crlf)
    )    
)

; GoalKeeper rules
(defrule gk-cat-1
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "young"))
    ;(footballer{age > 15}) (footballer{age <= 20}) 
 =>
    (if (= (str-compare ?*position* "GK") 0) then
    	(bind ?rating1 (gk-rating))
        (bind ?rating (* ?rating1 10)) 		; base rate, young goalkeepers are not values as high
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .8)))	
        ;(if (< ?*appearances* 60)then		;appearances low, less value for defenders specially with low experience
         ;   (bind ?rating (* ?rating .4)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .7)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1.5)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        
        ;(printout t "Goalkeeper is young, rating:"?rating1" estimated vale " ?rating "million Euros " crlf)
    )    
)

(defrule gk-cat-2
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "learning-age"))
    ;(footballer{age > 20}) (footballer{age <= 25}) 
 =>
    (if (= (str-compare ?*position* "GK") 0) then
    	(bind ?rating1 (gk-rating))
        (bind ?rating (* ?rating1 55)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .8)))	
        ;(if (< ?*appearances* 60)then		;appearances low, less value
         ;   (bind ?rating (* ?rating .6)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .8)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1.25)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        
        ;(printout t "Goalkeeper is in learning age, rating:"?rating1" estimated vale " ?rating "million Euros " crlf)
    )    
)

(defrule gk-cat-3
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "peak-age"))
    ;(footballer{age > 25}) (footballer{age <= 30}) 
 =>
    (if (= (str-compare ?*position* "GK") 0) then
    	(bind ?rating1 (gk-rating))
        (bind ?rating (* ?rating1 50)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .9)))	
        ;(if (< ?*appearances* 100)then		;appearances low, less value
         ;   (bind ?rating (* ?rating .7)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .7)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1.50)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        ;(bind ?*estimated-price* (integer ?*estimated-price*))
        ;(printout t "Goalkeeper is at Peak age, rating:"?rating1" estimated vale " ?rating "million Euros " crlf)
    )    
)

(defrule gk-cat-4
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "mature"))
    ;(footballer{age > 30}) (footballer{age <= 35}) 
 =>
    (if (= (str-compare ?*position* "GK") 0) then
    	(bind ?rating1 (gk-rating))
        (bind ?rating (* ?rating1 40)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .8)))	
      ;  (if (< ?*appearances* 120)then		;appearances low, less value
       ;     (bind ?rating (* ?rating .7)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .5)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1.25)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        ;(bind ?*estimated-price* (integer ?*estimated-price*))
        ;(printout t "Goalkeeper has Matured, rating:"?rating1" estimated vale " ?rating "million Euros " crlf)
    )    
)

(defrule gk-cat-5
    (declare (salience 90))
    (theAge ?a&:(fuzzy-match ?a "old"))
    ;(footballer{age > 35}) 
 =>
    (if (= (str-compare ?*position* "GK") 0) then
    	(bind ?rating1 (gk-rating))
        (bind ?rating (* ?rating1 20)) 		; base rate
        (if (< ?*league* 1)then 			;different leagues less value
            (bind ?rating (* ?rating .8)))	
       ;(if (< ?*appearances* 160)then		;appearances low, less value
        ;    (bind ?rating (* ?rating .5)))
        (if (> ?*division* 1)then		;lower division low, less value
            (bind ?rating (* ?rating .25)))
        ;(if (= ?*squad-status* 1)then		;important player are expensive
         ;   (bind ?rating (* ?rating 1)))
        (bind ?*rating-final* ?rating1)
        (bind ?*estimated-price* (+ ?*estimated-price* ?rating))
        (bind ?*count* (+ ?*count* 1))
        (assert (theCost (new nrc.fuzzy.FuzzyValue ?*cost* (new SingletonFuzzySet ?rating))))
        ;(bind ?*estimated-price* (integer ?*estimated-price*))
        ;(printout t "Goalkeeper has great experience and strong mentality, rating:"?rating1" estimated vale " ?rating "million Euros " crlf)
    )    
)

(defrule squad-status-backup
    (declare (salience 50))
    (theStatus ?s&:(fuzzy-match ?s "backup"))
    =>
    (bind ?*estimated-price*(* ?*estimated-price* .40))
    )

(defrule squad-status-subs
    (declare (salience 50))
    (theStatus ?s&:(fuzzy-match ?s "subs"))
    =>
    (bind ?*estimated-price*(* ?*estimated-price* .80))
    )

(defrule squad-status-starting
    (declare (salience 50))
    (theStatus ?s&:(fuzzy-match ?s "starting-team"))
    =>
    ;(printout t "Player Estimated Cost before status "?*estimated-price* "million Euros " crlf)
    (bind ?*estimated-price*(* ?*estimated-price* 1.15))
    ;(printout t "Player Estimated Cost after status "?*estimated-price* "million Euros " crlf)
    )

(defrule squad-status-indespensible
    (declare (salience 50))
    (theStatus ?s&:(fuzzy-match ?s "indespensible"))
    =>
    ;(printout t "Player Estimated Cost before status "?*estimated-price* "million Euros " crlf)
    (bind ?*estimated-price*(* ?*estimated-price* 1.50))
    ;(printout t "Player Estimated Cost after status "?*estimated-price* "million Euros " crlf)
    )

(defrule appearances-high
    (declare (salience 50))
    (theAppearance ?s&:(fuzzy-match ?s "high"))
    =>
    ;(printout t "Player Estimated Cost before app "?*estimated-price* "million Euros " crlf)
    (bind ?*estimated-price*(* ?*estimated-price* 1.20))
    ;(printout t "Player Estimated Cost after app "?*estimated-price* "million Euros " crlf)
    
    )

(defrule appearances-med
    (declare (salience 50))
    (theAppearance ?s&:(fuzzy-match ?s "medium"))
    =>
    (bind ?*estimated-price*(* ?*estimated-price* 1.0))
    )

(defrule appearances-low
    (declare (salience 50))
    (theAppearance ?s&:(fuzzy-match ?s "low"))
    =>
    (bind ?*estimated-price*(* ?*estimated-price* .85))
    )

(defrule pace-high
    (declare (salience 50))
    (thePace ?s&:(fuzzy-match ?s "high"))
    =>
    ;(printout t "Player Estimated Cost before pace "?*estimated-price* "million Euros " crlf)
    (bind ?*estimated-price*(* ?*estimated-price* 1.25))
    ;(printout t "Player Estimated Cost after pace "?*estimated-price* "million Euros " crlf)
    )
(defrule pace-low
    (declare (salience 50))
    (thePace ?s&:(fuzzy-match ?s "low"))
    =>
    ;(printout t "Player Estimated Cost before pace "?*estimated-price* "million Euros " crlf)
    (bind ?*estimated-price*(* ?*estimated-price* .85))
    ;(printout t "Player Estimated Cost after pace "?*estimated-price* "million Euros " crlf)
    )
(defrule pace-medium
    (declare (salience 50))
    (thePace ?s&:(fuzzy-match ?s "medium"))
    =>
    
    (bind ?*estimated-price*(* ?*estimated-price* 1))
    )


(defrule cost-deffuzyfi
    
    =>
    ;(printout t "Cost Rule is being called!!" ?c crlf)
    (bind ?*estimated-price* (/ ?*estimated-price* ?*count*))   
    (printout t "Player Estimated Cost is "?*estimated-price* "million Euros " crlf)
    ;(bind ?avgCost (momentDefuzzify ?c))
    ;(printout t "Player Estimated Cost is "?avgCost "million Euros " crlf)
    )


(reset)
(facts)
(run)
