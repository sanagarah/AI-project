;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;***  Birds Identification Expert System ***
;; *** Implemented in CLIPS ***
;; Load the program, then reset the facts by CLIPS command (reset)
;; Run the program by CLIPS command (run)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate rule 
   (multislot if)
   (multislot then))

(defrule propagate-goal ""
   (goal is ?goal)
   (rule (if ?variable $?)
         (then ?goal ? ?value))
   =>
   (assert (goal is ?variable)))

(defrule goal-satified ""
   (declare (salience 30))
   ?f <- (goal is ?goal)
   (variable ?goal ?value)
   (answer ? ?text ?goal)
   =>
   (retract ?f)
   (format t "%s%s%n" ?text ?value))

(defrule remove-rule-no-match ""
   (declare (salience 20))
   (variable ?variable ?value)
   ?f <- (rule (if ?variable ? ~?value $?))
   =>
   (retract ?f))

(defrule modify-rule-match ""
   (declare (salience 20))
   (variable ?variable ?value)
   ?f <- (rule (if ?variable ? ?value and $?rest))
   =>
   (modify ?f (if ?rest)))

(defrule rule-satisfied ""
   (declare (salience 20))
   (variable ?variable ?value)
   ?f <- (rule (if ?variable ? ?value)
               (then ?goal ? ?goal-value))
   =>
   (retract ?f)
   (assert (variable ?goal ?goal-value)))

(defrule ask-question-no-legalvalues ""
   (declare (salience 10))
   (not (legalanswers $?))
   ?f1 <- (goal is ?variable)
   ?f2 <- (question ?variable ? ?text)
   =>
   (retract ?f1 ?f2)
   (format t "%s " ?text)
   (assert (variable ?variable (read))))

(defrule ask-question-legalvalues ""
   (declare (salience 10))
   (legalanswers ? $?answers)
   ?f1 <- (goal is ?variable)
   ?f2 <- (question ?variable ? ?text)
   =>
   (retract ?f1)
   (format t "%s " ?text)
   (printout t ?answers " ")
   (bind ?reply (read))
   (if (member (lowcase ?reply) ?answers) 
     then (assert (variable ?variable ?reply))
          (retract ?f2)
     else (assert (goal is ?variable))))

;;;***************************************************
;;;*************  knowledge _ base  ******************
;;;***************************************************

(deffacts knowledge-base 
   (goal is type.veg)
   (legalanswers are no yes)
;;;;***********************
 ( rule (if green is yes)
        (then greens is green))
(rule (if green is no) 
         (then greens is notgreen))
   (question green is "Is Your Vegetable Green ?")
;;;;***********************
 ( rule (if greens is notgreen and  root is yes )
		(then type.veg is Carrot))
(rule (if  greens is notgreen and  root is no) 
         (then  roots is noroot))
   (question root is "Does your Vegetable have a root ?")
;;;;***********************
( rule (if roots is noroot and circular is yes )
		(then type.veg is Tomato))
(rule (if roots is noroot and circular is no) 
         (then circulars is Nocircular))
   (question circular is "Is Your Vegetable Circular ?")
;;;;***********************
( rule (if circulars is Nocircular  and yellow is yes )
		(then type.veg is Lemon))
(rule (if circulars is Nocircular and  yellow is no) 
         (then type.veg is Onion))
   (question yellow is "Is Your Vegetable Color Yellow ?")
;;;;***********************
 ( rule (if greens is green and frilly is yes)
        (then type.veg is Lettuce ))
(rule (if  greens is green and frilly is no) 
         (then frillys is noFrilly))
   (question frilly is "Is Your Vegetable Frilly ?")
;;;;***********************
 ( rule (if frillys is noFrilly and flower is yes)
        (then isFlower is Flower ))
(rule (if  frillys is noFrilly and flower is no) 
         (then isFlower is notFlower))
   (question flower is "Does Your Vegetable Look Like Flower ?")
;;;;***********************
 ( rule (if isFlower is Flower  and spread is yes)
        (then type.veg is Cauliflower))
(rule (if  isFlower is Flower  and spread is no) 
		(then type.veg is Brocolli))
   (question  spread is "Are its Florets Spread ?")
;;;;***********************   
 ( rule (if isFlower is notFlower  and wedgeShape is yes)
        (then type.veg is Celery))
(rule (if   isFlower is notFlower  and wedgeShape is  no) 
         (then wedgeShapes is notwedgeShape))
   (question  wedgeShape is "Does It Have Wedge Shape ?")
;;;;***********************   
 ( rule (if  wedgeShapes is notwedgeShape and paddleShape is yes)
        (then type.veg is Collard))
(rule (if  wedgeShapes is notwedgeShape and paddleShape is  no) 
         (then paddleShapes is notpaddleShape))
   (question  paddleShape  is "Does It Have Paddle Shape ?")
;;;;***********************   
 ( rule (if  paddleShapes is notpaddleShape and ovalShape is yes)
        (then ovalShapes is oval))
(rule (if paddleShapes is notpaddleShape and ovalShape is no)
        (then ovalShapes is notOval))
   (question  ovalShape is "Does It Have Oval Shape ?")
;;;;***********************   
 ( rule (if ovalShapes is oval and shortVeg is yes)
        (then type.veg is Spinach))
(rule (if ovalShapes is oval and shortVeg is no)
        (then type.veg is Watercress))
   (question  shortVeg is "IS It Short ?")
;;;;***********************   
( rule (if  ovalShapes is notOval and hardOuter is yes)
        (then hardOuters is hard))
(rule (if  ovalShapes is notOval and hardOuter is no)
        (then type.veg is Pea))
   (question  hardOuter is "Is Your Vegetable Have Hard Outer ?")
;;;;***********************   
( rule (if  hardOuters is hard and glossy is yes)
        (then type.veg is Pepper))
(rule (if hardOuters is hard and glossy is no)
        (then type.veg is Cucumber))
   (question  glossy is "Does It Have Glossy Look ?")
;;;;***********************   
   (answer is "I think your vegetable is a " type.veg))