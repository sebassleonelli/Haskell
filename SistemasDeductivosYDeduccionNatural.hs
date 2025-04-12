{- 
Los sistemas deductivos sirven para demostrar cosas, especificamente afirmaciones matematicas precisas 
Las afirmaciones se llaman juicios y los sistemas deductivos razonan sobre ellos
Los sistemas deductivos estan dados por reglas de inferencia, donde arriba de la linea van las premisas y abajo va la conclusion
Las reglas que no tienen premisas son axiomas y las premisas son condiciones suficientes para la conclusion pero la conclusion no alcanza para las premisas
Si valen las premisas, podemos deducir que vale la conclusion
Ejemplo: tenemos tres axiomas en los cuales un objeto es mayor a otro
         Y podemos decir que si x>y y y>z => x>z, a estas variables las llamamos variables esquematicas
         Demostremos que estrella es mayor que circulo => estrella es mayor que cuadrado por axioma 1, por axioma 2 cuadrado es mayor a triangulo, aplicamos la transitividad a los dos axiomas y queda que estrella es mayor a triangulo, a eso le agregamos el axioma 3 y aplicamos la transitividad de nuevo y queda que estrella es mayor a circulo.
         Otra manera es combinar el axioma 2 y 3, razonando de abajo hacia arriba, yo tengo que estrella es mayor a circulo, por transitividad me queda comodo decir que estrella es mayor a cuadrado y cuadrado es mayor a circulo, entonces nuestro nuevo problema es demostrar estos ultimos dos axiomas
         Le podemos aplicar la transitividad a ambos axiomas y nos queda cuadrado mayor a triangul0 y triangulo mayor a circulo sobre cuadrado mayor a circulo para el axioma 2, luego estas dos son instancias de los axiomas 2 y 3.


Una derivacion es un arbol finito formado por reglas de inferencia, parte de ciertas premisas y llega a una conclusion

Sistemas Deductivos:
    formulas:
    suponiendo un conjunto infinito de variables P = {P,Q,R,...}

En este sistema los jucios son de la forma "X form", lo que no sea eso no es un juicio

La p azul en las diapos es un generador de axiomas para cada elemento del conjunto

Derivar el juicio -(P => (q =>P)) form

Usamos la negacion y nos queda:
         (P =>(Q => P))form /-(P => (q =>P)) form
Usamos la implicacion y nos queda:
  P form   (Q => P) form/(P =>(Q => P))form /-(P => (q =>P)) form
Usamoa fP, y f=> de nuevo y queda:

            -------fp ------- fp 
            Q form   Pform 
    ------fp   -----------f=>   
    Pform   (Q => P) form/
     -------------------- f=> 
     (P =>(Q => P))form 
     ----------------------f -
     /-(P => (q =>P)) form

Si τ form es un juicio derivable, entonces τ tiene el mismo numero de “(” que de “)”

Formulas de la logica proposicional:

Las formulas son las expresiones que se pueden generar a partir de la siguiente gramatica τ, σ,ρ,... ::= P | (τ ∧σ) | (τ ⇒ σ) | (τ ∨σ) | ⊥ | ¬τ

Una expresion τ se puede generar a partir de la gramatica de arriba si y solo si el juicio τ form es derivable en el sistema de antes

Se suele omitir los parentesis mas externos

La implicacion es asociativa a derecha.

Los conectivos (∧,∨) no son conmutativos ni asociativos.

Contextos y juicios:

Es un conjunto finito de formulas y se denotan con letras griegas mayusculas, por ejemplo:

 Γ ={P ⇒Q,¬Q} este contexto gama tiene dos formulas (se suelen omitir las llaves)

El sistema de deduccion natural predica sobre juicios de la forma Γ |- t donde Γ es la hipotesis y la t es la tesis
informalmente, un juicio afirma que a partir de las hipotesis en el contexto Γ es posible deducir la formula de la tesis


Γ,t es equivalente a decir Γ u {t}, union de contextos

Tengo las hipotesis gama y dentro de estas esta t, entonces vale t

Si una formula de la tesis no esta en el contexto, entonces no se puede deducir la tesis

Introduccion de la conjuncion: si vale tavo para gamma y sigma para gamma entonces vale (tavo y sigma) para gama

Eliminacion de la conjuncion: si vale (tavo y sigma) para gamma, entonces vale tavo por si solo para gamma y lo misma para sigma


        --------- ax        ------------ ax
        p y q |- p y q      p y q |- p y q
        --------------e2      ------------ e1
        p y q |- q          p y q |- p
        ---------------------------------------introduccion de conjuncion
                        p y q |- q y p


p y (q y r) |- (p y q) y r          ---------------------------ax
                                    p y (q y r) |- p y (q y r)
       --------------------------ax ------------------- e2      ---------------------------ax
       p y (q y r) |- p y (q y r)   p y (q y r) |- q y r        p y (q y r) |- p y (q y r)
       -------------------e1        ----------------- e1        ---------------------e2
        p y (q y r) |- p            p y (q y r) |- q            p y (q y r) |- q y r
       -------------------------------------------------- i      ------------------e2
                        p y (q y r) |- p y q                      p y (q y r) |- r
        ----------------------------------------------------------------------------i
                                         p y (q y r) |- (p y q) y r   


Introduccion de la implicacion: Si un conjunto de hipotesis gamma tavo demuestro sigma, entonces vale que segun un contexto gamma, tavo implica sigma
                                Si quiero ver que tavo implica sigma, asumo tavo y demuestro sigma

Eliminacion de la implicacion: Si demostre la implicacion y demostre el antecedente, entonces vale sigma (modus ponems)

|- p => p

                    -------ax
                    p |- p 
                -----------------------------------------=>i
                            |- p => p


p => q => (q y p)

                --------------ax      -------------------ax
                    p,q |- q            p,q |- p
                -------------------------------------------i
                     p,q |- q y p
                -------------------------------------------=>i
                     p |- q => (q y p) 
                -------------------------------------------=>i 
                       |- p => q => (q y p)


p => q,q=>r |- p => r



                                           --------------------ax      ----------------ax
                                            p=> q,q=>r,p|-p=>q          p=>q,q=>r,p |-p
                ----------------------ax   -------------------------------------------=>e
                p => q,q=>r p|- q =>r        p =>q, q=>r,p|-q   
                ------------------------------------------------=>i
                        p => q, q => r,p |-r
                ------------------------------------------------=>i
                        p => q,q=>r |- p => r


Introduccion de la disyuncion: si quiero probar que vale tavo o sigma, vale con probar que vale tavo y viceversa

Eliminacion de la disyuncion: si pude demostrar que vale (tavo o sigma), bajo gamma asumiendo que vale tavo pruebo ro y bajo gamma asumiendo sigma pruebo ro, entonces bajo gamma vale ro


⊢ P ⇒ (P ∨P)

                -----------------ax
                    p |- p 
                ------------------oi1
                      p |- p v p 
                -------------------------------- =>i
                        ⊢ P ⇒ (P ∨P)

⊢ (P ∨ P) ⇒ P
                        --------------ax         --------------ax     ------------ax
                        p v p |- p v p         p v p, p |- p        p v p,p |- p 
                -------------------------------------------------------------------------- ve
                        p v p |- p
                ----------------------------------=>i
                        |- (p v p) => p

P ∨ Q ⊢ Q ∨ P

                                      ---------------ax   ------------------ax
                                      p v q, p |- p       p v q,q |- q
                    --------------ax  ----------------vi2 ------------------vi1
                    p v q |- p v q    p v q,p |- q v p    p v q, q |- q v p     
                -----------------------------------------------------------------ve
                                     P ∨ Q ⊢ Q ∨ P


Falsedad

El conectivo ⊥ representa la falsedad (contradiccion, absurdo)
El conectivo ⊥ no tiene reglas de introduccion

Eliminacion del falso: Si a partir de mis hipotesis pude demostrar falso, hay una contradiccion en mis hipotesis por que no tiene reglas de introduccion
                       Si llego a una contradiccion, puedo demostrar tavo


(P ∨Q ) ⇒⊥⊢P ⇒Q                                        ----------------------------- ax     
                                                          (p v q) => ⊥ , p |- p
                    ---------------------------------ax  ----------------------------- vi1
                    (p v q) => ⊥ , p |- (p v q) => ⊥     (p v q) => ⊥ , p |- p v q
                    ------------------------------------------------------------------=>e
                            (p v q) => ⊥ , p |- ⊥ 
                    ---------------------------------------⊥e
                            (p v q) => ⊥ , p |- q
                    --------------------------------------- => i 
                            (P ∨ Q ) ⇒⊥ ⊢ P ⇒Q

(P ∧Q ) ⇒⊥⊢P ⇒Q ⇒R  

                                                        --------------------ax --------------------ax
                                                         (P ∧Q ) ⇒⊥,P,Q |-p   (P ∧Q ) ⇒⊥,P,Q|- q   
                                  ------------------------------------------^i
                    (P ∧Q ) ⇒⊥,P,Q |- (p v q) => ⊥     (P ∧Q ) ⇒⊥,P,Q |- (p v q) 
                    --------------------------------------------=>e
                    (P ∧Q ) ⇒⊥,P,Q |- ⊥
                    --------------------------------------------⊥e                    
                    (P ∧Q ) ⇒⊥,P,Q |- R
                    ---------------------------------------------=>i
                      (P ∧Q ) ⇒⊥,P |- Q ⇒R  
                    --------------------------------------------- =>i
                                (P ∧Q ) ⇒⊥⊢P ⇒Q ⇒R


Introduccion de la negacion: si vale el contexto gamma y asumimos tavo y llegamos a un absurdo, entonces vale no tavo

Eliminacion de la negacion: si a partir de gamma llegamos a un absurdo, entonces valia tavo y no tavo


⊢P ⇒¬¬P  
                ----------ax      ------------ax
                p,-p |- p         p,-p |- -p
                ----------------------------- -e
                        p, -p |- absurdo
                ------------------------------ -i
                        p |- --p
                ------------------------------=>i
                        ⊢P ⇒¬¬P 

⊢¬(P ∧¬P )

                ----------------ax  -----------------ax
                p y -p |- p ^ -p    p ^ -p |- p ^ -p
                ------------^e1     ------------- ^e2
                p ^ -p |- p         p ^ -p |- -p
                -------------------------------- -e
                p ^ -p |- absurdo
                ------------------------------- -i
                            ⊢¬(P ∧¬P )


P ∨Q ⊢¬(¬P ∧¬Q ).
                                                ------------------ax                       ---------------------ax
                                                 gamma,p |- -p ^ -q                         gamma,q |- -p ^ -q
                                -------------ax  ---------------^e1        -------------ax ------------^e2
                                 gamma,p |-p      gamma,p |- -p             gamma,q |- q  gamma,q |- -q 
                --------------ax ------------------------------------ -e  ------------------------- -e
                gamma |- p v q   gamma,p |- absurdo                        gamma, q |- absurdo
                -----------------------------------------------------------------------------------ve
                gamma = {p v q, -p ^ -q} |- absurdo
                -------------------------------- -i
                        P ∨Q ⊢¬(¬P ∧¬Q ).


Teorema (debilitamiento)
Si Γ ⊢τ es derivable, entonces Γ,σ ⊢τ es derivable
Si agrego mas hipotesis, tavo sigue valiendo, por mas de que sean validas o no por que ya valia con gamma

Reglas Derivadas

Modus tollens
                    Γ ⊢τ ⇒σ Γ ⊢¬σ
                    ---------------- si para gamma vale tavo implica sigma y para gamma vale no tavo, entonces para gamma vale no tavo
                        Γ ⊢¬τ MT
Introduccion de la doble negacion
                        Γ ⊢τ
                    -----------------    
                       Γ ⊢¬¬τ ¬¬i


Demo:

                r |- t => sigma
                ------------------debilitamiento ----------ax
                r,t |- t => sigma                r,t |- t                r |- - sigma
                ------------------------------------------            ------------------  =>e/ debilitamiento
                r,t |- sigma                                             r,t |- no sigma
                ---------------------------------------------------------------------------- -e
                r,t |- abs
                ---------------- -i
                    Γ ⊢¬τ

Eliminacion de la doble negacion:

                    r |- t
                --------------W       ------------ax
                  r, -t |- t         t, -t |- -t
                -------------------------------- -e
                        r,-t |- abs
                ------------------------- -i
                        r |- --t


Principio de tercero excluido

                ------------
                  Γ ⊢τ ∨¬τ 

t => --t

La regla anterior y el principio de tercero excluido no se pueden deducir de las reglas anteriores

Logica intuicionista (nj) vs logica clasica(nk)

NK extiende a NJ con principios de razonamiento clasicos.Alcanza con agregar uno de ellos, por ejemplo ¬¬e 

Si un juicio es derivable en NJ, tambien es derivable en NK

NJ es mas restrictiva. No permite usar ¬¬e , LEM, PBC, etc

Interes de la logica intuicionista en computacion
▶ Permite razonar acerca de informaci ́on.
¿Que significa (hay vida en Marte ∨¬hay vida en Marte)?
▶ Las derivaciones en NJ se pueden entender como programas.
NJ es la base de un lenguaje de programacion funcional.

Semantica Bivaluada

|= indica que satisface una formula
Cada formula lleva su v(p) |= v/f y si satisface todas las formulas, satisface al contexto gamma

Teorema: Demostrar una formula en nk es equivalente a que el contexto gamma satisfae la formula tavo











}