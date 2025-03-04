# Diverse S-expression examples for syntactic analysis

# Simple Example - Basic subject-verb structure
(ROOT
  (S
    (NP
      (DET "Le")
      (N "chat"))
    (VP
      (V "dort"))))

# Medium Example - With prepositional phrase
(ROOT
  (S
    (NP
      (DET "Le")
      (ADJ "petit")
      (N "chat"))
    (VP
      (V "dort")
      (PP
        (P "sur")
        (NP
          (DET "le")
          (ADJ "vieux")
          (N "canapé"))))))

# Complex Example - With relative clause
(ROOT
  (S
    (NP
      (DET "L'")
      (N "homme")
      (REL
        (REL-PRO "qui")
        (VP
          (V "regardait")
          (PP
            (P "par")
            (NP
              (DET "la")
              (N "fenêtre"))))))
    (VP
      (V "attendait")
      (NP
        (DET "son")
        (N "ami")))))

# Very Complex Example - Multiple clauses from Proust
(ROOT
  (S
    (CP
      (SUB
        (REL
          (REL-PRO "Comme")
          (NP
            (PRO "je"))
          (VP
            (V "descendais")
            (PP
              (P "par")
              (NP
                (DET "l'")
                (N "escalier"))))))
      (NP
        (PRO "j'"))
      (VP
        (V "entendis")
        (NP
          (N "M.")
          (PP
            (P "de")
            (N "Charlus")))
        (REL
          (REL-PRO "qui")
          (VP
            (V "parlait")
            (PP
              (P "à")
              (NP
                (N "Jupien")))
            (PP
              (P "dans")
              (NP
                (DET "la")
                (N "cour")))))))))
