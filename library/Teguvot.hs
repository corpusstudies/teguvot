module Teguvot where

data Tense = Present | Future | Perfect
data Person = Person1 | Person2 | Person3
data NumberG = Singular | Plural
data Gender = Masculine | Feminine | Neuter
data CaseG = Nominative | Genitive | Dative
  | Accusative | Ablative | Vocative

data WordG
  = Verb Tense Person NumberG
  | Preposition
  | Noun CaseG Gender NumberG
  | RelativePronoun 

data Clause
  | NoV_adjutorium_inAbl
  | NoV_gloria_dat
  | NoV_in_nomine
  | V_adduxerunt_inAcc
  | V_affligit_acc
  | V_confitebor_dat
  | V_conturbas_acc
  | V_deduxerunt_acc
  | V_discerne_acc_de
  | V_emitte_acc
  | V_erat_time
  | V_erue_acc_ab
  | V_es_nom
  | V_fecit_acc
  | V_incedo
  | V_introibo_ad
  | V_judica_acc
  | V_laetificat_acc
  | V_repulisti_acc
  | V_spera_inAbl

main :: IO ()
main = do
  putStrLn "Hello"
