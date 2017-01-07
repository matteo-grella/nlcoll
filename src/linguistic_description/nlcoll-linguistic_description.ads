------------------------------------------------------------------------------
--                               N L C O L L
--  N a t u r a l   L a n g u a g e   C o m p o n e n t   C o l l e c t i o n
--
--          Copyright 2009-2011 M. Grella, M. Nicola, D. Christen
--
--  In collaboration with Politecnico di Torino and Università di Torino.
--
--  Supported by
--   (a) Fondazione CRT under a "VivoMeglio" grant (Speak2Home)
--   (b) Region Piedmont under a “Converging Technologies” programme (ATLAS)
--        
--  The project aims at developing open-source linguistic technologies for the
--  Italian language to improve the welfare of people, especially impaired users.
--
--  This is free software; you can redistribute it and/or modify it under
--  terms of the GNU General Public License as published by the Free Software
--  Foundation; either version 2, or (at your option) any later version.
--  This software is distributed in the hope that it will be useful, but WITH
--  OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
--  for more details. Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
------------------------------------------------------------------------------

pragma License (Modified_GPL);

with ARColl; use ARColl;

package NLColl.Linguistic_Description is
    
    type Language_Type is -- ISO-693-1
      (

       ----------------------
       -- Supported Languages
       ----------------------

       DE, -- German
       EN, -- English
       ES, -- Spanish
       FR, -- French
       IT, -- Italian

       ------------------------
       -- Unsupported Languages
       ------------------------

       AA,
       AB,
       AE,
       AF,
       AK,
       AM,
       AN,
       AR,
       AS,
       AV,
       AY,
       AZ,
       BA,
       BE,
       BG,
       BH,
       BI,
       BM,
       BN,
       BO,
       BR,
       BS,
       CA,
       CE,
       CH,
       CO,
       CR,
       CS,
       CU,
       CV,
       CY,
       DA,
       DV,
       DZ,
       EE,
       EL,
       EO,
       ET,
       EU,
       FA,
       FF,
       FI,
       FJ,
       FO,
       FY,
       GA,
       GD,
       GL,
       GN,
       GU,
       GV,
       HA,
       HE,
       HI,
       HO,
       HR,
       HT,
       HU,
       HY,
       HZ,
       IA,
       ID,
       IE,
       IG,
       II,
       IK,
       IO,
       IS_ISO_639_1_CODE,
       IU,
       JA,
       JV,
       KA,
       KG,
       KI,
       KJ,
       KK,
       KL,
       KM,
       KN,
       KO,
       KU,
       KY,
       LA,
       LB,
       LO,
       LT,
       LV,
       MG,
       MK,
       ML,
       MN,
       MR,
       MS,
       MT,
       NB,
       NE,
       NL,
       NN,
       NO,
       OC,
       OR_639_1_CODE,
       PA,
       PL,
       PS,
       PT,
       QU,
       RO,
       RU,
       RW,
       SE,
       SI,
       SK,
       SL,
       SQ,
       SR,
       SV,
       SW,
       TA,
       TE,
       TH,
       TL,
       TR,
       UG,
       UK,
       UR,
       VI,
       VO,
       WA,
       XH,
       ZH,
       ZU,
       NIL); -- Special

    pragma Unreferenced
      (EN, ES, FR, AA, AB, AE, AF, AK, AM, AN, AR, AS, AV, AY, AZ, BA, BE, BG,
       BH, BI, BM, BN, BO, BR, CA, BS, CE, CH, CO, CR, CS, CU, CV, CY, DA, DV,
       DZ, EE, EL, EO, ET, EU, FA, FF, FI, FJ, FO, FY, GA, GD, GL, GN, GU, GV,
       HA, HE, HI, HO, HR, HT, HU, HY, HZ, IA, IE, IG, ID, II, IK, IO, IU, JA,
       IS_ISO_639_1_CODE, JV, KG, KI, KA, KJ, KL, KK, KM, KN, KO, KU, KY, LA,
       LB, LO, LT, LV, MG, MK, ML, MN, MR, MS, MT, NB, NE, NL, NN, NO, OC,
       OR_639_1_CODE, PA, PL, PS, PT, QU, RO, RU, RW, SE, SI, SK, SL, SQ, SR,
       SV, SW, TA, TE, TH, TL, TR, UG, UK, UR, VI, VO, WA, XH, ZH, ZU);

    subtype Supported_Language is Language_Type range DE .. IT;
    -- German  (DE)
    -- English (EN)
    -- Spanish (ES)
    -- French  (FR)
    -- Italian (IT)
    
    Is_Supported_Language : constant array (Language_Type) of Boolean
      := (Supported_Language => True, others => False);
    
    -----------------
    -- Part of Speech
    -----------------

    type Misc_POS_Type is
      (VERB,      -- Verb
       NOUN,      -- Noun
       PRON,      -- Pronoun
       ADJ,       -- Adjective
       ADV,       -- Adverb
       NUM,       -- Number
       ART,       -- Article
       CONJ,      -- Conjunction
       PREP,      -- Preposition
       PREPART,   -- Preposition + Article
       PREDET,    -- Predeterminer
       POSTPOS,   -- Postposition
       PHRAS,     -- Phrasal Word
       INTERJ,    -- Interjection
       DATE,      -- Date
       HOUR,      -- Hour
       PUNCT,     -- Punctuation
       --------------------
       UNKNOWN,
       COORD,
       SUBORD,
       ADVERS,
       ANTEC,
       COMMON,
       COMPAR,
       CONCESS,
       CORREL,
       DEF,
       DEITT,
       DEMONS,
       DISJ,
       DISTR,
       DOUBLE,
       ENCLIT,
       EXCLAM,
       EXPLIC,
       INDEF,
       INTERR,
       LIMIT,
       AUX,
       MODAL,
       MONO,
       NEG,
       ORDIN,
       GERUND,
       PART,
       PERS,
       POLI,
       POSS,
       POST,
       PROCLIT,
       PROPER,
       QUALIF,
       QUANT,
       RELAT,
       STRENG,
       WH,
       SUCC,
       TIME,
       VARIANT,
       REFL,
       PER, -- Person
       ORG, -- Organization
       LOC, -- Location
       NONE)
      with Default_Value => NONE;
    
    subtype CPOS_Type is Misc_POS_Type range VERB .. PUNCT;

    type POS_Type_Array is Array (Index_Type range 1 .. 4) of Misc_POS_Type;
    -- Max four part-of-speech information
    
    Null_POS : constant POS_Type_Array := (others => NONE);
    
    Known_Part_Of_Speech : constant array (Natural range <>)
      of POS_Type_Array := 
        (  ---------
           --- Verbs
           ---------
            
             (VERB, others => NONE),
           (VERB, POLI, others => NONE),  -- spanish
           (VERB, MODAL, others => NONE), -- english
           (VERB, AUX, others => NONE),   -- english

           --------------
           --- Adjectives
           --------------
           (ADJ, others => NONE),
           (ADJ, DEITT, others => NONE),
           (ADJ, DEITT, POLI, others => NONE),
           (ADJ, DEMONS, others => NONE),
           (ADJ, DEMONS, ANTEC, others => NONE),
           (ADJ, DEMONS, SUCC, others => NONE),
           (ADJ, DEMONS, POLI, others => NONE),
           (ADJ, EXCLAM, others => NONE),
           (ADJ, EXCLAM, POLI, others => NONE),
           (ADJ, INDEF, others => NONE),
           (ADJ, INDEF, POLI, others => NONE),
           (ADJ, INDEF, SUBORD, others => NONE),
           (ADJ, INDEF, DISTR, others => NONE),
           (ADJ, INDEF, QUANT, others => NONE),
           (ADJ, INDEF, QUANT, POLI, others => NONE),
           (ADJ, INTERR, others => NONE),
           (ADJ, POSS, others => NONE),
           (ADJ, POSS, POLI, others => NONE),
           (ADJ, POLI, others => NONE),
           (ADJ, ORDIN, others => NONE),
           (ADJ, ORDIN, POLI, others => NONE),
           (ADJ, COMPAR, others => NONE),
           (ADJ, QUALIF, others => NONE),
           (ADJ, QUALIF, POLI, others => NONE),
           (ADJ, QUALIF, POST, others => NONE),
           (ADJ, RELAT, others => NONE),

           -----------
           --- Adverbs
           -----------
           (ADV, others => NONE),
           (ADV, ADVERS, others => NONE),
           (ADV, COMPAR, others => NONE),
           (ADV, COMPAR, POLI, others => NONE), -- fr
           (ADV, INDEF, others => NONE),
           (ADV, INDEF, SUBORD, others => NONE),
           (ADV, INTERR, others => NONE),
           (ADV, INTERR, POLI, others => NONE),
           (ADV, LIMIT, others => NONE),
           (ADV, LOC, others => NONE),
           (ADV, LOC, POLI, others => NONE),
           (ADV, NEG, others => NONE),
           (ADV, NEG, POLI, others => NONE),
           (ADV, PHRAS, others => NONE),
           (ADV, PHRAS, POLI, others => NONE),
           (ADV, POLI, others => NONE),
           (ADV, QUANT, others => NONE),
           (ADV, QUANT, POLI, others => NONE),
           (ADV, STRENG, others => NONE),
           (ADV, STRENG, POLI, others => NONE),
           (ADV, STRENG, NEG, others => NONE),
           (ADV, STRENG, NEG, POLI, others => NONE),
           (ADV, TIME, others => NONE),
           (ADV, TIME, POLI, others => NONE),
           (ADV, CONCESS, others => NONE),
           (ADV, DEITT, others => NONE),

           ------------
           --- Articles
           ------------
           (ART, others => NONE),
           (ART, POLI, others => NONE), -- fr
           (ART, DEF, others => NONE),
           (ART, INDEF, others => NONE),
           (ART, INDEF, PART, others => NONE),
           (ART, INDEF, PART, POLI, others => NONE),

           ----------------
           --- Conjunctions
           ----------------
           (CONJ, others => NONE),
           (CONJ, POLI, others => NONE), -- fr
           (CONJ, COMPAR, others => NONE),
           (CONJ, COMPAR, POLI, others => NONE), -- fr
           (CONJ, COMPAR, ANTEC, others => NONE),
           (CONJ, COMPAR, ANTEC, POLI, others => NONE),
           (CONJ, COMPAR, SUCC, others => NONE),
           (CONJ, CONCESS, others => NONE),
           (CONJ, COORD, others => NONE),
           (CONJ, COORD, POLI, others => NONE),
           (CONJ, COORD, ADVERS, others => NONE),
           (CONJ, COORD, DISJ, others => NONE),
           (CONJ, COORD, DISJ, POLI, others => NONE),
           (CONJ, COORD, EXPLIC, others => NONE),
           (CONJ, COORD, EXPLIC, POLI, others => NONE),
           (CONJ, COORD, NEG, others => NONE),
           (CONJ, CORREL, others => NONE),
           (CONJ, CORREL, ANTEC, others => NONE),       -- english
           (CONJ, CORREL, ANTEC, POLI, others => NONE), -- english
           (CONJ, CORREL, SUCC, others => NONE),        -- english
           (CONJ, CORREL, SUCC, POLI, others => NONE),  -- english
           (CONJ, SUBORD, others => NONE),
           (CONJ, SUBORD, POLI, others => NONE),
           (CONJ, SUBORD, ADVERS, others => NONE),
           (CONJ, SUBORD, ADVERS, POLI, others => NONE),
           (CONJ, COORD, ADVERS, POLI, others => NONE),
           (CONJ, SUBORD, INTERR, others => NONE),
           (CONJ, SUBORD, INTERR, POLI, others => NONE),
           (CONJ, SUBORD, WH, others => NONE),
           (CONJ, SUBORD, WH, POLI, others => NONE),

           --------
           --- Noun
           --------
           (NOUN, others => NONE),
           (NOUN, COMMON, others => NONE),
           (NOUN, COMMON, QUANT, others => NONE),
           (NOUN, PROPER, others => NONE),
           (NOUN, PROPER, POLI, others => NONE),
           (NOUN, PROPER, PER, others => NONE),
           (NOUN, PROPER, PER, POLI, others => NONE),
           (NOUN, PROPER, ORG, others => NONE),
           (NOUN, PROPER, ORG, POLI, others => NONE),
           (NOUN, PROPER, LOC, others => NONE),
           (NOUN, PROPER, LOC, POLI, others => NONE),
           (NOUN, COMMON, POLI, others => NONE),
           (NOUN, COMMON, GERUND, others => NONE),
           (NOUN, COMMON, GERUND, POLI, others => NONE),

           -----------
           --- Pronoun
           -----------
           (PRON, others => NONE),
           (PRON, EXCLAM, others => NONE),
           (PRON, ORDIN, others => NONE),
           (PRON, DEMONS, others => NONE),
           (PRON, DEMONS, POLI, others => NONE),
           (PRON, INDEF, others => NONE),
           (PRON, INDEF, POLI, others => NONE),
           (PRON, INDEF, SUBORD, others => NONE),
           (PRON, INDEF, DISTR, others => NONE),
           (PRON, INDEF, DISTR, POLI, others => NONE),
           (PRON, INDEF, QUANT, others => NONE),
           (PRON, INDEF, QUANT, POLI, others => NONE),
           (PRON, INTERR, others => NONE),
           (PRON, INTERR, POLI, others => NONE),
           (PRON, PERS, others => NONE),
           (PRON, PERS, VARIANT, others => NONE),
           (PRON, PERS, REFL, others => NONE), -- english
           (PRON, PERS, ENCLIT, others => NONE),
           (PRON, PERS, PROCLIT, others => NONE),
           (PRON, PERS, PROCLIT, POLI, others => NONE), -- fr
           (PRON, PERS, PROCLIT, REFL),
           (PRON, PERS, PROCLIT, VARIANT),
           (PRON, POLI, others => NONE),
           (PRON, POSS, others => NONE),
           (PRON, POSS, POLI, others => NONE), -- spanish
           (PRON, RELAT, others => NONE),
           (PRON, RELAT, POLI, others => NONE), -- spanish
           (PRON, RELAT, DOUBLE, others => NONE),
           (PRON, RELAT, DOUBLE, POLI, others => NONE),

           ----------------
           --- Prepositions
           ----------------
           (PREP, others => NONE),
           (PREP, MONO, others => NONE),
           (PREP, POLI, others => NONE),
           (PREP, POLI, COMPAR, others => NONE),
           (PREP, MONO, VARIANT, others => NONE), -- english
           (PREP, POSS, others => NONE),          -- english

           (PREPART, others => NONE), -- german

           ----------------
           -- Postpositions
           ----------------
           (POSTPOS, others => NONE),       -- english
           (POSTPOS, POSS, others => NONE), -- english

           ---------
           -- Others
           ---------
           (NUM, others => NONE),
           (NUM, POLI, others => NONE),
           (PREDET, others => NONE),
           (PREDET, POLI, others => NONE), -- fr
           (PUNCT, others => NONE),
           (INTERJ, others => NONE),
           (INTERJ, POLI, others => NONE),
           (PHRAS, others => NONE),
           (PHRAS, POLI, others => NONE),
           (PHRAS, EXCLAM, others => NONE),
           (DATE, others => NONE),
           (DATE, POLI, others => NONE),
           (HOUR, others => NONE),
           (UNKNOWN, others => NONE)
           
          );
    
    -----------------------
    -- Grammatical Features
    -----------------------

    type Number_Type is
      (SING,
       PLUR,
       BOTH,
       DUAL, -- arabic
       NONE)
      with Default_Value => NONE;

    type Gender_Type is
      (MASC,
       FEM,
       BOTH,
       NEUT, -- spanish
       NONE)
      with Default_Value => NONE;

    type Person_Type is
      (FIRST,
       SECOND,
       THIRD,
       ALLVAL,
       NONE)
      with Default_Value => NONE;

    type Mood_Type is
      (BASE, -- english
       INDICATIVE,
       CONDITIONAL,
       SUBJUNCTIVE,
       PARTICIPLE,
       GERUND,
       INFINITIVE,
       IMPERATIVE,
       NONE)
      with Default_Value => NONE;

    type Tense_Type is
      (BASE, -- english
       PRESENT,
       PAST,
       FUTURE,
       IMPERFECT,
       NONE)
      with Default_Value => NONE;

    type GCase_Type is
      (OBJ,
       SUBJ,
       IOBJ,
       PART,
       LOC,
       EMPTY,
       NEUT,
       NONE)
      with Default_Value => NONE;

    type GCase_Type_Array is Array (Index_Type range 1 .. 4) of GCase_Type;

    Null_GCase : constant GCase_Type_Array := (others => NONE);
    
    type Degree_Type is
      (SUPERLATIVE,
       INFERIORITY,
       COMPARATIVE,
       NONE)
      with Default_Value => NONE;

    type Voice_Type is 
      (ACTIVE,
       PASSIVE,
       AMBIGUOUS, 
       NONE)
      with Default_Value => NONE;
    
    type Source_Type is
      (DICT, 
       AUTO, 
       NONE)
      with Default_Value => NONE;
    
    ------------------------
    -- Multi-word expression
    ------------------------
    
    type MWE_Range_Index_Type is 
      (FIRST,
       LAST);
    
    type MWE_Range_Type is array (MWE_Range_Index_Type) of Index_Type;
    
    Null_MWE_Range : constant MWE_Range_Type := (others => 0);
    
end NLColl.Linguistic_Description;
