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

separate (NLColl.Linguistic_Description.Syntax.Dependency_Trees)

package body Statistics is

    procedure Print_Evaluation
      (Evaluation : in Evaluation_Type) is  
        
        procedure Put_Item (N : Real) is
        begin
            Ada.Float_Text_IO.Put(Float(N), 3, 3, 0);
            Put(";");
        end;
        
    begin

        Put (Evaluation.Iter'Img & ";");
        Put_Item(Evaluation.UAS_No_Punct);
        Put_Item(Evaluation.LAS_No_Punct);
        Put_Item(Evaluation.UAS);
        Put_Item(Evaluation.LAS);
        Put_Item(Evaluation.UEM_No_Punct);
        Put_Item(Evaluation.UEM);
        New_Line;
        
    end Print_Evaluation;
    
    procedure Print_Evaluations
      (Evaluations : in Evaluation_Vectors.Vector) is
    begin
        New_Line (Standard_Error);
        for Evaluation of Evaluations loop
            Print_Evaluation (Evaluation);
        end loop;
        New_Line (Standard_Error);
    end Print_Evaluations;
    
    function Get_Dependency_Trees_Stats
      (Dependency_Trees : in  Dependency_Tree_Vectors.Vector) 
       return Dependency_Trees_Stats_Type is
    begin
        return Dependency_Trees_Stats : Dependency_Trees_Stats_Type do
            
            -- Total number of sentences
            Dependency_Trees_Stats.Total := Natural(Dependency_Trees.Length);
            
            for Tree : Dependency_Tree_Type of Dependency_Trees loop

                -- Check if it is a valid tree
                if not Tree.Is_Tree then
                    Dependency_Trees_Stats.Non_Tree := Dependency_Trees_Stats.Non_Tree + 1;
                else
                    -- Check if it is projective
                    if not Tree.Is_Projective then
                        Dependency_Trees_Stats.Non_Projective 
                          := Dependency_Trees_Stats.Non_Projective + 1;
                    end if;

                    -- Check if it is single top (i.e. only one token linked to the Root)
                    if not Tree.Is_Single_Top then
                        Dependency_Trees_Stats.Multi_Top
                          := Dependency_Trees_Stats.Multi_Top + 1;
                    end if;
                end if;
                
            end loop;
            
        end return;

    end Get_Dependency_Trees_Stats;
    
    procedure Print_Dependency_Trees_Stats
      (Dependency_Trees_Display_Name : in String;
       Dependency_Trees_Stats        : in Dependency_Trees_Stats_Type) is
        
        Name           : String  renames Dependency_Trees_Display_Name;
        Total          : Natural renames Dependency_Trees_Stats.Total;
        Non_Tree       : Natural renames Dependency_Trees_Stats.Non_Tree;
        Multi_Top      : Natural renames Dependency_Trees_Stats.Multi_Top;
        Non_Projective : Natural renames Dependency_Trees_Stats.Non_Projective;
    begin
        
        New_Line(Standard_Error);
        
        ---
        
        Put_Line(Standard_Error, "[" & Name & "] size:" & Img(Total));
        
        ---
        
        Put(Standard_Error, "   " & Img(Non_Tree) & " tree(s) are illegal (");
        
        Put (Standard_Error, Float (Non_Tree) * 100.0 / Float (Total),
             Fore => 1, Aft  => 2, Exp  => 0);
        
        Put_Line(Standard_Error, "%).");

        ---
        
        Put(Standard_Error, "   " & Img(Dependency_Trees_Stats.Multi_Top) 
            & " tree(s) are legal but have multiple tops (");
        
        Put (Standard_Error, Float (Multi_Top) * 100.0 / Float (Total),
             Fore => 1, Aft  => 2, Exp  => 0);
        
        Put_Line (Standard_Error, "%).");

        ---
        
        Put (Standard_Error, "   " & Img(Non_Projective) & " tree(s) are legal but not projective (");
        
        Put (Standard_Error, Float (Non_Projective) * 100.0 / Float (Total),
             Fore => 1, Aft  => 2, Exp  => 0);
        
        Put_Line (Standard_Error, "%).");

        ---
        
        New_Line(Standard_Error);
        
    end Print_Dependency_Trees_Stats;
    
    procedure Evaluate_Dependency_Tree
      (Sentences      : in  Sentence_Vectors.Vector;
       Trees          : in  Dependency_Tree_Vectors.Vector;
       Gold_Trees     : in  Dependency_Tree_Vectors.Vector;
       Evaluation     : out Evaluation_Type) is

        Punctuation_Tags : String_Sets.Set;
        -- Skip words which are punctuation.
        -- Retrieve tags indicating punctuation in this treebank.

        Correct_Labels          : Natural := 0;
        Correct_Labels_No_Punc  : Natural := 0;
        Correct_Heads         : Natural := 0;
        Correct_Heads_No_Punc : Natural := 0;

        Correct_Trees         : Natural := 0;
        Correct_Trees_No_Punc : Natural := 0;
        Correct_Root          : Natural := 0;

        Sum_Arcs              : Natural := 0;
        Sum_Arcs_No_Punc      : Natural := 0;
        
    begin
        
        Punctuation_Tags.Insert ("``");
        Punctuation_Tags.Insert ("''");
        Punctuation_Tags.Insert (".");
        Punctuation_Tags.Insert (",");
        Punctuation_Tags.Insert (":");

        -----

        if Trees.Length /= Gold_Trees.Length then
            raise Constraint_Error with
              "Incorrect number of trees.";
        end if;

        for I in Trees.First_Index .. Trees.Last_Index loop
            
            --              if not NLPColl.Linguistic_Description.Syntax.Dependency_Trees.Is_Projective 
            --                (Gold_Trees.Element (I)) then
            
            declare
                Tokens : constant Token_Vectors.Vector
                  := Sentences.Element (I).Elements;

                Tree   : constant Dependency_Tree_Type
                  := Trees.Element (I);

                Gold_Tree : constant Dependency_Tree_Type
                  := Gold_Trees.Element (I);

                N_Correct_Head         : Natural := 0;
                N_Correct_Head_No_Punc : Natural := 0;
                N_No_Punc              : Natural := 0;
            begin

                if Tree.Get_Tokens_Count /= Gold_Tree.Get_Tokens_Count then
                    raise Constraint_Error with
                      "Incorrect number of nodes.";
                end if;

                if not Tree.Is_Tree then
                    raise Constraint_Error with
                      "Illegal.";
                end if;

                for J in Tree.Heads.First_Index .. Tree.Heads.Last_Index loop

                    if Tree.Get_Head (J) = Gold_Tree.Get_Head (J) then
                        Correct_Heads  := Correct_Heads + 1;
                        N_Correct_Head := N_Correct_Head + 1;

                        if Tree.Get_Deprel (J) = Gold_Tree.Get_Deprel (J) then
                            Correct_Labels := Correct_Labels + 1;
                        end if;
                    end if;

                    Sum_Arcs := Sum_Arcs + 1;

                    declare
                        Tag : constant String
                          := To_String (Get_Token (Tokens, J).POS);
                        pragma Unreferenced (Tag);
                    begin
                        --if not Punctuation_Tags.Contains (Tag) then
                        if Tree.Get_Deprel (J) /= "punct" then
                            Sum_Arcs_No_Punc := Sum_Arcs_No_Punc + 1;
                            N_No_Punc := N_No_Punc + 1;

                            if Tree.Get_Head (J) = Gold_Tree.Get_Head (J) then
                                Correct_Heads_No_Punc := Correct_Heads_No_Punc + 1;
                                N_Correct_Head_No_Punc := N_Correct_Head_No_Punc + 1;

                                if Tree.Get_Deprel (J) = Gold_Tree.Get_Deprel (J) then
                                    Correct_Labels_No_Punc := Correct_Labels_No_Punc + 1;
                                end if;
                            end if;
                        end if;
                    end;
                end loop;

                if N_Correct_Head = Natural (Tree.Get_Tokens_Count) then
                    Correct_Trees := Correct_Trees + 1;
                end if;

                if N_Correct_Head_No_Punc = N_No_Punc then
                    Correct_Trees_No_Punc := Correct_Trees_No_Punc + 1;
                end if;

                if Tree.Get_Top = Gold_Tree.Get_Top then
                    Correct_Root := Correct_Root + 1;
                end if;
            end;
            -- end if;
        end loop;

        Evaluation.UAS := Real (Correct_Heads) * 100.0 / Real (Sum_Arcs);
        Evaluation.UAS_No_Punct := Real (Correct_Heads_No_Punc) * 100.0 / Real (Sum_Arcs_No_Punc);
        
        Evaluation.LAS := Real (Correct_Labels) * 100.0 / Real (Sum_Arcs);
        Evaluation.LAS_No_Punct := Real (Correct_Labels_No_Punc) * 100.0 / Real (Sum_Arcs_No_Punc);

        Evaluation.UEM := Real (Correct_Trees) * 100.0 / Real (Trees.Length);
        Evaluation.UEM_No_Punct := Real (Correct_Trees_No_Punc) * 100.0 / Real (Trees.Length);
        
        Evaluation.ROOT := Real (Correct_Root) * 100.0 / Real (Trees.Length);

    end Evaluate_Dependency_Tree;
    
end Statistics;
