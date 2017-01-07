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

with Text_IO; use Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with ARColl.Strings; use ARColl.Strings;

package body NLColl.Linguistic_Description.Syntax.Dependency_Trees is

    package body Statistics is separate;
        
    procedure Clear
      (Dependency_Tree : in out Dependency_Tree_Type) is
    begin
        Dependency_Tree.Heads.Clear;
        Dependency_Tree.Deprels.Clear;
        Dependency_Tree.Dependents.Clear;
    end Clear;

    procedure Set_Arc
      (Dependency_Tree : in out Dependency_Tree_Type;
       Governor        : in     Extended_Token_Index_Type;
       Dependent       : in     Token_Index_Type;
       Deprel          : in     Unbounded_String) is
        
        Max_Token_Index : constant Extended_Token_Index_Type
          := (if Governor > Dependent
              then Governor
              else Dependent);
                  
        use Ada.Containers;
    begin
        
        -- Adjust Dependency_Tree size
        
        if Max_Token_Index > Dependency_Tree.Heads.Last_Index then
            for I in Dependency_Tree.Deprels.Last_Index + 1 .. Max_Token_Index loop
                Dependency_Tree.Heads.Append (NONE_ID);
                Dependency_Tree.Deprels.Append (Null_Unbounded_String);
                Dependency_Tree.Dependents.Append (Empty_Dependents_Structure);
            end loop;
        end if;
        
        -- Set Arc
        
        Dependency_Tree.Heads.Replace_Element (Dependent, Governor);
        Dependency_Tree.Deprels.Replace_Element (Dependent, Deprel);
        
        --Put_Line("Exec set_dependent gov" & Governor'Img & "  dep " & Dependent'Img);
        
        -- Set Dependent
        
        Set_Dependent
          (Dependency_Tree       => Dependency_Tree,
           Token_Index           => Governor, 
           Dependent_Token_Index => Dependent);
        
    end Set_Arc;

    procedure Set_Dependent
      (Dependency_Tree       : in out Dependency_Tree_Type;
       Token_Index           : in     Extended_Token_Index_Type;
       Dependent_Token_Index : in     Token_Index_Type) is
    begin
        
        if Token_Index > ROOT_ID then -- skip Root
            
            -- Check token position to insert into Left or Right dependents
            
            if Token_Index > Dependent_Token_Index then
                Dependency_Tree.Dependents.Reference(Token_Index)
                  .Left.Insert(Dependent_Token_Index);
            else
                Dependency_Tree.Dependents.Reference(Token_Index)
                  .Right.Insert(Dependent_Token_Index);
            end if;
            
        end if;
        
    end Set_Dependent;
    
    function Is_Resolved
      (ID                     : in Token_Index_Type;
       Dependency_Tree        : in Dependency_Tree_Type;
       Gold_Dependency_Tree   : in Dependency_Tree_Type) return Boolean is

        Number_Of_Dependent      : constant Natural
          := Get_Number_Of_Dependent (Dependency_Tree, ID);
        
        Number_Of_Gold_Dependent : constant Natural
          := Get_Number_Of_Dependent (Gold_Dependency_Tree, ID);
    begin
        return Number_Of_Gold_Dependent = Number_Of_Dependent;
    end Is_Resolved;
    
    function Get_Left_Dependent
      (Token           : in Token_Access;
       Position        : in Positive;
       Sentence        : in Sentence_Type;
       Dependency_Tree : in Dependency_Tree_Type) return Token_Access is
        
        use Token_Vectors;
    begin
        
        if Token /= null then
            declare
                Token_ID : constant Token_Index_Type 
                  := Token.ID;
                
                I        : Positive := 1; 
            begin

                for Dep_Token_ID of Dependency_Tree.Dependents.Element (Token_ID).Left loop
                        
                    if I = Position then
                        return Sentence.Elements (Index_Type (Dep_Token_ID - 1));
                    end if;
                    
                    I := I + 1;
                end loop;

            end;
        end if;
        
        return null;
        
    end Get_Left_Dependent;
    
    function Get_Right_Dependent
      (Token                : in Token_Access;
       Position             : in Positive;
       Sentence             : in Sentence_Type;
       Dependency_Tree      : in Dependency_Tree_Type) return Token_Access is
        
        use Token_Vectors;
    begin
        
        if Token /= null then
            declare
                Token_ID : constant Token_Index_Type 
                  := Token.ID;
                
                I        : Positive := 1; 
            begin
                for Dep_Token_ID of Dependency_Tree.Dependents.Element (Token_ID).Right loop
                        
                    if I = Position then
                        return Sentence.Elements (Index_Type (Dep_Token_ID - 1));
                    end if;
                        
                    I := I + 1;
                end loop;
            end;
        end if;
        
        return null;
    end Get_Right_Dependent;
    
    function Has_Dependent_With_Deprel
      (Dependency_Tree : in Dependency_Tree_Type;
       Token           : in Token_Index_Type;
       Deprel          : in String) return Boolean is
    begin
            
        for Dep_Token : Token_Index_Type of 
          Dependency_Tree.Dependents.Element (Token).Left loop
                
            if Dependency_Tree.Deprels.Element (Dep_Token) = Deprel then
                return True;
            end if;
        end loop;
            

        for Dep_Token : Token_Index_Type of 
          Dependency_Tree.Dependents.Element (Token).Right loop
                
            if Dependency_Tree.Deprels.Element (Dep_Token) = Deprel then
                return True;
            end if;
        end loop;
            
        return False;
            
    end Has_Dependent_With_Deprel;
    
    function Get_Top
      (Dependency_Tree : in Dependency_Tree_Type) return Extended_Token_Index_Type is
    begin
        return Top_Token_Index : Extended_Token_Index_Type := -1 do
            for Token_Index in 1 .. Dependency_Tree.Heads.Last_Index loop
                if Dependency_Tree.Heads.Element (Token_Index) = ROOT_ID then
                    Top_Token_Index := Token_Index;
                    exit;
                end if;
            end loop;
        end return;
    end Get_Top;

    function Get_Governors
      (Dependency_Tree    : in Dependency_Tree_Type;
       Token_Index_Vector : in Token_Index_Vectors.Vector) return Token_Index_Vectors.Vector is
    begin
        return Governor_List : Token_Index_Vectors.Vector do
            for T of Token_Index_Vector loop
                Governor_List.Append(Dependency_Tree.Get_Head(T));
            end loop;
        end return;
    end Get_Governors;

    
    function Is_Single_Top
      (Dependency_Tree : in Dependency_Tree_Type) return Boolean is

        Tops_Count : Natural := 0;
    begin
        for Token_Index in 1 .. Dependency_Tree.Heads.Last_Index loop
            if Dependency_Tree.Heads.Element (Token_Index) = 0 then
                Tops_Count := Tops_Count + 1;
            end if;
        end loop;

        return (Tops_Count = 1);
    end Is_Single_Top;

    function Is_Tree
      (Dependency_Tree : in Dependency_Tree_Type) return Boolean is

        H : Head_Vectors.Vector;
    begin
        H.Reserve_Capacity (Dependency_Tree.Heads.Length);
        H.Append (NONE_ID); -- ROOT (index 0)

        for Token_Index in 1 .. Dependency_Tree.Heads.Last_Index loop
            declare
                Token_Head : constant Extended_Token_Index_Type
                  := Dependency_Tree.Heads.Element (Token_Index);
            begin
                if Token_Head < 0
                  or else Token_Head > Dependency_Tree.Heads.Last_Index then
                    return False;
                end if;
                H.Append (NONE_ID);
            end;
        end loop;

        for Token_Index in 1 .. Dependency_Tree.Heads.Last_Index loop
            declare
                K : Extended_Token_Index_Type := Token_Index;
            begin
                while K > 0 loop
                    declare
                        H_K : constant Extended_Token_Index_Type
                          := H.Element (K);
                    begin
                        exit when H_K >= 0
                          and then H_K < Token_Index;

                        if H_K = Token_Index then
                            return False;
                        end if;

                        H.Replace_Element (K, Token_Index);
                        K := Dependency_Tree.Heads.Element (K);
                    end;
                end loop;
            end;
        end loop;

        return True;
    end Is_Tree;

    function Is_Projective
      (Dependency_Tree : in Dependency_Tree_Type) return Boolean is

        Counter : Extended_Token_Index_Type := Extended_Token_Index_Type'First;
        -- Counter for projectivity check

        function Visit_Tree
          (W : Extended_Token_Index_Type) return Boolean with Inline is
        -- Inner recursive function for checking projectivity of tree

        begin
            if W > 1 then
                for I in 1 .. W - 1 loop
                    if Dependency_Tree.Heads.Element (I) = W
                      and then not Visit_Tree (I) then
                        return False;
                    end if;
                end loop;
            end if;

            Counter := Counter + 1;
            if W /= Counter then
                return False;
            end if;

            for I in W + 1 .. Dependency_Tree.Heads.Last_Element loop
                if Dependency_Tree.Heads.Element (I) = W
                  and then not Visit_Tree (I) then
                    return False;
                end if;
            end loop;

            return True;
        end Visit_Tree;

    begin
        if not Dependency_Tree.Is_Tree then
            return False;
        end if;

        Counter := -1;
        return Visit_Tree (0);
    end Is_Projective;

    
        
    -----
    -- Controlled
    -----

    overriding procedure Initialize
      (Dependency_Tree : in out Dependency_Tree_Type) is
    begin
        Dependency_Tree.Clear;
    end Initialize;

    overriding procedure Finalize
      (Dependency_Tree : in out Dependency_Tree_Type) is
    begin
        Dependency_Tree.Clear;
    end Finalize;

end NLColl.Linguistic_Description.Syntax.Dependency_Trees;
