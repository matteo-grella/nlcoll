------------------------------------------------------------------------------
--                               N L C O L L
--  N a t u r a l   L a n g u a g e   C o m p o n e n t   C o l l e c t i o n
--
--                       Copyright 2014 M. Grella
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

separate (NLColl.Transition_Systems.Easy_First.State)

package body State_Context is
    
    procedure Print_Context
      (Context : in Context_Type) is
    begin
        Put("Grandparent => " & Img(Get_Token_ID(Context.Grandparent))); Put(" | ");
        Put("Great_Grandparent => " & Img(Get_Token_ID(Context.Great_Grandparent))); 
        Put("Governor => " & Img(Get_Token_ID(Context.Governor))); Put(" | ");
        Put("Dependent => " & Img(Get_Token_ID(Context.Dependent))); 
        New_Line;
    end Print_Context;
    
    procedure Get_Context
      (Context          : in out Context_Type;
       State            : in     State_Type;
       Action           : in     Action_Type;
       Sentence         : in     Sentence_Type) is
                               
        I : Index_Type renames Action.I;
        K : Index_Type renames Action.K;
    begin

        case Action.Name is
            
            when ROOT =>
                Context.Dependent
                  := Sentence.Elements(Index_Type(State.Pending.First_Element.Head_Token) - Index_Type (1));
                
            when ARC_LEFT =>

                Context.Governor := 
                  Get_Token_From_State(Sentence, State.Pending, Integer(I + 1), LEFT, K);

                Context.Dependent
                  := Sentence.Elements(Index_Type(State.Pending.Element(I).Head_Token) - Index_Type (1));
                
                if Context.Governor /= null then
                    Context.Grandparent :=    
                      Get_Token(Sentence.Elements, State.Dependency_Tree.Get_Head(Context.Governor.ID));
                      
                    if Context.Grandparent /= null then
                        Context.Great_Grandparent :=
                          Get_Token(Sentence.Elements, State.Dependency_Tree.Get_Head(Context.Grandparent.ID));
                    end if;
                end if;
                
            when ARC_RIGHT =>

                Context.Governor  := Get_Token_From_State(Sentence, State.Pending, Integer(I), RIGHT, K);
                Context.Dependent := Sentence.Elements(Index_Type(State.Pending.Element(I + 1).Head_Token) - Index_Type (1));
                
                if Context.Governor /= null then
                    Context.Grandparent :=    
                      Get_Token(Sentence.Elements, State.Dependency_Tree.Get_Head(Context.Governor.ID));
    
                    if Context.Grandparent /= null then
                        Context.Great_Grandparent :=
                          Get_Token(Sentence.Elements, State.Dependency_Tree.Get_Head(Context.Grandparent.ID));
                    end if;                    
                end if;

                
            when NONE =>
                raise Parser_State_Error with "Cannot get Context for NULL Action!";

        end case;
    end Get_Context;
    
end State_Context;
