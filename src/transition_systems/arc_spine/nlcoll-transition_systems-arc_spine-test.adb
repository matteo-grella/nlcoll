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

with Ada.Text_IO; use Ada.Text_IO;
with ARColl.Strings.Unbounded; use ARColl.Strings.Unbounded;

with NLColl.Corpus.Treebank; use NLColl.Corpus.Treebank;

package body NLColl.Transition_Systems.Arc_Spine.Test is

    procedure Test_Transition_Sequence
      (Sentence             : in     Sentence_Type;
       Gold_Dependency_Tree : in     Dependency_Tree_Type;
       Verbose              : in     Boolean := False) is

        State : State_Type;
    begin

        State_Initialize
          (Sentence       => Sentence,
           State          => State);

        Loop_State : while not Is_Final_State (State) loop

            if Verbose then
                New_Line;
                Put_Line (Standard_Error, "---------------------");
            end if;

            if Verbose then
                New_Line;
                Put_Line (Standard_Error, "[Sentence]");
                To_CoNLL
                  (Sentence        => Sentence,
                   Dependency_Tree => State.Dependency_Tree);

                New_Line;
                Put_Line (Standard_Error, "[State]");
                Print_State (State);
            end if;

            if Is_Deterministic_State (State) then
                Perform_Shift (State);
            else

                declare
                    Next_Gold_Action          : Action_Type;
                begin

                    Next_Gold_Action := Get_Next_Correct_Action
                      (State                => State,
                       Gold_Dependency_Tree => Gold_Dependency_Tree);

                    if Verbose then
                        New_Line;
                        Put_Line (Standard_Error, "[Next_Gold_Action]");
                        Put_Line
                          (Standard_Error,
                           "(" & Next_Gold_Action.Confidence'Img & ") "
                           & To_String (Next_Gold_Action));
                    end if;

                    if True then

                        if Next_Gold_Action.Name /= ROOT then
                            declare
                                use NLColl.Transition_Systems.Arc_Spine.State.State_Context;
                                Context : Context_Type;
                            begin

                                Get_Context
                                  (Context          => Context,
                                   State            => State,
                                   Transition       =>
                                     Transition_Type'(Name      => Next_Gold_Action.Name,
                                                      K         => Next_Gold_Action.K,
                                                      Governor  => Next_Gold_Action.Governor,
                                                      Dependent => Next_Gold_Action.Dependent),
                                   Sentence         => Sentence);

                                Print_Context(Context);
                            end;
                        end if;

                    end if;

                    Perform_Action
                      (State  => State,
                       Action => Next_Gold_Action);

                exception

                    when others =>

                        New_Line (Standard_Error);

                        To_CoNLL
                          (Sentence        => Sentence,
                           Dependency_Tree => Gold_Dependency_Tree);

                        New_Line (Standard_Error);

                        To_CoNLL
                          (Sentence        => Sentence,
                           Dependency_Tree => State.Dependency_Tree);

                        New_Line (Standard_Error);

                        Print_State (State);

                        raise;
                end;
            end if;
        end loop Loop_State;

        if Verbose then

            To_CoNLL
              (Sentence        => Sentence,
               Dependency_Tree => State.Dependency_Tree);

            New_Line (Standard_Error);

            Print_State (State);

        end if;

        if not Is_Tree (State.Dependency_Tree) then
            raise Parser_State_Error
              with "Anomaly: Resulting Dependency_Tree is not a Tree";
        end if;


        if State.Dependency_Tree /= Gold_Dependency_Tree then

            New_Line (Standard_Error);

            To_CoNLL
              (Sentence        => Sentence,
               Dependency_Tree => Gold_Dependency_Tree);

            New_Line (Standard_Error);

            To_CoNLL
              (Sentence        => Sentence,
               Dependency_Tree => State.Dependency_Tree);

            New_Line (Standard_Error);

            raise Parser_State_Error
              with "Anomaly: Dependency_Tree differs from the Gold_Dependency_Tree";
        end if;

        if False then

            To_CoNLL
              (Sentence        => Sentence,
               Dependency_Tree => State.Dependency_Tree);

            New_Line (Standard_Error);

        end if;

        State_Finalize (State);

    end Test_Transition_Sequence;

    function Get_Next_Correct_Action
      (State                : in State_Type;
       Gold_Dependency_Tree : in Dependency_Tree_Type) return Action_Type is

        Possible_Transitions : constant Transition_Type_Vectors.Vector
          := Get_Next_Possible_Transitions(State);
    begin

        for Transition of Possible_Transitions loop

            if Is_Correct
              (State                => State,
               Gold_Dependency_Tree => Gold_Dependency_Tree,
               Transition           => Transition,
               Flexibility          => False) then

                declare
                    Action : constant Action_Type
                      := Action_Type'
                        (Name       => Transition.Name,
                         Deprel     =>
                           (if Transition.Name in ARC_LEFT | ARC_RIGHT | ROOT
                            then Gold_Dependency_Tree.Get_Deprel(Transition.Dependent)
                            else UNKNOWN_DEPREL),
                         K          => Transition.K,
                         Governor   => Transition.Governor,
                         Dependent  => Transition.Dependent,
                         Confidence => 0.0);
                begin
                    return Action;
                end;
            end if;

        end loop;

        raise Parser_State_Error with "No Correct Action Found";
    end Get_Next_Correct_Action;

end NLColl.Transition_Systems.Arc_Spine.Test;
