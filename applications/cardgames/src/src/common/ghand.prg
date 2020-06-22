/****************************************************************************
** $HQL_BEGIN_LICENSE$
**
** Copyright (C) 2020 by Luigi Ferraris
**
** This file is part of HQL project
**
** HQL is free software: you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation, either version 3 of the License, or
** (at your option) any later version.
**
** HQL is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with HQL.  If not, see <http://www.gnu.org/licenses/>.
**
** $HQL_END_LICENSE$
****************************************************************************/
#include "hbclass.ch"
#include "cardgames.ch"

/*!

 \brief Returns a new g_hand object instance

FUNCTION gHand( ... )
RETURN g_hand():new( ... )
*/

/*!

 \brief define g_hand class

*/
CLASS g_hand

   EXPORTED:
   METHOD init
   METHOD append
   METHOD at
   METHOD choose
   METHOD clear
   METHOD find
   METHOD first                           INLINE ( ::at(1) )
   METHOD isValidIndex
   METHOD last                            INLINE ( ::at(::size()) )
   METHOD prepend
   METHOD setFace
   METHOD size                            INLINE ( LEN(::aList) )
   METHOD sort
   METHOD takeAt
   METHOD __enumStart                     // FOR EACH overloading harbour/tests/foreach2.prg

   PROTECTED:
   VAR aList                              INIT {}

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance
 \param(IN)
 \return self

*/
METHOD init() CLASS g_hand
RETURN self

/*!

 \brief append a card
 \param(IN) object
 \return self

*/
METHOD append( oCard ) CLASS g_hand
   IF ( hb_IsObject(oCard) .AND. oCard:isDerivedFrom("g_card") )
      AADD( ::aList, oCard )
   ENDIF
RETURN self

/*!

 \brief returns card at given index
 \param(IN) numeric
 \return card

*/
METHOD at( nIndex ) CLASS g_hand
   IF ( ::isValidIndex(nIndex) )
      RETURN ::aList[nIndex]
   ENDIF
RETURN g_card():new()

/*!

 \brief choose a card and returns index
 \param(IN)
 \return numeric

*/
METHOD choose() CLASS g_hand
   LOCAL nR
   SWITCH ( ::size() )
   CASE 0
      nR := 0
      EXIT
   CASE 1
      nR := 1
      EXIT
   OTHERWISE
      nR := hb_RandomInt( ::size() )
   ENDSWITCH
RETURN nR

/*!

 \brief clear/remove all cards
 \param(IN)
 \return self

*/
METHOD clear() CLASS g_hand
   ::aList := {}
RETURN self


/*!

 \brief returns position for given suit and rank; WARNING 0 can be returned
 \param(IN) numeric, numeric
 \return numeric

*/
METHOD find( nSuit, nRank ) CLASS g_hand
   LOCAL nI, nPos := 0
   nSuit := hb_DefaultValue( nSuit, -1 )  /* to make numeric */
   nRank := hb_DefaultValue( nRank, -1 )  /* to make numeric */
   FOR nI := 1 TO ::size()
      IF ( ::aList[nI]:suit() == nSuit .AND. ::aList[nI]:rank() == nRank )
         nPos := nI
         EXIT
      ENDIF
   NEXT
RETURN nPos

/*!

 \brief returns true if given index is valid
 \param(IN)
 \return self

*/
METHOD isValidIndex( nIndex ) CLASS g_hand
RETURN ( hb_IsNumeric(nIndex) .AND. nIndex >= 1 .AND. nIndex <= ::size() )

/*!

 \brief pre-pend a card
 \param(IN)
 \return self

*/
METHOD prepend( oCard ) CLASS g_hand
   IF ( hb_IsObject(oCard) .AND. oCard:isDerivedFrom("g_card") )
      hb_Ains( ::aList, 1, oCard, .T.)
   ENDIF
RETURN self

/*!

 \brief set face for all cards
 \param(IN)
 \return self

*/
METHOD setFace( arg1 ) CLASS g_hand
   LOCAL nAt
   IF ( hb_IsNumeric(arg1) .AND. (arg1 == FACE_BACK .OR. arg1 == FACE_FRONT) )
      FOR nAt := 1 TO ::size()
         ::aList[nAt]:setFace( arg1 )
      NEXT
   ENDIF
RETURN self

/*!

 \brief sort cards based on theri weight
 \param(IN)
 \return self

*/
METHOD sort() CLASS g_hand
   ASORT( ::aList,,, { | x, y | x:weight() < y:weight() } )
RETURN self

/*!

 \brief returns and removes element at given index
 \param(IN)
 \return self

*/
METHOD takeAt( nIndex ) CLASS g_hand
   LOCAL oItem
   IF ( ::isValidIndex(nIndex) )
      oItem := ::aList[nIndex]
      hb_Adel( ::aList, nIndex, .T. )
      RETURN oItem
   ENDIF
RETURN g_card():new()

/*!

 \brief FOR EACH overloading; returns true (can start) or false
      harbour/tests/foreach2.prg
      https://sourceforge.net/p/hmgs-minigui/svncode/223/tree/trunk/MiniGUI/SOURCE/HbOLE/win32ole.prg#l150
 \param(IN) ...
 \return bool

*/
METHOD __enumStart( enum, lDescend ) CLASS g_hand
   (@enum):__enumBase(::aList)
   HB_SYMBOL_UNUSED(lDescend)
RETURN ( LEN(::aList) > 0 ) /* .F. means stop iteration */
