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

/*!

 \brief Returns a new hql_chartData object instance

*/
FUNCTION hqlChartData( ... )
RETURN hql_chartData():new( ... )

/*!

 \brief define hql_chartData class

*/
CLASS hql_chartData INHERIT hql_chartObj

   EXPORTED:
   METHOD init
   METHOD addSerie
   METHOD at
   METHOD averageHeight
   METHOD averageWidth
   METHOD clear
   METHOD heightRange                     INLINE ( ::maxHeight() - ::minHeight() )
   METHOD isEmpty
   METHOD maxHeight
   METHOD maxPieces
   METHOD maxWidth
   METHOD minHeight
   METHOD minPieces
   METHOD minWidth
   METHOD pieceCount
   METHOD serieCount                      INLINE ( ::oSlist:size() )
   METHOD size                            INLINE ( ::oSlist:size() )
   METHOD swapValues
   METHOD widthRange                      INLINE ( ::maxWidth() - ::minWidth() )
   METHOD __enumStart                     // FOR EACH overloading harbour/tests/foreach2.prg

   PROTECTED:
   VAR oSlist                             INIT NIL

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance
 \param(IN) ...
 \return self

*/
METHOD init( cName, cColor, cText, cTooltip ) CLASS hql_chartData
   ::oSlist := hqlChartSlist()

   ::cColor := ::__randomColor()
   ::setName( cName )
   ::setColor( cColor )
   ::setText( cText )
   ::setTooltip( cTooltip )
RETURN self

/*!

 \brief add a new serie
 \param(IN) hql_chartSerie
 \return self

*/
METHOD addSerie( ... ) CLASS hql_chartData
   ::oSlist:append( ... )
RETURN self

/*!

 \brief returns serie at given index
 \param(IN) numeric
 \return hql_chartSerie

*/
METHOD at( ... ) CLASS hql_chartData
RETURN ( ::oSlist:at( ... ) )

/*!

 \brief returns the average height
 \param(IN)
 \return numeric

*/
METHOD averageHeight() CLASS hql_chartData
   LOCAL nAt, nReturn := 0
   IF ( ::oSlist:size() > 0 )
      FOR nAt := 1 TO ::oSlist:size()
         nReturn += ::oSlist:at(nAt):averageHeight()
      NEXT
      nReturn /= ::oSlist:size()
   ENDIF
RETURN nReturn

/*!

 \brief returns the average width
 \param(IN)
 \return numeric

*/
METHOD averageWidth() CLASS hql_chartData
   LOCAL nAt, nReturn := 0
   IF ( ::oSlist:size() > 0 )
      FOR nAt := 1 TO ::oSlist:size()
         nReturn += ::oSlist:at(nAt):averageWidth()
      NEXT
      nReturn /= ::oSlist:size()
   ENDIF
RETURN nReturn

/*!

 \brief removes data
 \param(IN)
 \return self

*/
METHOD clear() CLASS hql_chartData
   ::oSlist:clear()
RETURN self

/*!

 \brief returns true if no data otherwise false
 \param(IN)
 \return bool

*/
METHOD isEmpty() CLASS hql_chartData
   LOCAL nAt, lReturn := .T.
   FOR nAt := 1 TO ::oSlist:size()
      lReturn := ( lReturn .AND. ::oSlist:at(nAt):isEmpty() )
      IF ( !lReturn )
         EXIT
      ENDIF
   NEXT
RETURN lReturn

/*!

 \brief returns MAX height
 \param(IN)
 \return numeric

*/
METHOD maxHeight() CLASS hql_chartData
   LOCAL nAt, nReturn := 0
   IF ( ::oSlist:size() > 0 )
      nReturn := ::oSlist:at(1):maxHeight()
      FOR nAt := 2 TO ::oSlist:size()
         nReturn := MAX( nReturn, ::oSlist:at(nAt):maxHeight() )
      NEXT
   ENDIF
RETURN nReturn

/*!

 \brief returns MAX pieces
 \param(IN)
 \return numeric

*/
METHOD maxPieces() CLASS hql_chartData
   LOCAL nAt, nReturn := 0
   IF ( ::oSlist:size() > 0 )
      nReturn := ::oSlist:at(1):pieceCount()
      FOR nAt := 2 TO ::oSlist:size()
         nReturn := MAX( nReturn, ::oSlist:at(nAt):pieceCount() )
      NEXT
   ENDIF
RETURN nReturn

/*!

 \brief returns MAX width
 \param(IN)
 \return numeric

*/
METHOD maxWidth() CLASS hql_chartData
   LOCAL nAt, nReturn := 0
   IF ( ::oSlist:size() > 0 )
      nReturn := ::oSlist:at(1):maxWidth()
      FOR nAt := 2 TO ::oSlist:size()
         nReturn := MAX( nReturn, ::oSlist:at(nAt):maxWidth() )
      NEXT
   ENDIF
RETURN nReturn

/*!

 \brief returns MIN height
 \param(IN)
 \return numeric

*/
METHOD minHeight() CLASS hql_chartData
   LOCAL nAt, nReturn := 0
   IF ( ::oSlist:size() > 0 )
      nReturn := ::oSlist:at(1):minHeight()
      FOR nAt := 2 TO ::oSlist:size()
       nReturn := MIN( nReturn, ::oSlist:at(nAt):minHeight() )
      NEXT
   ENDIF
RETURN nReturn

/*!

 \brief returns MIN pieces
 \param(IN)
 \return numeric

*/
METHOD minPieces() CLASS hql_chartData
   LOCAL nAt, nReturn := 0
   IF ( ::oSlist:size() > 0 )
      nReturn := ::oSlist:at(1):pieceCount()
      FOR nAt := 2 TO ::oSlist:size()
         nReturn := MIN( nReturn, ::oSlist:at(nAt):pieceCount() )
      NEXT
   ENDIF
RETURN nReturn

/*!

 \brief returns MIN width
 \param(IN)
 \return numeric

*/
METHOD minWidth() CLASS hql_chartData
   LOCAL nAt, nReturn := 0
   IF ( ::oSlist:size() > 0 )
      nReturn := ::oSlist:at(1):minWidth()
      FOR nAt := 2 TO ::oSlist:size()
         nReturn := MIN( nReturn, ::oSlist:at(nAt):minWidth() )
      NEXT
   ENDIF
RETURN nReturn

/*!

 \brief returns total pieces number
 \param[in] none
 \return numeric

*/
METHOD pieceCount() CLASS hql_chartData
   LOCAL nAt, nReturn := 0
   FOR nAt := 1 TO ::oSlist:size()
      nReturn += ::oSlist:at(nAt):pieceCount()
   NEXT nAt
RETURN nReturn

/*!

 \brief swap width, height values
 \param[in] none
 \return self

*/
METHOD swapValues() CLASS hql_chartData
   LOCAL nAt
   FOR nAt := 1 TO ::oSlist:size()
      ::oSlist:at(nAt):swapValues()
   NEXT nAt
RETURN self

/*!

 \brief FOR EACH overloading; returns true (can start) or false
      harbour/tests/foreach2.prg
      https://sourceforge.net/p/hmgs-minigui/svncode/223/tree/trunk/MiniGUI/SOURCE/HbOLE/win32ole.prg#l150
 \param(IN) ...
 \return bool

*/
METHOD __enumStart( ... ) CLASS hql_chartData
RETURN ( ::oSlist:__enumStart( ... ) )

// ==================== PROTECTED section ====================


////////////////////////////////////////  ====  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


/*!

 \brief [public_function] creates hql_chartSlist object
 \param(IN)
 \return hql_chartSlist object

*/
STATIC FUNCTION hqlChartSlist( ... )
RETURN hql_chartSlist():new( ... )

/*!

 \brief hql_chartSlist class definition

*/
CLASS hql_chartSlist INHERIT hql_alist STATIC

   EXPORTED:
   METHOD init

   PROTECTED:
   METHOD __isValidData

   HIDDEN:

ENDCLASS

/*!

 \brief creates a new object instance
 \param(IN)
 \return SELF

*/
METHOD init( ... ) CLASS hql_chartSlist
   ::hql_alist:init()
RETURN Self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] returns true if data can be added/insert; by default always true
 \param(IN) ...
 \return bool

*/
METHOD __isValidData( arg1 ) CLASS hql_chartSlist
RETURN ( hb_IsObject(arg1) .AND. arg1:isDerivedFrom( "hql_chartserie" ) )
