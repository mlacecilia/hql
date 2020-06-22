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

 \brief Returns a new hql_chartSerie object instance

*/
FUNCTION hqlChartSerie( ... )
RETURN hql_chartSerie():new( ... )

/*!

 \brief define hql_chartSerie class

*/
CLASS hql_chartSerie INHERIT hql_chartObj

   EXPORTED:
   METHOD init
   METHOD addPiece
   METHOD at
   METHOD averageHeight
   METHOD averageWidth
   METHOD clear
   METHOD heightRange                     INLINE ( ::maxHeight() - ::minHeight() )
   METHOD isEmpty                         INLINE ( ::oPlist:size() == 0 )
   METHOD maxHeight
   METHOD maxWidth
   METHOD minHeight
   METHOD minWidth
   METHOD pieceCount                      INLINE ( ::size() )
   METHOD setUniqueColor
   METHOD size                            INLINE ( ::oPlist:size() )
   METHOD swapValues
   METHOD widthRange                      INLINE ( ::maxWidth() - ::minWidth() )
   METHOD __enumStart                     // FOR EACH overloading harbour/tests/foreach2.prg

   PROTECTED:
   VAR oPlist                             INIT NIL

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( cName, cColor, cText, cTooltip ) CLASS hql_chartSerie
   ::cColor := ::__randomColor()
   ::oPlist := hqlChartPlist()

   ::setName( cName )
   ::setColor( cColor )
   ::setText( cText )
   ::setTooltip( cTooltip )
RETURN self

/*!

 \brief add a new piece
 \param(IN) hql_chartPiece
 \return self

*/
METHOD addPiece( ... ) CLASS hql_chartSerie
   ::oPlist:append( ... )
RETURN self

/*!

 \brief returns piece at given index
 \param(IN) numeric
 \return hql_chartPiece

*/
METHOD at( ... ) CLASS hql_chartSerie
RETURN ( ::oPlist:at( ... ) )

/*!

 \brief returns the average height
 \param(IN)
 \return numeric

*/
METHOD averageHeight() CLASS hql_chartSerie
   LOCAL nAt, nReturn := 0
   IF ( ::oPlist:size() > 0 )
      FOR nAt := 1 TO ::oPlist:size()
         nReturn += ::oPlist:at(nAt):height()
      NEXT
      nReturn /= ::oPlist:size()
   ENDIF
RETURN nReturn

/*!

 \brief returns the average width
 \param(IN)
 \return numeric

*/
METHOD averageWidth() CLASS hql_chartSerie
   LOCAL nAt, nReturn := 0
   IF ( ::oPlist:size() > 0 )
      FOR nAt := 1 TO ::oPlist:size()
         nReturn += ::oPlist:at(nAt):width()
      NEXT
      nReturn /= ::oPlist:size()
   ENDIF
RETURN nReturn

/*!

 \brief removes data
 \param(IN)
 \return self

*/
METHOD clear() CLASS hql_chartSerie
   ::oPlist:clear()
RETURN self

/*!

 \brief returns MAX height
 \param(IN)
 \return numeric

*/
METHOD maxHeight() CLASS hql_chartSerie
   LOCAL nAt, nReturn := 0
   IF ( ::oPlist:size() > 0 )
      nReturn := ::oPlist:at(1):height()
      FOR nAt := 2 TO ::oPlist:size()
         nReturn := MAX( nReturn, ::oPlist:at(nAt):height() )
      NEXT
   ENDIF
RETURN nReturn

/*!

 \brief returns MAX width
 \param(IN)
 \return numeric

*/
METHOD maxWidth() CLASS hql_chartSerie
   LOCAL nAt, nReturn := 0
   IF ( ::oPlist:size() > 0 )
      nReturn := ::oPlist:at(1):width()
      FOR nAt := 2 TO ::oPlist:size()
         nReturn := MAX( nReturn, ::oPlist:at(nAt):width() )
      NEXT
   ENDIF
RETURN nReturn

/*!

 \brief returns MIN height
 \param(IN)
 \return numeric

*/
METHOD minHeight() CLASS hql_chartSerie
   LOCAL nAt, nReturn := 0
   IF ( ::oPlist:size() > 0 )
      nReturn := ::oPlist:at(1):height()
      FOR nAt := 2 TO ::oPlist:size()
         nReturn := MIN( nReturn, ::oPlist:at(nAt):height() )
      NEXT
   ENDIF
RETURN nReturn

/*!

 \brief returns MIN width
 \param(IN)
 \return numeric

*/
METHOD minWidth() CLASS hql_chartSerie
   LOCAL nAt, nReturn := 0
   IF ( ::oPlist:size() > 0 )
      nReturn := ::oPlist:at(1):width()
      FOR nAt := 2 TO ::oPlist:size()
         nReturn := MIN( nReturn, ::oPlist:at(nAt):width() )
      NEXT
   ENDIF
RETURN nReturn

/*!

 \brief set piece's color as serie's color
 \param(IN)
 \return self

*/
METHOD setUniqueColor() CLASS hql_chartSerie
   LOCAL nAt
   FOR nAt := 1 TO ::oPlist:size()
      ::oPlist:at(nAt):setColor( ::color() )
   NEXT
RETURN self

/*!

 \brief swap width, height values
 \param[in] none
 \return self

*/
METHOD swapValues() CLASS hql_chartSerie
   LOCAL nAt
   FOR nAt := 1 TO ::oPlist:size()
      ::oPlist:at(nAt):swapValues()
   NEXT
RETURN self

/*!

 \brief FOR EACH overloading; returns true (can start) or false
      harbour/tests/foreach2.prg
      https://sourceforge.net/p/hmgs-minigui/svncode/223/tree/trunk/MiniGUI/SOURCE/HbOLE/win32ole.prg#l150
 \param(IN) ...
 \return bool

*/
METHOD __enumStart( ... ) CLASS hql_chartSerie
RETURN ( ::oPlist:__enumStart( ... ) )

// ==================== PROTECTED section ====================


////////////////////////////////////////  ====  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


/*!

 \brief [public_function] creates hql_chartPlist object
 \param(IN)
 \return hql_chartPlist object

*/
STATIC FUNCTION hqlChartPlist( ... )
RETURN hql_chartPlist():new( ... )

/*!

 \brief hql_chartPlist class definition

*/
CLASS hql_chartPlist INHERIT hql_alist STATIC

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
METHOD init( ... ) CLASS hql_chartPlist
   ::hql_alist:init()
RETURN Self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] returns true if data can be added/insert; by default always true
 \param(IN) ...
 \return bool

*/
METHOD __isValidData( arg1 ) CLASS hql_chartPlist
RETURN ( hb_IsObject(arg1) .AND. arg1:isDerivedFrom( "hql_chartPiece" ) )
