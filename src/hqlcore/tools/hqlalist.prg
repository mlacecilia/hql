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

 \brief [public_function] creates hql_alist object
 \param(IN)
 \return hql_alist object

*/
FUNCTION hqlAlist( ... )
RETURN hql_alist():new( ... )

/*!

 \brief hql_alist class definition

*/
CLASS hql_alist INHERIT hql_hqlobj

   EXPORTED:
   METHOD init
   METHOD hqlCleaner
   METHOD append
   METHOD at
   METHOD clear
   METHOD cloned
   METHOD contains( ... )                 INLINE ( ::indexOf( ... ) > 0 )
   METHOD count                           INLINE ( ::size() )
   METHOD first                           INLINE ( ::at(1) )
   METHOD indexOf
   METHOD insert
   METHOD isEmpty                         INLINE ( LEN(::aItems) == 0 )
   METHOD isValidData( ... )              INLINE ( ::__isValidData( ... ) )
   METHOD isValidIndex( ... )             INLINE ( ::__isValidIndex( ... ) )
   METHOD items                           INLINE ( ACLONE(::aItems) )
   METHOD last                            INLINE ( ::at(::size()) )
   METHOD move
   METHOD prepend
   METHOD removeAt
   METHOD removeFirst                     INLINE ( ::removeAt(1) )
   METHOD removeFromTo
   METHOD removeLast                      INLINE ( ::removeAt(::size()) )
   METHOD replace
   METHOD size                            INLINE ( LEN(::aItems) )
   METHOD swap
   METHOD takeAt
   METHOD value
   METHOD __enumStart                     // FOR EACH overloading harbour/tests/foreach2.prg
   OPERATOR "[]" ARGS ...                 INLINE ::__h_OpArrayBrackets( ... )
   OPERATOR "==" ARGS ...                 INLINE ::__h_OpExactBinaryEqual( ... )

   PROTECTED:
   VAR aItems                             INIT {}
   METHOD __isValidData
   METHOD __isValidIndex

   HIDDEN:
   METHOD __h_hqlCleaner
   METHOD __h_OpArrayBrackets              // overloading "[]" operator eg object[n] := .... and xVar := object[n]
   METHOD __h_OpExactBinaryEqual           // overloading "==" operator

   METHOD __h_hqlparsingInit
ENDCLASS

/*!

 \brief creates a new object instance
 \param(IN)
 \return SELF

*/
METHOD init( ... ) CLASS hql_alist
   ::__h_hqlparsingInit( ... )
RETURN Self

/*!

 \brief callable hql destroyer
 \param(IN)
 \return NIL

*/
METHOD hqlCleaner() CLASS hql_alist
   ::__h_hqlCleaner()
RETURN NIL

/*!

 \brief append value to the end of list and returns new list size; WARNING value must be a valid value for the list
 \param(IN) ...
 \return numeric

*/
METHOD append( xValue ) CLASS hql_alist
RETURN ::insert( ::size()+1, xValue )

/*!

 \brief returns value at given index if valid OTHERWISE NIL
 \param(IN) numeric
 \return variant

*/
METHOD at( nI ) CLASS hql_alist
   nI := hb_DefaultValue(nI, 0 )
   IF ( ::__isValidIndex(nI) )
      RETURN ::aItems[nI]
   ENDIF
RETURN NIL

/*!

 \brief clear list
 \param(IN)
 \return self

*/
METHOD clear() CLASS hql_alist
   ::__h_hqlCleaner()
RETURN self

/*!

 \brief returns list object cloned
 \param(IN)
 \return list

*/
METHOD cloned() CLASS hql_alist
   LOCAL cBlock := "{ |oself| " + ::className() + "():new(oself)}"
RETURN EVAL( &cBlock, self )

/*!

 \brief returns index position for given value when found OTHERWISE zero returned; WARNING valu must be a valid data for the list
 \param(IN) variant [, fromIndex]
 \return numeric

*/
METHOD indexOf( arg1, nStart ) CLASS hql_alist
   LOCAL nI := 0
   nStart := hb_DefaultValue(nStart, 1)
   IF ( ::__isValidData( arg1 ) )
      nStart := IIF ( ::__isValidIndex(nStart), nStart, 1 )
      nI := ASCAN( ::aItems, { |item| ( VALTYPE(item)==VALTYPE(arg1) .AND. item==arg1 ) }, nStart )
   ENDIF
RETURN nI

/*!

 \brief insert value at given index; when index is out of range will be appended. Returns new list size.
   WARNING value must be a valid value for the list else -1 returned
 \param(IN) numeric, variant
 \return numeric

*/
METHOD insert( nI, xValue ) CLASS hql_alist
   nI := hb_DefaultValue(nI, 0 )
   IF ( ::__isValidData( xValue ) )
      IF ( ::__isValidIndex(nI) )
         hb_Ains(::aItems, nI, xValue, .T.)
      ELSE
         AADD( ::aItems, xValue )
         nI := ::size()
      ENDIF
   ELSE
      nI := -1 // to know error
   ENDIF
RETURN nI

/*!

 \brief move element from position to position. When position[s] out of range -1 returned
 \param(IN) numeric, numeric
 \return numeric

*/
METHOD move( nFrom, nTo ) CLASS hql_alist
   IF ( ::__isValidIndex( nFrom ) .AND. ::__isValidIndex( nTo ) )
      RETURN ::insert( nTo, ::takeAt( nFrom ) )
   ENDIF
RETURN -1

/*!

 \brief prepend value to the list and returns new list size. WARNING -1 can be returned
 \param(IN) value
 \return numeric

*/
METHOD prepend( xValue ) CLASS hql_alist
RETURN ::insert( 1, xValue )

/*!

 \brief remove item from list at given index and returns new list size; WARNING when index out of range -1 returned
 \param(IN) numeric
 \return numeric

*/
METHOD removeAt( nI ) CLASS hql_alist
   nI := hb_DefaultValue(nI, 0 )
   IF ( ::__isValidIndex(nI) )
      hb_Adel( ::aItems, nI, .T. )
      RETURN ::size()
   ENDIF
RETURN -1   // to know error

/*!

 \brief removes items fromIndex ... toIndex and returns new list size. Returns -1 when given index out of range
 \param(IN) numeric, numeric
 \return numeric

*/
METHOD removeFromTo( nFrom, nTo ) CLASS hql_alist
   LOCAL aTemp
   LOCAL nI
   LOCAL nReturn := -1
   IF ( ::__isValidIndex( nFrom ) .AND. ::__isValidIndex( nTo ) .AND. nTo >= nFrom )
      aTemp := {}
      FOR nI := 1 TO LEN( ::aItems )
         IF ( nI < nFrom .OR. nI > nTo )
            AADD( aTemp, ::aItems[ nI ] )
         ENDIF
      NEXT nI
      nReturn := LEN( ::aItems ) - LEN( aTemp )
      ::aItems := ACLONE( aTemp )
   ENDIF
RETURN nReturn

/*!

 \brief replace value at given index. Index must be in the range and value must be a valid value for the list otherwise -1 returned

*/
METHOD replace( nI, xValue ) CLASS hql_alist
   nI := hb_DefaultValue( nI, 0 )
   IF ( ::__isValidData( xValue ) .AND. ::__isValidIndex(nI) )
      ::aItems[nI] := xValue
      RETURN nI
   ENDIF
RETURN -1   // to know error

/*!

 \brief swap value for given index
 \param(IN) numeric, numeric
 \return self

*/
METHOD swap( nFrom, nTo ) CLASS hql_alist
   LOCAL xDummy
   IF ( ::__isValidIndex( nFrom ) .AND. ::__isValidIndex( nTo ) .AND. nFrom != nTo )
      xDummy := ::aItems[ nFrom ]
      ::aItems[ nFrom ] := ::aItems[ nTo ]
      ::aItems[ nTo ] := xDummy
   ENDIF
RETURN self

/*!

 \brief returns value at given index and removes from list. WARNING NIL returned when index out of range
 \param(IN) numeric
 \return variant

*/
METHOD takeAt( nI ) CLASS hql_alist
   LOCAL xValue
   nI := hb_DefaultValue(nI, 0)
   IF ( ::__isValidIndex(nI) )
      xValue := ::aItems[nI]
      hb_Adel( ::aItems, nI, .T. )
      RETURN xValue
   ENDIF
RETURN NIL

/*!

 \brief returns value at given index. Can be specified a default value to be returned when index out of range OTHERWISE NIL returned
 \param(IN) numeric [, default]
 \return variant

*/
METHOD value( nI, xDefault ) CLASS hql_alist
   nI := hb_DefaultValue( nI, 0 )
   IF ( ::__isValidIndex( nI ) )
      RETURN ::aItems[nI]
   ENDIF
   IF ( PCOUNT() == 2 )
      RETURN xDefault
   ENDIF
RETURN NIL

/*!

 \brief FOR EACH overloading; returns true (can start) or false
      harbour/tests/foreach2.prg
      https://sourceforge.net/p/hmgs-minigui/svncode/223/tree/trunk/MiniGUI/SOURCE/HbOLE/win32ole.prg#l150
 \param(IN) ...
 \return bool

*/
METHOD __enumStart( enum, lDescend ) CLASS hql_alist
   lDescend := hb_DefaultValue(lDescend, .F.)
   (@enum):__enumBase(::aItems)
   //   (@enum):__enumBase( IIF( lDescend, ASORT( ::aItems, , , {|e_,f_| e_[ 1 ] < f_[ 1 ] } ), ::aItems ) )
   //(@enum):__enumBase( IIF( lDescend, ASORT( ::aItems, , , { |e_,f_| e_ < f_ } ), ::aItems ) )
   HB_SYMBOL_UNUSED(lDescend)
RETURN ( LEN(::aItems) > 0 ) /* .F. means stop iteration */

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] returns true if data can be added/insert; by default always true
 \param(IN) ...
 \return bool

*/
METHOD __isValidData( ... ) CLASS hql_alist
RETURN .T.

/*!

 \brief [PROTECTED] returns true if index is in the range 1 <= x <= ::size() valid OTHERWISE false

*/
METHOD __isValidIndex( nI ) CLASS hql_alist
RETURN ( hb_IsNumeric(nI) .AND. nI >= 1 .AND. nI <= ::size() )

// ==================== HIDDEN section ====================

/*!

 \brief [HIDDEN] overloading "[]" operator

*/
METHOD __h_OpArrayBrackets( ... ) CLASS hql_alist
   IF ( PCOUNT() == 2 )
      ::replace(hb_Pvalue(1), hb_Pvalue(2))
   ENDIF
RETURN ( ::at(hb_Pvalue(1)) )

/*!

 \brief [HIDDEN] overloading "==" operator

*/
METHOD __h_OpExactBinaryEqual( arg1 ) CLASS hql_alist
   LOCAL lEqual := .F.
   LOCAL nI
   IF ( __vmItemId( self ) == __vmItemId( arg1 ) )
      lEqual := .T.
   ELSE
      IF ( ::size() == arg1:size() )
         lEqual := .T.
         FOR nI := 1 TO ::size()
            IF ( !( VALTYPE(::aItems[nI]) == VALTYPE(arg1[nI]) .AND. ::aItems[nI] == arg1[nI] ) )
               lEqual := .F.
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF
RETURN lEqual

/*!

 \brief [HIDDEN] handle init arguments
 \param(IN) ...
 \return NIL

*/
METHOD __h_hqlparsingInit( arg1 ) CLASS hql_alist
   LOCAL nAt

   IF ( hql_IsDerived(arg1, "hql_alist") )
      ::aItems := arg1:items()
      RETURN NIL
   ENDIF

   IF ( hb_IsArray(arg1) )
      FOR nAt := 1 TO LEN(arg1)
         IF ( ::__isValidData(arg1[nAt]) )
            AADD( ::aItems, arg1[nAt] )
         ENDIF
      NEXT
      RETURN NIL
   ENDIF

   IF ( PCOUNT() == 1 .AND. ::__isValidData(arg1) )
      AADD( ::aItems, arg1 )
   ENDIF
RETURN NIL

/*!

 \brief [HIDDEN] hql destroyer.
 \param(IN)
 \return NIL

*/
METHOD __h_hqlCleaner() CLASS hql_alist
   ::aItems := {}
RETURN NIL
