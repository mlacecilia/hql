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
#include "hbtrace.ch"
#include "hbclass.ch"

/*!

 \brief SINGLETON function returns a new hql_config object instance
 \param(IN)
 \return hql_config

*/
FUNCTION hqlSconfig()
   STATIC s_Object
   STATIC s_Once
   hb_ThreadOnce( @s_Once, {|| s_Object := hql_config():new() } )
RETURN s_Object

/*!

 \brief function returns a new hql_config object instance
 \param(IN)
 \return hql_config

*/
FUNCTION hqlLconfig()
RETURN hql_config():new()

/*!

 \brief hql_config class definition

*/
CLASS hql_config STATIC

   EXPORTED:
   METHOD init
   METHOD clear
   METHOD contains
   METHOD get
   METHOD isValidQuery
   METHOD pairAt
   METHOD remove
   METHOD set
   METHOD size                            INLINE ( LEN(::hItems) )
   METHOD type
   METHOD __enumStart                     // FOR EACH overloading harbour/tests/foreach2.prg

   PROTECTED:
   VAR hItems                             INIT { => }
   VAR cSep                               INIT ":"
   METHOD __isValidKey
   METHOD __isValidQuery
   METHOD __queryToTokens
   METHOD __recur_contains
   METHOD __recur_get
   METHOD __recur_remove
   METHOD __recur_set

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance
 \param(IN)
 \return self

*/
METHOD init() CLASS hql_config
RETURN self

/*!

 \brief remove any pair
 \param(IN)
 \return self

*/
METHOD clear() CLASS hql_config
   ::hItems := { => }
RETURN self

/*!

 \brief returns true if key exist otherwise false
 \param(IN) string
 \return bool

*/
METHOD contains( cKey ) CLASS hql_config
   IF ( ::__isValidQuery( cKey ) )
      RETURN ( ::__recur_contains( ::__queryToTokens( cKey ), 1, ::hItems ) )
   ENDIF
RETURN .F.

/*!

 \brief get value for given key
 \param(IN) string
 \return variant

*/
METHOD get( cKey, ... ) CLASS hql_config
   LOCAL xValue := NIL
   IF ( ::__isValidQuery( cKey ) )
      xValue := ::__recur_get( ::__queryToTokens( cKey ), 1, ::hItems )
   ENDIF
   IF ( PCOUNT() == 2 )
      xValue := hb_DefaultValue( xValue, hb_Pvalue(2) )
   ENDIF
RETURN xValue

/*!

 \brief returns true if string is a valid query
 \param(IN) string
 \return bool

*/
METHOD isValidQuery( cKey ) CLASS hql_config
RETURN ( ::__isValidQuery( cKey ) )

/*!

 \brief returns {key, value} at given index
 \param(IN) numeric
 \return array

*/
METHOD pairAt( nIndex ) CLASS hql_config
   IF ( hb_IsNumeric(nIndex) .AND. nIndex >= 1 .AND. nIndex <= ::size() )
      RETURN hb_HpairAt( ::hItems, nIndex )
   ENDIF
RETURN {}

/*!

 \brief remove key
 \param(IN) string
 \return bool

*/
METHOD remove( cKey ) CLASS hql_config
   IF ( ::__isValidQuery( cKey ) )
      RETURN ( ::__recur_remove( ::__queryToTokens( cKey ), 1, ::hItems ) )
   ENDIF
RETURN .F.

/*!

 \brief set pair key, value
 \param(IN) string, variant
 \return bool

*/
METHOD set( cKey, xValue ) CLASS hql_config
   IF ( ::__isValidQuery(cKey) .AND. PCOUNT() == 2 )
      RETURN ::__recur_set( ::__queryToTokens( cKey ), 1, ::hItems, xValue )
   ENDIF
RETURN .F.

/*!

 \brief returns type of value for given key
 \param(IN) string
 \return VALTYPE

*/
METHOD type( cKey ) CLASS hql_config
   IF ( ::__isValidQuery( cKey ) )
      RETURN ( VALTYPE( ::__recur_get( ::__queryToTokens( cKey ), 1, ::hItems ) ) )
   ENDIF
RETURN ( VALTYPE( NIL ) )

/*!

 \brief FOR EACH overloading; returns true (can start) or false
      harbour/tests/foreach2.prg
      https://sourceforge.net/p/hmgs-minigui/svncode/223/tree/trunk/MiniGUI/SOURCE/HbOLE/win32ole.prg#l150
 \param(IN) ...
 \return bool

*/
METHOD __enumStart( enum, lDescend ) CLASS hql_config
   (@enum):__enumBase(::hItems)
   HB_SYMBOL_UNUSED(lDescend)
RETURN ( LEN(::hItems) > 0 ) /* .F. means stop iteration */

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] returns true if string is a valid key otherwise false
   valid "goofy"
   invalid "", " goofy", " goofy "
 \param(IN) string
 \return bool

*/
METHOD __isValidKey( cKey ) CLASS hql_config
   cKey := hb_DefaultValue( cKey, "" )
RETURN ( LEN(ALLTRIM(cKey)) == LEN(cKey) .AND. LEN(ALLTRIM(cKey)) > 0 )

/*!

 \brief [PROTECTED] returns true if string is a valid query otherwise false
   valid "goofy", "goofy:duck"
   invalid "", ":", "::", " goofy", "goofy:", "goofy::"
 \param(IN) string
 \return bool

*/
METHOD __isValidQuery( cQry ) CLASS hql_config
   LOCAL aTokens, nAt, lValid
   cQry := hb_DefaultValue( cQry, "" )
   aTokens := ::__queryToTokens( cQry )
   lValid := .T.
   FOR nAt := 1 TO LEN(aTokens)
      IF ( !::__isValidKey( aTokens[nAt] ) )
         lValid := .F.
         EXIT
      ENDIF
   NEXT nAt
RETURN lValid

/*!

 \brief [PROTECTED] (recursive function) returns true if key exist otherwise false
 \param(IN) string, numeric, hash
 \return bool

*/
METHOD __recur_contains( aTokens, nLevel, hArray ) CLASS hql_config
   LOCAL xValue
   IF ( hb_Hhaskey( hArray, aTokens[nLevel] ) )
      xValue := hb_Hget( hArray, aTokens[nLevel] )
      IF ( LEN(aTokens) == nLevel )
         RETURN .T.
      ELSEIF ( LEN(aTokens) > nLevel .AND. hb_IsHash( xValue) )
         RETURN ::__recur_contains( aTokens, nLevel+1, xValue )
      ENDIF
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] (recursive function) returns value if key exist otherwise NIL
 \param(IN) string, numeric, hash
 \return variant

*/
METHOD __recur_get( aTokens, nLevel, hArray ) CLASS hql_config
   LOCAL xValue
   IF ( hb_Hhaskey( hArray, aTokens[nLevel] ) )
      xValue := hb_Hget( hArray, aTokens[nLevel] )
      IF ( LEN(aTokens) == nLevel )
         RETURN xValue
      ELSEIF ( LEN(aTokens) > nLevel .AND. hb_IsHash( xValue) )
         RETURN ::__recur_get( aTokens, nLevel+1, xValue )
      ENDIF
   ENDIF
RETURN NIL

/*!

 \brief [PROTECTED] (recursive function) remove key if exist
 \param(IN) string, numeric, hash
 \return bool

*/
METHOD __recur_remove( aTokens, nLevel, hArray ) CLASS hql_config
   LOCAL xValue
   IF ( hb_Hhaskey( hArray, aTokens[nLevel] ) )
      xValue := hb_Hget( hArray, aTokens[nLevel] )
      IF ( LEN(aTokens) == nLevel )
         hb_Hdel( hArray, aTokens[nLevel] )
         RETURN .T.
      ELSEIF ( LEN(aTokens) > nLevel .AND. hb_IsHash( xValue) )
         RETURN ::__recur_remove( aTokens, nLevel+1, xValue )
      ENDIF
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] (recursive function) set
 \param(IN) string, numeric, hash, variant
 \return bool

*/
METHOD __recur_set( aTokens, nLevel, hArray, xNewValue ) CLASS hql_config
   LOCAL xValue
   IF ( hb_Hhaskey( hArray, aTokens[nLevel] ) )
      xValue := hb_Hget( hArray, aTokens[nLevel] )
      IF ( LEN(aTokens) == nLevel )
         hb_Hset( hArray, aTokens[nLevel], xNewValue )
         RETURN .T.
      ELSEIF ( LEN(aTokens) > nLevel .AND. hb_IsHash( xValue) )
         RETURN ::__recur_set( aTokens, nLevel+1, xValue, xNewValue )
      ENDIF
   ELSE
      IF ( LEN(aTokens) == nLevel )
         hb_Hset( hArray, aTokens[nLevel], xNewValue )
         RETURN .T.
      ELSEIF ( LEN(aTokens) > nLevel )
         xValue := {=>}
         hb_Hset( hArray, aTokens[nLevel], xValue )
         RETURN ::__recur_set( aTokens, nLevel+1, xValue, xNewValue )
      ENDIF
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTED] split query into array of tokens
 \param(IN) string
 \return array

*/
METHOD __queryToTokens( cQry ) CLASS hql_config
   cQry := hb_DefaultValue( cQry, "" )
RETURN ( hb_Atokens( cQry, ::cSep, /*lSkipStrings*/, /*lDoubleQuoteOnly*/ ) )

// ==================== HIDDEN section ====================
