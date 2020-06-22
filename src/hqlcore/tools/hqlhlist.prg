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
#include "error.ch"

/*!

 \brief [public_function] creates hql_hlist object
 \param[in] none
 \return hql_hlist object

*/
FUNCTION hqlHlist( ... )
RETURN hql_hlist():new( ... )

/*!

 \brief hql_hlist class definition

*/
CLASS hql_hlist INHERIT hql_hqlobj

   ERROR HANDLER __errorHandler( ... )

   EXPORTED:
   METHOD init
   METHOD hqlCleaner                                      // override
   ACCESS caseMatch                 INLINE ::lCaseMatch
   METHOD castName                  INLINE ::cCastName
   METHOD clear
   ACCESS cloned
   METHOD contains
   ACCESS count                     INLINE ::size()
   ACCESS defaultValue
   METHOD hget
   METHOD hset
   ACCESS isEmpty
   METHOD isValidData
   METHOD isValidIndex
   METHOD items
   ACCESS keepOrder                 INLINE ::lKeepOrder
   METHOD keyAt
   METHOD keyExists
   METHOD keyPos
   ACCESS keys
   METHOD remove
   METHOD setCaseMatch
   METHOD setCastName
   METHOD setKeepOrder
   ACCESS size                      INLINE LEN( ::hItems )
   METHOD sort
   METHOD take
   METHOD value
   METHOD valueAt
   ACCESS values
   METHOD __enumStart               // FOR EACH overloading harbour/tests/foreach2.prg
   OPERATOR "==" ARGS ...           INLINE ::__h_OpExactBinaryEqual( ... )

   PROTECTED:
   VAR lCaseMatch                   INIT .T.
   VAR cCastName                    INIT ""
   VAR lKeepOrder                   INIT .T.
   VAR hItems                       INIT NIL

   HIDDEN:
   METHOD __h_emptyHash
   METHOD __h_hqlCleaner
   METHOD __h_OpExactBinaryEqual      // overloading "==" operator eg IF object == other; WARNING it checks size, dataType and both items

   METHOD __h_hqlparsingInit
ENDCLASS

/*!

 \brief creates a new object instance
 \param[in]
 \return SELF

*/
METHOD init( ... ) CLASS hql_hlist

   ::__h_hqlparsingInit( ... )

RETURN Self

/*!

 \brief callable hql destroyer
 \param[in] none
 \return NIL

*/
METHOD hqlCleaner() CLASS hql_hlist
   ::__h_hqlCleaner()
RETURN NIL

/*!

 \brief clear list
 \param[in] none
 \return SELF

*/
METHOD clear() CLASS hql_hlist
   ::__h_hqlCleaner()
RETURN Self

/*!

 \brief return a new hql_hlist object with cloned data
 \param[in] none
 \return object

*/
METHOD cloned() CLASS hql_hlist

   LOCAL oNew
   LOCAL cBlock := Self:className() + "():new()"

   WITH OBJECT oNew := EVAL( hb_MacroBlock( cBlock ) )
      :hItems := hb_Hclone( ::hItems )
      :cCastName := ::cCastName
      :lCaseMatch := ::lCaseMatch
      :lKeepOrder := ::lKeepOrder
   END WITH

RETURN oNew

/*!

 \brief Returns true if the vector contains an occurrence of value; otherwise returns false.
      This function requires the value type to have an implementation of operator==()
 \param[in] variant
 \return boolean

*/
METHOD contains( xValue ) CLASS hql_hlist

   LOCAL nAt

   FOR nAt := 1 TO ::size()

      IF ( VALTYPE( hb_HvalueAt( ::hItems, nAt ) ) == VALTYPE( xValue ) .AND. hb_HvalueAt( ::hItems, nAt ) == xValue )
         RETURN .T.
      ENDIF

   NEXT nAt

RETURN .F.

/*!

 \brief returns the default value accepted by the list
 \param[in] none
 \return variant

*/
METHOD defaultValue() CLASS hql_hlist

   LOCAL cBlock

   IF EMPTY( ::castName() )
      RETURN NIL
   ENDIF

   // ::castName() contains object class name eg "xxx_object"
   IF ( LEN( ::castName() ) > 1 )
      cBlock := ::castName() + "():new()"
      RETURN EVAL( hb_MacroBlock( cBlock ) )
   ENDIF

   /* see Harbour's src/vm/itemapi.c function hb_itemTypeStr */
   SWITCH ( UPPER( ::castName() ) )

   CASE "O"    // object
      cBlock := "NIL"   // can't known
      EXIT

   CASE "A"    // array
      cBlock := "{}"
      EXIT

   CASE "B"   //  block
      cBlock := ".T."
      EXIT

   CASE "D"   // date
      cBlock := "0d00000000"
      EXIT

   CASE "T"   // timestamp
      cBlock := "hb_DateTime( 0, 0, 0, 0, 0, 0, 0 )"
      EXIT

   CASE "L"   // logical
      cBlock := ".F."
      EXIT

   CASE "N"   // long, integer, double
      cBlock := "0"
      EXIT

   CASE "C"   // string
   CASE "M"   // memo
      cBlock := "''"
      EXIT

   CASE "H"   // hash
      cBlock := "{ => }"
      EXIT

   CASE "P"   // pointer
   CASE "S"   // symbol
      cBlock := "NIL"   // can't known
      EXIT

   OTHERWISE
      cBlock := "NIL"
      EXIT

   ENDSWITCH

RETURN EVAL( hb_MacroBlock( cBlock ) )

/*!

 \brief returns property value for given key
      Can be specified a default value to be returned when key is not found; if not specified it returns the defaultValue()

*/
METHOD hget( cKey, ... ) CLASS hql_hlist

   IF ::keyExists( cKey )
      RETURN hb_Hget( ::hItems, cKey )
   ENDIF

   IF ( PCOUNT() == 2 )
      RETURN hb_Pvalue(2)
   ENDIF

RETURN ::defaultValue()

/*!

 \brief sets a new item with the key and a value of value.
   If there is already an item with the key, that item's value is replaced with value.

*/
METHOD hset( cKey, xValue ) CLASS hql_hlist

   IF hb_IsString( cKey )

      IF ::isValidData( xValue )

         hb_hSet( ::hItems, cKey, xValue )

         RETURN .T.

      ENDIF

   ENDIF

RETURN .F.

/*!

 \brief returns true if lenght is 0 else false
 \param[in] none
 \return boolean

*/
METHOD isEmpty() CLASS hql_hlist
RETURN IIF( ::size() == 0, .T., .F. )

/*!

 \brief check if argument is acceptable for this list. when defaultValue() is NIL always true
 \param[in] variant
 \return boolean

*/
METHOD isValidData( ... ) CLASS hql_hlist

   // if not set, every value is accepted
   IF EMPTY( ::castName() )
      RETURN .T.
   ENDIF

   // ::castName() contains object class name eg "xxx_object"
   IF ( LEN( ::castName() ) > 1 )

      IF hb_IsObject( hb_Pvalue(1) ) .AND. ;
         hb_Pvalue(1):isDerivedFrom( UPPER( ::castName() ) )
         RETURN .T.
      ENDIF

   ELSE

      IF VALTYPE( hb_Pvalue(1) ) == UPPER( ::castName() )
         RETURN .T.
      ENDIF

   ENDIF

RETURN .F.

/*!

 \brief check if argument is a valid index for current list
 \param[in] (numeric) position
 \return boolean

*/
METHOD isValidIndex( ... ) CLASS hql_hlist

   IF ( hb_isNumeric( hb_Pvalue(1) ) .AND. hb_Pvalue(1) > 0 .AND. hb_Pvalue(1) <= ::size() )
      RETURN .T.
   ENDIF

RETURN .F.

/*!

 \brief returns cloned array
 \param[in] none
 \return array

*/
METHOD items() CLASS hql_hlist
RETURN hb_Hclone( ::hItems )

/*!

 \brief returns key at position; "" when not found
 \param[in] (numeric) position
 \return string

*/
METHOD keyAt( ... ) CLASS hql_hlist

   IF ::isValidIndex( hb_Pvalue(1) )
      RETURN hb_HkeyAt( ::hItems, hb_Pvalue(1) )
   ENDIF

RETURN ""

/*!

 \brief returns true if key exists else false
 \param[in] string
 \return boolean

*/
METHOD keyExists( ... ) CLASS hql_hlist

   IF hb_IsString( hb_Pvalue(1) )
      RETURN hb_HhasKey( ::hItems, hb_Pvalue(1) )
   ENDIF

RETURN .F.

/*!

 \brief returns key position if exists else 0
 \param[in] string
 \return boolean

*/
METHOD keyPos( ... ) CLASS hql_hlist

   IF hb_IsString( hb_Pvalue(1) )
      RETURN hb_Hpos( ::hItems, hb_Pvalue(1) )
   ENDIF

RETURN 0

/*!


 \brief returns array list of all the keys
 \param[in] null
 \return array

*/
METHOD keys() CLASS hql_hlist
RETURN hb_Hkeys( ::hItems )

/*!

 \brief removes key.
         returns -1 if not executed else the new number of items in the list

*/
METHOD remove( cKey ) CLASS hql_hlist

   IF ::keyExists( cKey )

      ::hItems := hb_Hdel( ::hItems, cKey )

      RETURN ::size()

   ENDIF

RETURN -1

/*!

 \brief set case match flag
 \param[in] boolean
 \return self

*/
METHOD setCaseMatch( ... ) CLASS hql_hlist

   IF ( hb_IsLogical( hb_Pvalue(1) ) .AND. !( hb_Pvalue(1) == ::lCaseMatch ) )
      ::lCaseMatch := hb_Pvalue(1)
      ::hItems := hb_HcaseMatch( ::hItems, ::lCaseMatch )
   ENDIF

RETURN Self

/*!

 \brief sets the casting name e.g. "N", "C", "L", etc. ONLY if list is empty
 \param[in] string
 \return self

*/
METHOD setCastName( ... ) CLASS hql_hlist

   IF ( hb_IsString( hb_Pvalue(1) ) .AND. ::isEmpty() )
      ::cCastName := ALLTRIM( hb_Pvalue(1) )
   ENDIF

RETURN Self

/*!

 \brief set keep order flag
 \param[in] boolean
 \return self

*/
METHOD setKeepOrder( ... ) CLASS hql_hlist

   IF ( hb_IsLogical( hb_Pvalue(1) ) .AND. !( hb_Pvalue(1) == ::lKeepOrder ) )
      ::lKeepOrder := hb_Pvalue(1)
      ::hItems := hb_HkeepOrder( ::hItems, ::lKeepOrder )
   ENDIF

RETURN Self

/*!

 \brief sort current list
 \param[in] none
 \return Self

*/
METHOD sort() CLASS hql_hlist
   ::hItems := hb_Hsort( ::hItems )
RETURN Self

/*!

 \brief removes key (if exists) and returns related value; if doesn't exsists default value returned
 \param[in] string
 \return variant

*/
METHOD take( cKey ) CLASS hql_hlist

   LOCAL xValue

   IF ::keyExists( cKey )

      xValue := hb_Hget( cKey )

      ::hItems := hb_Hdel( ::hItems, cKey )

      RETURN xValue

   ENDIF

RETURN ::defaultValue()

/*!

 \brief setg/get value
 \param[in] key
 \param[in] [OPTIONAL] new value
 \return variant

*/
METHOD value( ... ) CLASS hql_hlist

   IF ( PCOUNT() == 2 )
      ::hSet( ... )
   ENDIF

RETURN ::hGet( ... )

/*!

 \brief returns value at position; NIL can be returned
 \param[in] (numeric) position
 \return variant

*/
METHOD valueAt( ... ) CLASS hql_hlist

   IF ::isValidIndex( hb_Pvalue(1) )
      RETURN hb_HvalueAt( ::hItems, hb_Pvalue(1) )
   ENDIF

RETURN ::defaultValue()

/*!


 \brief returns array list of all the values
 \param[in] null
 \return array

*/
METHOD values() CLASS hql_hlist
RETURN hb_Hvalues( ::hItems )

/*!

 \brief FOR EACH overloading harbour/tests/foreach2.prg

*/
METHOD __enumStart( enum, lDescend ) CLASS hql_hlist
   lDescend := hb_DefaultValue(lDescend, .F.)
   // too dangerous and unexpected behaviour on sort
   // (@enum):__enumBase( IIF( lDescend, hb_Hsort( ::hItems ) , ::hItems ) )
   (@enum):__enumBase(::hItems)
  HB_SYMBOL_UNUSED(lDescend)
RETURN ( LEN(::hItems) > 0 ) /* .F. means stop iteration */

// ==================== PROTECTED section ====================

// ==================== HIDDEN section ====================

/*!

 \brief [HIDDEN] build an empty hash array
 \param[in] none
 \return hash_array

*/
METHOD __h_emptyHash() CLASS hql_hlist
   LOCAL hTemp
   hTemp := hb_Hash()
   hb_HcaseMatch( hTemp, ::lCaseMatch )
   hb_HkeepOrder( hTemp, ::lKeepOrder )
RETURN hTemp

/*!

 \brief [HIDDEN] hql destroyer.
 \param[in] none
 \return NIL

*/
METHOD __h_hqlCleaner() CLASS hql_hlist
   LOCAL nAt

   FOR nAt := 1 TO ::size()
      hb_HvalueAt( ::hItems, nAt, NIL )
   NEXT nAt

   ::hItems := hb_Hclone( ::__h_emptyHash() )

RETURN NIL

/*!

 \brief [HIDDEN] overloading "==" operator

*/
METHOD __h_OpExactBinaryEqual( ... ) CLASS hql_hlist
   LOCAL lEqual := .F.
   LOCAL nAt
   IF ( hql_IsDerived(hb_Pvalue(1), "hql_hlist") )
      IF ( __vmItemId( Self ) == __vmItemId( hb_Pvalue(1) ) )
         lEqual := .T.
      ELSE
         IF ::size() == hb_Pvalue(1):size()
            lEqual := .T.
            FOR nAt := 1 TO ::size()
               IF !( hb_Pvalue(1):keyExists( ::keyAt( nAt ) ) .AND. ;
                     ( VALTYPE( ::valueAt( nAt ) ) == VALTYPE( hb_Pvalue(1):hGet( ::keyAt( nAt ) ) ) ) .AND. ;
                     ( ::valueAt( nAt ) == hb_Pvalue(1):hGet( ::keyAt( nAt ) ) ) )
                  lEqual := .F.
                  EXIT
               ENDIF
            NEXT nAt
         ENDIF
      ENDIF
   ENDIF
RETURN lEqual

/*!

 \brief [HIDDEN] handle init arguments
 \param[in] none
 \return NIL

*/
METHOD __h_hqlparsingInit( ... ) CLASS hql_hlist

   LOCAL xItem

   ::hql_hqlobj:init()

   ::hItems := hb_Hclone( ::__h_emptyHash() )

   IF ( hql_IsDerived(hb_Pvalue(1), "hql_hlist") )
      ::cCastName   := hb_Pvalue(1):castName()
      ::lCaseMatch  := hb_Pvalue(1):caseMatch()
      ::lKeepOrder  := hb_Pvalue(1):keepOrder()
      ::hItems := hb_Hclone( hb_Pvalue(1):hItems )
      RETURN NIL
   ENDIF

   // hash [, castName [, caseMatch [, keepOrder ]]]
   IF ( PCOUNT() >= 4 )
      ::lKeepOrder := hb_Pvalue(4)
   ENDIF

   IF ( PCOUNT() >= 3 )
      ::lCaseMatch := hb_Pvalue(3)
   ENDIF

   IF ( PCOUNT() >= 2 )
      ::cCastName := hb_Pvalue(2)
   ENDIF

   IF ( ( PCOUNT() >= 1 ) .AND. hb_IsHash( hb_Pvalue(1) ) )
      FOR EACH xItem IN hb_Pvalue(1)
         ::hset( xItem:__enumKey(), xItem:__enumValue() )
      NEXT
   ENDIF

RETURN NIL

/*!

 \brief

   take from http://www.kresin.ru/en/hrbfaq_3.html
   ... When a non-existent variable ( or methods ) of the class is called, an error is produced and the method is called
   defined as ERROR HANDLER ( or ON ERROR ).
   From this method you can get the same non-existent name of the called variable with the help of the function __GetMessage().
   Moreover, if there was an attempt to write something in this variable, then the name is preceded by the symbol "_" and the
   method is passed by the recorded value as the first parameter.

*/
METHOD __errorHandler( ... ) CLASS hql_hlist

   LOCAL cMsg := __GetMessage()
   LOCAL nIndex
   LOCAL nCode

   IF !( ( nIndex := ::keyPos( IIF( LEFT( cMsg, 1 ) == hb_Uchar(0x5F), SUBSTR( cMsg, 2 ), cMsg ) ) ) == 0 )
      RETURN ::valueAt( nIndex )
   ENDIF

   IF ( LEFT( cMsg, 1 ) == hb_Uchar(0x5F) )
      nCode := 1005
      cMsg := SUBSTR( cMsg, 2 )
   ELSE
      nCode := 1004
   ENDIF

RETURN __errRT_SBASE( IIF( nCode == 1005, EG_NOVARMETHOD, EG_NOMETHOD ), nCode, NIL, ::className() + ":" + cMsg, 1, Self )
