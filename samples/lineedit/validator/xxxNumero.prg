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
#include "hqlhbqt.ch"

FUNCTION xxxNumero( ... )
RETURN xxx_numero():new( ... )


/*!

 \brief

*/
CLASS xxx_numero INHERIT hql_lineEdit

   EXPORTED:
   METHOD init

   PROTECTED:
   SIGNAL __hql_OnFixUp
   SIGNAL __hql_OnValidator

ENDCLASS

METHOD init ( ... ) CLASS xxx_numero
   ::hql_lineEdit:init( ... )
   ::__hqlValueSet( "0" )
   ::setCursorPosition( 0 )
   ::selectAll()
RETURN NIL

/*!

*/
SIGNAL __hql_OnFixUp( cText ) CLASS xxx_numero
hql_Trace( "__hql_OnFixUp >"+cText+"<" )

   IF ( EMPTY(cText) )
      cText := "0"
   ENDIF

   IF ( hb_IsEvalItem( ::bHqlOnFixUp ) )
      cText := EVAL( ::bHqlOnFixUp, cText )
   ENDIF

RETURN cText

SIGNAL __hql_OnValidator( cText, nPos ) CLASS xxx_numero
   LOCAL xReturn
   LOCAL cChar
hql_Trace( "__hql_OnValidator >"+cText+"< nPos: "+hb_NtoS(nPos) )

   IF ( nPos == 0 .AND. EMPTY(cText) )
      RETURN { "0", 1, .T. }
   ENDIF

   FOR EACH cChar IN cText
      IF ( !(cChar $ "0123456789+-,.") )
         RETURN .F.
      ENDIF
   NEXT

   // check sign[s]
   IF ( (AT("+", cText) > 1) .OR. ;
        (CHRCNT("+", cText) > 1) .OR. ;
        (AT("-", cText) > 1) .OR. ;
        (CHRCNT("-", cText) > 1) .OR. ;
        ( (AT("+", cText) > 0) .AND. (AT("-", cText) > 0) ) )
      RETURN .F.
   ENDIF

   // check group separator
   IF ( CHRCNT(",", cText) > 0 )
      RETURN .F.
   ENDIF

   // check decimal separator
   IF ( (CHRCNT(".", cText) > 1) )
      RETURN .F.
   ENDIF
   IF ( (AT(".", cText) == 1) )
      cText := "0" + cText
      nPos += 1
   ENDIF
   // insert "0" between <sign>.xxxx
   IF ( ( (AT("+", cText) > 0) .OR. (AT("-", cText) > 0) ) .AND. (AT(".", cText) == 2) )
      cText := STUFF( cText, 2, 0, "0" )
      nPos += 1
   ENDIF

   IF ( hb_IsEvalItem( ::bHqlOnValidator ) )
      xReturn := EVAL( ::bHqlOnValidator, cText, nPos )
      RETURN xReturn
   ENDIF

RETURN { cText, nPos, .T. }   // OR .T. this is the default to be returned

FUNCTION CHRCNT( char, string )
   LOCAL nCount := 0
   LOCAL nI
   char := LEFT(hb_DefaultValue(char, ""), 1)
   string := hb_DefaultValue(string, "")
   IF ( LEN(string) > 0 .AND. LEN(char) > 0 )
      FOR nI := 1 TO LEN(string)
         IF ( char == SUBSTR(string, nI, 1) )
            nCount += 1
         ENDIF
      NEXT
   ENDIF
RETURN nCount
