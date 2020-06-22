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

 \brief default dbf work area model

*/
FUNCTION countersDm( ... )
RETURN counters_dm():new( ... )

/*!

 \brief default dbf work area model

*/
CREATE CLASS counters_dm INHERIT wa_model

   EXPORTED:
   METHOD init
   METHOD create                          // override
   METHOD open                            // override

END CLASS

/*!

 \brief initialize

*/
METHOD init( ... ) CLASS counters_dm

   ::cTabName := "counters"
   ::cPath := IIF( hb_IsString( hb_Pvalue(1) ), hb_Pvalue(1), "" )
   AADD( ::aDbstru, { "cntcodeid", "C", 10, 0 } )
   AADD( ::aDbstru, { "cntdescri", "C", 50, 0 } )
   AADD( ::aDbstru, { "cntamount", "N",  8, 0 } )

RETURN NIL

/*!

 \brief create dbf and index

*/
METHOD create( lForced ) CLASS counters_dm

   hb_Default( @lForced, .F. )

   IF ::isOpen()
      RETURN .F.
   ENDIF

   IF ! lForced .AND. hb_VfExists( ::dbfName() ) .AND. hb_VfExists( ::idxName() )
      RETURN .T.
   ENDIF

   IF lForced
      IF hb_VfExists( ::dbfName() )
         IF ! ( hb_VfErase( ::dbfName() ) == 0 )
            RETURN .F.
         ENDIF
      ENDIF

      IF hb_VfExists( ::idxName() )
         IF ! ( hb_VfErase( ::idxName() ) == 0 )
            RETURN .F.
         ENDIF
      ENDIF
   ENDIF

   IF ! hb_VfExists( ::dbfName() )
      IF ! ::__dbcreate()
         RETURN .F.
      ENDIF
   ENDIF

   IF ! hb_VfExists( ::idxName() )
      IF ::__dbusearea( /*lShared*/, /*lReadonly*/ ) > 0
         IF ! ::__ordcreate( "bycode", "FIELD->cntcodeid" )
            ::close()
            RETURN .F.
         ENDIF
      ENDIF
   ENDIF

   ::close()

RETURN .T.

/*!

 \brief open dbf and index

*/
METHOD open( lShared, lReadonly ) CLASS counters_dm

   IF ::isOpen()
      RETURN .F.
   ENDIF

   IF ! hb_VfExists( ::dbfName() ) .OR. ! hb_VfExists( ::idxName() )
      RETURN .F.
   ENDIF

   ::__dbusearea( lShared, lReadonly )

   IF ::isOpen()
      (::workArea())->(ORDLISTADD( ::idxName(), "bycode" ))
      RETURN .T.
   ENDIF

RETURN .F.
