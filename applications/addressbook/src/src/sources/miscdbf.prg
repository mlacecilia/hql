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
#include "dbinfo.ch"
#include "dbstruct.ch"
#include "addrbook.ch"

//////////////////////////////////////// \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


/*!

 \brief default dbf work area model

*/
CREATE CLASS wa_model

   DESTRUCTOR __model_dtor

   EXPORTED:
   METHOD alias                           INLINE ::cAlias
   METHOD close
   METHOD codePage                        INLINE ( SET( _SET_DBCODEPAGE ) )
   METHOD create
   METHOD dbfName
   METHOD idxName
   METHOD isNotOpen                       INLINE ( ! ::isopen() )
   METHOD isOpen                          INLINE ( ::workArea() > 0 )
   METHOD open
   METHOD rdd                             INLINE ( RDDSETDEFAULT() )
   METHOD tabName                         INLINE ::cTabName
   ACCESS wa                              INLINE ( ::workArea() )
   METHOD workArea                        INLINE ::nWorkArea

   PROTECTED:
   METHOD __dbcreate
   METHOD __dbusearea
   METHOD __ordcreate
   METHOD __getNewAlias
   DATA aDbstru                           INIT {}
   DATA cAlias                            INIT ""
   DATA cPath                             INIT ""
   DATA cTabName                          INIT ""
   DATA nWorkArea                         INIT 0

END CLASS

/*!

 \brief close

*/
METHOD close() CLASS wa_model
   (::nWorkArea)->(DBCLOSEAREA())
   ::nWorkArea := 0
   ::cAlias := ""
RETURN Self

/*!

 \brief create dbf and index

*/
METHOD create( /*lForced*/ ) CLASS wa_model
   IF ::isOpen()
      RETURN .F.
   ENDIF
RETURN .F.

/*!

 \brief returns dbf file name

*/
METHOD dbfName() CLASS wa_model
RETURN hb_FnameMerge( ::cPath, ::cTabName, RDDINFO( RDDI_TABLEEXT ) )

/*!

 \brief create dbf and index

*/
METHOD idxName() CLASS wa_model
RETURN hb_FnameMerge( ::cPath, ::cTabName, RDDINFO( RDDI_ORDBAGEXT ) )

/*!

 \brief open dbf and index

*/
METHOD open( lShared, lReadonly ) CLASS wa_model
   IF ::isOpen()
      RETURN .F.
   ENDIF
   IF ! hb_VfExists( ::dbfName() )
      RETURN .F.
   ENDIF
RETURN ::__dbusearea( lShared, lReadonly )

/*!

 \brief dbcreate

*/
METHOD __dbcreate() CLASS wa_model

   LOCAL nOldArea := SELECT()
   LOCAL cAlias
   LOCAL lReturn

   IF ::isOpen()
      RETURN .F.
   ENDIF

   IF EMPTY( ::cPath ) .OR. EMPTY( ::cTabName ) .OR. EMPTY( ::aDbstru )
      RETURN .F.
   ENDIF

   cAlias := ::__getNewAlias( ::cTabName )

   NETERR( .F. )
   BEGIN SEQUENCE WITH { | oErr | BREAK( oErr ) }

      lReturn := DBCREATE( ::dbfName(), ;
                           ::aDbstru, ;
                           ::rdd(), ;
                           NIL, ;
                           cAlias, ;
                           NIL, ;            // this can't be used RDDINFO( RDDI_DELIMITER )
                           ::codePage(), ;
                           NIL )             // nConnection ????

      lReturn := IIF( NETERR(), .F., lReturn )

   RECOVER
      lReturn := .F.
   END SEQUENCE

   DBSELECTAREA(nOldArea)

RETURN lReturn

/*!

 \brief dbusearea

*/
METHOD __dbusearea( lShared, lReadonly ) CLASS wa_model

   LOCAL nOldArea := SELECT()
   LOCAL cAlias
   LOCAL lReturn

   hb_Default( @lShared, .T. )
   hb_Default( @lReadonly, .F. )

   IF ::isOpen()
      RETURN ::nWorkArea
   ENDIF

   IF EMPTY( ::cPath ) .OR. EMPTY( ::cTabName )
      RETURN 0
   ENDIF

   IF ! hb_vfExists( ::dbfName() )
      RETURN 0
   ENDIF

   cAlias := ::__getNewAlias( ::cTabName )

   lReturn := .T.
   NETERR( .F. )
   BEGIN SEQUENCE WITH { | oErr | BREAK( oErr ) }
      DBUSEAREA( .T., ;
                 ::rdd(), ;
                 ::dbfName(), ;
                 cAlias, ;
                 lShared, ;
                 lReadonly, ;
                 ::codePage() )
   RECOVER
      lReturn := .F.
   END SEQUENCE

   IF lReturn
      ::nWorkArea := SELECT()
      ::cAlias := cAlias
   ENDIF

   DBSELECTAREA(nOldArea)

RETURN ::nWorkArea

/*!

 \brief ordcreate

*/
METHOD __ordcreate( cOrderName, cExpKey ) CLASS wa_model

   LOCAL lReturn

   hb_Default( @cOrderName, "" )
   hb_Default( @cExpKey, "" )

   IF ::isNotOpen() .OR. EMPTY( cOrderName ) .OR. EMPTY( cExpKey )
      RETURN .F.
   ENDIF

   lReturn := .T.
   NETERR( .F. )
   BEGIN SEQUENCE WITH { | oErr | BREAK( oErr ) }
      ( ::workArea() )->( ORDCREATE( ::idxName(), ;
                                  cOrderName, ;
                                  cExpKey, ;
                                  hb_MacroBlock( cExpKey ) ) )
   RECOVER
      lReturn := .F.
   END SEQUENCE

RETURN lReturn

/*!

 \brief __getNewAlias

*/
METHOD __getNewAlias( cTabName ) CLASS wa_model

   LOCAL cBaseName
   LOCAL cNewAlias
   LOCAL nWa := 0

   hb_Default( @cTabName, "" )

   cBaseName := IIF( EMPTY( cTabName ), "alias", cTabName ) + "_"

   DO WHILE SELECT( cNewAlias := cBaseName + hb_NtoS( ++nWa ) ) != 0
   ENDDO

RETURN cNewAlias

METHOD __model_dtor() CLASS wa_model
   ::close()
RETURN NIL
