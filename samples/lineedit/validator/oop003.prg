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
#include "hqlinclude.ch"

FUNCTION op003( ... )
RETURN op_003():new( ... )

CLASS op_003 INHERIT hql_childWindow

   EXPORTED:
   METHOD init

   PROTECTED:
   METHOD __wdgCustomize
   METHOD __wdgEditingFinished
   METHOD __wdgTabPressed
   METHOD __wdgTextChanged
   METHOD __wdgValidate
   METHOD __wdgFixup

ENDCLASS

METHOD init( ... ) CLASS op_003
   ::hql_childWindow:init( ... )
   ::__wdgCustomize()
RETURN NIL

METHOD __wdgCustomize( ... ) CLASS op_003
   LOCAL oVlayout, oHlayout

   ::setWindowTitle("hqlOnValidator + hqlOnFixup TESTER")
   ::setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
   ::setCentralWidget( hqlWidget( /*name*/, self ) )
   ::resize( 800, 600 )

   oVlayout := QVBoxLayout( ::centralWidget() )
   ::centralWidget():setLayout( oVlayout )

   WITH OBJECT ::centralWidget()

      oHlayout := QHBoxLayout( ::centralWidget() )
      oHlayout:setContentsMargins( 0, 0, 0, 0 )
      WITH OBJECT hqlLabel()
         :setText( "not accept digits" )
         :hqlAddMeToLayout( oHlayout )
      END WITH
      WITH OBJECT hqlLineEdit( "fld1" )
         :setMaximumWidth( 200 )
         :hqlOnEditingFinished( { |oself| ::__wdgEditingFinished(oSelf) } )
         :hqlOnTabPressed( { |oself| ::__wdgTabPressed(oSelf) } )
         :hqlOnTextChanged( { |cBuff| ::__wdgTextChanged(cBuff) } )
         :hqlOnValidator( { |cBuff,nPos| ::__wdgValidate(cBuff,nPos) } )
         :hqlOnFixUp( { |cBuff| ::__wdgFixup(cBuff) } )
         :hqlAddMeToLayout( oHlayout )
         //:hqlValue( "ABC" )
      END WITH
      oHlayout:addStretch()
      oVlayout:addLayout( oHlayout, hb_BitOr(Qt_AlignVCenter,Qt_AlignLeft) )

      oHlayout := QHBoxLayout( ::centralWidget() )
      oHlayout:setContentsMargins( 0, 0, 0, 0 )
      oHlayout:addStretch()
      WITH OBJECT hqlPushButton()
         :setText( "only to gain focus or set value" )
         :setMinimumHeight( 60 )
         :hqlOnClicked( { || ::fld1:hqlValue("A,B.C") } )
         :hqlAddMeToLayout( oHlayout )
      END WITH
      oHlayout:addStretch()
      oVlayout:addLayout( oHlayout, hb_BitOr(Qt_AlignVCenter,Qt_AlignLeft) )

   END WITH
   oVlayout:addStretch()   // to push up all
RETURN NIL

METHOD __wdgEditingFinished( oself ) CLASS op_003
   hql_Trace( "finished text >"+oself:text()+"<" )
RETURN NIL

METHOD __wdgTabPressed( oself ) CLASS op_003
   hql_Trace( "tabPressed text >"+oself:text()+"<" )
RETURN NIL

METHOD __wdgTextChanged( cbuff ) CLASS op_003
   hql_Trace( "textChanged text >"+cbuff+"<" )
RETURN NIL

METHOD __wdgValidate(cBuff, nPos) CLASS op_003
   hql_Trace( "validate nPos: " + hb_NtoS(nPos) + " text >"+cBuff+"<" )

   IF ( nPos == 0 .AND. LEN(cBuff) == 0 )
      RETURN NIL  // call fixup
   ENDIF

   // reject character at given (editing) position
   IF ( nPos > 0 )
      IF ( SUBSTR(cBuff, nPos, 1) $ "0123456789" )
         RETURN .F.
      ENDIF
   ENDIF

   // always inspect all
   IF ( (AT(".", cBuff) > 0) .OR. (AT(",", cBuff) > 0) )
      //RETURN NIL  // call fixup it seems not executed when set ???
      cBuff := ::__wdgFixup(cBuff)
      nPos := IIF ( nPos > LEN(cBuff), LEN(cBuff), nPos )
      RETURN { cBuff, nPos, .T. }
   ENDIF

RETURN { cBuff, nPos, .T. }   // OR .T. this is the default to be returned

METHOD __wdgFixup(cBuff) CLASS op_003
   LOCAL nP

   hql_Trace( "fixup text >"+cBuff+"<" )

   IF ( LEN(cBuff) == 0 )
      cBuff := "FIX"
   ELSE
      WHILE ( (nP := hb_Ati(",", cBuff)) > 0 )
         cBuff := STUFF(cBuff, nP, 1, "_")
      END
      WHILE ( (nP := hb_Ati(".", cBuff)) > 0 )
         cBuff := STUFF(cBuff, nP, 1, "-")
      END
   ENDIF
RETURN cBuff
