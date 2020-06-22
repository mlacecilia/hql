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

/*
*/
CREATE CLASS oop002

   EXPORTED:
   METHOD init
   METHOD activate

   PROTECTED:
   DATA oDialog                           INIT NIL

   HIDDEN:
   METHOD __createForm

END CLASS

/*!

 \brief initialize object instance
 \param[in] widget parent
 \return SELF

*/
METHOD init( oParent ) CLASS oop002

   ::__createForm( oParent )

RETURN Self

/*!

 \brief activate this dialog
 \param[in] none
 \return SELF

*/
METHOD activate() CLASS oop002

   ::oDialog:hqlActivate()

RETURN Self

/*!

 \brief create form dialog
 \param[in] widget parent
 \return NIL

*/
METHOD __createForm( oParent ) CLASS oop002

   LOCAL oGlayout
   LOCAL nGrow := 0

   WITH OBJECT ::oDialog := hqlChildWindow( /*name*/, oParent )
      :setWindowTitle("HQLINPUTDATEC basic usage")
      :resize( 600, 400 )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget( /*name*/ ) )

      WITH OBJECT :centralWidget()

         WITH OBJECT oGlayout := hqlGridLayout( /*name*/ )
            :hqlSetLayoutOf( ::oDialog:centralWidget() )

            // row 0 - START
            WITH OBJECT hqlLabel( /*name*/ )
               :setMinimumSize( 120, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setText( "format on language" )
               :setAlignment( hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
               :hqlAddMeToLayout( oGlayout, nGrow, 0 )
               :hqlAlignMeToLayout( oGlayout, Qt_AlignRight )  // is QLayout::setAlignment(QWidget *, Qt::Alignment)
            END WITH

            WITH OBJECT hqlInputDateC( "txt_01" )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
//               :setClearButtonEnabled( .T. )  it obscures toolbutton
               :hqlAddMeToLayout( oGlayout, nGrow, 1 )
               :hqlOnEditingFinished( { |oself| udfFinished(oself) } )
               :hqlValue( hb_Date() )
            END WITH
            :addItem( QSpacerItem( 100, 30, QSizePolicy_Expanding, QSizePolicy_Expanding ), 0, 2, -1, -1 )
            ++nGrow
            // row 0 - END

            // row +1 - START
            WITH OBJECT hqlLabel( /*name*/ )
               :setMinimumSize( 120, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setText( "format 'YYYYMMDD' " )
               :setAlignment( hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
               :hqlAddMeToLayout( oGlayout, nGrow, 0 )
               :hqlAlignMeToLayout( oGlayout, Qt_AlignRight )  // is QLayout::setAlignment(QWidget *, Qt::Alignment)
            END WITH

            WITH OBJECT hqlInputDateC( "txt_02" )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
//               :setClearButtonEnabled( .T. )  it obscures toolbutton
               :hqlAddMeToLayout( oGlayout, nGrow, 1 )
               :hqlOnEditingFinished( { |oself| udfFinished(oself) } )
               :hqlInputMask( 'YYYYMMDD' )
            END WITH
            ++nGrow
            // row +1 - END

            // row +1 - START
            WITH OBJECT hqlLabel( /*name*/ )
               :setMinimumSize( 120, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setText( "set invalid date, format on language " )
               :setAlignment( hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
               :hqlAddMeToLayout( oGlayout, nGrow, 0 )
               :hqlAlignMeToLayout( oGlayout, Qt_AlignRight )  // is QLayout::setAlignment(QWidget *, Qt::Alignment)
            END WITH

            WITH OBJECT hqlInputDateC( "txt_03" )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
//               :setClearButtonEnabled( .T. )  it obscures toolbutton
               :hqlAddMeToLayout( oGlayout, nGrow, 1 )
               :hqlOnEditingFinished( { |oself| udfFinished(oself) } )
               :hqlValue( hb_Date( -1, 13, 33 ) )
            END WITH
            ++nGrow
            // row +1 - END

            // row +1 - START
            WITH OBJECT hqlLabel( /*name*/ )
               :setMinimumSize( 120, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setText( "format changed 'YYYY-MM-DD' " )
               :setAlignment( hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
               :hqlAddMeToLayout( oGlayout, nGrow, 0 )
               :hqlAlignMeToLayout( oGlayout, Qt_AlignRight )  // is QLayout::setAlignment(QWidget *, Qt::Alignment)
            END WITH

            WITH OBJECT hqlInputDateC( "txt_04" )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
//               :setClearButtonEnabled( .T. )  it obscures toolbutton
               :hqlAddMeToLayout( oGlayout, nGrow, 1 )
               :hqlOnEditingFinished( { |oself| udfFinished(oself) } )
               :hqlValue( hb_Date() )
               :hqlInputMask( 'YYYY-MM-DD' )
            END WITH
            ++nGrow
            // row +1 - END

            // row +1 - START
            WITH OBJECT hqlLabel( /*name*/ )
               :setMinimumSize( 120, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setText( "lineEdit to gain focus" )
               :setAlignment( hb_BitOr( Qt_AlignRight, Qt_AlignVCenter ) )
               :hqlAddMeToLayout( oGlayout, nGrow, 0 )
               :hqlAlignMeToLayout( oGlayout, Qt_AlignRight )  // is QLayout::setAlignment(QWidget *, Qt::Alignment)
            END WITH

            WITH OBJECT hqlLineEdit( /*name*/ )
               :setMinimumSize( 200, 30 )
               :setSizePolicy( QSizePolicy( QSizePolicy_Fixed, QSizePolicy_Fixed ) )
               :setClearButtonEnabled( .T. )
               :hqlAddMeToLayout( oGlayout, nGrow, 1 )
            END WITH
            // row +1 - END

         END WITH

      END WITH

   END WITH

RETURN NIL

STATIC PROCEDURE udfFinished(oself)
   hql_Trace( "finished SIGNAL" + ;
                           " objectName =" + oself:objectName() + ;
                           " value (as dtos)=" + DTOS(oself:hqlValue()) + ;
                           " mask=" + oself:hqlInputMask() )
RETURN

/*
STATIC PROCEDURE udfTabPressed(oself)
   hql_Trace( "tabPressed SIGNAL" + ;
                           " objectName =" + oself:objectName() + ;
                           " value (as dtos)=" + DTOS(oself:hqlValue()) + ;
                           " mask=" + oself:hqlInputMask() )
RETURN */
