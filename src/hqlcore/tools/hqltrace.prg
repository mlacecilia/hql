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
#include "fileio.ch"

/*!

 \brief Print out text on file adding EOL (default); first time you can set file name otherwise hql_traceLog is default
 \param(IN) string, [bool], [string]
 \return

*/
PROCEDURE hql_Trace( cString, lEol, cFileName )
   STATIC s_once
   STATIC s_file

   lEol := hb_DefaultValue( lEol, .T. )
   cString := hb_DefaultValue( cString, "" ) + IF( lEol, e"\n", "" )

   hb_ThreadOnce( @s_once, { || s_file := SUDFhql_trace( hb_DefaultValue( cFileName, "" ) ) } )

   IF( hb_IsPointer( s_file ) )
      hb_VfSeek( s_file, 0, FS_END )
      hb_VfWrite( s_file, cString, hb_Blen( cString ) )
      hb_VfFlush( s_file )
   ENDIF

RETURN

/*!

 \brief Open/create file log. Optional file name
 \param(IN) [string]
 \return pointer

*/
STATIC FUNCTION SUDFhql_trace( cFileName )
   LOCAL pFile
   LOCAL nAttr := hb_BitOr( FO_READWRITE, HB_FO_CREAT, HB_FO_TRUNC, FO_SHARED )

   IF( hb_Blen( cFileName ) == 0 )
      cFileName := hb_PathNormalize( hb_DirSepAdd( hb_FnameDir( hb_ProgName() ) ) + "hql_traceLog" )
   ELSE
      cFileName := hb_PathNormalize( hb_DirSepAdd( hb_FnameDir( hb_ProgName() ) ) + cFileName )
   ENDIF

   hb_VfErase( cFileName )
   pFile := hb_VfOpen( cFileName, nAttr )

RETURN pFile
