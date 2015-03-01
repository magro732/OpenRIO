/*******************************************************************************
 * 
 * RapidIO IP Library Core
 * 
 * This file is part of the RapidIO IP library project
 * http://www.opencores.org/cores/rio/
 * 
 * Description:
 * This file contains the implementation dependent information needed to build 
 * the riostack. Replace declarations and definitions in this file to customize 
 * for your own compiler environment.
 *
 * To Do:
 * -
 * 
 * Author(s): 
 * - Magnus Rosenius, magro732@opencores.org 
 * 
 *******************************************************************************
 * 
 * Copyright (C) 2013 Authors and OPENCORES.ORG 
 * 
 * This source file may be used and distributed without 
 * restriction provided that this copyright statement is not 
 * removed from the file and that any derivative work contains 
 * the original copyright notice and the associated disclaimer. 
 * 
 * This source file is free software; you can redistribute it 
 * and/or modify it under the terms of the GNU Lesser General 
 * Public License as published by the Free Software Foundation; 
 * either version 2.1 of the License, or (at your option) any 
 * later version. 
 * 
 * This source is distributed in the hope that it will be 
 * useful, but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
 * PURPOSE. See the GNU Lesser General Public License for more 
 * details. 
 * 
 * You should have received a copy of the GNU Lesser General 
 * Public License along with this source; if not, download it 
 * from http://www.opencores.org/lgpl.shtml 
 * 
 *******************************************************************************/

#ifndef __RIO_CONFIG
#define __RIO_CONFIG

/*******************************************************************************
* Includes
*******************************************************************************/

#include <stdint.h>
#include <stdlib.h>

/*******************************************************************************
* Global typedefs
*******************************************************************************/

#ifndef ASSERT
#ifdef MODULE_TEST
#include <stdio.h>
#define ASSERT(c, s) (c)?:fprintf(stderr, s)
#endif
#endif

#ifndef ASSERT0
#ifdef MODULE_TEST
#include <stdio.h>
#define ASSERT0(s) fprintf(stderr, s)
#endif
#endif

#ifndef DEBUG_OUT
#define DEBUG_OUT(...)
#endif

#define DEBUG_STATE(...) /*DEBUG_OUT(__VA_ARGS__)*/
#define DEBUG_FRAMING_RX(...) /*DEBUG_OUT(__VA_ARGS__)*/
#define DEBUG_FRAMING_TX(...) /*DEBUG_OUT(__VA_ARGS__)*/

typedef uint8_t bool_t;


/*******************************************************************************
* Global declarations
*******************************************************************************/
 
/*******************************************************************************
* Global function prototypes
*******************************************************************************/
 
#endif // __RIO_CONFIG
 
/*************************** end of file **************************************/
