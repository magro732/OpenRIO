/******************************************************************************
 * No copyright for this file since it has to be changed.
 ******************************************************************************/

/******************************************************************************
 * Description:
 * This file contains the implementation dependent information needed to build 
 * the riostack. Replace declarations and definitions in this file to customize 
 * for your own compiler environment.
 *******************************************************************************/

#ifndef __RIO_CONFIG
#define __RIO_CONFIG

/*******************************************************************************
* Includes
*******************************************************************************/

#include <stdint.h>

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
#define DEBUG_OUT(...) fprintf(stderr, __VA_ARGS__)
#endif

/*******************************************************************************
* Global declarations
*******************************************************************************/
 
/*******************************************************************************
* Global function prototypes
*******************************************************************************/
 
#endif // __RIO_CONFIG
 
/*************************** end of file **************************************/
