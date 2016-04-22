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

#ifdef MODULE_TEST
#include <CUnit/CUnit.h>
#endif

/*******************************************************************************
 * Global macros
 *******************************************************************************/

#ifdef MODULE_TEST
#define ASSERT0(s) { CU_ASSERT( TEST_numExpectedAssertsRemaining > 0 ); --TEST_numExpectedAssertsRemaining; }
#define ASSERT(c, s) { if( !(c) ) { ASSERT0( s ); } }
#endif

/*******************************************************************************
 * Global declarations
 *******************************************************************************/
 
#ifdef MODULE_TEST
extern int TEST_numExpectedAssertsRemaining;
#endif

/*******************************************************************************
 * Global function prototypes
 *******************************************************************************/
 
#endif // __RIO_CONFIG
 
/*************************** end of file **************************************/
