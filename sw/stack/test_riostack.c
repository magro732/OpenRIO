/******************************************************************************
 * (C) Copyright 2013-2015 Magnus Rosenius and the Free Software Foundation.
 *
 * This file is part of OpenRIO.
 *
 * OpenRIO is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * OpenRIO is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Lesser Public License
 * along with OpenRIO.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

/******************************************************************************
 * Description:
 * Unit tests for riostack.c.
 ******************************************************************************/


#define MODULE_TEST
#include "riostack.c"
#include "riopacket.c"
#include <CUnit/CUnit.h>
#include <stdio.h>
#include <stdlib.h>

#define PrintS(s) printf(s "\n")

#define TESTSTART(s) printf(s "...")
#define TESTEND printf(" done.\n");

#define TESTCOND(got) CU_ASSERT(got)
#define TESTEXPR(got, expected) CU_ASSERT_EQUAL((got), (expected))
#define TESTSYMBOL(got, expected) testSymbol((got), (expected))

#define QUEUE_LENGTH 8

int TEST_numExpectedAssertsRemaining = 0;

static RioStack_t stack;
static uint32_t rxPacketBuffer[(RIOPACKET_SIZE_MAX + 1) * QUEUE_LENGTH];
static uint32_t txPacketBuffer[(RIOPACKET_SIZE_MAX + 1) * QUEUE_LENGTH];

static int symbolEquals(RioSymbol_t got, RioSymbol_t expected)
{
  return got.type == expected.type && got.data == expected.data;
}

static void testSymbol(RioSymbol_t got, RioSymbol_t expected)
{
  const uint8_t stype0 = STYPE0_GET( got.data );
  const uint8_t parameter0 = PARAMETER0_GET( got.data );
  const uint8_t parameter1 = PARAMETER1_GET( got.data );
  const uint8_t stype1 = STYPE1_GET( got.data );
  const uint8_t cmd = CMD_GET( got.data );
  const uint8_t crc5 = CRC5_GET( got.data );

  TESTEXPR( got.type, expected.type );

  switch( got.type )
  {
    case RIOSTACK_SYMBOL_TYPE_CONTROL:
      TESTEXPR( stype0, STYPE0_GET( expected.data ) );
      TESTEXPR( parameter0, PARAMETER0_GET( expected.data ) );
      TESTEXPR( parameter1, PARAMETER1_GET( expected.data ) );
      TESTEXPR( stype1, STYPE1_GET( expected.data ) );
      TESTEXPR( cmd, CMD_GET( expected.data ) );
      TESTEXPR( crc5, CRC5_GET( expected.data ) );
      TESTEXPR( got.data, expected.data );
      break;
    case RIOSTACK_SYMBOL_TYPE_DATA:
      TESTEXPR( got.data, expected.data );
      break;
    case RIOSTACK_SYMBOL_TYPE_ERROR:
    case RIOSTACK_SYMBOL_TYPE_IDLE:
      break;
    default:
      CU_FAIL( "Unknown type" );
      break;
  }
}



static uint8_t createDoorbell(uint32_t *doorbell, uint8_t ackId, uint16_t destid, uint16_t srcId, uint8_t tid, uint16_t info)
{
  uint16_t crc;
  uint32_t content;

  /* ackId(4:0)|0|vc|crf|prio(1:0)|tt(1:0)|ftype(3:0)|destinationId(15:0) */
  /* ackId is set when the packet is transmitted. */
  content = 0x001aul << 16;
  content |= (uint32_t) destid;
  crc = RIOPACKET_crc32(content, 0xffffu);
  doorbell[0] = (((uint32_t) ackId) << 27) | content;

  /* sourceId(15:0)|rsrv(7:0)|srcTID(7:0) */
  content = ((uint32_t) srcId) << 16;
  content |= (uint32_t) tid;
  crc = RIOPACKET_crc32(content, crc);
  doorbell[1] = content;

  /* infoMSB(7:0)|infoLSB(7:0)|crc(15:0) */
  content = ((uint32_t) info) << 16;
  crc = RIOPACKET_crc16(info, crc);
  content |= ((uint32_t) crc);
  doorbell[2] = content;

  return 3;

}

static RioSymbol_t createSymbol(const RioSymbolType_t type)
{
  RioSymbol_t s;
  s.type = type;
  s.data = 0;
  return s;
}

static RioSymbol_t createDataSymbol(const uint32_t data)
{
  RioSymbol_t s;
  s.type = RIOSTACK_SYMBOL_TYPE_DATA;
  s.data = data;
  return s;
}

/* Open the stack and run it until link is up. */
static void startStack(uint32_t queueSize)
{
  RIOSTACK_open(&stack, NULL,
      RIOSTACK_BUFFER_SIZE*queueSize, rxPacketBuffer,
      RIOSTACK_BUFFER_SIZE*queueSize, txPacketBuffer);
  RIOSTACK_portSetTimeout(&stack, 1);
  RIOSTACK_portSetTime(&stack, 0);
  RIOSTACK_portSetStatus(&stack, 1);

  while(stack.rxState != RX_STATE_LINK_INITIALIZED)
  {
    RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 1, STYPE1_NOP, 0));
  }

  while(stack.txState != TX_STATE_LINK_INITIALIZED)
  {
    RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 1, STYPE1_NOP, 0));
    (void)RIOSTACK_portGetSymbol(&stack);
  }

  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);
}

/* Transmit a waiting outbound packet */
static void transmitPacket(const uint32_t buffer[], uint32_t bufferLength, uint8_t ackedId, uint8_t inboundQueueAvailable,
    int withEnd)
{
  uint32_t i;

  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, ackedId, inboundQueueAvailable, STYPE1_START_OF_PACKET, 0));

  for( i = 0; i < bufferLength; i++ )
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(buffer[i]));
  }

  if( withEnd )
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, ackedId, inboundQueueAvailable, STYPE1_END_OF_PACKET, 0));
  }
}

/* Receive an inbound packet */
static void receivePacket(const uint32_t buffer[], uint32_t bufferLength, uint8_t ackedId, uint8_t inboundQueueAvailable,
    int withEnd)
{
  uint32_t i;

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, ackedId, inboundQueueAvailable, STYPE1_START_OF_PACKET, 0));

  for( i = 0; i < bufferLength; i++ )
  {
    RIOSTACK_portAddSymbol(&stack, createDataSymbol(buffer[i]));
  }

  if( withEnd )
  {
    RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, ackedId, inboundQueueAvailable, STYPE1_END_OF_PACKET, 0));
  }
}

/* Over-fill the inbound queue to cause Packet-Retry. */
static void causeSendPacketRetry(void)
{
  uint32_t j = 0;
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  while(stack.txState != TX_STATE_SEND_PACKET_RETRY)
  {
    uint32_t i;
    uint32_t packet[69];
    uint32_t packetLength;

    packetLength = createDoorbell(packet, j, 0, 0, j, 0);
    receivePacket(packet, packetLength, j, 1, 1);
    j++;
  }
  TESTEXPR(RIOSTACK_getInboundQueueAvailable(&stack), 0);
}

/* Over-fill the input queue while reading to cause Input Retry-Stopped state. */
static void causeInputRetryStopped(void)
{
  int j = 0;
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  while(stack.rxState != RX_STATE_INPUT_RETRY_STOPPED)
  {
    uint32_t i;
    uint32_t packet[69];
    uint32_t packetLength;

    packetLength = createDoorbell(packet, j, 0, 0, j, 0);
    receivePacket(packet, packetLength, j, 1, 1);

    (void)RIOSTACK_portGetSymbol(&stack);

    j++;
  }
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueAvailable(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);
}

static void causeInputErrorStopped(void)
{
  RIOSTACK_portAddSymbol(&stack, createSymbol(RIOSTACK_SYMBOL_TYPE_ERROR));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_NOT_ACCEPTED);
}

/*******************************************************************************
 * Module test for this file.
 *******************************************************************************/

static void allTests(void)
{
  RioPacket_t rioPacket;
  uint32_t packet[RIOPACKET_SIZE_MAX];
  RioSymbol_t s;
  int i;
  int j;
  uint16_t length;
  uint16_t dstid;
  uint16_t srcid;
  uint8_t tid;
  uint16_t info;
  uint32_t packetLength;

  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("TG_riostack");
  PrintS("----------------------------------------------------------------------");
  PrintS("TG_riostack-TC1");
  PrintS("Description: Test link initialization and normal packet exchange.");
  PrintS("Requirement: XXXXX");
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 1:");
  PrintS("Action: Send packets when port is uninitialized.");
  PrintS("Result: All packets should be ignored during initialization.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC1-Step1");
  /******************************************************************************/

  /* Check that states are reported correctly */
  {
    RioTransmitterState_t txState;
    for( txState = TX_STATE_UNINITIALIZED; txState <= TX_STATE_OUTPUT_ERROR_STOPPED; txState++ )
    {
      RioReceiverState_t rxState;
      int txOn = ( ( txState != TX_STATE_UNINITIALIZED ) && ( txState != TX_STATE_PORT_INITIALIZED ) );
      stack.txState = txState;
      for( rxState = RX_STATE_UNINITIALIZED; rxState <= RX_STATE_INPUT_ERROR_STOPPED; rxState++ )
      {
        int rxOn = ( ( rxState != RX_STATE_UNINITIALIZED ) && ( rxState != RX_STATE_PORT_INITIALIZED ) );
        stack.rxState = rxState;
        TESTEXPR(RIOSTACK_getStatus(&stack), ( rxOn || txOn ));
      }
    }
  }

  /* Open the stack and set the port status to initialized. */
  RIOSTACK_open(&stack, NULL,
      RIOSTACK_BUFFER_SIZE*QUEUE_LENGTH, &rxPacketBuffer[0],
      RIOSTACK_BUFFER_SIZE*QUEUE_LENGTH, &txPacketBuffer[0]);
  TESTEXPR(stack.rxState, RX_STATE_UNINITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_UNINITIALIZED);

  /* Set the port timeout. */
  RIOSTACK_portSetTimeout(&stack, 1);

  /* Set the current port time. */
  RIOSTACK_portSetTime(&stack, 0);

  /* Place a packet in the outbound queue to check that it is received once 
     the transmitter is placed in the correct state. */
  RIOPACKET_setDoorbell(&rioPacket, 1, 0xffff, 0, 0xdeaf);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);
  TESTEXPR(stack.rxState, RX_STATE_UNINITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_UNINITIALIZED);

  /* Check that only idle symbols are transmitted when the port has not been
     initialized even if statuses are received. */
  for(i = 0; i < 1024; i++)
  {
    RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 1, STYPE1_NOP, 0));
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
    TESTEXPR(stack.rxState, RX_STATE_UNINITIALIZED);
    TESTEXPR(stack.txState, TX_STATE_UNINITIALIZED);
  }
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /******************************************************************************/
  TESTEND;
  /*****************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 2:");
  PrintS("Action: Set port initialized and get symbols from the stack.");
  PrintS("Result: Status-control-symbols should be generated each 256 symbol.");
  PrintS("----------------------------------------------------------------------");
  /*****************************************************************************/
  TESTSTART("TG_riostack-TC1-Step2");
  /*****************************************************************************/

  /* Set the port status to initialized. */
  RIOSTACK_portSetStatus(&stack, 1);
  TESTEXPR(stack.rxState, RX_STATE_PORT_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);

  /* Set the port status to uninitialized. */
  RIOSTACK_portSetStatus(&stack, 0);
  TESTEXPR(stack.rxState, RX_STATE_UNINITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_UNINITIALIZED);

  /* Set the port status to initialized again. */
  RIOSTACK_portSetStatus(&stack, 1);
  TESTEXPR(stack.rxState, RX_STATE_PORT_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);

  /* Set port time. */
  RIOSTACK_portSetTime(&stack, 1);

  /* Check that status-control-symbols are transmitted once every 256 symbol. */
  for(j = 0; j < 15; j++)
  {
    for(i = 0; i < 255; i++)
    {
      TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
      TESTEXPR(stack.rxState, RX_STATE_PORT_INITIALIZED);
      TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);
    }
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_NOP, 0));
    TESTEXPR(stack.rxState, RX_STATE_PORT_INITIALIZED);
    TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);
  }
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 3:");
  PrintS("Action: Add a status-control-symbol to the receiver.");
  PrintS("Result: Status-control-symbols should be generated each 15 symbol.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC1-Step3");
  /*****************************************************************************/

  /* Insert a status-control-symbol in the receive. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_PORT_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);

  /* Check that status-control-symbols are transmitted once every 16 symbol. */
  for(i = 0; i < 15; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
    TESTEXPR(stack.rxState, RX_STATE_PORT_INITIALIZED);
    TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_PORT_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);

  /* Ignored */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 0, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_PORT_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 4:");
  PrintS("Action: Add a packet to the receiver.");
  PrintS("Result: Packet should be ignored until the link is initialized.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC1-Step4");
  /*****************************************************************************/

  /* Send a packet. Note that the start and end of the packet contains a status. */
  packetLength = createDoorbell(packet, 0, 0, 0, 0, 0);
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_START_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_PORT_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);
  for(i = 0; i < packetLength; i++)
  {
    RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[i]));
    TESTEXPR(stack.rxState, RX_STATE_PORT_INITIALIZED);
    TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);
  }
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_PORT_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);

  /* Check that packet was not received. */
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 5:");
  PrintS("Action: Add four more status-control-symbols followed by one with error in ");
  PrintS("        CRC5. Then send a packet.");
  PrintS("Result: The receiver should remain in port initialized and packet should ");
  PrintS("        still be ignored.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC1-Step5");
  /*****************************************************************************/

  /* Send 4 more status-control-symbols followed by one erroneous. */
  for(i = 0; i < 4; i++)
  {
    RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 1, STYPE1_NOP, 0));
  }
  s = createControlSymbol(STYPE0_STATUS, 0, 1, STYPE1_NOP, 0);
  s.data ^= 1;
  RIOSTACK_portAddSymbol(&stack, s);
  TESTEXPR(stack.rxState, RX_STATE_PORT_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);

  /* Send a packet. Note that the start and end of the packet contains status. */
  packetLength = createDoorbell(packet, 0, 0, 0, 0, 0);
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[i]));
  }
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_PORT_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);

  /* Check that the packet was ignored. */
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 6:");
  PrintS("Action: Add six more status-control-symbols. Then send a packet.");
  PrintS("Result: The receiver should enter link initialized and the packet should ");
  PrintS("        be received.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC1-Step6");
  /*****************************************************************************/

  /* Send 6 more status-control-symbols. */
  for(i = 0; i < 6; i++)
  {
    TESTEXPR(stack.rxState, RX_STATE_PORT_INITIALIZED);
    TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);
    RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 1, STYPE1_NOP, 0));
  }
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);

  /* Send a packet and check that it is accepted. */
  /* The ackId on receiver in testobject is updated when this has been transmitted. */
  packetLength = createDoorbell(packet, 0, 0, 0, 0, 0);
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[i]));
  }
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);

  /* Check that the packet is received. */
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 1);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 7:");
  PrintS("Action: Get symbols from transmitter.");
  PrintS("Result: Status-control-symbols should still be generated each 15 symbol ");
  PrintS("until a total of 15 status-control-symbols has been transmitted. Once these ");
  PrintS("has been transmitted, the transmitter will be link initialized.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC1-Step7");
  /*****************************************************************************/

  /* Note that the available buffers in the receiver should have decremented once 
     since the previously received packet has not been read from the application 
     side of the stack yet. */
  for(j = 0; j < 14; j++)
  {
    for(i = 0; i < 15; i++)
    {
      TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
    }
    TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
    TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 1, 7, STYPE1_NOP, 0));
  }
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 1);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 8:");
  PrintS("Action: Get the first symbol from the transmitter once the link-initialized ");
  PrintS("        state has been entered.");
  PrintS("Result: A packet-accepted-symbol should be received for the newly received ");
  PrintS("        packet.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC1-Step8");
  /*****************************************************************************/

  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_PACKET_ACCEPTED, 0, 7, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 1);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 9:");
  PrintS("Action: Get the next symbols from the transmitter.");
  PrintS("Result: The packet placed in the outbound queue at startup should be ");
  PrintS("        received. Don't acknowledge the packet yet.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC1-Step9");
  /*****************************************************************************/

  /* Create a packet. */
  packetLength = createDoorbell(packet, 0, 1, 0xffff, 0, 0xdeaf);

  /* Receive the start of the frame. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 1, 7, STYPE1_START_OF_PACKET, 0));

  /* Receive the data of the frame. */
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }

  /* Receive the end of the frame. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 1, 7, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 1);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 10:");
  PrintS("Action: Remove the packet from the inbound queue. Dont acknowledge the");
  PrintS("        transmitted packet yet.");
  PrintS("Result: Check that status-control-symbols are sent each 256 symbol and that ");
  PrintS("        the buffer count is updated when the inbound packet has been read.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC1-Step10");
  /*****************************************************************************/

  /* Simulate the application reading the received packet to free one reception 
     buffer. */
  RIOSTACK_getInboundPacket(&stack, &rioPacket);

  /* Check that the status-control-symbols are generated each 256 symbol. */
  for(i = 0; i < 255; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
  }

  /* Check that the buffer status has been updated. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 1, 8, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 11:");
  PrintS("Action: Send a packet when an acknowledge has not been received.");
  PrintS("Result: Only idle and status control symbols should be transmitted until ");
  PrintS("        the packet-accepted symbol has been received.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC1-Step11");
  /*****************************************************************************/

  /* Place a packet in the outbound queue. */
  RIOPACKET_setDoorbell(&rioPacket, 2, 0xffff, 1, 0xc0de);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);

  packetLength = createDoorbell(packet, 1, 2, 0xffff, 1, 0xc0de);

  /* Receive the packet. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 1, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 1, 8, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 2);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 12:");
  PrintS("Action: Send a packet-accepted symbol.");
  PrintS("Result: Check that the new packet is transmitted.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC1-Step12");
  /*****************************************************************************/

  /* Send acknowledge for the first frame, and make sure that latency is updated. */
  RIOSTACK_portSetTimeout(&stack, 6);
  RIOSTACK_portSetTime(&stack, 5);
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 0, 1, STYPE1_NOP, 0));
  RIOSTACK_portSetTimeout(&stack, 1);
  RIOSTACK_portSetTime(&stack, 1);

  /* Check that status-control-symbols are transmitted once every 256 symbol with 
     updated ackId. */
  for(i = 0; i < 255; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 1, 8, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 13:");
  PrintS("Action: Send a packet-accepted symbol.");
  PrintS("Result: Check that only idle and status-control-symbols are transmitted ");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC1-Step13");
  /*****************************************************************************/

  /* Acknowledge the second frame. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 1, 1, STYPE1_NOP, 0));

  /* Check that status-control-symbols are transmitted once every 256 symbol with 
     updated ackId. */
  for(i = 0; i < 255; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 1, 8, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("TG_riostack-TC2");
  PrintS("Description: Test flow control.");
  PrintS("Requirement: XXXXX");
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 1:");
  PrintS("Action: Send packets to receiver but don't acknowledge them.");
  PrintS("Result: The reception queue of the stack is full.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC2-Step1");
  /******************************************************************************/

  /* Fill input queue in receiver. */
  for(j = 0; j < QUEUE_LENGTH; j++)
  {
    packetLength = createDoorbell(packet, 1+j, 0, 0, 1+j, 0);

    RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 2, 1, STYPE1_START_OF_PACKET, 0));
    for(i = 0; i < packetLength; i++)
    {
      RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[i]));
    }
    RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 2, 1, STYPE1_END_OF_PACKET, 0));

    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_PACKET_ACCEPTED, 1+j, 7-j, STYPE1_NOP, 0));
  }
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueAvailable(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 2:");
  PrintS("Action: Send a packet when the inbound queue of the stack is full.");
  PrintS("Result: The stack sends a packet-retry symbol. The receiver will end up in ");
  PrintS("input-retry-stopped state.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC2-Step2");
  /******************************************************************************/

  /* Send another packet. */
  packetLength = createDoorbell(packet, 9, 0, 0, 9, 0);
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 2, 1, STYPE1_START_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_RETRY);
  for(i = 0; i < packetLength; i++)
  {
    RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[i]));
    TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
    TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_RETRY);
  }
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 2, 1, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_RETRY);

  /* Receive indication from stack that the packet must be retried. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_PACKET_RETRY, 9, 0, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueAvailable(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 3:");
  PrintS("Action: Send a packet when the receiver is in input-retry-stopped.");
  PrintS("Result: The receiver should ignore the new packet.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC2-Step3");
  /******************************************************************************/

  /* Resend the packet. */
  packetLength = createDoorbell(packet, 9, 0, 0, 9, 0);
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 2, 1, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[i]));
  }
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 2, 1, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Check that nothing is transmitted. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Discard unexpected type */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_IMPLEMENTATION_DEFINED, 2, 1, STYPE1_START_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_IMPLEMENTATION_DEFINED, 2, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_IMPLEMENTATION_DEFINED, 2, 1, STYPE1_MULTICAST_EVENT, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_IMPLEMENTATION_DEFINED, 2, 1, STYPE1_RESERVED, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_RESERVED, 2, 1, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_VC_STATUS, 2, 1, STYPE1_STOMP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createDataSymbol(0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueAvailable(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 4:");
  PrintS("Action: Send restart-from-retry and resend the previous packet.");
  PrintS("Result: The receiver should leave the input-retry-stopped state and receive ");
  PrintS("        the new frame.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC2-Step4");
  /******************************************************************************/

  /* Send restart-from-retry. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 2, 1, STYPE1_RESTART_FROM_RETRY, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Check that the transaction id is correct and remove a packet from the inbound 
     queue. One entry in the inbound queue will be empty. */
  RIOSTACK_getInboundPacket(&stack, &rioPacket);
  RIOPACKET_getDoorbell(&rioPacket, &dstid, &srcid, &tid, &info);
  TESTEXPR(tid, 1);

  /* Check that the buffer status has changed to show that a buffer is available. */
  s = RIOSTACK_portGetSymbol(&stack);  
  while(s.type == RIOSTACK_SYMBOL_TYPE_IDLE)
  {
    s = RIOSTACK_portGetSymbol(&stack);  
  }
  TESTSYMBOL(s, createControlSymbol(STYPE0_STATUS, 9, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Resend the packet and check that it is received. */
  packetLength = createDoorbell(packet, 9, 0, 0, 9, 0);
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 2, 1, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[i]));
  }
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 2, 1, STYPE1_END_OF_PACKET, 0));
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_PACKET_ACCEPTED, 9, 0, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueAvailable(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 5:");
  PrintS("Action: Place receiver in input-retry-stopped state.");
  PrintS("Result: Check that packets may be transmitted normally.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC2-Step5");
  /******************************************************************************/

  /* Send another packet and check that the receiver indicates that it should be retried. */
  packetLength = createDoorbell(packet, 10, 0, 0, 10, 0);
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 2, 1, STYPE1_START_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_RETRY);
  for(i = 0; i < packetLength; i++)
  {
    RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[i]));
  }
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 2, 1, STYPE1_END_OF_PACKET, 0));
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_PACKET_RETRY, 10, 0, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send two packets to see that the first acknowledge has been processed. */
  RIOPACKET_setDoorbell(&rioPacket, 0, 0xffff, 2, 0xfeed);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);
  RIOPACKET_setDoorbell(&rioPacket, 0, 0xffff, 3, 0xdeed);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);

  /* Get the first packet. */
  packetLength = createDoorbell(packet, 2, 0, 0xffff, 2, 0xfeed);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 0, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Get the second packet. */
  packetLength = createDoorbell(packet, 3, 0, 0xffff, 3, 0xdeed);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 0, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 0, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Indicate the packets must be retransmitted. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_RETRY, 2, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_RETRY_STOPPED);

  /* Receive confirmation that the packet will be retransmitted. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 0, STYPE1_RESTART_FROM_RETRY, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Get the retransmission of the first packet. */
  packetLength = createDoorbell(packet, 2, 0, 0xffff, 2, 0xfeed);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 0, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Get the retransmission of the second packet. */
  packetLength = createDoorbell(packet, 3, 0, 0xffff, 3, 0xdeed);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 0, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 0, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Confirm the reception of the packets. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 2, 1, STYPE1_NOP, 0));
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 3, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueAvailable(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 6:");
  PrintS("Action: Send status-control-symbol to show that no packets can be ");
  PrintS("        transmitted.");
  PrintS("Result: No packets should be transmitted.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC2-Step6");
  /******************************************************************************/

  /* Send status with bufferStatus set to zero. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 4, 0, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send a packet. */
  RIOPACKET_setDoorbell(&rioPacket, 0, 0xffff, 4, 0xf00d);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);

  /* Check that nothing is transmitted but status-control-symbols. */  
  for(i = 0; i < 255; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
    TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
    TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 0, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueAvailable(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 7:");
  PrintS("Action: Indicate free buffers and receive a frame, then request it to be ");
  PrintS("retried.");
  PrintS("Result: The packet should be retransmitted.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC2-Step7");
  /******************************************************************************/

  /* Send status with bufferStatus set to available. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 4, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Get the packet but request it to be retried. */
  packetLength = createDoorbell(packet, 4, 0, 0xffff, 4, 0xf00d);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 0, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 0, STYPE1_END_OF_PACKET, 0));
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_RETRY, 4, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_RETRY_STOPPED);

  /* Check the acknowledge of the retransmission. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 0, STYPE1_RESTART_FROM_RETRY, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Get the packet and acknowledge it. */
  packetLength = createDoorbell(packet, 4, 0, 0xffff, 4, 0xf00d);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 0, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 0, STYPE1_END_OF_PACKET, 0));
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 4, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueAvailable(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 8:");
  PrintS("Action: Read all inbound packets from the reception queue.");
  PrintS("Result: The buffer status should be updated.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC2-Step8");
  /******************************************************************************/

  for(j = 0; j < 8; j++)
  {
    RIOSTACK_getInboundPacket(&stack, &rioPacket);
    RIOPACKET_getDoorbell(&rioPacket, &dstid, &srcid, &tid, &info);
    TESTEXPR(tid, j+2);
    
    for(i = 0; i < 255; i++)
    {
      TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
      TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
      TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
    }
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, j+1, STYPE1_NOP, 0));
    TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
    TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  }
  
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 9:");
  PrintS("Action: Send a restart-from-retry to make the receiver leave the ");
  PrintS("        input-retry-stopped state.");
  PrintS("Result: New packets should be received again.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC2-Step9");
  /******************************************************************************/

  /* Send restart-from-retry. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, STYPE1_RESTART_FROM_RETRY, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("TG_riostack-TC3");
  PrintS("Description: Test receiver error handling.");
  PrintS("Requirement: XXXXX");
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 1:");
  PrintS("Action: Send invalid ack id in packet.");
  PrintS("Result: Input-error-stopped state should be entered and link-response ");
  PrintS("        should indicate an ackId error.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC3-Step1");
  /******************************************************************************/

  /* Send packet with invalid ackId, same as sent previously. */
  packetLength = createDoorbell(packet, 9, 0, 0, 10, 0);
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, STYPE1_START_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  for(i = 0; i < packetLength; i++)
  {
    RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[i]));
    TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
    TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_NOT_ACCEPTED);
  }
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_NOT_ACCEPTED);

  /* Check that the packet is not accepted. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_UNEXPECTED_ACKID, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send a link-request. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, 
                                                     STYPE1_LINK_REQUEST, LINK_REQUEST_INPUT_STATUS));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_SEND_LINK_RESPONSE);

  /* Check that a link-response is returned. */
  /* Note that the status of the input-port will be reported as ok since a 
     link-request has been received. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_LINK_RESPONSE, 10, 16, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Check that a status is transmitted directly after the link-response. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 2:");
  PrintS("Action: Send packet with invalid CRC.");
  PrintS("Result: Input-error-stopped state should be entered and link-response ");
  PrintS("        should indicate a CRC error.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC3-Step2");
  /******************************************************************************/

  /* Send packet with invalid crc. */
  packetLength = createDoorbell(packet, 10, 0, 0, 10, 0);
  packet[0] ^= 0x00000001;
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[i]));
  }
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_NOT_ACCEPTED);
  TESTEXPR(stack.statusInboundErrorPacketCrc, 1);
  stack.statusInboundErrorPacketCrc = 0;

  /* Check that the packet is not accepted. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_PACKET_CRC, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send a link-request. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, STYPE1_LINK_REQUEST, LINK_REQUEST_INPUT_STATUS));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_SEND_LINK_RESPONSE);

  /* Check that a link-response is returned. */
  /* Note that the status of the input-port will be reported as ok since a 
     link-request has been received. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_LINK_RESPONSE, 10, 16, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Check that a status is transmitted directly after the link-response. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 3:");
  PrintS("Action: Send a packet that is too short.");
  PrintS("Result: Input-error-stopped state should be entered and link-response ");
  PrintS("        should indicate a packet error.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC3-Step3");
  /******************************************************************************/

  /* Send packet with valid ackid and crc but too short. */
  packetLength = createDoorbell(packet, 10, 0, 0, 10, 0);
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, STYPE1_START_OF_PACKET, 0));
  RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[0]));
  RIOSTACK_portAddSymbol(&stack, createDataSymbol(((uint32_t) RIOPACKET_crc32(packet[0] & 0x07ffffff, 0xffff)) << 16));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.statusInboundErrorGeneral, 1);
  stack.statusInboundErrorGeneral = 0;
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_NOT_ACCEPTED);

  /* Check that the packet is not accepted. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_GENERAL, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send a link-request. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, STYPE1_LINK_REQUEST, LINK_REQUEST_INPUT_STATUS));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_SEND_LINK_RESPONSE);

  /* Check that a link-response is returned. */
  /* Note that the status of the input-port will be reported as ok since a 
     link-request has been received. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_LINK_RESPONSE, 10, 16, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Check that a status is transmitted directly after the link-response. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 4:");
  PrintS("Action: Send a packet that is too long.");
  PrintS("Result: Input-error-stopped state should be entered and link-response ");
  PrintS("        should indicate a packet error.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC3-Step4");
  /******************************************************************************/

  /* Send packet with too many data symbols and without a end-of-packet. */
  packetLength = createDoorbell(packet, 10, 0, 0, 10, 0);
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[i]));
  }
  for(; i < 70; i++)
  {
    TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
    TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
    RIOSTACK_portAddSymbol(&stack, createDataSymbol(i));
  }
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_NOT_ACCEPTED);
  TESTEXPR(stack.statusInboundErrorGeneral, 1);
  stack.statusInboundErrorGeneral = 0;

  /* Check that the packet is not accepted. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_GENERAL, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send a link-request. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, STYPE1_LINK_REQUEST, LINK_REQUEST_INPUT_STATUS));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_SEND_LINK_RESPONSE);

  /* Check that a link-response is returned. */
  /* Note that the status of the input-port will be reported as ok since a 
     link-request has been received. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_LINK_RESPONSE, 10, 16, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Check that a status is transmitted directly after the link-response. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 5:");
  PrintS("Action: Send a data symbol without starting a packet.");
  PrintS("Result: Input-error-stopped state should be entered and link-response ");
  PrintS("        should indicate a packet error.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC3-Step5");
  /******************************************************************************/

  /* Send a data symbol. */
  packetLength = createDoorbell(packet, 10, 0, 0, 10, 0);
  RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[0]));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_NOT_ACCEPTED);
  TESTEXPR(stack.statusInboundErrorGeneral, 1);
  stack.statusInboundErrorGeneral = 0;

  /* Check that the packet is not accepted. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_GENERAL, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send a link-request. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, 
                                                     STYPE1_LINK_REQUEST, LINK_REQUEST_INPUT_STATUS));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_SEND_LINK_RESPONSE);

  /* Check that a link-response is returned. */
  /* Note that the status of the input-port will be reported as ok since a 
     link-request has been received. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_LINK_RESPONSE, 10, 16, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Check that a status is transmitted directly after the link-response. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 6:");
  PrintS("Action: Send end-of-packet without matching start.");
  PrintS("Result: Input-error-stopped state should be entered and link-response ");
  PrintS("        should indicate a packet error.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC3-Step6");
  /******************************************************************************/

  /* Send end-of-packet. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_NOT_ACCEPTED);
  TESTEXPR(stack.statusInboundErrorGeneral, 1);
  stack.statusInboundErrorGeneral = 0;

  /* Check that the packet is not accepted. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_GENERAL, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send a link-request. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, 
                                                     STYPE1_LINK_REQUEST, LINK_REQUEST_INPUT_STATUS));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_SEND_LINK_RESPONSE);

  /* Check that a link-response is returned. */
  /* Note that the status of the input-port will be reported as ok since a 
     link-request has been received. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_LINK_RESPONSE, 10, 16, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Check that a status is transmitted directly after the link-response. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 7:");
  PrintS("Action: Send a symbol indicating a codec error.");
  PrintS("Result: Input-error-stopped state should be entered and link-response ");
  PrintS("        should indicate a symbol error.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC3-Step7");
  /******************************************************************************/

  /* Send error-symbol. */
  RIOSTACK_portAddSymbol(&stack, createSymbol(RIOSTACK_SYMBOL_TYPE_ERROR));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_NOT_ACCEPTED);
  TESTEXPR(stack.statusInboundErrorIllegalCharacter, 1);
  stack.statusInboundErrorIllegalCharacter = 0;

  /* Check that the packet is not accepted. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_ILLEGAL_CHARACTER, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send a link-request. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, 
                                                     STYPE1_LINK_REQUEST, LINK_REQUEST_INPUT_STATUS));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_SEND_LINK_RESPONSE);

  /* Check that a link-response is returned. */
  /* Note that the status of the input-port will be reported as ok since a 
     link-request has been received. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_LINK_RESPONSE, 10, 16, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Check that a status is transmitted directly after the link-response. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("TG_riostack-TC4");
  PrintS("Description: Test transmitter error handling.");
  PrintS("Requirement: XXXXX");
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 1:");
  PrintS("Action: Send acknowledge for a frame that has not been transmitted and ");
  PrintS("        without any frame being expected.");
  PrintS("Result: The transmitter should enter output-error-stopped and send ");
  PrintS("        link-request.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC4-Step1");
  /******************************************************************************/

  /* Packet acknowledge for unsent frame. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 5, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

  /* Check that a link-request is received as the transmitter enters 
     output-error-stopped state. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
      createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_LINK_REQUEST, LINK_REQUEST_INPUT_STATUS));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

  /* Send link-response with expected ackId. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_LINK_RESPONSE, 5, 16, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send a status directly afterwards. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 5, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Check that packets are relayed after this. */
  RIOPACKET_setDoorbell(&rioPacket, 0, 0xffff, 5, 2);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);
  packetLength = createDoorbell(packet, 5, 0, 0xffff, 5, 2);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_END_OF_PACKET, 0));
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 5, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 2:");
  PrintS("Action: Send a packet and send acknowledge for a previous frame. Then send ");
  PrintS("        a link-response indicating that the packet was received (accepted ");
  PrintS("        but reply corrupted).");
  PrintS("Result: The transmitter should enter output-error-stopped state and send ");
  PrintS("        link-request and proceed with the next packet.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC4-Step2");
  /******************************************************************************/

  /* Send a packet. */
  RIOPACKET_setDoorbell(&rioPacket, 0, 0xffff, 6, 2);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);
  packetLength = createDoorbell(packet, 6, 0, 0xffff, 6, 2);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send acknowledge for another packet. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 5, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

  /* Check that a link-request is received as the transmitter enters 
     output-error-stopped state. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
      createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_LINK_REQUEST, LINK_REQUEST_INPUT_STATUS));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

  /* Send link-response with expected ackId. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_LINK_RESPONSE, 7, 16, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send a status directly afterwards. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 7, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 3:");
  PrintS("Action: Send a packet and let the packet-accepted time out. Then send a ");
  PrintS("        link-response indicating that the packet was not received.");
  PrintS("Result: The transmitter should enter output-error-stopped state, send a");
  PrintS("        link-request and then resend the packet.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC4-Step3");
  /******************************************************************************/

  /* Set the time at frame transmission. */
  RIOSTACK_portSetTime(&stack, 2);

  /* Send an output packet. */
  RIOPACKET_setDoorbell(&rioPacket, 0, 0xffff, 7, 2);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);

  /* Receive the transmitted packet. */
  packetLength = createDoorbell(packet, 7, 0, 0xffff, 7, 2);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Indicate that time has passed to trigger a timeout. */
  RIOSTACK_portSetTime(&stack, 3);

  /* Check that a link-request is received as the transmitter enters 
     output-error-stopped state. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
      createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_LINK_REQUEST, LINK_REQUEST_INPUT_STATUS));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

  /* Send link-response with expected ackId. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_LINK_RESPONSE, 7, 16, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send a status directly afterwards. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 7, 1, STYPE1_NOP, 0));

  /* Receive retransmitted packet. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_END_OF_PACKET, 0));

  /* Send acknowledge for the retransmitted packet. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 7, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 4:");
  PrintS("Action: Send a packet and then indicate that the packet was not accepted. ");
  PrintS("        Then send a link-response indicating that the packet was not received.");
  PrintS("Result: The transmitter should enter output-error-stopped state, send a");
  PrintS("        link-request and then resend the packet.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC4-Step4");
  /******************************************************************************/

  /* Send an output packet. */
  RIOPACKET_setDoorbell(&rioPacket, 0, 0xffff, 8, 3);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);

  /* Receive the transmitted packet. */
  packetLength = createDoorbell(packet, 8, 0, 0xffff, 8, 3);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send packet-not-accepted indicating CRC error. */
  RIOSTACK_portAddSymbol(&stack,
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_PACKET_CRC, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

  /* Check that a link-request is received as the transmitter enters 
     output-error-stopped state. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
      createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_LINK_REQUEST, LINK_REQUEST_INPUT_STATUS));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

  /* Send link-response with expected ackId. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_LINK_RESPONSE, 8, 16, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send a status directly afterwards. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 8, 1, STYPE1_NOP, 0));

  /* Receive retransmitted packet. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_END_OF_PACKET, 0));

  /* Send acknowledge for the retransmitted packet. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 8, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 5:");
  PrintS("Action: Send a packet-retry for an unexpected packet. Then send a");
  PrintS("        link-response indicating the expected ackId and a normal packet.");
  PrintS("Result: The transmitter should enter output-error-stopped state, send a");
  PrintS("        link-request and then the normal packet.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC4-Step5");
  /******************************************************************************/

  /* Send packet-retry indicating that a packet should be retransmitted. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_RETRY, 8, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

  /* Check that a link-request is received as the transmitter enters 
     output-error-stopped state. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
      createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_LINK_REQUEST, LINK_REQUEST_INPUT_STATUS));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

  /* Send link-response with expected ackId. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_LINK_RESPONSE, 9, 16, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send a status directly afterwards. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 9, 1, STYPE1_NOP, 0));

  /* Send an output packet. */
  RIOPACKET_setDoorbell(&rioPacket, 0, 0xffff, 9, 4);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);

  /* Receive retransmitted packet. */
  packetLength = createDoorbell(packet, 9, 0, 0xffff, 9, 4);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_END_OF_PACKET, 0));

  /* Send acknowledge for the retransmitted packet. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 9, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND; 
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 6:");
  PrintS("Action: Fill outbound queue with packets, then check retransmission when ");
  PrintS("        packet-retry is encountered. ");
  PrintS("Result: Packets should be retried until packet-accepted is received.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC4-Step6");
  /******************************************************************************/

  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueAvailable(&stack), QUEUE_LENGTH);

  for( j = 0; j < QUEUE_LENGTH; ++j )
  {
    RIOPACKET_setDoorbell(&rioPacket, 0, 0xffff, 20 + j, 0xbabe);
    RIOSTACK_setOutboundPacket(&stack, &rioPacket);
  }

  /* Receive transmitted packets. */
  for( j = 0; j < QUEUE_LENGTH; ++j )
  {
    packetLength = createDoorbell(packet, 10 + j, 0, 0xffff, 20 + j, 0xbabe);
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_START_OF_PACKET, 0));
    for(i = 0; i < packetLength; i++)
    {
      TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
    }
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_END_OF_PACKET, 0));

  for(i = 0; i < 10; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
  }
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Request retransmission. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_RETRY, 10, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_RETRY_STOPPED);

  /* Acknowledge retransmission. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_RESTART_FROM_RETRY, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Check retransmitted packets. */
  packetLength = createDoorbell(packet, 10, 0, 0xffff, 20, 0xbabe);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  packetLength = createDoorbell(packet, 11, 0, 0xffff, 21, 0xbabe);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  packetLength = createDoorbell(packet, 12, 0, 0xffff, 22, 0xbabe);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }

  /* Acknowledge. */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 10, 1, STYPE1_NOP, 0));
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 11, 1, STYPE1_NOP, 0));

  packetLength = createDoorbell(packet, 13, 0, 0xffff, 23, 0xbabe);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  packetLength = createDoorbell(packet, 14, 0, 0xffff, 24, 0xbabe);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 12, 1, STYPE1_NOP, 0));

  packetLength = createDoorbell(packet, 15, 0, 0xffff, 25, 0xbabe);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 13, 1, STYPE1_NOP, 0));

  packetLength = createDoorbell(packet, 16, 0, 0xffff, 26, 0xbabe);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_START_OF_PACKET, 0));
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 14, 1, STYPE1_NOP, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  packetLength = createDoorbell(packet, 17, 0, 0xffff, 27, 0xbabe);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_START_OF_PACKET, 0));
  for(i = 0; i < packetLength; i++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 15, 1, STYPE1_NOP, 0));
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 10, 8, STYPE1_END_OF_PACKET, 0));

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 16, 1, STYPE1_NOP, 0));
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 17, 1, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 7:");
  PrintS("Action: Finish a packet by sending a new packet (no end). ");
  PrintS("Result: Packets should be accepted.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC4-Step7");
  /******************************************************************************/

  startStack(QUEUE_LENGTH);

  /* Valid packets */

  packetLength = createDoorbell(packet, 0, 0, 0xffff, 0, 0);
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_START_OF_PACKET, 0));
  for( i = 0; i < packetLength; ++i )
  {
    RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[i]));
  }

  packetLength = createDoorbell(packet, 1, 0, 0xffff, 1, 0);
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 1, 8, STYPE1_START_OF_PACKET, 0));
  for( i = 0; i < packetLength; ++i )
  {
    RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[i]));
  }
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 1, 8, STYPE1_END_OF_PACKET, 0));

  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_PACKET_ACCEPTED, 0, 6, STYPE1_NOP, 0));
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_PACKET_ACCEPTED, 1, 6, STYPE1_NOP, 0));

  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 2);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 8:");
  PrintS("Action: Send a packet with invalid CRC.");
  PrintS("Result: A packet-not-accepted should be received.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC4-Step8");
  /******************************************************************************/

  startStack(QUEUE_LENGTH);

  packetLength = createDoorbell(packet, 0, 0, 0xffff, 0, 0);
  packet[0] ^= 0x00000001; /* Ruin CRC */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_START_OF_PACKET, 0));
  for( i = 0; i < packetLength; ++i )
  {
    RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[i]));
  }
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_START_OF_PACKET, 0));

  TESTEXPR(stack.rxErrorCause, PACKET_NOT_ACCEPTED_CAUSE_PACKET_CRC);
  TESTEXPR(stack.statusInboundErrorPacketCrc, 1);
  stack.statusInboundErrorPacketCrc = 0;
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_NOT_ACCEPTED);

  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_PACKET_CRC, STYPE1_NOP, 0));

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 9:");
  PrintS("Action: Send a packet that is shorter than full length.");
  PrintS("Result: A packet-not-accepted should be received.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC4-Step9");
  /******************************************************************************/

  startStack(QUEUE_LENGTH);

  packetLength = createDoorbell(packet, 0, 0, 0xffff, 36, 0);
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_START_OF_PACKET, 0));
  for( i = 0; i < 2; i++ ) /* Partial data */
  {
    RIOSTACK_portAddSymbol(&stack, createDataSymbol(packet[i]));
  }
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_START_OF_PACKET, 0));

  TESTEXPR(stack.rxErrorCause, PACKET_NOT_ACCEPTED_CAUSE_GENERAL);
  TESTEXPR(stack.statusInboundErrorGeneral, 1);
  stack.statusInboundErrorGeneral = 0;
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_NOT_ACCEPTED);

  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_GENERAL, STYPE1_NOP, 0));

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 10:");
  PrintS("Action: Send an egress packet and dont accept it for too many retries.");
  PrintS("Result: The packet should be dropped once the retransmission limit has");
  PrintS("        been reached.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC4-Step10");
  /******************************************************************************/

  startStack(QUEUE_LENGTH);

  /* Create a packet that will not be accepted. */
  RIOPACKET_setDoorbell(&rioPacket, 0, 0xffff, 0x1d, 0xabab);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);
  packetLength = createDoorbell(packet, 0, 0, 0xffff, 0x1d, 0xabab);

  /* Check that the packet is retransmitted when an packet-not-accepted is being received. */
  for(j = 0; j < (MAX_PACKET_ERROR_RETRIES+1); j++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
               createControlSymbol(STYPE0_STATUS, stack.rxAckId, getBufferStatus(&stack), STYPE1_START_OF_PACKET, 0));
    for( i = 0; i < packetLength; i++ )
    {
      TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
    }
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
               createControlSymbol(STYPE0_STATUS, stack.rxAckId, getBufferStatus(&stack), STYPE1_END_OF_PACKET, 0));
    RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, 8, STYPE1_NOP, 0));

    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
               createControlSymbol(STYPE0_STATUS, stack.rxAckId, getBufferStatus(&stack), STYPE1_LINK_REQUEST, 4));
    RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_LINK_RESPONSE, 0, 8, STYPE1_NOP, 0));
  }

  /* Create a new packet that is different from the one previously sent. */
  RIOPACKET_setDoorbell(&rioPacket, 0, 0xdead, 0xbe, 0xefff);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);
  packetLength = createDoorbell(packet, 0, 0, 0xdead, 0xbe, 0xefff);

  /* Check that the new packet is transmitted. The previous packet should have been discarded. */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
             createControlSymbol(STYPE0_STATUS, stack.rxAckId, getBufferStatus(&stack), STYPE1_START_OF_PACKET, 0));
  for( i = 0; i < packetLength; i++ )
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
             createControlSymbol(STYPE0_STATUS, stack.rxAckId, getBufferStatus(&stack), STYPE1_END_OF_PACKET, 0));
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 0, 8, STYPE1_NOP, 0));
  
  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("TG_riostack-TC5");
  PrintS("Description: Handling of some incoming errors");
  PrintS("Requirement:");
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 1:");
  PrintS("Action: Get some invalid symbols.");
  PrintS("Result: Output is stopped or symbols are discarded.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC5-Step1");
  /******************************************************************************/

  startStack(QUEUE_LENGTH);

  /* Discard unexpected type */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_IMPLEMENTATION_DEFINED, 0, 8, STYPE1_MULTICAST_EVENT, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_RESERVED, 0, 8, STYPE1_RESERVED, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_VC_STATUS, 0, 8, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Invalid CRC */
  i = stack.statusInboundErrorControlCrc;
  s = createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_NOP, 0);
  s.data ^= 1; /* ruin CRC */
  RIOSTACK_portAddSymbol(&stack, s);
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_NOT_ACCEPTED);
  TESTEXPR(stack.rxErrorCause, PACKET_NOT_ACCEPTED_CAUSE_CONTROL_CRC);
  TESTEXPR(stack.statusInboundErrorControlCrc, i+1);

  /* Restore TX state */
  (void)RIOSTACK_portGetSymbol(&stack);
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Discard unexpected type */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_IMPLEMENTATION_DEFINED, 0, 8, STYPE1_START_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_RESERVED, 0, 8, STYPE1_RESERVED, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_VC_STATUS, 0, 8, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_LINK_RESPONSE, 0, 8, STYPE1_END_OF_PACKET, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_IMPLEMENTATION_DEFINED, 0, 8, STYPE1_STOMP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_IMPLEMENTATION_DEFINED, 0, 8, STYPE1_RESTART_FROM_RETRY, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_IMPLEMENTATION_DEFINED, 0, 8, STYPE1_MULTICAST_EVENT, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_IMPLEMENTATION_DEFINED, 0, 8, STYPE1_RESERVED, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createSymbol(RIOSTACK_SYMBOL_TYPE_DATA));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  RIOSTACK_portAddSymbol(&stack, createSymbol(RIOSTACK_SYMBOL_TYPE_ERROR));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Restore RX state */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_LINK_REQUEST, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Receive an extra link request */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_LINK_REQUEST, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 2:");
  PrintS("Action: Get a stomp or restart-from-retry from peer.");
  PrintS("Result: Restart packet.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC5-Step2");
  /******************************************************************************/

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 9, STYPE1_START_OF_PACKET, 0));
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 9, STYPE1_STOMP, 0));
  TESTEXPR(stack.rxCounter, 0);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));

  /* Check that packet was not received. */
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 9, STYPE1_START_OF_PACKET, 0));
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 9, STYPE1_RESTART_FROM_RETRY, 0));
  TESTEXPR(stack.rxCounter, 0);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));

  /* Check that packet was not received. */
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 3:");
  PrintS("Action: Get some error reports in input-retry-stopped.");
  PrintS("Result: Go to output-error-stopped and update statistics.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC5-Step3");
  /******************************************************************************/

  startStack(QUEUE_LENGTH);
  causeInputRetryStopped();

  /* Invalid ackId reported */

  RIOSTACK_portAddSymbol(&stack,
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_UNEXPECTED_ACKID, STYPE1_NOP, 0));
  TESTEXPR(stack.statusPartnerErrorPacketAckId, 1);
  stack.statusPartnerErrorPacketAckId = 0;
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

  /* Invalid control symbol CRC reported */

  RIOSTACK_portAddSymbol(&stack,
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_CONTROL_CRC, STYPE1_NOP, 0));
  TESTEXPR(stack.statusPartnerErrorControlCrc, 1);
  stack.statusPartnerErrorControlCrc = 0;
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

  /* Illegal character reported */

  RIOSTACK_portAddSymbol(&stack,
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_ILLEGAL_CHARACTER, STYPE1_NOP, 0));
  TESTEXPR(stack.statusPartnerErrorIllegalCharacter, 1);
  stack.statusPartnerErrorIllegalCharacter = 0;
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

  /* Other error reported */

  RIOSTACK_portAddSymbol(&stack,
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_GENERAL, STYPE1_NOP, 0));
  TESTEXPR(stack.statusPartnerErrorGeneral, 1);
  stack.statusPartnerErrorGeneral = 0;
  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

  /* Other error reported */

  RIOSTACK_portAddSymbol(&stack, createSymbol(RIOSTACK_SYMBOL_TYPE_ERROR));
  TESTEXPR(stack.rxErrorCause, PACKET_NOT_ACCEPTED_CAUSE_ILLEGAL_CHARACTER);
  TESTEXPR(stack.statusInboundErrorIllegalCharacter, 1);
  stack.statusInboundErrorIllegalCharacter = 0;
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_NOT_ACCEPTED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 8);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 3:");
  PrintS("Action: Get some errors in input-retry-stopped.");
  PrintS("Result: Go to output-error-stopped and update statistics.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC5-Step3");
  /******************************************************************************/

  startStack(QUEUE_LENGTH);
  causeInputRetryStopped();

  /* Invalid control symbol CRC */

  i = stack.statusInboundErrorControlCrc;
  s = createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_NOP, 0);
  s.data ^= 1; /* ruin CRC */
  RIOSTACK_portAddSymbol(&stack, s);
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_NOT_ACCEPTED);
  TESTEXPR(stack.rxErrorCause, PACKET_NOT_ACCEPTED_CAUSE_CONTROL_CRC);
  TESTEXPR(stack.statusInboundErrorControlCrc, i+1);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 8);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 4:");
  PrintS("Action: Get a link request in input-retry-stopped.");
  PrintS("Result: State restored.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC5-Step4");
  /******************************************************************************/

  startStack(QUEUE_LENGTH);
  causeInputRetryStopped();

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_LINK_REQUEST, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 8);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 5:");
  PrintS("Action: Get responses in input-error-stopped.");
  PrintS("Result: Accepted.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC5-Step5");
  /******************************************************************************/

  /* Get accepted */

  startStack(QUEUE_LENGTH);
  causeInputErrorStopped();

  RIOPACKET_setDoorbell(&rioPacket, 1, 2, 0, 0);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);
  while(!symbolEquals(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_END_OF_PACKET, 0)))
  {
  }

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 0, 8, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /* Get retry */

  startStack(QUEUE_LENGTH);
  causeInputErrorStopped();

  RIOPACKET_setDoorbell(&rioPacket, 1, 2, 0, 0);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);
  while(!symbolEquals(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_END_OF_PACKET, 0)))
  {
  }

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_RETRY, 0, 8, STYPE1_NOP, 0));

  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_RETRY_STOPPED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /* Get not accepted */

  startStack(QUEUE_LENGTH);
  causeInputErrorStopped();

  RIOPACKET_setDoorbell(&rioPacket, 1, 2, 0, 0);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);
  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_NOT_ACCEPTED);
  while(!symbolEquals(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_END_OF_PACKET, 0)))
  {
  }

  RIOSTACK_portAddSymbol(&stack,
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_GENERAL, STYPE1_NOP, 0));

  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 6:");
  PrintS("Action: Get control symbols with wrong CRC in input-error-stopped.");
  PrintS("Result: Discarded.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC5-Step5");
  /******************************************************************************/

  startStack(QUEUE_LENGTH);
  causeInputErrorStopped();

  s = createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_NOP, 0);
  s.data ^= 1; /* Ruin CRC */
  RIOSTACK_portAddSymbol(&stack, s);

  TESTEXPR(stack.rxState, RX_STATE_INPUT_ERROR_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_SEND_PACKET_NOT_ACCEPTED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 7: Unexpected link response.");
  PrintS("Action: Get an unexpected link response in input-retry-stopped.");
  PrintS("Result: Assertion fails.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC5-Step7");
  /******************************************************************************/

  startStack(QUEUE_LENGTH);
  causeInputRetryStopped();

  RIOSTACK_portAddSymbol(&stack,
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_GENERAL, STYPE1_NOP, 0));

  TEST_numExpectedAssertsRemaining = 1;
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_LINK_RESPONSE, 1, 0, STYPE1_NOP, 0));
  TESTEXPR(TEST_numExpectedAssertsRemaining, 0);
  TESTEXPR(stack.txState, TX_STATE_UNINITIALIZED);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 8:");
  PrintS("Action: Sent packet is not acknowledged, inbound queue fills up");
  PrintS("Result: Packet-retry is sent.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC5-Step8");
  /******************************************************************************/

  startStack(QUEUE_LENGTH);
  RIOPACKET_setDoorbell(&rioPacket, 1, 2, 0, 0);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);
  causeSendPacketRetry();

  for(j = 0; j < QUEUE_LENGTH; j++)
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_PACKET_ACCEPTED, j, 0, STYPE1_NOP, 0));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_PACKET_RETRY, QUEUE_LENGTH, 0, STYPE1_NOP, 0));

  TESTEXPR(stack.rxState, RX_STATE_INPUT_RETRY_STOPPED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueAvailable(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 9:");
  PrintS("Action: Sent packet is not accepted, inbound queue fills up, get link request");
  PrintS("Result: Assertion fails.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC5-Step9");
  /******************************************************************************/

  startStack(QUEUE_LENGTH);

  /* Send packet. */
  RIOPACKET_setDoorbell(&rioPacket, 1, 2, 0, 0);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Peer gets Doorbell. */
  packetLength = createDoorbell(packet, 0, 1, 2, 0, 0);
  transmitPacket(packet, packetLength, 0, QUEUE_LENGTH, 1);
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  /* Send packet-not-accepted. */
  RIOSTACK_portAddSymbol(&stack,
      createControlSymbol(STYPE0_PACKET_NOT_ACCEPTED, 0, PACKET_NOT_ACCEPTED_CAUSE_GENERAL, STYPE1_NOP, 0));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

  /* Get link-request */
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
      createControlSymbol(STYPE0_STATUS, 0, QUEUE_LENGTH, STYPE1_LINK_REQUEST, LINK_REQUEST_INPUT_STATUS));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

  /* Get 4 timeouts */
  for( i = 1; i < 5; i++ )
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
    TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
    TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

    RIOSTACK_portSetTime(&stack, i);

    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack),
        createControlSymbol(STYPE0_STATUS, 0, QUEUE_LENGTH, STYPE1_LINK_REQUEST, LINK_REQUEST_INPUT_STATUS));
    TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
    TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);
  }

  /* On timeout 5, get an assert failure */

  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_OUTPUT_ERROR_STOPPED);

  RIOSTACK_portSetTime(&stack, i);

  TEST_numExpectedAssertsRemaining = 1;
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
  TESTEXPR(TEST_numExpectedAssertsRemaining, 0);
  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_UNINITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 10:");
  PrintS("Action: Send packets, get full peer buffer before received, then not full.");
  PrintS("Result: Sending is paused until receiver is ready.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC5-Step10");
  /******************************************************************************/

  startStack(QUEUE_LENGTH);

  /* Send packets */
  RIOPACKET_setDoorbell(&rioPacket, 1, 2, 0, 0);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);
  RIOPACKET_setDoorbell(&rioPacket, 1, 2, 1, 0);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);

  /* Peer becomes full during first packet, packet will be finished anyway */
  packetLength = createDoorbell(packet, 0, 1, 2, 0, 0);
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_START_OF_PACKET, 0));
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, 0, STYPE1_NOP, 0));
  for( i = 0; i < packetLength; i++ )
  {
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createDataSymbol(packet[i]));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 0, 8, STYPE1_END_OF_PACKET, 0));
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));

  /* Peer is not full anymore, remaining packet is sent */
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 1, 1, STYPE1_NOP, 0));
  packetLength = createDoorbell(packet, 1, 1, 2, 1, 0);
  transmitPacket(packet, packetLength, 0, 8, 1);

  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 2);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 11:");
  PrintS("Action: 32 packets are sent without acknowledgment.");
  PrintS("Result: The transmission is paused after 31 packets, due to field limits");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC5-Step11");
  /******************************************************************************/

  startStack(32);
  for( i = 0; i < 32; i++)
  {
    RIOPACKET_setDoorbell(&rioPacket, 1, 2, i, 0);
    RIOSTACK_setOutboundPacket(&stack, &rioPacket);
  }
  for( i = 0; i < 31; i++)
  {
    packetLength = createDoorbell(packet, i, 1, 2, i, 0);
    transmitPacket(packet, packetLength, 0, 31, i == 30);
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_PACKET_ACCEPTED, 0, 1, STYPE1_NOP, 0));
  packetLength = createDoorbell(packet, i, 1, 2, i, 0);
  transmitPacket(packet, packetLength, 0, 31, 1);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("TG_riostack-TC6");
  PrintS("Description: Error handling.");
  PrintS("Requirement: XXXXX");
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 1:");
  PrintS("Action: Set the link to uninitialized.");
  PrintS("Result: State indicates unavailable.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC6-Step1");
  /******************************************************************************/

  startStack(QUEUE_LENGTH);

  RIOSTACK_portSetStatus(&stack, 0);
  TESTEXPR(stack.rxState, RX_STATE_UNINITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_UNINITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 2:");
  PrintS("Action: Add outbound packet when queue is full.");
  PrintS("Result: Assertion fails.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC6-Step2");
  /******************************************************************************/

  startStack(1);

  RIOPACKET_setDoorbell(&rioPacket, 1, 2, 0, 0);
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);
  TEST_numExpectedAssertsRemaining = 1;
  RIOSTACK_setOutboundPacket(&stack, &rioPacket);
  TESTEXPR(TEST_numExpectedAssertsRemaining, 0);

  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 1);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 3:");
  PrintS("Action: Get inbound packet when queue is empty.");
  PrintS("Result: Assertion fails.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC6-Step3");
  /******************************************************************************/

  startStack(QUEUE_LENGTH);

  TEST_numExpectedAssertsRemaining = 1;
  RIOSTACK_getInboundPacket(&stack, &rioPacket);
  TESTEXPR(TEST_numExpectedAssertsRemaining, 0);

  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);
  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 3:");
  PrintS("Action: Get symbols from transmitter.");
  PrintS("Result: Status-control-symbols should still be generated each 15 symbol ");
  PrintS("until a total of 15 status-control-symbols has been transmitted. Once these ");
  PrintS("has been transmitted, the transmitter will be link initialized.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC6-Step3");
  /*****************************************************************************/

  RIOSTACK_open(&stack, NULL,
      RIOSTACK_BUFFER_SIZE*QUEUE_LENGTH, rxPacketBuffer,
      RIOSTACK_BUFFER_SIZE*QUEUE_LENGTH, txPacketBuffer);
  RIOSTACK_portSetTimeout(&stack, 1);
  RIOSTACK_portSetTime(&stack, 0);
  RIOSTACK_portSetStatus(&stack, 1);

  RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, QUEUE_LENGTH, STYPE1_NOP, 0));

  for(j = 0; j < 17; j++)
  {
    for(i = 0; i < 15; i++)
    {
      TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));
    }
    TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createControlSymbol(STYPE0_STATUS, 0, QUEUE_LENGTH, STYPE1_NOP, 0));
  }
  TESTEXPR(stack.rxState, RX_STATE_PORT_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_PORT_INITIALIZED);

  for(i = 0; i < 8; i++)
  {
    RIOSTACK_portAddSymbol(&stack, createControlSymbol(STYPE0_STATUS, 0, QUEUE_LENGTH, STYPE1_NOP, 0));
  }
  TESTSYMBOL(RIOSTACK_portGetSymbol(&stack), createSymbol(RIOSTACK_SYMBOL_TYPE_IDLE));

  TESTEXPR(stack.rxState, RX_STATE_LINK_INITIALIZED);
  TESTEXPR(stack.txState, TX_STATE_LINK_INITIALIZED);

  TESTEXPR(RIOSTACK_getInboundQueueLength(&stack), 0);
  TESTEXPR(RIOSTACK_getOutboundQueueLength(&stack), 0);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
}

CU_ErrorCode addTests( void )
{
  if( CUE_SUCCESS != CU_initialize_registry() )
  {
    return CU_get_error();
  }

  CU_pSuite suite = CU_add_suite( "RIOSTACKTEST", NULL, NULL );
  if( NULL == suite )
  {
    CU_cleanup_registry();
    return CU_get_error();
  }

  CU_add_test( suite, "allTests", &allTests );

  return CUE_SUCCESS;
}


int main(int argc, char argv[])
{
  addTests();
  CU_basic_run_tests();
  return CU_get_error();
}


/*************************** end of file **************************************/
