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
 * This file contains an automatic test for the RIOPACKET module.
 ******************************************************************************/

#define MODULE_TEST
#include "riopacket.c"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "CUnit/CUnit.h"

#define PrintS(s) printf(s "\n")

#define TESTSTART(s) printf(s)
#define TESTEND printf(" done.\n");

#define TESTCOND(got) CU_ASSERT(got)
#define TESTEXPR(got, expected) CU_ASSERT_EQUAL(got, expected)
#define TESTPACKET(got, expected) testSymbol(__LINE__, #got, (got), (expected))

int TEST_numExpectedAssertsRemaining = 0;

void testPacket(uint32_t line, char *expression, RioPacket_t got, RioPacket_t expected)
{
  uint8_t i;
  (void)line;
  (void)expression;

  CU_ASSERT_EQUAL_FATAL(got.size, expected.size);

  for(i = 0; i < got.size; i++)
  {
    CU_ASSERT_EQUAL(got.payload[i], expected.payload[i]);
  }
}

void packetClear(RioPacket_t *packet)
{
  uint16_t i;

  for(i = 0; i < RIOPACKET_SIZE_MAX; i++)
  {
    packet->payload[i] = 0xdeadbeef;
  }
}

/*******************************************************************************
 * Module test for this file.
 *******************************************************************************/
void allTests(void)
{
  RioPacket_t packet;
  int i, j, k;
  uint16_t length;
  uint16_t dstidExpected, dstid;
  uint16_t srcidExpected, srcid;
  uint8_t tidExpected, tid;
  uint8_t hopExpected, hop;
  uint8_t mailboxExpected, mailbox;
  uint16_t infoExpected, info;
  uint32_t addressExpected, address;
  uint32_t dataExpected, data;
  uint8_t status;
  uint8_t offset;
  uint32_t componentTag;
  uint32_t portErrorDetect;
  uint32_t implementationSpecific;
  uint8_t portId;
  uint32_t logicalTransportErrorDetect;

  uint16_t payloadSizeExpected, payloadSize;
  uint8_t payloadExpected[256], payload[256];

  uint8_t buffer[512];
  uint16_t bufferSize;

  srand(0);

  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("TG_riopacket-TC1");
  PrintS("Description: Test packet initialization, validation and appending.");
  PrintS("Requirement: XXXXX");
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 1:");
  PrintS("Action: ");
  PrintS("Result: ");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC1-Step1");
  /******************************************************************************/

  RIOPACKET_init(&packet);

  TESTEXPR(RIOPACKET_size(&packet), 0);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_append(&packet, 0x001a0001);

  TESTEXPR(RIOPACKET_size(&packet), 1);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_append(&packet, 0xffff0000);

  TESTEXPR(RIOPACKET_size(&packet), 2);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_append(&packet, 0xdeaf9903);

  TESTEXPR(RIOPACKET_size(&packet), 3);
  TESTCOND(RIOPACKET_valid(&packet));

  /* Check that altering the ackid does not affect the validity of the packet. */
  packet.payload[0] |= 0xfc000000;
  TESTCOND(RIOPACKET_valid(&packet));

  /* Access the packet and check its content. */
  TESTEXPR(RIOPACKET_getFtype(&packet), RIOPACKET_FTYPE_DOORBELL);
  TESTEXPR(RIOPACKET_getDestination(&packet), 0x0001);
  TESTEXPR(RIOPACKET_getSource(&packet), 0xffff);
  TESTEXPR(RIOPACKET_getTid(&packet), 0x00);
  RIOPACKET_getDoorbell(&packet, &dstid, &srcid, &tid, &info);
  TESTEXPR(dstid, 0x0001);
  TESTEXPR(srcid, 0xffff);
  TESTEXPR(tid, 0x00);
  TESTEXPR(info, 0xdeaf);

  bufferSize = RIOPACKET_serialize(&packet, sizeof(buffer), buffer);
  TESTEXPR(bufferSize, 13);
  TESTEXPR(buffer[0], 0x03);
  TESTEXPR(buffer[1], 0xfc);
  TESTEXPR(buffer[2], 0x1a);
  TESTEXPR(buffer[3], 0x00);
  TESTEXPR(buffer[4], 0x01);
  TESTEXPR(buffer[5], 0xff);
  TESTEXPR(buffer[6], 0xff);
  TESTEXPR(buffer[7], 0x00);
  TESTEXPR(buffer[8], 0x00);
  TESTEXPR(buffer[9], 0xde);
  TESTEXPR(buffer[10], 0xaf);
  TESTEXPR(buffer[11], 0x99);
  TESTEXPR(buffer[12], 0x03);

  RIOPACKET_init(&packet);
  RIOPACKET_deserialize(&packet, bufferSize, buffer);
  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(RIOPACKET_getFtype(&packet), RIOPACKET_FTYPE_DOORBELL);
  TESTEXPR(RIOPACKET_getDestination(&packet), 0x0001);
  TESTEXPR(RIOPACKET_getSource(&packet), 0xffff);
  TESTEXPR(RIOPACKET_getTid(&packet), 0x00);
  RIOPACKET_getDoorbell(&packet, &dstid, &srcid, &tid, &info);
  TESTEXPR(dstid, 0x0001);
  TESTEXPR(srcid, 0xffff);
  TESTEXPR(tid, 0x00);
  TESTEXPR(info, 0xdeaf);

  /* Check that serializing into too small buffer fails */
  TESTEXPR(RIOPACKET_serialize(&packet, 3*4, buffer), 0);

  /* Check that deserializing too small packet fails */
  buffer[0] = RIOPACKET_SIZE_MIN - 1;
  (void)RIOPACKET_deserialize(&packet, sizeof(buffer), buffer);
  TESTCOND(!RIOPACKET_valid(&packet));

  /* Check that deserializing too large packet fails */
  buffer[0] = RIOPACKET_SIZE_MAX + 1;
  (void)RIOPACKET_deserialize(&packet, sizeof(buffer), buffer);
  TESTCOND(!RIOPACKET_valid(&packet));

  /* Check that deserializing from too small buffer fails */
  buffer[0] = RIOPACKET_SIZE_MAX;
  (void)RIOPACKET_deserialize(&packet, RIOPACKET_SIZE_MAX*4, buffer);
  TESTCOND(!RIOPACKET_valid(&packet));

  /* Check that extra appended payload is discarded */
  TEST_numExpectedAssertsRemaining = 1;
  RIOPACKET_init(&packet);
  for(length = 0; length < RIOPACKET_SIZE_MAX + 1; ++length)
  {
    RIOPACKET_append(&packet, length);
  }
  packet.size = setPacketPayload(packet.payload, 8, 0, RIOPACKET_SIZE_MAX * 4u - 12, (uint8_t*)&packet.payload[8]);
  TESTEXPR(RIOPACKET_size(&packet), RIOPACKET_SIZE_MAX);
  TESTCOND(RIOPACKET_valid(&packet));

  /* Check that extra payload invalidates the packet */
  ++packet.size;
  TESTCOND(!RIOPACKET_valid(&packet));
  --packet.size;
  TESTCOND(RIOPACKET_valid(&packet));

  /* Check that modifying the packet invalidates the CRC */
  --packet.payload[5u];
  TESTCOND(!RIOPACKET_valid(&packet));

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 2:");
  PrintS("Action: Check different address/size combinations");
  PrintS("Result: ");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC1-Step2");
  /******************************************************************************/

  {
    const uint32_t alignedAddresses[] = { 0, 8, 16, 0xfffffff0 };

    for( i = 0; i < (int)(sizeof(alignedAddresses) / sizeof(*alignedAddresses)); ++i )
    {
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 1), 1);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 2), 2);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 3), 3);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 4), 4);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 5), 5);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 6), 6);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 7), 7);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 8), 8);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 15), 8);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 16), 16);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 31), 16);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 32), 32);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 63), 32);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 64), 64);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 95), 64);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 96), 96);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 127), 96);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 128), 128);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 159), 128);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 160), 160);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 191), 160);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 192), 192);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 223), 192);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 224), 224);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 255), 224);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 256), 256);
      TESTEXPR( RIOPACKET_getReadPacketSize(alignedAddresses[i], 257), 256);
    }
    TESTEXPR( RIOPACKET_getReadPacketSize(1, 0), 0);
    TESTEXPR( RIOPACKET_getReadPacketSize(1, 1), 1);
    TESTEXPR( RIOPACKET_getReadPacketSize(1, 6), 1);
    TESTEXPR( RIOPACKET_getReadPacketSize(1, 7), 7);
    TESTEXPR( RIOPACKET_getReadPacketSize(1, 8), 7);
    TESTEXPR( RIOPACKET_getReadPacketSize(1, 0xffffffff), 7);
    TESTEXPR( RIOPACKET_getReadPacketSize(2, 0), 0);
    TESTEXPR( RIOPACKET_getReadPacketSize(2, 1), 1);
    TESTEXPR( RIOPACKET_getReadPacketSize(2, 2), 2);
    TESTEXPR( RIOPACKET_getReadPacketSize(2, 5), 2);
    TESTEXPR( RIOPACKET_getReadPacketSize(2, 6), 6);
    TESTEXPR( RIOPACKET_getReadPacketSize(2, 7), 6);
    TESTEXPR( RIOPACKET_getReadPacketSize(2, 0xffffffff), 6);
    TESTEXPR( RIOPACKET_getReadPacketSize(3, 0), 0);
    TESTEXPR( RIOPACKET_getReadPacketSize(3, 1), 1);
    TESTEXPR( RIOPACKET_getReadPacketSize(3, 4), 1);
    TESTEXPR( RIOPACKET_getReadPacketSize(3, 5), 5);
    TESTEXPR( RIOPACKET_getReadPacketSize(3, 6), 5);
    TESTEXPR( RIOPACKET_getReadPacketSize(3, 0xffffffff), 5);
    TESTEXPR( RIOPACKET_getReadPacketSize(4, 0), 0);
    TESTEXPR( RIOPACKET_getReadPacketSize(4, 1), 1);
    TESTEXPR( RIOPACKET_getReadPacketSize(4, 2), 2);
    TESTEXPR( RIOPACKET_getReadPacketSize(4, 3), 2);
    TESTEXPR( RIOPACKET_getReadPacketSize(4, 4), 4);
    TESTEXPR( RIOPACKET_getReadPacketSize(4, 5), 4);
    TESTEXPR( RIOPACKET_getReadPacketSize(4, 0xffffffff), 4);
    TESTEXPR( RIOPACKET_getReadPacketSize(5, 0), 0);
    TESTEXPR( RIOPACKET_getReadPacketSize(5, 1), 1);
    TESTEXPR( RIOPACKET_getReadPacketSize(5, 2), 1);
    TESTEXPR( RIOPACKET_getReadPacketSize(5, 3), 3);
    TESTEXPR( RIOPACKET_getReadPacketSize(5, 4), 3);
    TESTEXPR( RIOPACKET_getReadPacketSize(5, 0xffffffff), 3);
    TESTEXPR( RIOPACKET_getReadPacketSize(6, 0), 0);
    TESTEXPR( RIOPACKET_getReadPacketSize(6, 1), 1);
    TESTEXPR( RIOPACKET_getReadPacketSize(6, 2), 2);
    TESTEXPR( RIOPACKET_getReadPacketSize(6, 3), 2);
    TESTEXPR( RIOPACKET_getReadPacketSize(6, 0xffffffff), 2);
    TESTEXPR( RIOPACKET_getReadPacketSize(7, 0), 0);
    TESTEXPR( RIOPACKET_getReadPacketSize(7, 1), 1);
    TESTEXPR( RIOPACKET_getReadPacketSize(7, 2), 1);
    TESTEXPR( RIOPACKET_getReadPacketSize(7, 0xffffffff), 1);
    TESTEXPR( RIOPACKET_getReadPacketSize(0xffffffff, 0), 0);
    TESTEXPR( RIOPACKET_getReadPacketSize(0xffffffff, 1), 1);
    TESTEXPR( RIOPACKET_getReadPacketSize(0xffffffff, 2), 1);
    TESTEXPR( RIOPACKET_getReadPacketSize(0xffffffff, 0xffffffff), 1);

    for( i = 0; i < (int)(sizeof(alignedAddresses) / sizeof(*alignedAddresses)); ++i )
    {
      TESTEXPR( RIOPACKET_getWritePacketSize(alignedAddresses[i], 0), 0);
      TESTEXPR( RIOPACKET_getWritePacketSize(alignedAddresses[i], 1), 1);
      TESTEXPR( RIOPACKET_getWritePacketSize(alignedAddresses[i], 7), 7);
      TESTEXPR( RIOPACKET_getWritePacketSize(alignedAddresses[i], 8), 8);
      TESTEXPR( RIOPACKET_getWritePacketSize(alignedAddresses[i], 9), 8);
      TESTEXPR( RIOPACKET_getWritePacketSize(alignedAddresses[i], 15), 8);
      TESTEXPR( RIOPACKET_getWritePacketSize(alignedAddresses[i], 16), 16);
      TESTEXPR( RIOPACKET_getWritePacketSize(alignedAddresses[i], 17), 16);
      TESTEXPR( RIOPACKET_getWritePacketSize(alignedAddresses[i], 255), 248);
      TESTEXPR( RIOPACKET_getWritePacketSize(alignedAddresses[i], 256), 256);
      TESTEXPR( RIOPACKET_getWritePacketSize(alignedAddresses[i], 257), 256);
      TESTEXPR( RIOPACKET_getWritePacketSize(alignedAddresses[i], 264), 256);
      TESTEXPR( RIOPACKET_getWritePacketSize(alignedAddresses[i], 0xffffffff), 256);
    }

    TESTEXPR( RIOPACKET_getWritePacketSize(1, 0), 0);
    TESTEXPR( RIOPACKET_getWritePacketSize(1, 1), 1);
    TESTEXPR( RIOPACKET_getWritePacketSize(1, 6), 1);
    TESTEXPR( RIOPACKET_getWritePacketSize(1, 7), 7);
    TESTEXPR( RIOPACKET_getWritePacketSize(1, 8), 7);
    TESTEXPR( RIOPACKET_getWritePacketSize(1, 0xffffffff), 7);
    TESTEXPR( RIOPACKET_getWritePacketSize(2, 0), 0);
    TESTEXPR( RIOPACKET_getWritePacketSize(2, 1), 1);
    TESTEXPR( RIOPACKET_getWritePacketSize(2, 2), 2);
    TESTEXPR( RIOPACKET_getWritePacketSize(2, 3), 2);
    TESTEXPR( RIOPACKET_getWritePacketSize(2, 5), 2);
    TESTEXPR( RIOPACKET_getWritePacketSize(2, 6), 6);
    TESTEXPR( RIOPACKET_getWritePacketSize(2, 7), 6);
    TESTEXPR( RIOPACKET_getWritePacketSize(2, 0xffffffff), 6);
    TESTEXPR( RIOPACKET_getWritePacketSize(3, 0), 0);
    TESTEXPR( RIOPACKET_getWritePacketSize(3, 1), 1);
    TESTEXPR( RIOPACKET_getWritePacketSize(3, 2), 1);
    TESTEXPR( RIOPACKET_getWritePacketSize(3, 4), 1);
    TESTEXPR( RIOPACKET_getWritePacketSize(3, 5), 5);
    TESTEXPR( RIOPACKET_getWritePacketSize(3, 6), 5);
    TESTEXPR( RIOPACKET_getWritePacketSize(3, 0xffffffff), 5);
    TESTEXPR( RIOPACKET_getWritePacketSize(4, 0), 0);
    TESTEXPR( RIOPACKET_getWritePacketSize(4, 1), 1);
    TESTEXPR( RIOPACKET_getWritePacketSize(4, 2), 2);
    TESTEXPR( RIOPACKET_getWritePacketSize(4, 3), 2);
    TESTEXPR( RIOPACKET_getWritePacketSize(4, 4), 4);
    TESTEXPR( RIOPACKET_getWritePacketSize(4, 5), 4);
    TESTEXPR( RIOPACKET_getWritePacketSize(4, 0xffffffff), 4);
    TESTEXPR( RIOPACKET_getWritePacketSize(5, 0), 0);
    TESTEXPR( RIOPACKET_getWritePacketSize(5, 1), 1);
    TESTEXPR( RIOPACKET_getWritePacketSize(5, 2), 1);
    TESTEXPR( RIOPACKET_getWritePacketSize(5, 3), 3);
    TESTEXPR( RIOPACKET_getWritePacketSize(5, 4), 3);
    TESTEXPR( RIOPACKET_getWritePacketSize(5, 0xffffffff), 3);
    TESTEXPR( RIOPACKET_getWritePacketSize(6, 0), 0);
    TESTEXPR( RIOPACKET_getWritePacketSize(6, 1), 1);
    TESTEXPR( RIOPACKET_getWritePacketSize(6, 2), 2);
    TESTEXPR( RIOPACKET_getWritePacketSize(6, 3), 2);
    TESTEXPR( RIOPACKET_getWritePacketSize(6, 0xffffffff), 2);
    TESTEXPR( RIOPACKET_getWritePacketSize(7, 0), 0);
    TESTEXPR( RIOPACKET_getWritePacketSize(7, 1), 1);
    TESTEXPR( RIOPACKET_getWritePacketSize(7, 2), 1);
    TESTEXPR( RIOPACKET_getWritePacketSize(7, 0xffffffff), 1);
    TESTEXPR( RIOPACKET_getWritePacketSize(0xffffffff, 0), 0);
    TESTEXPR( RIOPACKET_getWritePacketSize(0xffffffff, 1), 1);
    TESTEXPR( RIOPACKET_getWritePacketSize(0xffffffff, 2), 1);
    TESTEXPR( RIOPACKET_getWritePacketSize(0xffffffff, 0xffffffff), 1);
  }

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("TG_riopacket-TC2");
  PrintS("Description: Test maintenance packets.");
  PrintS("Requirement: XXXXX");
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 1:");
  PrintS("Action: ");
  PrintS("Result: ");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC2-Step1");
  /******************************************************************************/

  RIOPACKET_setMaintReadRequest(&packet, 0xc0de, 0xbabe, 0x13, 0x41, 0xffffffff);
  TESTEXPR(RIOPACKET_getTransaction(&packet), RIOPACKET_TRANSACTION_MAINT_READ_REQUEST);
  RIOPACKET_getMaintReadRequest(&packet, &dstid, &srcid, &hop, &tid, &address);

  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(dstid, 0xc0de);
  TESTEXPR(srcid, 0xbabe);
  TESTEXPR(hop, 0x13);
  TESTEXPR(tid, 0x41);
  TESTEXPR(address, 0x00fffffc);

  RIOPACKET_setMaintReadResponse(&packet, 0x1234, 0x2345, 0x34, 0xde, 0x4567);
  RIOPACKET_getMaintReadResponse(&packet, &dstid, &srcid, &tid, &status, &data);

  TESTEXPR(RIOPACKET_getTransaction(&packet), RIOPACKET_TRANSACTION_MAINT_READ_RESPONSE);
  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(dstid, 0x1234);
  TESTEXPR(srcid, 0x2345);
  TESTEXPR(tid, 0x34);
  TESTEXPR(status, 0x0e);
  TESTEXPR(data, 0x4567);

  RIOPACKET_setMaintWriteRequest(&packet, 0xc0de, 0xbabe, 0x13, 0x41, 0xffffffff, 0x12345678);
  RIOPACKET_getMaintWriteRequest(&packet, &dstid, &srcid, &hop, &tid, &address, &data);

  TESTEXPR(RIOPACKET_getTransaction(&packet), RIOPACKET_TRANSACTION_MAINT_WRITE_REQUEST);
  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(dstid, 0xc0de);
  TESTEXPR(srcid, 0xbabe);
  TESTEXPR(hop, 0x13);
  TESTEXPR(tid, 0x41);
  TESTEXPR(address, 0x00fffffc);
  TESTEXPR(data, 0x12345678);

  RIOPACKET_setMaintWriteResponse(&packet, 0x1234, 0x2345, 0x34, 0xad);
  RIOPACKET_getMaintWriteResponse(&packet, &dstid, &srcid, &tid, &status);

  TESTEXPR(RIOPACKET_getTransaction(&packet), RIOPACKET_TRANSACTION_MAINT_WRITE_RESPONSE);
  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(dstid, 0x1234);
  TESTEXPR(srcid, 0x2345);
  TESTEXPR(tid, 0x34);
  TESTEXPR(status, 0x0d);

  RIOPACKET_setMaintPortWrite(&packet, 0x1234, 0x2345, 0x34567890, 0x45678901, 0x56789012, 0x67, 0x78901234);
  RIOPACKET_getMaintPortWrite(&packet, &dstid, &srcid, &componentTag, &portErrorDetect, &implementationSpecific, &portId, &logicalTransportErrorDetect);

  TESTEXPR(RIOPACKET_getTransaction(&packet), RIOPACKET_TRANSACTION_MAINT_PORT_WRITE_REQUEST);
  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(dstid, 0x1234);
  TESTEXPR(srcid, 0x2345);
  TESTEXPR(componentTag, 0x34567890);
  TESTEXPR(portErrorDetect, 0x45678901);
  TESTEXPR(implementationSpecific, 0x789012);
  TESTEXPR(portId, 0x67);
  TESTEXPR(logicalTransportErrorDetect, 0x78901234);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("TG_riopacket-TC3");
  PrintS("Description: Test input/output packets.");
  PrintS("Requirement: XXXXX");
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 1:");
  PrintS("Action: ");
  PrintS("Result: ");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC3-Step1");
  /******************************************************************************/

  for(payloadSizeExpected = 0; payloadSizeExpected < 256; ++payloadSizeExpected )
  {
    payloadExpected[payloadSizeExpected] = payloadSizeExpected;
  }

  /* Nwrite */

  memset(payload, 0, sizeof(payload));
  RIOPACKET_setNwrite(&packet, 0x1234, 0x2345, 0x34567890, payloadSizeExpected, payloadExpected);
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);

  TESTEXPR(RIOPACKET_getTransaction(&packet), RIOPACKET_TRANSACTION_WRITE_NWRITE);
  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(dstid, 0x1234);
  TESTEXPR(srcid, 0x2345);
  TESTEXPR(address, 0x34567890);
  TESTEXPR(payloadSize, payloadSizeExpected);
  TESTEXPR(memcmp(payload, payloadExpected, payloadSizeExpected), 0);

  /* Check that a small size is adjusted */
  RIOPACKET_setNwrite(&packet, 0x1234, 0x2345, 0x34567890, 15, payloadExpected);
  TESTEXPR(RIOPACKET_size(&packet), 8);
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(payloadSize, 8);

  /* NwriteR */

  memset(payload, 0, sizeof(payload));
  RIOPACKET_setNwriteR(&packet, 0x1234, 0x2345, 0x45, 0x34567890, payloadSizeExpected, payloadExpected);
  RIOPACKET_getNwriteR(&packet, &dstid, &srcid, &tid, &address, &payloadSize, payload);

  TESTEXPR(RIOPACKET_getTransaction(&packet), RIOPACKET_TRANSACTION_WRITE_NWRITER);
  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(dstid, 0x1234);
  TESTEXPR(srcid, 0x2345);
  TESTEXPR(tid, 0x45);
  TESTEXPR(address, 0x34567890);
  TESTEXPR(payloadSize, payloadSizeExpected);
  TESTEXPR(memcmp(payload, payloadExpected, payloadSizeExpected), 0);

  /* Check that a small size is adjusted */
  RIOPACKET_setNwriteR(&packet, 0x1234, 0x2345, 0x45, 0x34567890, 15, payloadExpected);
  TESTEXPR(RIOPACKET_size(&packet), 8);
  RIOPACKET_getNwriteR(&packet, &dstid, &srcid, &tid, &address, &payloadSize, payload);
  TESTEXPR(payloadSize, 8);

  /* Nread */

  RIOPACKET_setNread(&packet, 0x1234, 0x2345, 0x45, 0x34567890, payloadSizeExpected);
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);

  TESTEXPR(RIOPACKET_getTransaction(&packet), RIOPACKET_TRANSACTION_REQUEST_NREAD);
  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(dstid, 0x1234);
  TESTEXPR(srcid, 0x2345);
  TESTEXPR(tid, 0x45);
  TESTEXPR(address, 0x34567890);
  TESTEXPR(payloadSize, payloadSizeExpected);

  /* Response without payload */

  RIOPACKET_setResponseNoPayload(&packet, 0x1234, 0x2345, 0x34, RIOPACKET_RESPONSE_STATUS_ERROR);
  RIOPACKET_getResponseNoPayload(&packet, &dstid, &srcid, &tid, &status);

  TESTEXPR(RIOPACKET_getTransaction(&packet), RIOPACKET_TRANSACTION_RESPONSE_NO_PAYLOAD);
  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(dstid, 0x1234);
  TESTEXPR(srcid, 0x2345);
  TESTEXPR(tid, 0x34);
  TESTEXPR(status, RIOPACKET_RESPONSE_STATUS_ERROR);

  /* Response with payload */

  memset(payload, 0, sizeof(payload));
  RIOPACKET_setResponseWithPayload(&packet, 0x1234, 0x2345, 0x34, 0, payloadSizeExpected, payloadExpected);
  payloadSize = RIOPACKET_getResponseWithPayload(&packet, &dstid, &srcid, &tid, 0, payloadSizeExpected, payload);

  TESTEXPR(RIOPACKET_getTransaction(&packet), RIOPACKET_TRANSACTION_RESPONSE_WITH_PAYLOAD);
  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(dstid, 0x1234);
  TESTEXPR(srcid, 0x2345);
  TESTEXPR(tid, 0x34);
  TESTEXPR(payloadSize, payloadSizeExpected);
  TESTEXPR(memcmp(payload, payloadExpected, payloadSizeExpected), 0);

  /* ...and offset */

  memset(payload, 0, sizeof(payload));
  RIOPACKET_setResponseWithPayload(&packet, 0x1234, 0x2345, 0x34, 13, payloadSizeExpected-13, payloadExpected);
  payloadSize = RIOPACKET_getResponseWithPayload(&packet, &dstid, &srcid, &tid, 13, payloadSizeExpected-13, payload);

  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(payloadSize, payloadSizeExpected-13);
  TESTEXPR(memcmp(payload, payloadExpected, payloadSizeExpected-13), 0);

  /* Response with small payload */

  memset(payload, 0x55, sizeof(payload));
  RIOPACKET_setResponseWithPayload(&packet, 0x1234, 0x2345, 0x34, 0, 1, payloadExpected);
  payloadSize = RIOPACKET_getResponseWithPayload(&packet, &dstid, &srcid, &tid, 0, 1, payload);

  TESTEXPR(RIOPACKET_getTransaction(&packet), RIOPACKET_TRANSACTION_RESPONSE_WITH_PAYLOAD);
  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(dstid, 0x1234);
  TESTEXPR(srcid, 0x2345);
  TESTEXPR(tid, 0x34);
  TESTEXPR(payloadSize, 1);
  TESTEXPR(payload[0], payloadExpected[0]);
  for(i = 1; i < sizeof(payload); i++)
  {
    TESTEXPR(payload[i], 0x55);
  }
  
  /* Response with unknown sized payload */

  memset(payload, 0x55, sizeof(payload));
  RIOPACKET_setResponseWithPayload(&packet, 0x1234, 0x2345, 0x34, 0, 1, payloadExpected);
  payloadSize = RIOPACKET_getResponseWithPayload(&packet, &dstid, &srcid, &tid, 0, 0, payload);

  TESTEXPR(RIOPACKET_getTransaction(&packet), RIOPACKET_TRANSACTION_RESPONSE_WITH_PAYLOAD);
  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(dstid, 0x1234);
  TESTEXPR(srcid, 0x2345);
  TESTEXPR(tid, 0x34);
  TESTEXPR(payloadSize, 8);
  TESTEXPR(payload[0], payloadExpected[0]);
  for(i = 1; i < 8; i++)
  {
    TESTEXPR(payload[i], 0x00);
  }
  for(i = 8; i < sizeof(payload); i++)
  {
    TESTEXPR(payload[i], 0x55);
  }
  
  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 2:");
  PrintS("Action: Address/payloadSize combinations");
  PrintS("Result: According to table 4-3 (rdsize) and 4-4 (wrsize)");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC3-Step2");
  /******************************************************************************/

  /* Nwrite */

  RIOPACKET_setNwrite(&packet, 1, 2, 0, 0, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 0, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 1, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 1, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 1);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 1, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 2);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 1, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 3);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 1, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 4);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 1, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 5);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 1, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 6);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 1, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 7);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 1, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffff8, 1, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 1, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xffffffff);
  TESTEXPR(payloadSize, 1);


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 2, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 2);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 2, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 2, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 2);
  TESTEXPR(payloadSize, 2);

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 2, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 2, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 4);
  TESTEXPR(payloadSize, 2);

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 2, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 2, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 6);
  TESTEXPR(payloadSize, 2);

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 2, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 2, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 2);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffffe, 2, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffffe);
  TESTEXPR(payloadSize, 2);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 2, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 3, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 3);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 3, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 3, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 3, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 3, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 3, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 5);
  TESTEXPR(payloadSize, 3);

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 3, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 3, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 3, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 3);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffffd, 3, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffffd);
  TESTEXPR(payloadSize, 3);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 3, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 4, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 4);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 4, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 4, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 4, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 4, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 4);
  TESTEXPR(payloadSize, 4);

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 4, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 4, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 4, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 4, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 4);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffffc, 4, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffffc);
  TESTEXPR(payloadSize, 4);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 4, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 5, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 5);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 5, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 5, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 5, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 3);
  TESTEXPR(payloadSize, 5);

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 5, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 5, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 5, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 5, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 5, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 5);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffffb, 5, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffffb);
  TESTEXPR(payloadSize, 5);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 5, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 6, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 6);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 6, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 6, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 2);
  TESTEXPR(payloadSize, 6);

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 6, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 6, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 6, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 6, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 6, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 6, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 6);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffffa, 6, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffffa);
  TESTEXPR(payloadSize, 6);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 6, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 7, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 7);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 7, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 1);
  TESTEXPR(payloadSize, 7);

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 7, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 7, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 7, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 7, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 7, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 7, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 7, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 7);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffff9, 7, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffff9);
  TESTEXPR(payloadSize, 7);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 7, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 8, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 8);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 8, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 8, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 8, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 8, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 8, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 8, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 8, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 8, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 8);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffff8, 8, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 8);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 8, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 15, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(((packet.payload[1] >> 8) & 0x0F), 11);
  TESTEXPR(((packet.payload[2] >> 2) & 0xF), 0);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 15, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 15, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 15, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 15, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 15, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 15, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 15, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 15, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(((packet.payload[1] >> 8) & 0x0F), 11);
  TESTEXPR(((packet.payload[2] >> 2) & 0xF), 2);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffff8, 15, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 8);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 15, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 16, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 16);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 16, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 16, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 16, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 16, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 16, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 16, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 16, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 16, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 16);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffff8, 16, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 16);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 16, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 23, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 16);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 23, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 23, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 23, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 23, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 23, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 23, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 23, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 23, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 16);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffff8, 23, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 16);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 23, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 24, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 24);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 24, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 24, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 24, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 24, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 24, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 24, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 24, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 24, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 24);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffff8, 24, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 24);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 24, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 39, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 40);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 39, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 39, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 39, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 39, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 39, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 39, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 39, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 39, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 40);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffff8, 39, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 40);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 39, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 40, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 40);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 40, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 40, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 40, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 40, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 40, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 40, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 40, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 40, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 40);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffff8, 40, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 40);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 40, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 71, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 72);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 71, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 71, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 71, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 71, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 71, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 71, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 71, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 71, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 72);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffff8, 71, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 72);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 71, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 72, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 72);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 72, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 72, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 72, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 72, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 72, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 72, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 72, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 72, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 72);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffff8, 72, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 72);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 72, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 135, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 136);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 135, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 135, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 135, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 135, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 135, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 135, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 135, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 135, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 136);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffff8, 135, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 136);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 135, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 136, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 136);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 136, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 136, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 136, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 136, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 136, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 136, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 136, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 136, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 136);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffff8, 136, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 136);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 136, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 255, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 256);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 255, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 255, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 255, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 255, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 255, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 255, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 255, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 255, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 256);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffff8, 255, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 256);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 255, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNwrite(&packet, 1, 2, 0, 256, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 256);

  RIOPACKET_setNwrite(&packet, 1, 2, 1, 256, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 2, 256, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 3, 256, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 4, 256, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 5, 256, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 6, 256, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 7, 256, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNwrite(&packet, 1, 2, 8, 256, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 256);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xfffffff8, 256, payloadExpected);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNwrite(&packet, &dstid, &srcid, &address, &payloadSize, payload);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 256);

  RIOPACKET_setNwrite(&packet, 1, 2, 0xffffffff, 256, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  TEST_numExpectedAssertsRemaining = 1;
  RIOPACKET_setNwrite(&packet, 1, 2, 0, 263, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  TEST_numExpectedAssertsRemaining = 1;
  RIOPACKET_setNwrite(&packet, 1, 2, 8, 263, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  TEST_numExpectedAssertsRemaining = 1;
  RIOPACKET_setNwrite(&packet, 1, 2, 0, 264, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  TEST_numExpectedAssertsRemaining = 1;
  RIOPACKET_setNwrite(&packet, 1, 2, 8, 264, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  TEST_numExpectedAssertsRemaining = 1;
  RIOPACKET_setNwrite(&packet, 1, 2, 0, 0xffff, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));

  TEST_numExpectedAssertsRemaining = 1;
  RIOPACKET_setNwrite(&packet, 1, 2, 8, 0xffff, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  /* NwriteR */

  RIOPACKET_setNwriteR(&packet, 0x1234, 0x2345, 0x45, 0x34567891, payloadSizeExpected, payloadExpected);
  TESTCOND(!RIOPACKET_valid(&packet));


  /* Nread */

  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 0);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 0);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 1);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 1);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 1);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 1);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 2);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 1);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 3);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 1);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 4);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 1);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 5);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 1);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 6);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 1);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 7);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 1);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 1);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 1);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 1);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xffffffff);
  TESTEXPR(payloadSize, 1);


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 2);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 2);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 2);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 2);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 2);
  TESTEXPR(payloadSize, 2);

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 2);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 2);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 4);
  TESTEXPR(payloadSize, 2);

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 2);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 2);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 6);
  TESTEXPR(payloadSize, 2);

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 2);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 2);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 2);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffffe, 2);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffffe);
  TESTEXPR(payloadSize, 2);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 2);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 3);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 3);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 3);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 3);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 3);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 3);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 3);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 5);
  TESTEXPR(payloadSize, 3);

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 3);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 3);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 3);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 3);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffffd, 3);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffffd);
  TESTEXPR(payloadSize, 3);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 3);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 4);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 4);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 4);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 4);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 4);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 4);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 4);
  TESTEXPR(payloadSize, 4);

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 4);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 4);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 4);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 4);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 4);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffffc, 4);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffffc);
  TESTEXPR(payloadSize, 4);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 4);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 5);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 5);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 5);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 5);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 5);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 3);
  TESTEXPR(payloadSize, 5);

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 5);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 5);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 5);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 5);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 5);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 5);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff3, 5);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff3);
  TESTEXPR(payloadSize, 5);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 5);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 6);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 6);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 6);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 6);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 2);
  TESTEXPR(payloadSize, 6);

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 6);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 6);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 6);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 6);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 6);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 6);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 6);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffffa, 6);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffffa);
  TESTEXPR(payloadSize, 6);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 6);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 7);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 7);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 7);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 1);
  TESTEXPR(payloadSize, 7);

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 7);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 7);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 7);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 7);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 7);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 7);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 7);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 7);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff9, 7);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff9);
  TESTEXPR(payloadSize, 7);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 7);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 8);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 8);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 8);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 8);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 8);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 8);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 8);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 8);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 8);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 8);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 8);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 8);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 8);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 8);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 15);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 8);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 15);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 15);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 15);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 15);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 15);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 15);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 15);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 15);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 8);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 15);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 8);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 15);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 16);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 16);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 16);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 16);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 16);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 16);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 16);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 16);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 16);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 16);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 16);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 16);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 16);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 16);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 23);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 16);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 23);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 23);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 23);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 23);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 23);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 23);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 23);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 23);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 16);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 23);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 16);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 23);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 24);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 32);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 32);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 32);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 32);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 32);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 32);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 32);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 32);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 32);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 32);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 32);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 32);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 32);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 32);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 39);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 32);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 39);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 39);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 39);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 39);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 39);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 39);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 39);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 39);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 32);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 39);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 32);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 39);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 40);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 48);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 56);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 64);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 64);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 64);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 64);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 64);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 64);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 64);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 64);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 64);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 64);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 64);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 64);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 64);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 64);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 71);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 64);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 71);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 71);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 71);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 71);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 71);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 71);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 71);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 71);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 64);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 71);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 64);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 71);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 72);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 80);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 88);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 96);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 96);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 96);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 96);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 96);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 96);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 96);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 96);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 96);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 96);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 96);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 96);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 96);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 96);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 103);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 96);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 103);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 103);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 103);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 103);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 103);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 103);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 103);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 103);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 96);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 103);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 96);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 103);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 104);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 112);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 120);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 128);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 128);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 128);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 128);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 128);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 128);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 128);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 128);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 128);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 128);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 128);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 128);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 128);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 128);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 135);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 128);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 135);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 135);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 135);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 135);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 135);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 135);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 135);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 135);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 128);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 135);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 128);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 135);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 136);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 144);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 152);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 160);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 160);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 160);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 160);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 160);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 160);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 160);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 160);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 160);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 160);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 160);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 160);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 160);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 160);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 167);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 160);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 167);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 167);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 167);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 167);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 167);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 167);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 167);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 167);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 160);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 167);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 160);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 167);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 168);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 176);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 184);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 192);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 192);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 192);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 192);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 192);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 192);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 192);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 192);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 192);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 192);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 192);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 192);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 192);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 192);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 199);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 192);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 199);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 199);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 199);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 199);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 199);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 199);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 199);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 199);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 192);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 199);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 192);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 199);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 200);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 208);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 216);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 224);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 224);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 224);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 224);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 224);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 224);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 224);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 224);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 224);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 224);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 224);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 224);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 224);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 224);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 231);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 224);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 231);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 231);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 231);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 231);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 231);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 231);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 231);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 231);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 224);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 231);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 224);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 231);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 232);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 240);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 248);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 256);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 256);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 256);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 256);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 256);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 256);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 256);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 256);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 256);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 256);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 256);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 256);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 256);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 256);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 0, 263);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0);
  TESTEXPR(payloadSize, 256);

  RIOPACKET_setNread(&packet, 1, 2, 3, 1, 263);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 2, 263);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 3, 263);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 4, 263);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 5, 263);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 6, 263);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 7, 263);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 263);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 8);
  TESTEXPR(payloadSize, 256);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xfffffff8, 263);
  TESTCOND(RIOPACKET_valid(&packet));
  RIOPACKET_getNread(&packet, &dstid, &srcid, &tid, &address, &payloadSize);
  TESTEXPR(address, 0xfffffff8);
  TESTEXPR(payloadSize, 256);

  RIOPACKET_setNread(&packet, 1, 2, 3, 0xffffffff, 263);
  TESTCOND(!RIOPACKET_valid(&packet));


  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 264);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 0xffff);
  TESTCOND(!RIOPACKET_valid(&packet));

  RIOPACKET_setNread(&packet, 1, 2, 3, 8, 0xfff8);
  TESTCOND(!RIOPACKET_valid(&packet));

  /* Run unreachable code for code coverage */

  rdsizeToOffset(16, 0, &offset, &payloadSize);

  offset = 1;
  payloadSize = 1;
  wrsizeToOffset(14, 0, &offset, &payloadSize);
  TESTEXPR(offset, 0);
  TESTEXPR(payloadSize, 0);

  wrsizeToOffset(16, 0, &offset, &payloadSize);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("TG_riopacket-TC4");
  PrintS("Description: Test message passing packets.");
  PrintS("Requirement: XXXXX");
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 1:");
  PrintS("Action:");
  PrintS("Result:");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC4-Step1");
  /******************************************************************************/

  /* Doorbell */

  RIOPACKET_setDoorbell(&packet, 0x1234, 0x2345, 0x45, 0x3456);
  RIOPACKET_getDoorbell(&packet, &dstid, &srcid, &tid, &info);

  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(dstid, 0x1234);
  TESTEXPR(srcid, 0x2345);
  TESTEXPR(tid, 0x45);
  TESTEXPR(info, 0x3456);

  /* Message response */

  RIOPACKET_setResponseMessage(&packet, 0x1234, 0x2345, 0x45, RIOPACKET_RESPONSE_STATUS_DONE);
  RIOPACKET_getResponseMessage(&packet, &dstid, &srcid, &mailbox, &status);

  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(dstid, 0x1234);
  TESTEXPR(srcid, 0x2345);
  TESTEXPR(mailbox, 0x45);
  TESTEXPR(status, RIOPACKET_RESPONSE_STATUS_DONE);

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 2:");
  PrintS("Action: Send a message with invalid payload length.");
  PrintS("Result: No packet should be generated.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC4-Step2");
  /******************************************************************************/

  /* Doorbell */

  RIOPACKET_setDoorbell(&packet, 0x1234, 0x2345, 0x45, 0x3456);
  RIOPACKET_getDoorbell(&packet, &dstid, &srcid, &tid, &info);

  TESTCOND(RIOPACKET_valid(&packet));
  TESTEXPR(dstid, 0x1234);
  TESTEXPR(srcid, 0x2345);
  TESTEXPR(tid, 0x45);
  TESTEXPR(info, 0x3456);

  /* Message */

  RIOPACKET_setMessage(&packet, 0xdead, 0xbeef, 0xc0, 0, payloadExpected);

  TESTCOND(!RIOPACKET_valid(&packet));

  TEST_numExpectedAssertsRemaining = 1;
  RIOPACKET_setMessage(&packet, 0xdead, 0xbeef, 0xc0, 257, payloadExpected);

  TESTCOND(!RIOPACKET_valid(&packet));

  /******************************************************************************/
  TESTEND;
  /******************************************************************************/
  PrintS("----------------------------------------------------------------------");
  PrintS("Step 3:");
  PrintS("Action: Test sending all possible payload sizes on random deviceIds ");
  PrintS("        and mailboxes.");
  PrintS("Result: The content of the packet should be equal to what was entered.");
  PrintS("----------------------------------------------------------------------");
  /******************************************************************************/
  TESTSTART("TG_riostack-TC4-Step3");
  /******************************************************************************/

  for(i = 1; i <= 256; i++)
  {
    dstidExpected = rand();
    srcidExpected = rand();
    mailboxExpected = rand();

    if((i%8) == 0)
    {
      payloadSizeExpected = 8*(i/8);
    }
    else
    {
      payloadSizeExpected = 8*(i/8+1);
    }

    for(j = 0; j < i; j++)
    {
      payloadExpected[j] = rand();
    }
    
    RIOPACKET_setMessage(&packet, dstidExpected, srcidExpected, mailboxExpected, 
                         i, &payloadExpected[0]);
    TESTCOND(RIOPACKET_valid(&packet));

    memset(payload, 0, sizeof(payload));
    RIOPACKET_getMessage(&packet, &dstid, &srcid, &mailbox, &payloadSize, &(payload[0]));
    TESTEXPR(dstid, dstidExpected);
    TESTEXPR(srcid, srcidExpected);
    TESTEXPR(mailbox, mailboxExpected);
    TESTEXPR(payloadSize, payloadSizeExpected);
    for(j = 0; j < i; j++)
    {
      TESTEXPR(payload[j], payloadExpected[j]);
    }
  }

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

  CU_pSuite suite = CU_add_suite( "RIOPACKETTEST", NULL, NULL );
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
