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
 * This file contains an "object" that can create and parse RapidIO packets. 
 * See riopacket.h for more info.
 *
 * Any application specific tailoring needed to compile properly should be done 
 * in rioconfig.h.
 ******************************************************************************/


/*******************************************************************************
 * Includes
 *******************************************************************************/

#include "riopacket.h"

/* Get definitions of ASSERT(), assumed to not return. */
#include "rioconfig.h"


/*******************************************************************************
 * Local macro definitions
 *******************************************************************************/

/* Macros to get entries from a packet in a buffer. */
/*lint -save */
/*lint -e961 Macros are needed due to performance reasons. They are also 
  trivial and clarifies the code. */
/*lint -e750 Allow unused macros for future usage. */
#define FTYPE_GET(p) (uint8_t) (((p)[0u] >> 16u) & 0xfu)
#define DESTID_GET(p) (uint16_t) ((p)[0u] & 0xffffu)
#define SRCID_GET(p) (uint16_t) (((p)[1u] >> 16u) & 0xffffu)
#define TRANSACTION_GET(p) (uint8_t) (((p)[1u] >> 12u) & 0xfu)
#define SSIZE_GET(p) (uint8_t) (((p)[1u] >> 8u) & 0xfu)
#define LETTER_GET(p) (uint8_t) (((p)[1u] >> 6u) & 0x3u)
#define MBOX_GET(p) (uint8_t) (((p)[1u] >> 4u) & 0x3u)
#define MSGSEG_GET(p) (uint8_t) ((p)[1u] & 0xfu)
#define XMBOX_GET(p) MSGSEG_GET(p)
#define RDSIZE_GET(p) SSIZE_GET(p)
#define WRSIZE_GET(p) SSIZE_GET(p)
#define STATUS_GET(p) SSIZE_GET(p)
#define TID_GET(p) (uint8_t) ((p)[1u] & 0xffu)
#define HOP_GET(p) (uint8_t) (((p)[2u] >> 24u) & 0xffu)
#define CONFIG_OFFSET_GET(p) (uint32_t) ((p)[2u] & 0x00fffffcul)
#define INFO_GET(p) (uint16_t) (((p)[2u] >> 16u) & 0xffffu)
#define ADDRESS_GET(p) (uint32_t) ((p)[2u] & 0xfffffff8ul)
#define WDPTR_GET(p) (uint8_t) (((p)[2u] >> 2u) & 0x1u)
#define DOUBLE_WORD_MSB_GET(p, i) (uint32_t) ((p)[3u+((2u*(i))+0u)])
#define DOUBLE_WORD_LSB_GET(p, i) (uint32_t) ((p)[3u+((2u*(i))+1u)])
/*lint -restore */

/* The maximum size of a packet that does not contain two CRCs. */
#define PACKET_SIZE_EMBEDDED_CRC ((uint8_t) 20u)

/*******************************************************************************
 * Local function prototypes
 *******************************************************************************/

/* Functions to help get and set payload in the packets. */
static uint16_t getPacketPayload(const uint32_t packet[], const uint16_t payloadOffset, 
                                 const uint16_t dataOffset, 
                                 const uint16_t dataSize, uint8_t data[]);
static uint8_t setPacketPayload(uint32_t packet[], const uint16_t payloadOffset, 
                                const uint16_t dataOffset, 
                                const uint16_t dataSize, const uint8_t data[]);

/* Functions to help in conversions between rdsize/wrsize and size/offset. */
static uint16_t rdsizeGet(const uint32_t address, const uint16_t size);
static uint16_t wrsizeGet(const uint32_t address, const uint16_t size);
static void rdsizeToOffset(uint8_t rdsize, uint8_t wdptr, 
                           uint8_t *offset, uint16_t *size);
static void wrsizeToOffset(uint8_t wrsize, uint8_t wdptr, 
                           uint8_t *offset, uint16_t *size);



/*******************************************************************************
 * Global function prototypes
 *******************************************************************************/

void RIOPACKET_init(RioPacket_t *packet)
{
  ASSERT(packet != NULL, "Invalid packet pointer");
  packet->size = 0u;
}


uint8_t RIOPACKET_size(const RioPacket_t *packet)
{
  ASSERT(packet != NULL, "Invalid packet pointer");
  return packet->size;
}


void RIOPACKET_append(RioPacket_t *packet, uint32_t word)
{
  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(packet->size < RIOPACKET_SIZE_MAX, "Appending makes packet too large.");

  packet->payload[packet->size] = word;
  packet->size++;
}


uint8_t RIOPACKET_valid(const RioPacket_t *packet)
{
  uint8_t returnValue;
  uint32_t i;
  uint16_t crc;


  ASSERT(packet != NULL, "Invalid packet pointer");

  /* Check that the size of the packet is ok. */
  if((packet->size >= RIOPACKET_SIZE_MIN) &&
     (packet->size <= RIOPACKET_SIZE_MAX))
  {
    /* The packet has a valid length. */

    /* Calculate CRC on the first word and disregard the ackId. */
    crc = RIOPACKET_crc32((uint32_t) (packet->payload[0] & 0x03fffffful), 0xffffu);

    /* Check if the packet contains an embedded crc. */
    if(packet->size < PACKET_SIZE_EMBEDDED_CRC)
    {
      /* The packet contains only one trailing crc. */
      for(i = 1u; i < packet->size; i++)
      {
        crc = RIOPACKET_crc32(packet->payload[i], crc);
      }
      returnValue = (uint8_t) (crc == 0x0000u);
    }
    else
    {
      /* The packet contains both a trailing and an embedded crc. */

      /* Read payload to the embedded crc. Include the embedded crc in 
         the crc calculation.*/
      for(i = 1u; i < PACKET_SIZE_EMBEDDED_CRC; i++)
      {
        crc = RIOPACKET_crc32(packet->payload[i], crc);
      }

      /* Check the embedded crc. */
      if(crc != ((uint16_t) (packet->payload[i] >> 16)))
      {
        /* The embedded crc is not ok. */
        returnValue = 0u;
      }
      else
      {
        /* Read the rest of the payload including the trailing crc. */
        for(i = PACKET_SIZE_EMBEDDED_CRC; i < packet->size; i++)
        {
          crc = RIOPACKET_crc32(packet->payload[i], crc);
        }
        returnValue = (uint8_t) (crc == 0x0000u);
      }
    }
  }
  else
  {    
    /* The packet does not have a valid length. */
    returnValue = 0u;
  }

  return returnValue;
}


uint16_t RIOPACKET_serialize(const RioPacket_t *packet, const uint16_t size, uint8_t buffer[])
{
  uint16_t returnValue;
  uint32_t i;


  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(buffer != NULL, "Invalid buffer pointer");

  /* Check if the packet fits into the provided buffer. */
  if(size >= ((4u*((uint16_t) packet->size))+1u))
  {
    /* The packet fits. */

    /* Write the size of the packet and the packet content itself to the buffer. */
    buffer[0u] = packet->size;
    for(i = 0u; i < packet->size; i++)
    {
      buffer[(4u*i)+1u] = (uint8_t) ((packet->payload[i] >> 24u) & 0xffu);
      buffer[(4u*i)+2u] = (uint8_t) ((packet->payload[i] >> 16u) & 0xffu);
      buffer[(4u*i)+3u] = (uint8_t) ((packet->payload[i] >> 8u) & 0xffu);
      buffer[(4u*i)+4u] = (uint8_t) (packet->payload[i] & 0xffu);
    }
    
    /* Write the number of bytes that were written. */
    returnValue = (uint16_t) ((4u*((uint16_t) packet->size))+1u);
  }
  else
  {
    /* The packet does not fit into the provided buffer. */
    returnValue = 0u;
  }

  return returnValue;
}


uint8_t RIOPACKET_deserialize(RioPacket_t *packet, const uint16_t size, const uint8_t buffer[])
{
  uint32_t i;
  uint32_t temp = 0u;


  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(buffer != NULL, "Invalid buffer pointer");

  /* Check if the buffer contains a valid packet length. */
  if(((buffer[0u] >= RIOPACKET_SIZE_MIN) &&
      (buffer[0u] <= RIOPACKET_SIZE_MAX)) &&
     (((4u*((uint16_t) buffer[0u]))+1u) <= size))
  {
    /* The buffer contains a valid packet length. */

    /* Read the size of the packet and the packet content itself from the buffer. */
    packet->size = buffer[0u];
    for(i = 0u; i < ((uint32_t) (4u*((uint32_t) packet->size))); i++)
    {
      temp <<= 8u;
      temp |= (uint32_t) buffer[i+1u];
      if((i%4u) == 3u)
      {
        packet->payload[i/4u] = temp;
      }
    }
  }
  else
  {
    /* The buffer does not contain a valid packet length. */
    packet->size = 0u;
  }

  return packet->size;
}


uint8_t RIOPACKET_getFtype(const RioPacket_t *packet)
{
  ASSERT(packet != NULL, "Invalid packet pointer");

  return (uint8_t) FTYPE_GET(packet->payload);
}


uint16_t RIOPACKET_getDestination(const RioPacket_t *packet)
{
  ASSERT(packet != NULL, "Invalid packet pointer");

  return (uint16_t) DESTID_GET(packet->payload);
}


uint16_t RIOPACKET_getSource(const RioPacket_t *packet)
{
  ASSERT(packet != NULL, "Invalid packet pointer");

  return (uint16_t) SRCID_GET(packet->payload);
}


uint8_t RIOPACKET_getTransaction(const RioPacket_t *packet)
{
  ASSERT(packet != NULL, "Invalid packet pointer");

  return (uint8_t) TRANSACTION_GET(packet->payload);
}


uint8_t RIOPACKET_getTid(const RioPacket_t *packet)
{
  ASSERT(packet != NULL, "Invalid packet pointer");

  return (uint8_t) TID_GET(packet->payload);
}


/*******************************************************************************************
 * Logical I/O MAINTENANCE-READ functions.
 *******************************************************************************************/

void RIOPACKET_setMaintReadRequest(RioPacket_t *packet,
                                   uint16_t dstId, uint16_t srcId, uint8_t hop,
                                   uint8_t tid, uint32_t offset)
{
  uint32_t content;
  uint16_t crc = 0xffffu;


  ASSERT(packet != NULL, "Invalid packet pointer");

  /* ackId(4:0)|0|vc|crf|prio(1:0)|tt(1:0)|ftype(3:0)|destinationId(15:0) */
  /* ackId is set when the packet is transmitted but must be set to zero. */
  content = ((uint32_t) RIOPACKET_TT_16BITS) << 20;
  content |= ((uint32_t) RIOPACKET_FTYPE_MAINTENANCE) << 16;
  content |= (uint32_t) dstId;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[0] = content;

  /* sourceId(15:0)|transaction(3:0)|rdsize(3:0)|srcTID(7:0) */
  content = ((uint32_t) srcId) << 16;
  content |= (uint32_t) RIOPACKET_TRANSACTION_MAINT_READ_REQUEST << 12; /*lint !e835 !e845 The constant is supposed to be zero. */
  content |= (uint32_t) 8ul << 8;
  content |= (uint32_t) tid;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[1] = content;

  /* hopcount(7:0)|configOffset(20:0)|wdptr|reserved(1:0) */
  content = ((uint32_t) hop) << 24;
  content |= (uint32_t) (offset & 0x00fffffcul);
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[2] = content;

  /* crc(15:0)|pad(15:0) */
  content = ((uint32_t) crc) << 16;
  packet->payload[3] = content;

  /* Set the size of the packet. */
  packet->size = 4u;
}


void RIOPACKET_getMaintReadRequest(const RioPacket_t *packet,
                                   uint16_t *dstId, uint16_t *srcId, uint8_t *hop,
                                   uint8_t *tid, uint32_t *offset)
{
  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(dstId != NULL, "Invalid dstId pointer");
  ASSERT(srcId != NULL, "Invalid srcId pointer");
  ASSERT(hop != NULL, "Invalid hop pointer");
  ASSERT(tid != NULL, "Invalid tid pointer");
  ASSERT(offset != NULL, "Invalid offset pointer");

  *dstId = (uint16_t) DESTID_GET(packet->payload);
  *srcId = (uint16_t) SRCID_GET(packet->payload);
  *tid = (uint8_t) TID_GET(packet->payload);
  *hop = (uint8_t) HOP_GET(packet->payload);
  *offset = (uint32_t) CONFIG_OFFSET_GET(packet->payload);
}


void RIOPACKET_setMaintReadResponse(RioPacket_t *packet,
                                    uint16_t dstId, uint16_t srcId, 
                                    uint8_t tid, uint32_t data)
{
  uint32_t content;
  uint16_t crc = 0xffffu;


  ASSERT(packet != NULL, "Invalid packet pointer");

  /* ackId(4:0)|0|vc|crf|prio(1:0)|tt(1:0)|ftype(3:0)|destinationId(15:0) */
  /* ackId is set when the packet is transmitted. */
  content = ((uint32_t) RIOPACKET_TT_16BITS) << 20;
  content |= ((uint32_t) RIOPACKET_FTYPE_MAINTENANCE) << 16;
  content |= (uint32_t) dstId;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[0] = content;

  /* sourceId(15:0)|transaction(3:0)|status(3:0)|srcTID(7:0) */
  content = (uint32_t) srcId << 16;
  content |= (uint32_t) RIOPACKET_TRANSACTION_MAINT_READ_RESPONSE << 12;
  content |= (uint32_t) RIOPACKET_RESPONSE_STATUS_DONE << 8; /*lint !e835 !e845 The constant is supposed to be zero. */
  content |= (uint32_t) tid;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[1] = content;

  /* hopcount(7:0)|reserved(23:0) */
  /* HopCount should always be set to 0xff in responses. */
  content = 0xff000000ul;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[2] = content;

  /* double-word(0)(63:0) */
  /* Note that both words are filled in to avoid looking at the offset. The receiver will not 
     look at the other part anyway. The standard does not say anything about the value of the padding. */
  content = data;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[3] = content;
  content = data;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[4] = content;

  /* crc(15:0)|pad(15:0) */
  content = ((uint32_t) crc) << 16;
  packet->payload[5] = content;

  /* Set the size of the packet. */
  packet->size = 6u;
}


void RIOPACKET_getMaintReadResponse(const RioPacket_t *packet,
                                    uint16_t *dstId, uint16_t *srcId,
                                    uint8_t *tid, uint32_t *data)
{
  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(dstId != NULL, "Invalid dstId pointer");
  ASSERT(srcId != NULL, "Invalid srcId pointer");
  ASSERT(tid != NULL, "Invalid tid pointer");
  ASSERT(data != NULL, "Invalid data pointer");

  *dstId = (uint16_t) DESTID_GET(packet->payload);
  *srcId = (uint16_t) SRCID_GET(packet->payload);
  *tid = (uint8_t) TID_GET(packet->payload);
  *data = (uint32_t) DOUBLE_WORD_MSB_GET(packet->payload, 0u); /*lint !e835 !e845 The payload at index=0 should be fetched. */
  *data |= (uint32_t) DOUBLE_WORD_LSB_GET(packet->payload, 0u);  /*lint !e835 !e845 The payload at index=0 should be fetched. */
}


/*******************************************************************************************
 * Logical I/O MAINTENANCE-WRITE functions.
 *******************************************************************************************/

void RIOPACKET_setMaintWriteRequest(RioPacket_t *packet,
                                    uint16_t dstId, uint16_t srcId, uint8_t hop, 
                                    uint8_t tid, uint32_t offset, uint32_t data)
{
  uint32_t content;
  uint16_t crc = 0xffffu;


  ASSERT(packet != NULL, "Invalid packet pointer");

  /* ackId(4:0)|0|vc|crf|prio(1:0)|tt(1:0)|ftype(3:0)|destinationId(15:0) */
  /* ackId is set when the packet is transmitted. */
  content = ((uint32_t) RIOPACKET_TT_16BITS) << 20;
  content |= ((uint32_t) RIOPACKET_FTYPE_MAINTENANCE) << 16;
  content |= (uint32_t) dstId;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[0] = content;

  /* sourceId(15:0)|transaction(3:0)|rdsize(3:0)|srcTID(7:0) */
  content = ((uint32_t) srcId) << 16;
  content |= (uint32_t) RIOPACKET_TRANSACTION_MAINT_WRITE_REQUEST << 12;
  content |= (uint32_t) 8ul << 8;
  content |= (uint32_t) tid;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[1] = content;

  /* hopcount(7:0)|configOffset(20:0)|wdptr|reserved(1:0) */
  content = ((uint32_t) hop) << 24;
  content |= (uint32_t) (offset & 0x00fffffcul);
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[2] = content;

  /* double-word(0)(63:0) */
  /* Note that both words are filled in to avoid looking at the offset. The receiver will not 
     look at the other part anyway. The standard does not say anything about the value of the padding. */
  content = data;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[3] = content;
  content = data;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[4] = content;

  /* crc(15:0)|pad(15:0) */
  content = ((uint32_t) crc) << 16;
  packet->payload[5] = content;

  /* Set the size of the packet. */
  packet->size = 6u;
}


void RIOPACKET_getMaintWriteRequest(const RioPacket_t *packet,
                                    uint16_t *dstId, uint16_t *srcId, uint8_t *hop, 
                                    uint8_t *tid, uint32_t *offset, uint32_t *data)
{
  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(dstId != NULL, "Invalid dstId pointer");
  ASSERT(srcId != NULL, "Invalid srcId pointer");
  ASSERT(hop != NULL, "Invalid hop pointer");
  ASSERT(tid != NULL, "Invalid tid pointer");
  ASSERT(offset != NULL, "Invalid offset pointer");
  ASSERT(data != NULL, "Invalid data pointer");

  *dstId = (uint16_t) DESTID_GET(packet->payload);
  *srcId = SRCID_GET(packet->payload);
  *tid = TID_GET(packet->payload);
  *hop = HOP_GET(packet->payload);
  *offset = CONFIG_OFFSET_GET(packet->payload);
  *data = DOUBLE_WORD_MSB_GET(packet->payload, 0u); /*lint !e835 !e845 The payload at index=0 should be fetched. */
  *data |= DOUBLE_WORD_LSB_GET(packet->payload, 0u); /*lint !e835 !e845 The payload at index=0 should be fetched. */
}


void RIOPACKET_setMaintWriteResponse(RioPacket_t *packet,
                                     uint16_t dstId, uint16_t srcId, 
                                     uint8_t tid)
{
  uint32_t content;
  uint16_t crc = 0xffffu;


  ASSERT(packet != NULL, "Invalid packet pointer");

  /* ackId(4:0)|0|vc|crf|prio(1:0)|tt(1:0)|ftype(3:0)|destinationId(15:0) */
  /* ackId is set when the packet is transmitted. */
  content = ((uint32_t) RIOPACKET_TT_16BITS) << 20;
  content |= ((uint32_t) RIOPACKET_FTYPE_MAINTENANCE) << 16;
  content |= (uint32_t) dstId;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[0] = content;

  /* sourceId(15:0)|transaction(3:0)|status(3:0)|srcTID(7:0) */
  content = ((uint32_t) srcId) << 16;
  content |= (uint32_t) RIOPACKET_TRANSACTION_MAINT_WRITE_RESPONSE << 12;
  content |= (uint32_t) RIOPACKET_RESPONSE_STATUS_DONE << 8; /*lint !e835 !e845 The constant value is equal to zero. */
  content |= (uint32_t) tid;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[1] = content;

  /* hopcount(7:0)|reserved(23:0) */
  /* HopCount should always be set to 0xff in responses. */
  content = 0xff000000ul;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[2] = content;

  /* crc(15:0)|pad(15:0) */
  content = ((uint32_t) crc) << 16;
  packet->payload[3] = content;

  /* Set the size of the packet. */
  packet->size = 4u;
}


void RIOPACKET_getMaintWriteResponse(const RioPacket_t *packet,
                                     uint16_t *dstId, uint16_t *srcId, 
                                     uint8_t *tid)
{
  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(dstId != NULL, "Invalid dstId pointer");
  ASSERT(srcId != NULL, "Invalid srcId pointer");
  ASSERT(tid != NULL, "Invalid tid pointer");

  *dstId = DESTID_GET(packet->payload);
  *srcId = SRCID_GET(packet->payload);
  *tid = TID_GET(packet->payload);
}


/*******************************************************************************************
 * Logical I/O MAINTENANCE-PORTWRITE functions.
 *******************************************************************************************/

void RIOPACKET_setMaintPortWrite(RioPacket_t *packet,
                                 uint16_t dstId, uint16_t srcId, 
                                 uint32_t componentTag, uint32_t portErrorDetect,
                                 uint32_t implementationSpecific, uint8_t portId,
                                 uint32_t logicalTransportErrorDetect)
{
  uint32_t content;
  uint16_t crc = 0xffffu;


  ASSERT(packet != NULL, "Invalid packet pointer");

  /* ackId(4:0)|0|vc|crf|prio(1:0)|tt(1:0)|ftype(3:0)|destinationId(15:0) */
  /* ackId is set when the packet is transmitted. */
  content = ((uint32_t) RIOPACKET_TT_16BITS) << 20;
  content |= ((uint32_t) RIOPACKET_FTYPE_MAINTENANCE) << 16;
  content |= (uint32_t) dstId;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[0] = content;

  /* sourceId(15:0)|transaction(3:0)|rdsize(3:0)|srcTID(7:0) */
  content = (uint32_t) srcId << 16;
  content |= (uint32_t) RIOPACKET_TRANSACTION_MAINT_PORT_WRITE_REQUEST << 12;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[1] = content;

  /* hopcount(7:0)|reserved(23:0) */
  content = 0x00000000ul;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[2] = content;

  /* double-word(0)(63:0) */
  content = componentTag;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[3] = content;
  content = portErrorDetect;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[4] = content;

  /* double-word(1)(63:0) */
  content = implementationSpecific << 8;
  content |= (uint32_t) portId;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[5] = content;
  content = logicalTransportErrorDetect;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[6] = content;

  /* crc(15:0)|pad(15:0) */
  content = ((uint32_t) crc) << 16;
  packet->payload[7] = content;

  /* Set the size of the packet. */
  packet->size = 8u;
}


void RIOPACKET_getMaintPortWrite(const RioPacket_t *packet,
                                 uint16_t *dstId, uint16_t *srcId, 
                                 uint32_t *componentTag, uint32_t *portErrorDetect,
                                 uint32_t *implementationSpecific, uint8_t *portId,
                                 uint32_t *logicalTransportErrorDetect)
{
  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(dstId != NULL, "Invalid dstId pointer");
  ASSERT(srcId != NULL, "Invalid srcId pointer");
  ASSERT(componentTag != NULL, "Invalid componentTag pointer");
  ASSERT(portErrorDetect != NULL, "Invalid portErrorDetect pointer");
  ASSERT(implementationSpecific != NULL, "Invalid implementationSpecific pointer");
  ASSERT(portId != NULL, "Invalid portId pointer");
  ASSERT(logicalTransportErrorDetect != NULL, "Invalid logicalTransportErrorDetect pointer");

  *dstId = DESTID_GET(packet->payload);
  *srcId = SRCID_GET(packet->payload);
  *componentTag = packet->payload[3];
  *portErrorDetect = packet->payload[4];
  *implementationSpecific = packet->payload[5] >> 8;
  *portId = (uint8_t) (packet->payload[5] & 0x000000fful);
  *logicalTransportErrorDetect = packet->payload[6];
}


/*******************************************************************************************
 * Logical I/O NWRITE/NWRITER functions.
 *******************************************************************************************/

void RIOPACKET_setNwrite(RioPacket_t *packet, uint16_t dstId, uint16_t srcId,
                         uint32_t address, uint16_t payloadSize, const uint8_t *payload)
{
  uint32_t content;
  uint16_t wrsize;

  
  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(payload != NULL, "Invalid payload pointer");
  ASSERT(payloadSize <= 256u, "Invalid payload size");

  /* Convert the address and size to the wrsize field and check if the combination is valid. */
  wrsize = wrsizeGet(address, payloadSize);
  if(wrsize != 0xffffu)
  {
    /* The address and size field combination is valid. */

    /* ackId(4:0)|0|vc|crf|prio(1:0)|tt(1:0)|ftype(3:0)|destinationId(15:0) */
    /* ackId is set when the packet is transmitted. */
    content = ((uint32_t) RIOPACKET_TT_16BITS) << 20;
    content |= ((uint32_t) RIOPACKET_FTYPE_WRITE) << 16;
    content |= (uint32_t) dstId;
    packet->payload[0] = content;
    
    /* sourceId(15:0)|transaction(3:0)|wrsize(3:0)|reserved(7:0) */
    content = ((uint32_t) srcId) << 16;
    content |= (uint32_t) RIOPACKET_TRANSACTION_WRITE_NWRITE << 12;
    content |= (uint32_t) (((uint32_t) wrsize) & 0x00000f00ul); 
    packet->payload[1] = content;
    
    /* address(28:0)|wdptr|xamsbs(1:0) */
    /* wrsize also contains wdptr in the lower nibble. */
    /* REMARK: Note that xamsbs cannot be used if the address is a word. If the 2 msb bits in the 
       34-bit address should be used, another mechanism to set it should be used. */
    content = (uint32_t) (address & 0xfffffff8ul);
    content |= (uint32_t) ((((uint32_t) wrsize) & 0x0000000ful) << 2);
    packet->payload[2] = content;
    
    /* Place the payload buffer into the payload of the packet. */
    /* This function also calculates the CRC. */
    packet->size = setPacketPayload(&(packet->payload[0]), 12u, (uint16_t) (address & 0x7ul), 
                                    payloadSize, payload);
  }
  else
  {
    /* The address and size field combination is not valid. */
    /* Cannot create a packet from these arguments, indicate this by setting the packet size to zero. */
    packet->size = 0u;
  }
}


void RIOPACKET_getNwrite(const RioPacket_t *packet, uint16_t *dstId, uint16_t *srcId, 
                         uint32_t *address, uint16_t *payloadSize, uint8_t *payload)
{
  uint8_t wrsize;
  uint8_t wdptr;
  uint8_t offset = 0u;
  uint16_t size = 0u;


  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(dstId != NULL, "Invalid dstId pointer");
  ASSERT(srcId != NULL, "Invalid srcId pointer");
  ASSERT(address != NULL, "Invalid address pointer");
  ASSERT(payloadSize != NULL, "Invalid payloadSize pointer");
  ASSERT(payload != NULL, "Invalid payload pointer");

  wrsize = WRSIZE_GET(packet->payload);
  wdptr = WDPTR_GET(packet->payload);
  wrsizeToOffset(wrsize, wdptr, &offset, &size);

  *dstId = DESTID_GET(packet->payload);
  *srcId = SRCID_GET(packet->payload);
  *address = ADDRESS_GET(packet->payload) | offset;

  if(size > 16u)
  {
    size = 4u * (((uint16_t) packet->size)-4u);
  }
  else
  {
    /* The size already contains the correct value. */
  }

  *payloadSize = getPacketPayload(&(packet->payload[0]), 12u, (uint16_t) offset, size, payload);
}



void RIOPACKET_setNwriteR(RioPacket_t *packet, uint16_t dstId, uint16_t srcId, uint8_t tid, 
                          uint32_t address, uint16_t payloadSize, const uint8_t *payload)
{
  uint32_t content;
  uint16_t wrsize;

  
  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(payload != NULL, "Invalid payload pointer");
  ASSERT(payloadSize <= 256u, "Invalid payload size");

  /* Convert the address and size to the wrsize field and check if the combination is valid. */
  wrsize = wrsizeGet(address, payloadSize);
  if(wrsize != 0xffffu)
  {
    /* The address and size field combination is valid. */

    /* ackId(4:0)|0|vc|crf|prio(1:0)|tt(1:0)|ftype(3:0)|destinationId(15:0) */
    /* ackId is set when the packet is transmitted. */
    content = ((uint32_t) RIOPACKET_TT_16BITS) << 20;
    content |= ((uint32_t) RIOPACKET_FTYPE_WRITE) << 16;
    content |= (uint32_t) dstId;
    packet->payload[0] = content;
    
    /* sourceId(15:0)|transaction(3:0)|wrsize(3:0)|srcTID(7:0) */
    content = ((uint32_t) srcId) << 16;
    content |= (uint32_t) RIOPACKET_TRANSACTION_WRITE_NWRITER << 12;
    content |= (uint32_t) (((uint32_t) wrsize) & 0x00000f00ul); 
    content |= (uint32_t) tid;
    packet->payload[1] = content;
    
    /* address(28:0)|wdptr|xamsbs(1:0) */
    /* wrsize also contains wdptr in the lower nibble. */
    /* REMARK: Note that xamsbs cannot be used if the address is a word. If the 2 msb bits in the 
       34-bit address should be used, another mechanism to set it should be used. */
    content = (uint32_t) (address & 0xfffffff8ul);
    content |= (uint32_t) ((((uint32_t) wrsize) & 0x000ful) << 2);
    packet->payload[2] = content;
    
    /* Place the payload buffer into the payload of the packet. */
    /* This function also calculates the CRC. */
    packet->size = setPacketPayload(&(packet->payload[0]), 12u, (uint16_t) (address & 0x7ul), 
                                    payloadSize, payload);
  }
  else
  {
    /* The address and size field combination is not valid. */
    /* Cannot create a packet from these arguments, indicate this by setting the packet size to zero. */
    packet->size = 0u;
  }
}


void RIOPACKET_getNwriteR(const RioPacket_t *packet, uint16_t *dstId, uint16_t *srcId, uint8_t *tid,
                          uint32_t *address, uint16_t *payloadSize, uint8_t *payload)
{
  uint8_t wrsize;
  uint8_t wdptr;
  uint8_t offset = 0u;
  uint16_t size = 0u;


  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(dstId != NULL, "Invalid dstId pointer");
  ASSERT(srcId != NULL, "Invalid srcId pointer");
  ASSERT(tid != NULL, "Invalid tid pointer");
  ASSERT(address != NULL, "Invalid address pointer");
  ASSERT(payloadSize != NULL, "Invalid payloadSize pointer");
  ASSERT(payload != NULL, "Invalid payload pointer");

  wrsize = WRSIZE_GET(packet->payload);
  wdptr = WDPTR_GET(packet->payload);
  wrsizeToOffset(wrsize, wdptr, &offset, &size);
  
  *dstId = DESTID_GET(packet->payload);
  *srcId = SRCID_GET(packet->payload);
  *tid = TID_GET(packet->payload);
  *address = ADDRESS_GET(packet->payload) | offset;
  
  if(size > 16u)
  {
    size = 4u*(((uint16_t) packet->size)-4u);
  }
  else
  {
    /* The size already contains the correct value. */
  }

  *payloadSize = getPacketPayload(&(packet->payload[0]), 12u, (uint16_t) offset, size, payload);
}


/*******************************************************************************************
 * Logical I/O NREAD functions.
 *******************************************************************************************/

void RIOPACKET_setNread(RioPacket_t *packet, uint16_t dstId, uint16_t srcId, uint8_t tid, 
                        uint32_t address, uint16_t payloadSize)
{
  uint32_t content;
  uint16_t crc = 0xffffu;
  uint16_t rdsize;


  ASSERT(packet != NULL, "Invalid packet pointer");

  /* Convert the address and size to the rdsize field and check if the combination is valid. */
  rdsize = rdsizeGet(address, payloadSize);
  if(rdsize != 0xffffu)
  {
    /* The address and size field combination is valid. */

    /* ackId(4:0)|0|vc|crf|prio(1:0)|tt(1:0)|ftype(3:0)|destinationId(15:0) */
    /* ackId is set when the packet is transmitted. */
    content = ((uint32_t) RIOPACKET_TT_16BITS) << 20;
    content |= ((uint32_t) RIOPACKET_FTYPE_REQUEST) << 16;
    content |= (uint32_t) dstId;
    crc = RIOPACKET_crc32(content, crc);
    packet->payload[0] = content;

    /* sourceId(15:0)|transaction(3:0)|rdsize(3:0)|srcTID(7:0) */
    content = ((uint32_t) srcId) << 16;
    content |= ((uint32_t) RIOPACKET_TRANSACTION_REQUEST_NREAD) << 12;
    content |= (uint32_t) (((uint32_t) rdsize) & 0x0f00ul);
    content |= (uint32_t) tid;
    crc = RIOPACKET_crc32(content, crc);
    packet->payload[1] = content;

    /* address(28:0)|wdptr|xamsbs(1:0) */
    /* rdsize also contains wdptr in the lower nibble. */
    /* REMARK: Note that xamsbs cannot be used if the address is a word. If the 2 msb bits in the 
       34-bit address should be used, another mechanism to set it should be used. */
    content = (uint32_t) (address & 0xfffffff8ul);
    content |= (uint32_t) ((((uint32_t) rdsize) & 0x000ful) << 2);
    crc = RIOPACKET_crc32(content, crc);
    packet->payload[2] = content;

    /* crc(15:0)|pad(15:0) */
    content = ((uint32_t) crc) << 16;
    packet->payload[3] = content;

    /* Set the size of the packet. */
    packet->size = 4u;
  }
  else
  {
    /* The address and size field combination is not valid. */
    /* Cannot create a packet from these arguments, indicate this by setting the packet size to zero. */
    packet->size = 0u;
  }
}


void RIOPACKET_getNread(const RioPacket_t *packet, uint16_t *dstId, uint16_t *srcId, uint8_t *tid, 
                        uint32_t *address, uint16_t *payloadSize)
{
  uint8_t rdsize;
  uint8_t wdptr;
  uint8_t offset = 0u;
  uint16_t size = 0u;


  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(dstId != NULL, "Invalid dstId pointer");
  ASSERT(srcId != NULL, "Invalid srcId pointer");
  ASSERT(tid != NULL, "Invalid tid pointer");
  ASSERT(address != NULL, "Invalid address pointer");
  ASSERT(payloadSize != NULL, "Invalid payloadSize pointer");

  rdsize = RDSIZE_GET(packet->payload);
  wdptr = WDPTR_GET(packet->payload);
  rdsizeToOffset(rdsize, wdptr, &offset, &size);

  *dstId = DESTID_GET(packet->payload);
  *srcId = SRCID_GET(packet->payload);
  *tid = TID_GET(packet->payload);
  *address = ADDRESS_GET(packet->payload) | offset;
  *payloadSize = size;
}



/*******************************************************************************************
 * Logical message passing DOORBELL and MESSAGE functions.
 *******************************************************************************************/

void RIOPACKET_setDoorbell(RioPacket_t *packet, uint16_t dstId, uint16_t srcId, uint8_t tid, 
                           uint16_t info)
{
  uint32_t content;
  uint16_t crc = 0xffffu;


  ASSERT(packet != NULL, "Invalid packet pointer");

  /* ackId(4:0)|0|vc|crf|prio(1:0)|tt(1:0)|ftype(3:0)|destinationId(15:0) */
  /* ackId is set when the packet is transmitted. */
  content = ((uint32_t) RIOPACKET_TT_16BITS) << 20;
  content |= ((uint32_t) RIOPACKET_FTYPE_DOORBELL) << 16;
  content |= (uint32_t) dstId;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[0] = content;

  /* sourceId(15:0)|rsrv(7:0)|srcTID(7:0) */
  content = ((uint32_t) srcId) << 16;
  content |= (uint32_t) tid;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[1] = content;

  /* infoMSB(7:0)|infoLSB(7:0)|crc(15:0) */
  content = ((uint32_t) info) << 16;
  crc = RIOPACKET_crc16(info, crc);
  content |= crc;
  packet->payload[2] = content;

  /* Set the size of the packet. */
  packet->size = 3u;
}


void RIOPACKET_getDoorbell(const RioPacket_t *packet, uint16_t *dstId, uint16_t *srcId, uint8_t *tid, 
                           uint16_t *info)
{
  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(dstId != NULL, "Invalid dstId pointer");
  ASSERT(srcId != NULL, "Invalid srcId pointer");
  ASSERT(tid != NULL, "Invalid tid pointer");
  ASSERT(info != NULL, "Invalid info pointer");

  *dstId = DESTID_GET(packet->payload);
  *srcId = SRCID_GET(packet->payload);
  *tid = TID_GET(packet->payload);
  *info = INFO_GET(packet->payload);
}


void RIOPACKET_setMessage(RioPacket_t *packet, uint16_t dstId, uint16_t srcId, uint8_t mailbox, 
                          uint16_t payloadSize, const uint8_t *payload)
{
  uint32_t content;


  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(payloadSize <= 256u, "Invalid payload size");

  /* Make sure that the message payload size is larger than zero. */
  if((payloadSize > 0u) && (payloadSize <= 256u))
  {
    /* The payload size is larger than zero. */

    /* ackId(4:0)|0|vc|crf|prio(1:0)|tt(1:0)|ftype(3:0)|destinationId(15:0) */
    /* ackId is set when the packet is transmitted. */
    content = ((uint32_t) RIOPACKET_TT_16BITS) << 20;
    content |= ((uint32_t) RIOPACKET_FTYPE_MESSAGE) << 16;
    content |= (uint32_t) dstId;
    packet->payload[0] = content;
  
    /* sourceId(15:0)|msglen(3:0)|ssize(3:0)|letter(1:0)|mbox(1:0)|msgseg(3:0)/xmbox(3:0) */
    content = ((uint32_t) srcId) << 16;
    if(payloadSize <= 8u)
    {
      content |= (uint32_t) 0x00000900ul;
    }
    else if(payloadSize <= 16u)
    {
      content |= (uint32_t) 0x00000a00ul;
    }
    else if(payloadSize <= 32u)
    {
      content |= (uint32_t) 0x00000b00ul;
    }
    else if(payloadSize <= 64u)
    {
      content |= (uint32_t) 0x00000c00ul;
    }
    else if(payloadSize <= 128u)
    {
      content |= (uint32_t) 0x00000d00ul;
    }
    else
    {
      content |= (uint32_t) 0x00000e00ul;
    }
    content |= (uint32_t) ((((uint32_t) mailbox) & 0xful) << 4);
    content |= (uint32_t) (((uint32_t) mailbox) >> 4);
    packet->payload[1] = content;

    /* Place data buffer into the payload of the packet and set the size. */
    packet->size = setPacketPayload(&(packet->payload[0]), 8u, 0u, payloadSize, payload);
  }
  else
  {
    /* The payload size is not allowed. */
    /* Unable to create the new packet. */
    packet->size = 0u;
  }
}


void RIOPACKET_getMessage(const RioPacket_t *packet, uint16_t *dstId, uint16_t *srcId, uint8_t *mailbox, 
                          uint16_t *payloadSize, uint8_t *payload)
{
  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(dstId != NULL, "Invalid dstId pointer");
  ASSERT(srcId != NULL, "Invalid srcId pointer");
  ASSERT(mailbox != NULL, "Invalid mailbox pointer");
  ASSERT(payloadSize != NULL, "Invalid payloadSize pointer");
  ASSERT(payload != NULL, "Invalid payload pointer");

  *dstId = DESTID_GET(packet->payload);
  *srcId = SRCID_GET(packet->payload);
  *mailbox = XMBOX_GET(packet->payload);
  *mailbox <<= 2;
  *mailbox |= LETTER_GET(packet->payload);
  *mailbox <<= 2;
  *mailbox |= MBOX_GET(packet->payload);
  *payloadSize = getPacketPayload(&(packet->payload[0]), 8u, 0u, 
                                  (((uint16_t) packet->size)-3u)*4u, payload);
}




/*******************************************************************************************
 * Logical I/O RESPONSE-DONE-PAYLOAD, RESPONSE-DONE, RESPONSE-RETRY and RESPONSE-ERROR 
 * functions.
 *******************************************************************************************/

void RIOPACKET_setResponseNoPayload(RioPacket_t *packet, 
                                    uint16_t dstId, uint16_t srcId, 
                                    uint8_t tid, uint8_t status)
{
  uint32_t content;
  uint16_t crc = 0xffffu;


  ASSERT(packet != NULL, "Invalid packet pointer");

  /* ackId(4:0)|0|vc|crf|prio(1:0)|tt(1:0)|ftype(3:0)|destinationId(15:0) */
  /* ackId is set when the packet is transmitted. */
  content = ((uint32_t) RIOPACKET_TT_16BITS) << 20;
  content |= ((uint32_t) RIOPACKET_FTYPE_RESPONSE) << 16;
  content |= (uint32_t) dstId;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[0] = content;

  /* sourceId(15:0)|transaction(3:0)|status(3:0)|targetTID(7:0) */
  content = ((uint32_t) srcId) << 16;
  content |= ((uint32_t) RIOPACKET_TRANSACTION_RESPONSE_NO_PAYLOAD) << 12; /*lint !e835 !e845 The constant value is zero. */
  content |= (uint32_t) ((((uint32_t) status) & 0xful) << 8);
  content |= (uint32_t) tid;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[1] = content;

  /* crc(15:0)|pad(15:0) */
  content = ((uint32_t) crc) << 16;
  packet->payload[2] = content;

  /* Set the size of the packet. */
  packet->size = 3u;
}


void RIOPACKET_getResponseNoPayload(const RioPacket_t *packet, 
                                    uint16_t *dstId, uint16_t *srcId, 
                                    uint8_t *tid, uint8_t *status)
{
  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(dstId != NULL, "Invalid dstId pointer");
  ASSERT(srcId != NULL, "Invalid srcId pointer");
  ASSERT(tid != NULL, "Invalid tid pointer");
  ASSERT(status != NULL, "Invalid status pointer");

  *dstId = DESTID_GET(packet->payload);
  *srcId = SRCID_GET(packet->payload);
  *tid = TID_GET(packet->payload);
  *status = STATUS_GET(packet->payload);
}



void RIOPACKET_setResponseWithPayload(RioPacket_t *packet, 
                                      uint16_t dstId, uint16_t srcId, 
                                      uint8_t tid, uint8_t offset, 
                                      uint16_t payloadSize, const uint8_t *payload)
{
  uint32_t content;


  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(payload != NULL, "Invalid payload pointer");
  ASSERT(payloadSize <= 256u, "Invalid payload size");

  /* ackId(4:0)|0|vc|crf|prio(1:0)|tt(1:0)|ftype(3:0)|destinationId(15:0) */
  /* ackId is set when the packet is transmitted. */
  content = ((uint32_t) RIOPACKET_TT_16BITS) << 20;
  content |= ((uint32_t) RIOPACKET_FTYPE_RESPONSE) << 16;
  content |= (uint32_t) dstId;
  packet->payload[0] = content;
    
  /* sourceId(15:0)|transaction(3:0)|status(3:0)|targetTID(7:0) */
  /* status=DONE is 0. */
  content = ((uint32_t) srcId) << 16;
  content |= ((uint32_t) RIOPACKET_TRANSACTION_RESPONSE_WITH_PAYLOAD) << 12;
  content |= (uint32_t) tid;
  packet->payload[1] = content;

  packet->size = setPacketPayload(&(packet->payload[0]), 8u, ((uint16_t) offset) & 0x7u, 
                                  payloadSize, payload);
}


void RIOPACKET_getResponseWithPayload(const RioPacket_t *packet, 
                                      uint16_t *dstId, uint16_t *srcId, 
                                      uint8_t *tid, uint8_t offset, 
                                      uint16_t *payloadSize, uint8_t *payload)
{
  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(dstId != NULL, "Invalid dstId pointer");
  ASSERT(srcId != NULL, "Invalid srcId pointer");
  ASSERT(tid != NULL, "Invalid tid pointer");
  ASSERT(payloadSize != NULL, "Invalid payloadSize pointer");
  ASSERT(payload != NULL, "Invalid payload pointer");

  *dstId = DESTID_GET(packet->payload);
  *srcId = SRCID_GET(packet->payload);
  *tid = TID_GET(packet->payload);
  *payloadSize = getPacketPayload(&(packet->payload[0]), 8u, ((uint16_t) offset) & 0x7u, 
                                  (((uint16_t) packet->size)-3u)*4u, payload);
}


void RIOPACKET_setResponseMessage(RioPacket_t *packet, 
                                  uint16_t dstId, uint16_t srcId, 
                                  uint8_t mailbox, uint8_t status)
{
  uint32_t content;
  uint16_t crc = 0xffffu;


  ASSERT(packet != NULL, "Invalid packet pointer");

  /* ackId(4:0)|0|vc|crf|prio(1:0)|tt(1:0)|ftype(3:0)|destinationId(15:0) */
  /* ackId is set when the packet is transmitted. */
  content = ((uint32_t) RIOPACKET_TT_16BITS) << 20;
  content |= ((uint32_t) RIOPACKET_FTYPE_RESPONSE) << 16;
  content |= (uint32_t) dstId;
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[0] = content;

  /* sourceId(15:0)|transaction(3:0)|status(3:0)|letter(1:0|mbox(1:0)|msgseg(3:0) */
  content = ((uint32_t) srcId) << 16;
  content |= ((uint32_t) RIOPACKET_TRANSACTION_RESPONSE_MESSAGE_RESPONSE) << 12;
  content |= (uint32_t) ((((uint32_t) status) & 0xful) << 8);
  content |= (uint32_t) ((((uint32_t) mailbox) & 0xful) << 4);
  content |= (uint32_t) (((uint32_t) mailbox) >> 4);
  crc = RIOPACKET_crc32(content, crc);
  packet->payload[1] = content;

  /* crc(15:0)|pad(15:0) */
  content = ((uint32_t) crc) << 16;
  packet->payload[2] = content;

  /* Set the size of the packet. */
  packet->size = 3u;
}


void RIOPACKET_getResponseMessage(const RioPacket_t *packet, 
                                  uint16_t *dstId, uint16_t *srcId, 
                                  uint8_t *mailbox, uint8_t *status)
{
  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(dstId != NULL, "Invalid dstId pointer");
  ASSERT(srcId != NULL, "Invalid srcId pointer");
  ASSERT(mailbox != NULL, "Invalid mailbox pointer");
  ASSERT(status != NULL, "Invalid status pointer");

  *dstId = DESTID_GET(packet->payload);
  *srcId = SRCID_GET(packet->payload);
  *mailbox = XMBOX_GET(packet->payload);
  *mailbox <<= 2;
  *mailbox |= LETTER_GET(packet->payload);
  *mailbox <<= 2;
  *mailbox |= MBOX_GET(packet->payload);
  *status = STATUS_GET(packet->payload);
}


uint16_t RIOPACKET_crc16(const uint16_t data, const uint16_t crc)
{
  static const uint16_t crcTable[] = {
    0x0000u, 0x1021u, 0x2042u, 0x3063u, 0x4084u, 0x50a5u, 0x60c6u, 0x70e7u,
    0x8108u, 0x9129u, 0xa14au, 0xb16bu, 0xc18cu, 0xd1adu, 0xe1ceu, 0xf1efu,
    0x1231u, 0x0210u, 0x3273u, 0x2252u, 0x52b5u, 0x4294u, 0x72f7u, 0x62d6u,
    0x9339u, 0x8318u, 0xb37bu, 0xa35au, 0xd3bdu, 0xc39cu, 0xf3ffu, 0xe3deu,
    0x2462u, 0x3443u, 0x0420u, 0x1401u, 0x64e6u, 0x74c7u, 0x44a4u, 0x5485u,
    0xa56au, 0xb54bu, 0x8528u, 0x9509u, 0xe5eeu, 0xf5cfu, 0xc5acu, 0xd58du,
    0x3653u, 0x2672u, 0x1611u, 0x0630u, 0x76d7u, 0x66f6u, 0x5695u, 0x46b4u,
    0xb75bu, 0xa77au, 0x9719u, 0x8738u, 0xf7dfu, 0xe7feu, 0xd79du, 0xc7bcu,
    0x48c4u, 0x58e5u, 0x6886u, 0x78a7u, 0x0840u, 0x1861u, 0x2802u, 0x3823u,
    0xc9ccu, 0xd9edu, 0xe98eu, 0xf9afu, 0x8948u, 0x9969u, 0xa90au, 0xb92bu,
    0x5af5u, 0x4ad4u, 0x7ab7u, 0x6a96u, 0x1a71u, 0x0a50u, 0x3a33u, 0x2a12u,
    0xdbfdu, 0xcbdcu, 0xfbbfu, 0xeb9eu, 0x9b79u, 0x8b58u, 0xbb3bu, 0xab1au,
    0x6ca6u, 0x7c87u, 0x4ce4u, 0x5cc5u, 0x2c22u, 0x3c03u, 0x0c60u, 0x1c41u,
    0xedaeu, 0xfd8fu, 0xcdecu, 0xddcdu, 0xad2au, 0xbd0bu, 0x8d68u, 0x9d49u,
    0x7e97u, 0x6eb6u, 0x5ed5u, 0x4ef4u, 0x3e13u, 0x2e32u, 0x1e51u, 0x0e70u,
    0xff9fu, 0xefbeu, 0xdfddu, 0xcffcu, 0xbf1bu, 0xaf3au, 0x9f59u, 0x8f78u,
    0x9188u, 0x81a9u, 0xb1cau, 0xa1ebu, 0xd10cu, 0xc12du, 0xf14eu, 0xe16fu,
    0x1080u, 0x00a1u, 0x30c2u, 0x20e3u, 0x5004u, 0x4025u, 0x7046u, 0x6067u,
    0x83b9u, 0x9398u, 0xa3fbu, 0xb3dau, 0xc33du, 0xd31cu, 0xe37fu, 0xf35eu,
    0x02b1u, 0x1290u, 0x22f3u, 0x32d2u, 0x4235u, 0x5214u, 0x6277u, 0x7256u,
    0xb5eau, 0xa5cbu, 0x95a8u, 0x8589u, 0xf56eu, 0xe54fu, 0xd52cu, 0xc50du,
    0x34e2u, 0x24c3u, 0x14a0u, 0x0481u, 0x7466u, 0x6447u, 0x5424u, 0x4405u,
    0xa7dbu, 0xb7fau, 0x8799u, 0x97b8u, 0xe75fu, 0xf77eu, 0xc71du, 0xd73cu,
    0x26d3u, 0x36f2u, 0x0691u, 0x16b0u, 0x6657u, 0x7676u, 0x4615u, 0x5634u,
    0xd94cu, 0xc96du, 0xf90eu, 0xe92fu, 0x99c8u, 0x89e9u, 0xb98au, 0xa9abu,
    0x5844u, 0x4865u, 0x7806u, 0x6827u, 0x18c0u, 0x08e1u, 0x3882u, 0x28a3u,
    0xcb7du, 0xdb5cu, 0xeb3fu, 0xfb1eu, 0x8bf9u, 0x9bd8u, 0xabbbu, 0xbb9au,
    0x4a75u, 0x5a54u, 0x6a37u, 0x7a16u, 0x0af1u, 0x1ad0u, 0x2ab3u, 0x3a92u,
    0xfd2eu, 0xed0fu, 0xdd6cu, 0xcd4du, 0xbdaau, 0xad8bu, 0x9de8u, 0x8dc9u,
    0x7c26u, 0x6c07u, 0x5c64u, 0x4c45u, 0x3ca2u, 0x2c83u, 0x1ce0u, 0x0cc1u,
    0xef1fu, 0xff3eu, 0xcf5du, 0xdf7cu, 0xaf9bu, 0xbfbau, 0x8fd9u, 0x9ff8u,
    0x6e17u, 0x7e36u, 0x4e55u, 0x5e74u, 0x2e93u, 0x3eb2u, 0x0ed1u, 0x1ef0u
  };

  uint16_t result;
  uint8_t index;
  

  result = crc;
  index = (uint8_t) ((data >> 8) ^ (result >> 8));
  result = (uint16_t) (crcTable[index] ^ (uint16_t)(result << 8));
  index = (uint8_t) ((data) ^ (result >> 8));
  result = (uint16_t) (crcTable[index] ^ (uint16_t)(result << 8));

  return result;
}


uint16_t RIOPACKET_crc32(const uint32_t data, uint16_t crc)
{
  crc = RIOPACKET_crc16((uint16_t) (data >> 16), crc);
  crc = RIOPACKET_crc16((uint16_t) (data), crc);
  return crc;
}


uint32_t RIOPACKET_getWritePacketSize(uint32_t address, uint32_t size)
{
  uint32_t returnValue;

  if(size > 0u)
  {
    switch(address%8u)
    {
      case 0:
        if(size >= 256u)
        {
          returnValue = 256u;
        }
        else if(size >= 8u)
        {
          returnValue = size - (size % 8u);
        }
        else
        {
          returnValue = size;
        }
        break;

      case 1:
        if(size >= 7u)
        {
          returnValue = 7u;
        }
        else
        {
          returnValue = 1u;
        }
        break;

      case 2:
        if(size >= 6u)
        {
          returnValue = 6u;
        }
        else if(size >= 2u)
        {
          returnValue = 2u;
        }
        else
        {
          returnValue = 1u;
        }
        break;

      case 3:
        if(size >= 5u)
        {
          returnValue = 5u;
        }
        else
        {
          returnValue = 1u;
        }
        break;

      case 4:
        if(size >= 4u)
        {
          returnValue = 4u;
        }
        else if(size >= 2u)
        {
          returnValue = 2u;
        }
        else
        {
          returnValue = 1u;
        }
        break;

      case 5:
        if(size >= 3u)
        {
          returnValue = 3u;
        }
        else
        {
          returnValue = 1u;
        }
        break;

      case 6:
        if(size >= 2u)
        {
          returnValue = 2u;
        }
        else
        {
          returnValue = 1u;
        }
        break;

      default:
        returnValue = 1u;
        break;
    }
  }
  else
  {
    returnValue = 0u;
  }

  return returnValue;
}


uint32_t RIOPACKET_getReadPacketSize(uint32_t address, uint32_t size)
{
  uint32_t returnValue;

  if(size > 0u)
  {
    switch(address%8u)
    {
      case 0:
        if(size >= 256u)
        {
          returnValue = 256u;
        }
        else if(size >= 224u)
        {
          returnValue = 224u;
        }
        else if(size >= 192u)
        {
          returnValue = 192u;
        }
        else if(size >= 160u)
        {
          returnValue = 160u;
        }
        else if(size >= 128u)
        {
          returnValue = 128u;
        }
        else if(size >= 96u)
        {
          returnValue = 96u;
        }
        else if(size >= 64u)
        {
          returnValue = 64u;
        }
        else if(size >= 32u)
        {
          returnValue = 32u;
        }
        else if(size >= 16u)
        {
          returnValue = 16u;
        }
        else if(size >= 8u)
        {
          returnValue = 8u;
        }
        else
        {
          returnValue = size;
        }
        break;

      case 1:
        if(size >= 7u)
        {
          returnValue = 7u;
        }
        else
        {
          returnValue = 1u;
        }
        break;

      case 2:
        if(size >= 6u)
        {
          returnValue = 6u;
        }
        else if(size >= 2u)
        {
          returnValue = 2u;
        }
        else
        {
          returnValue = 1u;
        }
        break;

      case 3:
        if(size >= 5u)
        {
          returnValue = 5u;
        }
        else
        {
          returnValue = 1u;
        }
        break;

      case 4:
        if(size >= 4u)
        {
          returnValue = 4u;
        }
        else if(size >= 2u)
        {
          returnValue = 2u;
        }
        else
        {
          returnValue = 1u;
        }
        break;

      case 5:
        if(size >= 3u)
        {
          returnValue = 3u;
        }
        else
        {
          returnValue = 1u;
        }
        break;

      case 6:
        if(size >= 2u)
        {
          returnValue = 2u;
        }
        else
        {
          returnValue = 1u;
        }
        break;

      default:
        returnValue = 1u;
        break;
    }
  }
  else
  {
    returnValue = 0u;
  }

  return returnValue;
}



/*******************************************************************************
 * Locally used helper functions.
 *******************************************************************************/

/**
 * \brief Move the payload part of a packet to a user defined storage.
 *
 * \param[in] packet The address of a full RapidIO packet including headers.
 * \param[in] payloadOffset The word offset where the payload starts in the packet.
 * \param[in] dataOffset The byte offset in the first word to start copy data from.
 * \param[in] dataSize The number of bytes to copy.
 * \param[out] data The buffer to store the resulting payload.
 */
static uint16_t getPacketPayload(const uint32_t packet[], const uint16_t payloadOffset, const uint16_t dataOffset, 
                                 const uint16_t dataSize, uint8_t data[])
{
  uint32_t content = 0ul;
  uint16_t packetIndex;
  uint16_t payloadIndex;
  uint16_t dataIndex;


  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(data != NULL, "Invalid data pointer");

  /* Move payload bytes from RapidIO packet into a user buffer. */
  /* Long packets contain a CRC in byte 80-81, this is removed when the buffer 
     is copied. */
  packetIndex = payloadOffset;
  payloadIndex = 0u;
  dataIndex = 0u;
  while(dataIndex < dataSize)
  {
    /* Check if a new word should be read from the inbound queue. */
    if((packetIndex & 0x3u) == 0u)
    {
      /* Get a new word. */
      content = packet[packetIndex>>2];
    }
    else
    {
      /* Update the current word. Remove the MSB, it has already be moved 
         to the user buffer. */
      content <<= 8;
    }

    /* Check if the current byte is CRC. */
    if((packetIndex != 80u) && (packetIndex != 81u) && (payloadIndex >= dataOffset))
    {
      /* Not CRC. */
      /* Move the byte to the user buffer. */
      data[dataIndex] = (uint8_t) (content >> 24);
      dataIndex++;
    }

    /* Increment to the next position in the packet. */
    packetIndex++;
    payloadIndex++;
  }

  return dataIndex;
}


/**
 * \brief Move a user defined storage into the payload part of a packet.
 *
 * \param[in] packet The address of a full RapidIO packet including headers.
 * \param[in] payloadOffset The word offset where the payload starts in the packet.
 * \param[in] dataOffset The byte offset in the first word to start copy data to.
 * \param[in] dataSize The number of bytes to copy.
 * \param[in] data The buffer to store in the packet.
 */
static uint8_t setPacketPayload(uint32_t packet[], const uint16_t payloadOffset, const uint16_t dataOffset, 
                                const uint16_t dataSize, const uint8_t data[])
{
  uint16_t crc = 0xffffu;
  uint32_t content = 0u;
  uint16_t packetIndex;
  uint16_t payloadIndex;
  uint16_t dataIndex;


  ASSERT(packet != NULL, "Invalid packet pointer");
  ASSERT(data != NULL, "Invalid data pointer");


  /***************************************************
   * Calculate the CRC for the packet header.
   ***************************************************/
  for(packetIndex = 0u; packetIndex < payloadOffset; packetIndex += 4u)
  {
    crc = RIOPACKET_crc32(packet[packetIndex>>2], crc);
  }

  /***************************************************
   * Pad the data before the actual data is written.
   ***************************************************/
  payloadIndex = 0u;
  while(payloadIndex < dataOffset)
  {
    content <<= 8;

    if((packetIndex & 0x3u) == 3u)
    {
      crc = RIOPACKET_crc32(content, crc);
      packet[packetIndex>>2] = content;
    }

    payloadIndex++;
    packetIndex++;
  }

  /***************************************************
   * Write content and any embedded CRC.
   ***************************************************/
  dataIndex = 0u;
  while(dataIndex < dataSize)
  {
    content <<= 8;

    /* Check if CRC or content should be entered into the packet. */
    if(packetIndex == 80u)
    {
      /* CRC MSB. */
      content |= ((uint32_t) crc) >> 8;
    }
    else if(packetIndex == 81u)
    {
      /* CRC LSB. */
      content |= (uint32_t) (((uint32_t) crc) & 0xfful);
    }
    else
    {
      /* Data content. */
      content |= data[dataIndex];
      dataIndex++;
      payloadIndex++;
    }

    if((packetIndex & 0x3u) == 3u)
    {
      crc = RIOPACKET_crc32(content, crc);
      packet[packetIndex>>2] = content;
    }

    packetIndex++;
  }

  /***************************************************
   * Pad the data to an even double word.
   ***************************************************/
  while((payloadIndex & 0x7u) != 0u)
  {
    content <<= 8;

    if((packetIndex & 0x3u) == 3u)
    {
      crc = RIOPACKET_crc32(content, crc);
      packet[packetIndex>>2] = content;
    }

    packetIndex++;
    payloadIndex++;
  }

  /***************************************************
   * Write the CRC into the packet.
   ***************************************************/
  if((packetIndex & 0x3u) == 0u)
  {
    /* crc(15:0)|pad(15:0) */
    content = ((uint32_t) crc) << 16;
  }
  else
  {
    /* double-wordN-LSB|crc(15:0) */
    content &= (uint32_t) 0x0000fffful;
    crc = RIOPACKET_crc16((uint16_t) content, crc);
    content <<= 16;
    content |= crc;
  }
  packet[packetIndex>>2] = content;

  return (uint8_t) ((packetIndex>>2) + 1u);
}



/**
 * \brief Calculate the rdsize and wdptr values used in NREAD from an address and a size.
 *
 * \param[in] address An address to use when calculating rdsize.
 * \param[in] size A size to use when calculating rdsize.
 *
 * \returns Returns wdptr and rdsize in a combined 16-bit half-word where 
 * wdptr is in the lower byte and the rdsize in the high byte.
 *
 * \note See the RapidIO 3.1, part1, table 4-3 for details.
 */
static uint16_t rdsizeGet(const uint32_t address, const uint16_t size)
{
  uint16_t returnValue;
  uint8_t wdptr;
  uint8_t rdsize;


  switch(size/8u)
  {
    case 0:
      /**************************************************************
       * Sub double-word access.
       **************************************************************/
      switch(size%8u)
      {
        case 0:
          /* Not supported by protocol. */
          wdptr = 0xffu;
          rdsize = 0xffu;
          break;

        case 1:
          /* Reading one byte. */
          /* Any address is allowed. */
          wdptr = (uint8_t) (address >> 2);
          wdptr &= (uint8_t) 0x1;
          rdsize = (uint8_t) (address & 0x3ul);
          break;

        case 2:
          /* Reading two bytes. */
          /* Address 0, 2, 4, 6 are valid. */
          if((address & 0x1ul) == 0u)
          {
            wdptr = (uint8_t) (address >> 2);
            wdptr &= (uint8_t) 0x1;
            rdsize = (uint8_t) ((address & 0x7ul) | 0x4ul);
          }
          else
          {
            /* Not supported by protocol. */
            wdptr = 0xffu;
            rdsize = 0xffu;
          }
          break;

        case 3:
          /* Reading 3 bytes. */
          /* Address 0 and 5 are valid. */
          if(((address & 0x7ul) == 0u) ||
             ((address & 0x7ul) == 5u))
          {
            wdptr = (uint8_t) (address >> 2);
            wdptr &= (uint8_t) 0x1;
            rdsize = 0x5u;
          }
          else
          {
            /* Not supported by protocol. */
            wdptr = 0xffu;
            rdsize = 0xffu;
          }
          break;

        case 4:
          /* Reading 4 bytes. */
          /* Address 0 and 4 are valid. */
          if(((address & 0x7ul) == 0u) ||
             ((address & 0x7ul) == 4u))
          {
            wdptr = (uint8_t) (address >> 2);
            wdptr &= (uint8_t) 0x1;
            rdsize = 0x8u;
          }
          else
          {
            /* Not supported by protocol. */
            wdptr = 0xffu;
            rdsize = 0xffu;
          }
          break;

        case 5:
          /* Reading 5 bytes. */
          /* Address 0 and 3 are valid. */
          if(((address & 0x7ul) == 0u) ||
             ((address & 0x7ul) == 3u))
          {
            wdptr = (uint8_t) (address >> 1);
            wdptr &= (uint8_t) 0x1;
            rdsize = 0x7u;
          }
          else
          {
            /* Not supported by protocol. */
            wdptr = 0xffu;
            rdsize = 0xffu;
          }
          break;

        case 6:
          /* Reading 6 bytes. */
          /* Addresses 0 and 2 are valid. */
          if(((address & 0x7ul) == 0u) ||
             ((address & 0x7ul) == 2u))
          {
            wdptr = (uint8_t) (address >> 1);
            wdptr &= (uint8_t) 0x1;
            rdsize = 0x9u;
          }
          else
          {
            /* Not supported by protocol. */
            wdptr = 0xffu;
            rdsize = 0xffu;
          }
          break;

        default:
          /* Reading 7 bytes. */
          /* Addresses 0 and 1 are valid. */
          if(((address & 0x7ul) == 0u) ||
             ((address & 0x7ul) == 1u))
          {
            wdptr = (uint8_t) (address & 0x1ul);
            rdsize = 0xau;
          }
          else
          {
            /* Not supported by protocol. */
            wdptr = 0xffu;
            rdsize = 0xffu;
          }
          break;
      }
      break;

    case 1:
      /* Reading 8 bytes. */
      /* Only even double-word address are valid. */
      if((address % 8ul) == 0ul)
      {
        wdptr = 0u;
        rdsize = 0xbu;
      }
      else
      {
        /* Not supported by protocol. */
        wdptr = 0xffu;
        rdsize = 0xffu;
      }
      break;

    case 2:
      /* Reading 16 bytes max. */
      /* Only even double-word address are valid. */
      if((address % 8ul) == 0ul)
      {
        wdptr = 1u;
        rdsize = 0xbu;
      }
      else
      {
        /* Not supported by protocol. */
        wdptr = 0xffu;
        rdsize = 0xffu;
      }
      break;

    case 3:
      /* Not supported by protocol. */
      wdptr = 0xffu;
      rdsize = 0xffu;
      break;

    case 4:
      /* Reading 32 bytes max. */
      /* Only even double-word address are valid. */
      if((address & 0x7ul) == 0ul)
      {
        wdptr = 0u;
        rdsize = 0xcu;
      }
      else
      {
        /* Not supported by protocol. */
        wdptr = 0xffu;
        rdsize = 0xffu;
      }
      break;

    case 5:
    case 6:
    case 7:
      /* Not supported by protocol. */
      wdptr = 0xffu;
      rdsize = 0xffu;
      break;

    case 8:
      /* Reading 64 bytes max. */
      /* Only even double-word address are valid. */
      if((address & 0x7ul) == 0ul)
      {
        wdptr = 1u;
        rdsize = 0xcu;
      }
      else
      {
        /* Not supported by protocol. */
        wdptr = 0xffu;
        rdsize = 0xffu;
      }
      break;

    case 9:
    case 10:
    case 11:
      /* Not supported by protocol. */
      wdptr = 0xffu;
      rdsize = 0xffu;
      break;

    case 12:
      /* Reading 96 bytes. */
      /* Only even double-word address are valid. */
      if((address & 0x7ul) == 0ul)
      {
        wdptr = 0u;
        rdsize = 0xdu;
      }
      else
      {
        /* Not supported by protocol. */
        wdptr = 0xffu;
        rdsize = 0xffu;
      }
      break;

    case 13:
    case 14:
    case 15:
      /* Not supported by protocol. */
      wdptr = 0xffu;
      rdsize = 0xffu;
      break;

    case 16:
      /* Reading 128 bytes max. */
      /* Only even double-word address are valid. */
      if((address & 0x7ul) == 0ul)
      {
        wdptr = 1u;
        rdsize = 0xdu;
      }
      else
      {
        /* Not supported by protocol. */
        wdptr = 0xffu;
        rdsize = 0xffu;
      }
      break;

    case 17:
    case 18:
    case 19:
      /* Not supported by protocol. */
      wdptr = 0xffu;
      rdsize = 0xffu;
      break;

    case 20:
      /* Reading 160 bytes. */
      /* Only even double-word address are valid. */
      if((address & 0x7ul) == 0ul)
      {
        wdptr = 0u;
        rdsize = 0xeu;
      }
      else
      {
        /* Not supported by protocol. */
        wdptr = 0xffu;
        rdsize = 0xffu;
      }
      break;

    case 21:
    case 22:
    case 23:
      /* Not supported by protocol. */
      wdptr = 0xffu;
      rdsize = 0xffu;
      break;

    case 24:
      /* Reading 192 bytes. */
      /* Only even double-word address are valid. */
      if((address & 0x7ul) == 0ul)
      {
        wdptr = 1u;
        rdsize = 0xeu;
      }
      else
      {
        /* Not supported by protocol. */
        wdptr = 0xffu;
        rdsize = 0xffu;
      }
      break;

    case 25:
    case 26:
    case 27:
      /* Not supported by protocol. */
      wdptr = 0xffu;
      rdsize = 0xffu;
      break;

    case 28:
      /* Reading 224 bytes. */
      /* Only even double-word address are valid. */
      if((address & 0x7ul) == 0u)
      {
        wdptr = 0u;
        rdsize = 0xfu;
      }
      else
      {
        /* Not supported by protocol. */
        wdptr = 0xffu;
        rdsize = 0xffu;
      }
      break;

    case 29:
    case 30:
    case 31:
      /* Not supported by protocol. */
      wdptr = 0xffu;
      rdsize = 0xffu;
      break;

    case 32:
      /* Reading 256 bytes max. */
      /* Only even double-word address are valid. */
      if((address & 0x7ul) == 0ul)
      {
        wdptr = 1u;
        rdsize = 0xfu;
      }
      else
      {
        /* Not supported by protocol. */
        wdptr = 0xffu;
        rdsize = 0xffu;
      }
      break;

    default:
      /* Not supported by protocol. */
      wdptr = 0xffu;
      rdsize = 0xffu;
      break;
  }

  returnValue = ((uint16_t) rdsize) << 8;
  returnValue |= (uint16_t) wdptr;
  
  return returnValue;
}


/**
 * \brief Calculate an offset and a size based on rdsize and wdptr values.
 *
 * \param[in] rdsize An rdsize with a value from the standard.
 * \param[in] wdptr An wdptr with a value from the standard.
 * \param[out] offset A byte offset into a double-word corresponding to (rdsize,wdptr).
 * \param[out] size The number of bytes  corresponding to (rdsize,wdptr).
 *
 * \note See the RapidIO 3.1, part1, table 4-3 for details.
 */
static void rdsizeToOffset(uint8_t rdsize, uint8_t wdptr, uint8_t *offset, uint16_t *size)
{
  ASSERT(offset != NULL, "Invalid offset pointer");
  ASSERT(size != NULL, "Invalid size pointer");

  switch(rdsize)
  {
    case 0:
    case 1:
    case 2:
    case 3:
      *offset = (uint8_t) (wdptr << 2);
      *offset |= rdsize;
      *size = 1u;
      break;

    case 4:
    case 6:
      *offset = (uint8_t) (wdptr << 2);
      *offset |= rdsize & 0x02u;
      *size = 2u;
      break;

    case 5:
      *offset = (uint8_t) (wdptr * 5u);
      *size = 3u;
      break;

    case 8:
      *offset = (uint8_t) (wdptr * 4u);
      *size = 4u;
      break;

    case 7:
      *offset = (uint8_t) (wdptr * 3u);
      *size = 5u;
      break;

    case 9:
      *offset = (uint8_t) (wdptr * 2u);
      *size = 6u;
      break;

    case 10:
      *offset = (uint8_t) (wdptr * 1u);
      *size = 7u;
      break;

    case 11:
      *offset = 0u;
      *size = 8u + (8u*((uint16_t) wdptr));
      break;

    case 12:
      *offset = 0u;
      *size = 32u + (32u*((uint16_t) wdptr));
      break;

    case 13:
      *offset = 0u;
      *size = 96u + (32u*((uint16_t) wdptr));
      break;
 
    case 14:
      *offset = 0u;
      *size = 160u + (32u*((uint16_t) wdptr));
      break;

    default:
      *offset = 0u;
      *size = 224u + (32u*((uint16_t) wdptr));
      break;
  }
}


/**
 * \brief Calculate the wrsize and wdptr values used in NWRITE/NWRITER from an address and a size.
 *
 * \param[in] address An address to use when calculating rdsize.
 * \param[in] size A size to use when calculating rdsize.
 *
 * \returns Returns wdptr and wrsize in a combined 16-bit half-word where 
 * wdptr is in the lower byte and the wrsize in the high byte.
 *
 * \note See the RapidIO 3.1, part1, table 4-4 for details.
 */
static uint16_t wrsizeGet(const uint32_t address, const uint16_t size)
{
  uint16_t returnValue;
  uint8_t wdptr;
  uint8_t wrsize;


  switch(size/8u)
  {
    case 0:
      /**************************************************************
       * Sub double-word access.
       **************************************************************/
      switch(size%8u)
      {
        case 0:
          /* Not supported by protocol. */
          wdptr = 0xffu;
          wrsize = 0xffu;
          break;

        case 1:
          /* Writing one byte. */
          /* Any address is allowed. */
          wdptr = (uint8_t) (address >> 2);
          wdptr &= (uint8_t) 0x1;
          wrsize = (uint8_t) (address & 0x3ul);
          break;

        case 2:
          /* Writing two bytes. */
          /* Address 0, 2, 4, 6 are valid. */
          if((address & 0x1ul) == 0ul)
          {
            wdptr = (uint8_t) (address >> 2);
            wdptr &= (uint8_t) 0x1;
            wrsize = (uint8_t) ((address & 0x7ul) | 0x4ul);
          }
          else
          {
            /* Not supported by protocol. */
            wdptr = 0xffu;
            wrsize = 0xffu;
          }
          break;

        case 3:
          /* Writing 3 bytes. */
          /* Address 0 and 5 are valid. */
          if(((address & 0x7ul) == 0ul) ||
             ((address & 0x7ul) == 5ul))
          {
            wdptr = (uint8_t) (address >> 2);
            wdptr &= (uint8_t) 0x1;
            wrsize = 0x5u;
          }
          else
          {
            /* Not supported by protocol. */
            wdptr = 0xffu;
            wrsize = 0xffu;
          }
          break;

        case 4:
          /* Writing 4 bytes. */
          /* Address 0 and 4 are valid. */
          if(((address & 0x7ul) == 0u) ||
             ((address & 0x7ul) == 4u))
          {
            wdptr = (uint8_t) (address >> 2);
            wdptr &= (uint8_t) 0x1;
            wrsize = 0x8u;
          }
          else
          {
            /* Not supported by protocol. */
            wdptr = 0xffu;
            wrsize = 0xffu;
          }
          break;

        case 5:
          /* Writing 5 bytes. */
          /* Address 0 and 3 are valid. */
          if(((address & 0x7ul) == 0ul) ||
             ((address & 0x7ul) == 3ul))
          {
            wdptr = (uint8_t) (address >> 1);
            wdptr &= (uint8_t) 0x1;
            wrsize = 0x7u;
          }
          else
          {
            /* Not supported by protocol. */
            wdptr = 0xffu;
            wrsize = 0xffu;
          }
          break;

        case 6:
          /* Writing 6 bytes. */
          /* Addresses 0 and 2 are valid. */
          if(((address & 0x7ul) == 0ul) ||
             ((address & 0x7ul) == 2ul))
          {
            wdptr = (uint8_t) (address >> 1);
            wdptr &= (uint8_t) 0x1;
            wrsize = 0x9u;
          }
          else
          {
            /* Not supported by protocol. */
            wdptr = 0xffu;
            wrsize = 0xffu;
          }
          break;

        default:
          /* Writing 7 bytes. */
          /* Addresses 0 and 1 are valid. */
          if(((address & 0x7ul) == 0ul) ||
             ((address & 0x7ul) == 1ul))
          {
            wdptr = (uint8_t) (address & 0x1ul);
            wrsize = 0xau;
          }
          else
          {
            /* Not supported by protocol. */
            wdptr = 0xffu;
            wrsize = 0xffu;
          }
          break;
      }
      break;
    case 1:
      /* Writing 8 bytes. */
      /* Only even double-word address are valid. */
      if((address % 8ul) == 0ul)
      {
        wdptr = 0u;
        wrsize = 0xbu;
      }
      else
      {
        /* Not supported by protocol. */
        wdptr = 0xffu;
        wrsize = 0xffu;
      }
      break;

    case 2:
      /* Writing 16 bytes max. */
      /* Only even double-word address are valid. */
      if((address % 8ul) == 0ul)
      {
        wdptr = 1u;
        wrsize = 0xbu;
      }
      else
      {
        /* Not supported by protocol. */
        wdptr = 0xffu;
        wrsize = 0xffu;
      }
      break;

    case 3:
    case 4:
      /* Writing 32 bytes max. */
      /* Only even double-word address are valid. */
      if((address & 0x7ul) == 0ul)
      {
        wdptr = 0u;
        wrsize = 0xcu;
      }
      else
      {
        /* Not supported by protocol. */
        wdptr = 0xffu;
        wrsize = 0xffu;
      }
      break;

    case 5:
    case 6:
    case 7:
    case 8:
      /* Writing 64 bytes max. */
      /* Only even double-word address are valid. */
      if((address & 0x7ul) == 0ul)
      {
        wdptr = 1u;
        wrsize = 0xcu;
      }
      else
      {
        /* Not supported by protocol. */
        wdptr = 0xffu;
        wrsize = 0xffu;
      }
      break;

    case 9:
    case 10:
    case 11:
    case 12:
    case 13:
    case 14:
    case 15:
    case 16:
      /* Writing 128 bytes max. */
      /* Only even double-word address are valid. */
      if((address & 0x7ul) == 0ul)
      {
        wdptr = 1u;
        wrsize = 0xdu;
      }
      else
      {
        /* Not supported by protocol. */
        wdptr = 0xffu;
        wrsize = 0xffu;
      }
      break;

    case 17:
    case 18:
    case 19:
    case 20:
    case 21:
    case 22:
    case 23:
    case 24:
    case 25:
    case 26:
    case 27:
    case 28:
    case 29:
    case 30:
    case 31:
    case 32:
      /* Writing 256 bytes max. */
      /* Only even double-word address are valid. */
      if((address & 0x7ul) == 0ul)
      {
        wdptr = 1u;
        wrsize = 0xfu;
      }
      else
      {
        /* Not supported by protocol. */
        wdptr = 0xffu;
        wrsize = 0xffu;
      }
      break;

    default:
      /* Not supported by protocol. */
      wdptr = 0xffu;
      wrsize = 0xffu;
      break;
  }
  
  returnValue = ((uint16_t) wrsize) << 8;
  returnValue |= (uint16_t) wdptr;
  
  return returnValue;
}


/**
 * \brief Calculate an offset and a size based on wrsize and wdptr values.
 *
 * \param[in] wrsize An wrsize with a value from the standard.
 * \param[in] wdptr An wdptr with a value from the standard.
 * \param[out] offset A byte offset into a double-word corresponding to (wrsize,wdptr).
 * \param[out] size The number of bytes  corresponding to (wrsize,wdptr).
 *
 * \note See the RapidIO 3.1, part1, table 4-4 for details.
 */
static void wrsizeToOffset(uint8_t wrsize, uint8_t wdptr, uint8_t *offset, uint16_t *size)
{
  ASSERT(offset != NULL, "Invalid offset pointer");
  ASSERT(size != NULL, "Invalid size pointer");

  switch(wrsize)
  {
    case 0:
    case 1:
    case 2:
    case 3:
      *offset = (uint8_t) (wdptr << 2);
      *offset |= wrsize;
      *size = 1u;
      break;

    case 4:
    case 6:
      *offset = (uint8_t) (wdptr << 2);
      *offset |= wrsize & 0x02u;
      *size = 2u;
      break;

    case 5:
      *offset = (uint8_t) (wdptr * 5u);
      *size = 3u;
      break;

    case 7:
      *offset = (uint8_t) (wdptr * 3u);
      *size = 5u;
      break;

    case 8:
      *offset = (uint8_t) (wdptr * 4u);
      *size = 4u;
      break;

    case 9:
      *offset = (uint8_t) (wdptr * 2u);
      *size = 6u;
      break;

    case 10:
      *offset = (uint8_t) (wdptr * 1u);
      *size = 7u;
      break;

    case 11:
      *offset = 0u;
      *size = 8u + (8u*((uint16_t) wdptr));
      break;

    case 12:
      *offset = 0u;
      *size = 32u + (32u*((uint16_t) wdptr));
      break;

    case 13:
      *offset = 0u;
      *size = 128u*((uint16_t) wdptr);
      break;

    case 14:
      *offset = 0u;
      *size = 0u;
      break;

    default:
      *offset = 0u;
      *size = 256u*((uint16_t) wdptr);
      break;
  }
}

/*************************** end of file **************************************/
