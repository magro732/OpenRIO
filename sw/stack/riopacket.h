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
 * This file contains an abstraction of a RapidIO packet.
 * It is used in the SW RapidIO stack, riostack.c, but can also be used
 * stand-alone together with other software, for example to tunnel RapidIO
 * packet over an arbitrary network.
 * More details about the usage can be found in the module tests in
 * test_riopacket.c.
 * The same convension as in the RapidIO standard has been used when naming 
 * bit-sizes, i.e. double-word=64-bit, word=32-bit, half-word=16-bit, 
 * byte=8-bit.
 *
 * Any application specific tailoring needed to compile properly should be done 
 * in rioconfig.h.
 *
 * To Do:
 * - Add packet handlers for 8-bit deviceIds.
 ******************************************************************************/

#ifndef __RIOPACKET_H
#define __RIOPACKET_H

/*******************************************************************************
 * Include files.
 *******************************************************************************/

#include <stdint.h>


/*******************************************************************************
 * Global macros.
 *******************************************************************************/

/* The maximum size of a RapidIO packet in words (32-bit). */
#define RIOPACKET_SIZE_MIN ((uint8_t) 3u)
#define RIOPACKET_SIZE_MAX ((uint8_t) 69u)

/* Configuration space offsets. */
typedef enum
{
  RIOPACKET_DEVICE_IDENTITY_CAR=((uint32_t)0x00000000ul),
  RIOPACKET_DEVICE_INFORMATION_CAR=((uint32_t)0x00000004ul),
  RIOPACKET_ASSEMBLY_IDENTITY_CAR=((uint32_t)0x00000008ul),
  RIOPACKET_ASSEMBLY_INFORMATION_CAR=((uint32_t)0x0000000cul),
  RIOPACKET_PROCESSING_ELEMENT_FEATURES_CAR=((uint32_t)0x00000010ul),
  RIOPACKET_SWITCH_PORT_INFORMATION_CAR=((uint32_t)0x00000014ul),
  RIOPACKET_SOURCE_OPERATIONS_CAR=((uint32_t)0x00000018ul),
  RIOPACKET_DESTINATION_OPERATIONS_CAR=((uint32_t)0x0000001cul),
  RIOPACKET_SWITCH_ROUTE_TABLE_DESTINATION_ID_LIMIT_CAR=((uint32_t)0x00000034ul),
  RIOPACKET_PROCESSING_ELEMENT_LOGICAL_LAYER_CONTROL_CSR=((uint32_t)0x0000004cul),
  RIOPACKET_BASE_DEVICE_ID_CSR=((uint32_t)0x00000060ul),
  RIOPACKET_HOST_BASE_DEVICE_ID_LOCK_CSR=((uint32_t)0x00000068ul),
  RIOPACKET_COMPONENT_TAG_CSR=((uint32_t)0x0000006cul),
  RIOPACKET_STANDARD_ROUTE_CONFIGURATION_DESTINATION_ID_SELECT_CSR=((uint32_t)0x00000070ul),
  RIOPACKET_STANDARD_ROUTE_CONFIGURATION_PORT_SELECT_CSR=((uint32_t)0x00000074ul),
  RIOPACKET_STANDARD_ROUTE_DEFAULT_PORT_CSR=((uint32_t)0x00000078ul),
  RIOPACKET_EXTENDED_FEATURES_OFFSET=((uint32_t)0x00000100ul),
  RIOPACKET_IMPLEMENTATION_DEFINED_OFFSET=((uint32_t)0x00010000ul)
} RioPacketConfigSpaceOffset_t;

#define RIOPACKET_LP_SERIAL_REGISTER_BLOCK_HEADER(offset) (offset)
#define RIOPACKET_PORT_LINK_TIMEOUT_CONTROL_CSR(offset) ((offset) + 0x00000020ul)
#define RIOPACKET_PORT_RESPONSE_TIMEOUT_CONTROL_CSR(offset) ((offset) + 0x00000024ul)
#define RIOPACKET_PORT_GENERAL_CONTROL_CSR(offset) ((offset) + 0x0000003cul)
#define RIOPACKET_PORT_N_LOCAL_ACKID_CSR(offset, n) ((offset) + (0x00000048ul+((n)*0x00000020ul)))
#define RIOPACKET_PORT_N_ERROR_AND_STATUS_CSR(offset, n) ((offset) + (0x00000058ul+((n)*0x00000020ul)))
#define RIOPACKET_PORT_N_CONTROL_CSR(offset, n) ((offset) + (0x0000005cul+((n)*0x00000020ul)))

#define RIOPACKET_ERROR_MANAGEMENT_EXTENSIONS_BLOCK_HEADER(offset) (offset)
#define RIOPACKET_ERROR_MANAGEMENT_HOT_SWAP_EXTENSIONS_BLOCK_CAR(offset) ((offset)+0x4ul)
#define RIOPACKET_PORT_WRITE_TARGET_DEVICE_ID_CSR(offset) ((offset)+0x28ul)
#define RIOPACKET_PORT_WRITE_TRANSMISSION_CONTROL_CSR(offset) ((offset)+0x34ul)
#define RIOPACKET_PORT_N_ERROR_DETECT_CSR(offset, port) ((offset)+(0x40ul*(port)))
#define RIOPACKET_PORT_N_ERROR_RATE_ENABLE_CSR(offset, port) ((offset)+(0x44ul*(port)))

/** @note Deprecated constants. Use the RIOPACKET_ prefixed constants instead. */
#define DEVICE_IDENTITY_CAR ((uint32_t)0x00000000ul)
#define DEVICE_INFORMATION_CAR ((uint32_t)0x00000004ul)
#define ASSEMBLY_IDENTITY_CAR ((uint32_t)0x00000008ul)
#define ASSEMBLY_INFORMATION_CAR ((uint32_t)0x0000000cul)
#define PROCESSING_ELEMENT_FEATURES_CAR ((uint32_t)0x00000010ul)
#define SWITCH_PORT_INFORMATION_CAR ((uint32_t)0x00000014ul)
#define SOURCE_OPERATIONS_CAR ((uint32_t)0x00000018ul)
#define DESTINATION_OPERATIONS_CAR ((uint32_t)0x0000001cul)
#define SWITCH_ROUTE_TABLE_DESTINATION_ID_LIMIT_CAR ((uint32_t)0x00000034ul)
#define PROCESSING_ELEMENT_LOGICAL_LAYER_CONTROL_CSR ((uint32_t)0x0000004cul)
#define BASE_DEVICE_ID_CSR ((uint32_t)0x00000060ul)
#define HOST_BASE_DEVICE_ID_LOCK_CSR ((uint32_t)0x00000068ul)
#define COMPONENT_TAG_CSR ((uint32_t)0x0000006cul)
#define STANDARD_ROUTE_CONFIGURATION_DESTINATION_ID_SELECT_CSR ((uint32_t)0x00000070ul)
#define STANDARD_ROUTE_CONFIGURATION_PORT_SELECT_CSR ((uint32_t)0x00000074ul)
#define STANDARD_ROUTE_DEFAULT_PORT_CSR ((uint32_t)0x00000078ul)
#define EXTENDED_FEATURES_OFFSET ((uint32_t)0x00000100ul)
#define IMPLEMENTATION_DEFINED_OFFSET ((uint32_t)0x00010000ul)
#define LP_SERIAL_REGISTER_BLOCK_HEADER(offset) (offset)
#define PORT_LINK_TIMEOUT_CONTROL_CSR(offset) ((offset) + 0x00000020ul)
#define PORT_RESPONSE_TIMEOUT_CONTROL_CSR(offset) ((offset) + 0x00000024ul)
#define PORT_GENERAL_CONTROL_CSR(offset) ((offset) + 0x0000003cul)
#define PORT_N_LOCAL_ACKID_CSR(offset, n) ((offset) + (0x00000048ul+((n)*0x00000020ul)))
#define PORT_N_ERROR_AND_STATUS_CSR(offset, n) ((offset) + (0x00000058ul+((n)*0x00000020ul)))
#define PORT_N_CONTROL_CSR(offset, n) ((offset) + (0x0000005cul+((n)*0x00000020ul)))
#define ERROR_MANAGEMENT_EXTENSIONS_BLOCK_HEADER(offset) (offset)
#define ERROR_MANAGEMENT_HOT_SWAP_EXTENSIONS_BLOCK_CAR(offset) ((offset)+0x4ul)
#define PORT_WRITE_TARGET_DEVICE_ID_CSR(offset) ((offset)+0x28ul)
#define PORT_WRITE_TRANSMISSION_CONTROL_CSR(offset) ((offset)+0x34ul)
#define PORT_N_ERROR_DETECT_CSR(offset, port) ((offset)+(0x40ul*(port)))
#define PORT_N_ERROR_RATE_ENABLE_CSR(offset, port) ((offset)+(0x44ul*(port)))

/* Packet tt constants (2-bits). */
typedef enum
{
  RIOPACKET_TT_16BITS = 1
} RioPacketTt_t;

/* Packet ftype constants (4-bits). */
typedef enum
{
  RIOPACKET_FTYPE_REQUEST=0x2,
  RIOPACKET_FTYPE_WRITE=0x5,
  RIOPACKET_FTYPE_MAINTENANCE=0x8,
  RIOPACKET_FTYPE_DOORBELL=0xa,
  RIOPACKET_FTYPE_MESSAGE=0xb,
  RIOPACKET_FTYPE_RESPONSE=0xd
} RioPacketFtype_t;

/* Maintenance packet transaction types. */
typedef enum
{
  RIOPACKET_TRANSACTION_MAINT_READ_REQUEST=0ul,
  RIOPACKET_TRANSACTION_MAINT_WRITE_REQUEST=1ul,
  RIOPACKET_TRANSACTION_MAINT_READ_RESPONSE=2ul,
  RIOPACKET_TRANSACTION_MAINT_WRITE_RESPONSE=3ul,
  RIOPACKET_TRANSACTION_MAINT_PORT_WRITE_REQUEST=4ul
} RioPacketTransactionMaint_t;

/* Write packet transaction types. */
typedef enum
{
  RIOPACKET_TRANSACTION_WRITE_NWRITE=4ul,
  RIOPACKET_TRANSACTION_WRITE_NWRITER=5ul
} RioPacketTransactionWrite_t;

/* Request packet transaction types. */
typedef enum
{
  RIOPACKET_TRANSACTION_REQUEST_NREAD=4ul
} RioPacketTransactionRequest_t;

/* Response packet transaction types. */
typedef enum
{
  RIOPACKET_TRANSACTION_RESPONSE_NO_PAYLOAD=0ul,
  RIOPACKET_TRANSACTION_RESPONSE_MESSAGE_RESPONSE=1ul,
  RIOPACKET_TRANSACTION_RESPONSE_WITH_PAYLOAD=8ul
} RioPacketTransactionResponse_t;

/* Response status constants. */
typedef enum
{
  RIOPACKET_RESPONSE_STATUS_DONE=0ul,
  RIOPACKET_RESPONSE_STATUS_RETRY=3ul,
  RIOPACKET_RESPONSE_STATUS_ERROR=7ul
} RioPacketResponseStatus_t;


/*******************************************************************************
 * Global typedefs
 *******************************************************************************/

/* The structure containing a RapidIO packet. */
typedef struct
{
  /* Size in words. */
  uint8_t size;
  uint32_t payload[RIOPACKET_SIZE_MAX];
} RioPacket_t;



/*******************************************************************************
 * Global function prototypes
 *******************************************************************************/

/**
 * \brief Initialize a packet to an empty packet.
 *
 * \param[in] packet The packet to operate on.
 *
 * This function sets the size of a packet to zero. 
 *
 * \note Any previous content is NOT purged.
 */
void RIOPACKET_init(RioPacket_t *packet);


/**
 * \brief Return the size of a packet.
 *
 * \param[in] packet The packet to operate on.
 * \return The size of the packet.
 *
 * This function gets the size of a packet in words (32-bit).
 */
uint8_t RIOPACKET_size(const RioPacket_t *packet);


/**
 * \brief Append data to a packet.
 *
 * \param[in] packet The packet to operate on.
 * \param[in] word The word to append.
 *
 * This function appends a specificed word (32-bit) to the end of a packet.
 */
void RIOPACKET_append(RioPacket_t *packet, uint32_t word);


/**
 * \brief Check if a packet is a valid RapidIO packet.
 *
 * \param[in] packet The packet to operate on.
 * \return The return value is zero if not ok, non-zero otherwise.
 *
 * This function checks if a packet has a correct length and a correct CRC. 
 * Both the embedded crc and the trailing crc are checked. 
 */
uint8_t RIOPACKET_valid(const RioPacket_t *packet);


/**
 * \brief Convert (serializes) a packet into an array of bytes.
 *
 * \param[in] packet The packet to operate on.
 * \param[in] size The size of the buffer to write to.
 * \param[out] buffer The address to write the result to.
 * \return The number of bytes that were written. The value 0 will be returned if the 
 * serialized buffer does not fit into the provided buffer.
 *
 * This function serializes a packet into an array of bytes that can be transfered on 
 * a transmission channel.
 */
uint16_t RIOPACKET_serialize(const RioPacket_t *packet, const uint16_t size, uint8_t *buffer);


/**
 * \brief Convert (deserializes) an array of bytes to a packet.
 *
 * \param[in] packet The packet to operate on.
 * \param[in] size The size of the buffer to read from.
 * \param[in] buffer The address to read from.
 * \return The number of words contained in the resulting packet. The value 0 is 
 * returned if the deserialization was unsuccessfull.
 *
 * This function deserializes a packet from a byte array that was previously created
 * by RIOPACKET_serialize().
 *
 * \note It is recommended to use RIOPACKET_valid() to verify the integrity of the packet 
 * once it has been deserialized.
 */
uint8_t RIOPACKET_deserialize(RioPacket_t *packet, const uint16_t size, const uint8_t *buffer);


/**
 * \brief Return the ftype of a packet.
 *
 * \param[in] packet The packet to operate on.
 * \return The ftype of the packet.
 *
 * This function gets the ftype of a packet.
 */
uint8_t RIOPACKET_getFtype(const RioPacket_t *packet);


/**
 * \brief Return the destination deviceId of a packet.
 *
 * \param[in] packet The packet to operate on.
 * \return The destination deviceId of the packet.
 *
 * This function gets the destination deviceId of a packet.
 */
uint16_t RIOPACKET_getDestination(const RioPacket_t *packet);


/**
 * \brief Return the source deviceId of a packet.
 *
 * \param[in] packet The packet to operate on.
 * \return The source deviceId of the packet.
 *
 * This function gets the source deviceId of a packet.
 */
uint16_t RIOPACKET_getSource(const RioPacket_t *packet);


/**
 * \brief Return the transaction of a packet.
 *
 * \param[in] packet The packet to operate on.
 * \return The transaction of the packet.
 *
 * This function gets the transaction field of a packet.
 *
 * \note Not all packets contain a transaction field.
 */
uint8_t RIOPACKET_getTransaction(const RioPacket_t *packet);


/**
 * \brief Return the transaction identifier of a packet.
 *
 * \param[in] packet The packet to operate on.
 * \return The transaction identifier of the packet.
 *
 * This function gets the transaction identifier field of a packet.
 *
 * \note Not all packets contain a transaction identifier field.
 */
uint8_t RIOPACKET_getTid(const RioPacket_t *packet);

/**
 * \brief Set the packet to contain a maintenance read request.
 *
 * \param[in] packet The packet to operate on.
 * \param[in] dstId The deviceId to use as destination in the packet.
 * \param[in] srcId The deviceId to use as source in the packet.
 * \param[in] hop The hop_count to set in the packet.
 * \param[in] tid The transaction identifier to set in the packet.
 * \param[in] offset The byte address (24-bit) in the configuration space to read.
 *
 * This function sets the content of a packet to a maintenance read request packet containing 
 * a request to read one word in configuration space.
 *
 */
void RIOPACKET_setMaintReadRequest(RioPacket_t *packet,
                                   uint16_t dstId, uint16_t srcId, uint8_t hop,
                                   uint8_t tid, uint32_t offset);


/**
 * \brief Get entries from a maintenance read request.
 *
 * \param[in] packet The packet to operate on.
 * \param[out] dstId The destination deviceId in this packet.
 * \param[out] srcId The source deviceId in this packet.
 * \param[out] hop The hop_count in this packet.
 * \param[out] tid The transaction id to be returned in the response to this request.
 * \param[out] offset The byte address (24-bit) in the configuration space to read.
 *
 * This function returns the content of a packet as if it contained a maintenance read
 * request packet. 
 *
 * \note Use the ftype and transaction fields to see if the packet is indeed a 
 * maintenance read request.
 * \note If the packet does not contain a maintenance read request, the result 
 * will be undefined.
 */
void RIOPACKET_getMaintReadRequest(const RioPacket_t *packet,
                                   uint16_t *dstId, uint16_t *srcId, uint8_t *hop,
                                   uint8_t *tid, uint32_t *offset);


/**
 * \brief Set the packet to contain a maintenance read response.
 *
 * \param[in] packet The packet to operate on.
 * \param[in] dstId The deviceId to use as destination in the packet.
 * \param[in] srcId The deviceId to use as source in the packet.
 * \param[in] tid The transaction identifier to set in the packet.
 * \param[in] status The status to set in the packet.
 * \param[in] data The data to send in the packet.
 *
 * This function sets the content of a packet to a maintanance read response packet
 * containing a response to a request reading one word in configuration space. 
 * The status field can be used to indicate success or failure.
 */
void RIOPACKET_setMaintReadResponse(RioPacket_t *packet,
                                    uint16_t dstId, uint16_t srcId,
                                    uint8_t tid, uint8_t status, uint32_t data);


/**
 * \brief Get entries from a maintenance read response.
 *
 * \param[in] packet The packet to operate on.
 * \param[out] dstId The destination deviceId in this packet.
 * \param[out] srcId The source deviceId in this packet.
 * \param[out] tid The transaction identifier in the response.
 * \param[out] status The status in the response.
 * \param[out] data The data in the response.
 *
 * This function returns the content of a packet as if it contained a maintenance 
 * read response packet.
 *
 * \note Use the ftype and transaction fields to see if the packet is indeed a 
 * maintenance read response.
 * \note If the packet does not contain a maintenance read response, the result 
 * will be undefined.
 */
void RIOPACKET_getMaintReadResponse(const RioPacket_t *packet,
                                    uint16_t *dstId, uint16_t *srcId,
                                    uint8_t *tid, uint8_t *status,
                                    uint32_t *data);


/**
 * \brief Set the packet to contain a maintenance write request.
 *
 * \param[in] packet The packet to operate on.
 * \param[in] dstId The deviceId to use as destination in the packet.
 * \param[in] srcId The deviceId to use as source in the packet.
 * \param[in] hop The hop_count to set in the packet.
 * \param[in] tid The transaction identifier to set in the packet.
 * \param[in] offset The byte address (24-bit) in the configuration space to write to.
 * \param[in] data The data to write in configuration space.
 *
 * This function sets the content of a packet to a maintenance write request packet 
 * containing a request to write one word in configuration space.
 */
void RIOPACKET_setMaintWriteRequest(RioPacket_t *packet,
                                    uint16_t dstId, uint16_t srcId, uint8_t hop, 
                                    uint8_t tid, uint32_t offset, uint32_t data);


/**
 * \brief Get entries from a maintenance write request.
 *
 * \param[in] packet The packet to operate on.
 * \param[out] dstId The destination deviceId in this packet.
 * \param[out] srcId The source deviceId in this packet.
 * \param[out] hop The hop_count in this packet.
 * \param[out] tid The transaction id in this packet.
 * \param[out] offset The byte address in the configuration space to read.
 * \param[out] data The data to requested to be written in configuration space.
 *
 * This function returns the content of a packet as if it contained a maintenance write
 * request packet. 
 *
 * \note Use the ftype and transaction fields to see if the packet is indeed a 
 * maintenance write request.
 * \note If the packet does not contain a maintenance write request, the result 
 * will be undefined.
 */
void RIOPACKET_getMaintWriteRequest(const RioPacket_t *packet,
                                    uint16_t *dstId, uint16_t *srcId, uint8_t *hop, 
                                    uint8_t *tid, uint32_t *offset, uint32_t *data);


/**
 * \brief Set the packet to contain a maintenance write response.
 *
 * \param[in] packet The packet to operate on.
 * \param[in] dstId The deviceId to use as destination in the packet.
 * \param[in] srcId The deviceId to use as source in the packet.
 * \param[in] tid The transaction identifier to set in the packet.
 * \param[in] status The status to set in the packet.
 *
 * This function sets the content of a packet to a maintanance write response packet
 * containing a response to a request writing one word in configuration space.
 * The status field can be used to indicate success or failure.
 */
void RIOPACKET_setMaintWriteResponse(RioPacket_t *packet,
                                     uint16_t dstId, uint16_t srcId, 
                                     uint8_t tid, uint8_t status);


/**
 * \brief Get entries from a maintenance write response.
 *
 * \param[in] packet The packet to operate on.
 * \param[out] dstId The destination deviceId in this packet.
 * \param[out] srcId The source deviceId in this packet.
 * \param[out] tid The transaction identifier in the response.
 * \param[out] status The status in the response.
 *
 * This function returns the content of a packet as if it contained a maintenance 
 * write response packet.
 *
 * \note Use the ftype and transaction fields to see if the packet is indeed a 
 * maintenance write response.
 * \note If the packet does not contain a maintenance write response, the result 
 * will be undefined.
 */
void RIOPACKET_getMaintWriteResponse(const RioPacket_t *packet,
                                     uint16_t *dstId, uint16_t *srcId, 
                                     uint8_t *tid, uint8_t *status);


/**
 * \brief Set the packet to contain a maintenance port-write request.
 *
 * \param[in] packet The packet to operate on.
 * \param[in] dstId The deviceId to use as destination in the packet.
 * \param[in] srcId The deviceId to use as source in the packet.
 * \param[in] componentTag The value of the componentTag register to set in the packet.
 * \param[in] portErrorDetect The value of the Port N Error Detect CSR to set in the packet.
 * \param[in] implementationSpecific An implementation specific value to set in the packet.
 * \param[in] portId The port ID of the port to set in the packet.
 * \param[in] logicalTransportErrorDetect The value of the Logical/Transport Layer 
 * Error Detect CSR to set in the packet.
 *
 * This function sets the content of a packet to a maintenance port-write request packet.
 */
void RIOPACKET_setMaintPortWrite(RioPacket_t *packet,
                                 uint16_t dstId, uint16_t srcId, 
                                 uint32_t componentTag, uint32_t portErrorDetect,
                                 uint32_t implementationSpecific, uint8_t portId,
                                 uint32_t logicalTransportErrorDetect);


/**
 * \brief Get entries from a maintenance port-write request.
 *
 * \param[in] packet The packet to operate on.
 * \param[out] dstId The device id of the destination end point.
 * \param[out] srcId The device id of the source end point.
 * \param[out] componentTag The value of the componentTag register in this packet.
 * \param[out] portErrorDetect The value of the Port N Error Detect CSR in this packet.
 * \param[out] implementationSpecific An implementation specific value in this packet.
 * \param[out] portId The port ID of the port in this packet.
 * \param[out] logicalTransportErrorDetect The value of the Logical/Transport Layer 
 * Error Detect CSR in this packet.
 *
 * This function returns the content of a packet as if it contained a maintenance port-write
 * request packet.
 *
 * \note Use the ftype and transaction fields to see if the packet is indeed a 
 * maintenance port-write request.
 * \note If the packet does not contain a maintenance port-write request, the result 
 * will be undefined.
 */
void RIOPACKET_getMaintPortWrite(const RioPacket_t *packet,
                                 uint16_t *dstId, uint16_t *srcId, 
                                 uint32_t *componentTag, uint32_t *portErrorDetect,
                                 uint32_t *implementationSpecific, uint8_t *portId,
                                 uint32_t *logicalTransportErrorDetect);


/**
 * \brief Set a packet to contain an NWRITE.
 *
 * \param[in] packet The packet to operate on.
 * \param[in] dstId The deviceId to use as destination in the packet.
 * \param[in] srcId The deviceId to use as source in the packet.
 * \param[in] address The byte address in IO-space to write to.
 * \param[in] payloadSize The number of bytes to write. The largest allowed size is 256 bytes.
 * \param[in] payload A pointer to the array of bytes to write.
 *
 * This function sets the content of a packet to an NWRITE containing a request 
 * to write the number of bytes specified by payloadSize to the address specified by the 
 * address argument.
 *
 * \note The address is a byte address.
 *
 * \note Not all combinations of addresses and sizes are allowed. The packet will be empty 
 * if an unallowed address/payloadSize combination is used. Use RIOPACKET_getWritePacketSize() 
 * to get the maximum size to use based on the address and payloadSize.
 */
void RIOPACKET_setNwrite(RioPacket_t *packet, 
                         uint16_t dstId, uint16_t srcId, 
                         uint32_t address, uint16_t payloadSize, const uint8_t *payload);


/**
 * \brief Get entries from a NWRITE.
 *
 * \param[in] packet The packet to operate on.
 * \param[out] dstId The destination deviceId in this packet.
 * \param[out] srcId The source deviceId in this packet.
 * \param[out] address The byte address into IO-space requested to be written.
 * \param[out] payloadSize The number of bytes requested to be written.
 * \param[out] payload The data requested to be written.
 *
 * This function returns the content of a packet as if it contained an NWRITE. 
 *
 * \note The address is a byte address.
 *
 * \note Any padding contained in double-word0 will be removed and the content 
 * will be placed where the payload pointer is pointing.
 */
void RIOPACKET_getNwrite(const RioPacket_t *packet, 
                         uint16_t *dstId, uint16_t *srcId, 
                         uint32_t *address, uint16_t *payloadSize, uint8_t *payload);


/**
 * \brief Set a packet to contain an NWRITER.
 *
 * \param[in] packet The packet to operate on.
 * \param[in] dstId The deviceId to use as destination in the packet.
 * \param[in] srcId The deviceId to use as source in the packet.
 * \param[in] tid The transaction identifier to set in the packet.
 * \param[in] address The byte address in IO-space to write to.
 * \param[in] payloadSize The number of bytes to write. The largest allowed size is 256 bytes.
 * \param[in] payload A pointer to the array of bytes to write.
 *
 * This function sets the content of a packet to an NWRITER containing a request 
 * to write the number of bytes specified by payloadSize to the address specified by the 
 * address argument. This packet requires a RESPONSE containing the transaction identifier 
 * specified in this packet.
 *
 * \note The address is a byte address.
 *
 * \note Not all combinations of addresses and sizes are allowed. The packet will be empty 
 * if an unallowed address/payloadSize combination is used. Use RIOPACKET_getWritePacketSize() 
 * to get the maximum size to use based on the address and payloadSize.
 */
void RIOPACKET_setNwriteR(RioPacket_t *packet, 
                          uint16_t dstId, uint16_t srcId, 
                          uint8_t tid, 
                          uint32_t address, uint16_t payloadSize, const uint8_t *payload);


/**
 * \brief Get entries from a NWRITER.
 *
 * \param[in] packet The packet to operate on.
 * \param[out] dstId The destination deviceId in this packet.
 * \param[out] srcId The source deviceId in this packet.
 * \param[out] tid The transaction id in this packet.
 * \param[out] address The byte address into IO-space requested to be written.
 * \param[out] payloadSize The number of bytes requested to be written.
 * \param[out] payload The data requested to be written.
 *
 * This function returns the content of a packet as if it contained an NWRITER. 
 *
 * \note The address is a byte address.
 *
 * \note Any padding contained in double-word0 will be removed and the content 
 * will be placed where the payload pointer is pointing.
 */
void RIOPACKET_getNwriteR(const RioPacket_t *packet, 
                          uint16_t *dstId, uint16_t *srcId, 
                          uint8_t *tid,
                          uint32_t *address, uint16_t *payloadSize, uint8_t *payload);


/**
 * \brief Set a packet to contain an NREAD.
 *
 * \param[in] packet The packet to operate on.
 * \param[in] dstId The deviceId to use as destination in the packet.
 * \param[in] srcId The deviceId to use as source in the packet.
 * \param[in] tid The transaction id to set in the response.
 * \param[in] address The byte address to read from.
 * \param[in] payloadSize The number of bytes to read. The largest allowed size is 256 bytes.
 *
 * This function sets the content of a packet to an NREAD containing a request 
 * to read the number of bytes specified by payloadSize from the address specified by the
 * address argument.
 *
 * \note The address is a byte address.
 *
 * \note Not all combinations of address and length are allowed. The packet will be empty
 * if an unallowed address/payloadSize combination is used. Use RIOPACKET_getReadPacketSize() 
 * to get the maximum size to use based on the address and payloadSize.
 */
void RIOPACKET_setNread(RioPacket_t *packet, 
                        uint16_t dstId, uint16_t srcId, 
                        uint8_t tid, 
                        uint32_t address, uint16_t payloadSize);


/**
 * \brief Get entries from an NREAD.
 *
 * \param[in] packet The packet to operate on.
 * \param[out] dstId The destination deviceId in this packet.
 * \param[out] srcId The source deviceId in this packet.
 * \param[out] tid The transaction id in this packet.
 * \param[out] address The byte address into IO-space requested to be written.
 * \param[out] payloadSize The number of bytes requested to be read.
 *
 * This function returns the content of a packet as if it contained an NREAD.
 *
 * \note The address is a byte address.
 */
void RIOPACKET_getNread(const RioPacket_t *packet, 
                        uint16_t *dstId, uint16_t *srcId, 
                        uint8_t *tid, 
                        uint32_t *address, uint16_t *payloadSize);


/**
 * \brief Set a packet to contain a DOORBELL.
 *
 * \param[in] packet The packet to operate on.
 * \param[in] dstId The deviceId to use as destination in the packet.
 * \param[in] srcId The deviceId to use as source in the packet.
 * \param[in] tid The transaction identifier to set in the packet.
 * \param[in] info The information to send with the doorbell.
 *
 * This function sets the content of a packet to a DOORBELL.
 */
void RIOPACKET_setDoorbell(RioPacket_t *packet,
                           uint16_t dstId, uint16_t srcId, 
                           uint8_t tid, uint16_t info);


/**
 * \brief Get entries from a DOORBELL.
 *
 * \param[in] packet The packet to operate on.
 * \param[out] dstId The destination deviceId in this packet.
 * \param[out] srcId The source deviceId in this packet.
 * \param[out] tid The transaction identifier in this packet.
 * \param[out] info The information field in this packet.
 *
 * This function returns the content of a packet as if it contained a DOORBELL.
 */
void RIOPACKET_getDoorbell(const RioPacket_t *packet,
                           uint16_t *dstId, uint16_t *srcId, 
                           uint8_t *tid, uint16_t *info);


/**
 * \brief Set a packet to contain a MESSAGE.
 *
 * \param[in] packet The packet to operate on.
 * \param[in] dstId The deviceId to use as destination in the packet.
 * \param[in] srcId The deviceId to use as source in the packet.
 * \param[in] mailbox The mailbox to send the message to.
 * \param[in] payloadSize The number of bytes to place into the message.
 * \param[in] payload A pointer to the array of bytes to place into the message.
 *
 * This function sets the content of a packet to contain a MESSAGE.
 *
 * \note The mailbox argument maps to the packet fields as: 
 * {xmbox(3:0), letter(1:0), mbox(1:0)} which means that mailbox 0-15 can support
 * multipacket messages and 16-255 can handle only single packet messages.
 *
 * \note The payload size has to be larger than zero and less than 256.
 *
 * \note Only payloads of even double-words are supported by the protocol itself. Payload 
 * that is shorter will be padded.
 */
void RIOPACKET_setMessage(RioPacket_t *packet,
                          uint16_t dstId, uint16_t srcId, 
                          uint8_t mailbox, 
                          uint16_t payloadSize, const uint8_t *payload); 


/**
 * \brief Get entries from a MESSAGE.
 *
 * \param[in] packet The packet to operate on.
 * \param[out] dstId The destination deviceId in this packet.
 * \param[out] srcId The source deviceId in this packet.
 * \param[out] mailbox The mailbox the message is received on.
 * \param[out] payloadSize The number of bytes in the payload.
 * \param[out] payload The payload of the packet.
 *
 * This function returns the content of a packet as if it contained a MESSAGE.
 *
 * \note The mailbox argument maps to the packet fields as: 
 * {xmbox(3:0), letter(1:0), mbox(1:0)} which means that mailbox 0-15 can support
 * multipacket messages and 16-255 can handle only single packet messages.
 *
 * \note Only payloads of even double-words are supported by the protocol itself so the 
 * returned payloadSize is always an even multiple of eight.
 */
void RIOPACKET_getMessage(const RioPacket_t *packet,
                          uint16_t *dstId, uint16_t *srcId, 
                          uint8_t *mailbox, 
                          uint16_t *payloadSize, uint8_t *payload); 


/**
 * \brief Set a packet to contain a RESPONSE without payload.
 *
 * \param[in] packet The packet to operate on.
 * \param[in] dstId The deviceId to use as destination in the packet.
 * \param[in] srcId The deviceId to use as source in the packet.
 * \param[in] tid The transaction id to send the response for.
 * \param[in] status The status to send in the packet.
 *
 * This function sets the content of a packet to contain a RESPONSE without payload.
 *
 * \note The tid field must be the same value as the packet contained that this is the 
 * response for.
 *
 * \note The status field should be either of the values RIOPACKET_RESPONSE_STATUS_XXXX.
 */
void RIOPACKET_setResponseNoPayload(RioPacket_t *packet, 
                                    uint16_t dstId, uint16_t srcId, 
                                    uint8_t tid, uint8_t status);


/**
 * \brief Get entries from a RESPONSE without payload.
 *
 * \param[in] packet The packet to operate on.
 * \param[out] dstId The destination deviceId in this packet.
 * \param[out] srcId The source deviceId in this packet.
 * \param[out] tid The transaction identifier in this packet.
 * \param[out] status The status in this packet.
 *
 * This function returns the content of a packet as if it contained a RESPONSE.
 */
void RIOPACKET_getResponseNoPayload(const RioPacket_t *packet, 
                                    uint16_t *dstId, uint16_t *srcId, 
                                    uint8_t *tid, uint8_t *status);


/**
 * \brief Set a packet to contain a RESPONSE also containing payload.
 *
 * \param[in] packet The packet to operate on.
 * \param[in] dstId The deviceId to use as destination in the packet.
 * \param[in] srcId The deviceId to use as source in the packet.
 * \param[in] tid The transaction id to send the response for.
 * \param[in] offset The offset into the payload to start to write the input payload to.
 * \param[in] payloadSize The size of the payload to return in the reply.
 * \param[in] payload The payload to return in the reply.
 *
 * This function sets the content of a packet to contain a RESPOSE with payload.
 *
 * \note The tid field must be the same value as the packet contained that this is the 
 * response for.
 *
 * \note Response packets can only contain double-word (64-bit) entries. This means that 
 * if, for example an NREAD request, need to return a single byte it must place this byte 
 * in the correct position in the returned double-word.
 * Example: 
 *  inbound: RIOPACKET_getNread(..., address, payloadSize) -> (address=0x0000000a, 
 *                                                             payloadSize=1)
 *  ...
 *  outbound: RIOPACKET_setResponseWithPayload(..., offset=0x0000000a, payloadSize=1, 
 *                                             payload={byte array that should be returned})
 *  The NREAD accesses a single byte in a double-word at the next most significant byte. 
 *  When the response is sent back the single byte is placed in that position
 *  in the returned double-word and needs to be written starting from that position. 
 *  This is done by using the received address to offset the payload in the generated 
 *  response. The address returned by RIOPACKET_getNread() can be used directly since 
 *  a modulo8 is done on it before it is used as offset.
 *
 * \note The payloadSize must match the size of the packet that this is the
 * response for.
 */
void RIOPACKET_setResponseWithPayload(RioPacket_t *packet, 
                                      uint16_t dstId, uint16_t srcId, 
                                      uint8_t tid, uint8_t offset,
                                      uint16_t payloadSize, const uint8_t *payload);


/**
 * \brief Get entries from a RESPONSE containing payload.
 *
 * \param[in] packet The packet to operate on.
 * \param[out] dstId The destination deviceId in this packet.
 * \param[out] srcId The source deviceId in this packet.
 * \param[out] tid The transaction identifier in this packet.
 * \param[in] offset The offset into the payload to start reading from.
 * \param[out] payloadSize The number of bytes in the payload.
 * \param[out] payload The payload of the packet.
 *
 * This function returns the content of a packet as if it contained a RESPONSE with payload.
 *
 * \note Response packets can only contain double-word (64-bit) entries. This means that 
 * if, for example an NREAD request, need to fetch a single byte it must read this byte 
 * in the correct position in the received double-word.
 * Example: 
 *  outbound: RIOPACKET_setNread(..., address=0x0000000a, payloadSize=1)
 *  ....
 *  inbound: RIOPACKET_getResponseWithPayload(..., offset=0x0000000a, payloadSize=1, 
 *                                            payload={byte array that is returned})
 *  The NREAD accesses a single byte in a double-word at the next most significant byte. 
 *  When the response is received the single byte is placed in that position
 *  in the returned double-word and needs to be read starting from that position. 
 *  This is done by using the sent address to offset the payload in the generated 
 *  response. The address used in the RIOPACKET_setNread() can be used directly since
 *  a modulo8 is done on it before it is used as offset.
 */
void RIOPACKET_getResponseWithPayload(const RioPacket_t *packet, 
                                      uint16_t *dstId, uint16_t *srcId, 
                                      uint8_t *tid, uint8_t offset,
                                      uint16_t *payloadSize, uint8_t *payload);


/**
 * \brief Set a packet to contains a RESPONSE to a message.
 *
 * \param[in] packet The packet to operate on.
 * \param[in] dstId The deviceId to use as destination in the packet.
 * \param[in] srcId The deviceId to use as source in the packet.
 * \param[in] mailbox The mailbox to send the message to.
 * \param[in] status The status to send in the packet.
 *
 * This function is used to send a response indicating a successfull
 * completion in reply to a previously received packet.
 *
 * \note The mailbox field should contain the same value as the packet that this is the 
 * response to.
 *
 * \note The status field should be either of the values RIOPACKET_RESPONSE_STATUS_XXXX.
 */
void RIOPACKET_setResponseMessage(RioPacket_t *packet, 
                                  uint16_t dstId, uint16_t srcId, 
                                  uint8_t mailbox, uint8_t status);


/**
 * \brief Get entries from a RESPONSE to a message.
 *
 * \param[in] packet The packet to operate on.
 * \param[out] dstId The destination deviceId in this packet.
 * \param[out] srcId The source deviceId in this packet.
 * \param[out] mailbox The mailbox the response should be sent to.
 * \param[out] status The status in the packet.
 *
 * This function returns the content of a packet as if it contained a RESPONSE to a message.
 */
void RIOPACKET_getResponseMessage(const RioPacket_t *packet, 
                                  uint16_t *dstId, uint16_t *srcId, 
                                  uint8_t *mailbox, uint8_t *status);

/**
 * \brief Calculate a new CRC16 value.
 *
 * \param[in] data The new data (16-bit) to update the current crc value with.
 * \param[in] crc The old crc value that should be updated.
 * \returns The new crc value based on the input arguments.
 *
 * This function calculates a new crc value using the generator polynom 
 * P(X)=x16+x12+x5+1. It is defined in RapidIO 3.0 part6 chapter 2.4.2.
 */
uint16_t RIOPACKET_crc16(const uint16_t data, const uint16_t crc);


/**
 * \brief Calculate a new CRC16 value.
 *
 * \param[in] data The new data (32-bit) to update the current crc value with.
 * \param[in] crc The old crc value that should be updated.
 * \returns The new crc value based on the input arguments.
 *
 * This function calculates a new crc value using the generator polynom 
 * P(X)=x16+x12+x5+1. It is defined in RapidIO 3.0 part6 chapter 2.4.2.
 */
uint16_t RIOPACKET_crc32(const uint32_t data, uint16_t crc);


/**
 * \brief Get the maximum size of an NWRITE/NWRITER payload.
 *
 * \param[in] address The starting address to write to in the NWRITE/NWRITER.
 * \param[in] size The total size of the access to NWRITE/NWRITER.
 * \returns The maximum number of bytes that are allowed to send in a single 
 * NWRITE packet that conforms to the RapidIO standard.
 *
 * This function calculates the maximum sized NWRITE/NWRITER packet payload that are 
 * possible to send without breaking the limitations in the RapidIO specification. 
 * It is intended to be called repeatedly.
 *
 * Example: An area with address=0x00007 and size=258 needs to be written.
 *
 *          Call RIOPACKET_getWritePacketSize(0x00007, 258)->1.
 *          Send an NWRITE/NWRITER to address=0x00007 and size=1.
 *          Update the address and size with the returned value->
 *          address+=1->address=0x00008 size-=1->size=257.
 *
 *          Call RIOPACKET_getWritePacketSize(0x00008, 257)->256.
 *          Send an NWRITE/NWRITER to address=0x00008 and size=256.
 *          Update the address and size with the returned value->
 *          address+=256->address=0x00108 size-=256->size=1.
 *
 *          Call RIOPACKET_getWritePacketSize(0x00108, 1)->1.
 *          Send an NWRITE/NWRITER to address=0x00108 and size=1.
 *          Update the address and size with the returned value->
 *          address+=1->address=0x00109 size-=1->size=0.
 *
 *          All data has been written.
 *
 */
uint32_t RIOPACKET_getReadPacketSize(uint32_t address, uint32_t size);


/**
 * \brief Get the maximum size of an NREAD payload.
 *
 * \param[in] address The starting address to read from in the NREAD.
 * \param[in] size The total size of the access to NREAD.
 * \returns The maximum number of bytes that are allowed to send in a single 
 * NREAD packet that conforms to the RapidIO standard.
 *
 * This function calculates the maximum sized NREAD packet payload that are 
 * possible to send without breaking the limitations in the RapidIO specification. 
 * It is intended to be called repeatedly.
 *
 * Example: An area with address=0x00007 and size=258 needs to be read.
 *
 *          Call RIOPACKET_getReadPacketSize(0x00007, 258)->1.
 *          Send an NREAD to address=0x00007 and size=1.
 *          Update the address and size with the returned value->
 *          address+=1->address=0x00008 size-=1->size=257.
 *
 *          Call RIOPACKET_getReadPacketSize(0x00008, 257)->256.
 *          Send an NREAD to address=0x00008 and size=256.
 *          Update the address and size with the returned value->
 *          address+=256->address=0x00108 size-=256->size=1.
 *
 *          Call RIOPACKET_getReadPacketSize(0x00108, 1)->1.
 *          Send an NREAD to address=0x00108 and size=1.
 *          Update the address and size with the returned value->
 *          address+=1->address=0x00109 size-=1->size=0.
 *
 *          All data has been read.
 *
 */
uint32_t RIOPACKET_getWritePacketSize(uint32_t address, uint32_t size);

#endif

/*************************** end of file **************************************/
