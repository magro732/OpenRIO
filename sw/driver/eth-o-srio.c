/*******************************************************************************
 * 
 * RapidIO IP Library Core
 * 
 * This file is part of the RapidIO IP library project
 * http://www.opencores.org/cores/rio/
 * 
 * Description:
 * This file contains an ethernet over SRIO driver based on SRIO MESSAGE
 * A specific protocol is used to handle hardware requirement of having
 * 64 bits aligned packet to avoid having to copy the packet into a
 * well aligned buffer: it means it's not the pure ethernet frame which
 * is transferred
 *
 * To Do:
 * - Broadcast is not supported meaning ARP request are not handled
 *   you have to use static MAC adresses
 * 
 * Author(s): 
 * - Anthony Smock anthony.smock@thalesgroup.com
 * - Arnaud Samama arnaud.samama@thalesgroup.com
 * 
 *******************************************************************************
 * 
 * Copyright (C) 2015 Authors and OPENCORES.ORG 
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


#include <linux/init.h>
#include <linux/module.h>
#include <asm/io.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/uaccess.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <linux/io.h>
#include <linux/slab.h>
#include <linux/interrupt.h>
#include <linux/moduleparam.h>
#include <linux/of_platform.h>
#include <linux/proc_fs.h>
#include <linux/sysfs.h>
#include <linux/gfp.h>
#include <asm/cacheflush.h>
#include <asm/atomic.h>

#define DRIVER_NAME			"eth-o-srio"

#define TX_TIMEOUT			(60*HZ)		// Timeout after which kernel retries transmission 
#define SRIO_REGS_SIZE			4096
#define SRIO_INTERRUPT			61
#define SRIO_REGS_BASEADDR	 		0x43C00000
#define SRIO_MAGIC_OFFSET		    0x00
#define SRIO_REVISION_OFFSET		0x04
#define SRIO_TARGETADDR_OFFSET		0x08
#define SRIO_SOURCEADDR_OFFSET		0xC
#define SRIO_XFERSIZE_OFFSET		0x10
#define SRIO_XFER_ID		        0x14
#define SRIO_MAILBOXBASEADDR_OFFSET	0x20
#define SRIO_MAILBOXSIZE_OFFSET		0x24
#define SRIO_MAXBEATLEN_OFFSET		0x28
#define SRIO_LINKSTATUS_OFFSET		0x30

#define SRIO_INTERRUPTSTATUS_OFFSET	0x34
#define SRIO_INTERRUPTSTATUS_RX         0x1
#define SRIO_INTERRUPTSTATUS_TX_DONE    0x2

#define SRIO_AXIERROR_OFFSET		0x40
#define SRIO_DROPPEDFRAME_OFFSET	0x44
#define SRIO_RECEIVEDFRAME_OFFSET	0x48
#define SRIO_XMIT_MESSAGE_OFFSET	0x50
#define SRIO_RCVD_MESSAGE_OFFSET	0x4C

//
// Use to create maintenance packet over the fabric
//
#define SRIO_MAINTPACKET_ADDR_OFFSET 0xA0
#define SRIO_MAINTPACKET_DATA_OFFSET 0xA4

#define SRIO_MAGIC			0x600d0510

#define SRIO_MSG_SIZE		4096
// ASA #define SRIO_INVALID_MSG_SIZE		0xFFFFFFFF
#define SRIO_INVALID_MSG_SIZE		0xFFFF0000

#define PRNT_ID			"EthoSRIO:"

#define BEAT_LEN_LEN		2
//
// Format of each message written inside the mailbox
//
#define SRIO_MBOX_RECEIVER_ID_LEN   6
#define SRIO_MBOX_SENDER_ID_LEN   6
#define SRIO_MBOX_MSG_SIZE_LEN   4
#define SRIO_MBOX_HEADER_SIZE (SRIO_MBOX_MSG_SIZE_LEN + SRIO_MBOX_SENDER_ID_LEN + SRIO_MBOX_RECEIVER_ID_LEN)

#define SRIO_WRITE_MBOX_CURRENT_MSG_SIZE( nlp, value) writel(value, nlp->mbox_ptr + nlp->pos * SRIO_MSG_SIZE)

#undef DEBUG_SRIO

#ifdef DEBUG_SRIO
#	define DEBUG
#	define dprintk printk			//Debug macro
#	define print_hex_buffer print_hex_buffer
#else
#	define dprintk if (0) printk
#	define print_hex_buffer(x,y) (void)0  
#endif


/* Match table for of_platform binding */
static struct of_device_id axienet_of_match[] = {
	{ .compatible = "trt,axi-srio", },
	{},
};

MODULE_DEVICE_TABLE(of, axienet_of_match);

//
// Default Ethernet address of the SRIO interface
//
static char* macaddr = "00:00:00:04:05:06";
module_param(macaddr, charp, S_IRUGO);

//
// The mailbox contains 2^mbox_size_order messages.
// As a message is 4KB long, the total mailbox size is
//       2^mbox_size_order * 4096
//
static int mbox_size_order = 5;
module_param(mbox_size_order, int,  S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);

struct tasklet_struct *srio_tx_tasklet;

struct tasklet_struct *srio_rx_tasklet;


static struct net_device_ops srio_netdev_ops;	//struct containing references to our device's functions

struct net_local {				//struct containing datas about our device

	struct net_device *ndev;		//Our device inherit from net devices
	
//	struct kobject kobj;

	void __iomem *base_addr;		//Base adress of our device

	spinlock_t reset_lock;	
	struct sk_buff *deferred_skb;		//contains the skb when an error occured

	struct sk_buff *tx_skb;
    dma_addr_t tx_dma_addr;
    unsigned int tx_dma_count;

    //
    // Pointer to the Mailbox area shared with the DMA to be accessed from the CPU
    //
	volatile void *mbox_ptr;

    //
    // Bus address of the Mailbox area shared with the DMA to be accessed from the DMA
    //
    dma_addr_t mbox_bus_addr;


    //
    // Size (bytes) of the Mailbox area shared with the DMA
    //
	unsigned long mbox_size;
	unsigned long mbox_entry_count;

	unsigned int pos;
	struct device *dev;

	struct proc_dir_entry *trt_srio_dir, *beat_len_file;
	//char beat_len_data[3];

    unsigned long rx_intr;
    unsigned long tx_intr;
    unsigned long all_intr;

    atomic_t pending_intr;
};

struct net_local *pdevice;

 #ifdef DEBUG

static void print_hex_buffer( volatile void* buffer, size_t buffer_size ) 
{
    size_t i_byte;

	dprintk("\n Displaying  %d / 0x%02x bytes\n", buffer_size, buffer_size);
	dprintk("------------------------------------------------------------------------");
	for( i_byte = 0; i_byte < buffer_size; i_byte++) {
		if (i_byte % 16 == 0) {
			dprintk("\n0x%02x ",i_byte);
        }
		dprintk( "%02x ", ((unsigned char*)buffer)[i_byte] );
	}
	dprintk("\n");

}
#endif

char temp_buff[2048];
static ssize_t beat_len_read(struct file *filp, char *buffer, size_t length, loff_t * offset)
{
	static int finished = 0;
	u32 reg_beatl, reg_baddr, reg_istat, reg_rcvframe;
	u32 reg_xmit_message, reg_rcvd_message;
	size_t size_temp = 0;

	/* 
	 * We return 0 to indicate end of file, that we have
	 * no more information. Otherwise, processes will
	 * continue to read from us in an endless loop. 
	 */
	if ( finished ) {
		finished = 0;
		return 0;
	}
	
	finished = 1;


	reg_beatl = readl(pdevice->base_addr + SRIO_MAXBEATLEN_OFFSET);
	reg_baddr = readl(pdevice->base_addr + SRIO_MAILBOXBASEADDR_OFFSET);
	reg_istat = readl(pdevice->base_addr + SRIO_INTERRUPTSTATUS_OFFSET);
	reg_rcvframe = readl(pdevice->base_addr + SRIO_RECEIVEDFRAME_OFFSET);
	reg_xmit_message = readl(pdevice->base_addr + SRIO_XMIT_MESSAGE_OFFSET);
	reg_rcvd_message = readl(pdevice->base_addr + SRIO_RCVD_MESSAGE_OFFSET);

	sprintf(temp_buff, "reg@MAX_BEAT_LEN : 0x%08x\n"
            "reg@BASE_ADDR : 0x%08x\n"
            "reg@INT_STATUS : 0x%08x\n"
            "reg@RECEIVED_FRAME : 0x%08x\n" 
            "reg@XMIT_MESSAGE : 0x%08x\n" 
            "reg@RCVD_MESSAGE : 0x%08x\n" 
            "mbox_pos : %u\n" 
            "tx_intr : %lu\n" 
            "rx_intr : %lu\n" 
            "all_intr : %lu\n", 
            reg_beatl, reg_baddr, reg_istat, reg_rcvframe, reg_xmit_message, reg_rcvd_message,
            pdevice->pos,
            pdevice->tx_intr, pdevice->rx_intr, pdevice->all_intr);

	size_temp = strlen(temp_buff);
		
	if ( copy_to_user(buffer, temp_buff, size_temp )) {
		return -EFAULT;
	}

	return size_temp;
}

static void write_maintenance( struct net_local* lp, u32 hop_count, u32 offset, u32 value)
{
	u32 maint_data, maint_addr;

    //
    // Enable input and output port 1 (0 is reserved to the CPU)
    //
	maint_addr = ((hop_count & 0xFF) << 23) | offset;
    __raw_writel(maint_addr, lp->base_addr + SRIO_MAINTPACKET_ADDR_OFFSET);
	wmb();
	maint_data = value;
    __raw_writel(maint_data, lp->base_addr + SRIO_MAINTPACKET_DATA_OFFSET);

}

static u32 read_maintenance( struct net_local* lp, u32 hop_count, u32 offset)
{
	u32  maint_addr;

	maint_addr = ((hop_count & 0xFF) << 23) | offset;
    __raw_writel(maint_addr, lp->base_addr + SRIO_MAINTPACKET_ADDR_OFFSET);
	wmb();

	return  __raw_readl(lp->base_addr + SRIO_MAINTPACKET_DATA_OFFSET); 

}


static int beat_len_open(struct inode *inode, struct file *file)
{
	try_module_get(THIS_MODULE);
	return 0;
}

static ssize_t beat_len_write(struct file *file, const char *buffer, size_t len, loff_t * off)
{
/*	u32 *data;
	if (len == sizeof(*data))
	{
		if ( copy_from_user(data, buffer, len) ) {
		return -EFAULT;
		}
		__raw_writel(*data, pdevice->base_addr + SRIO_MAXBEATLEN_OFFSET);
	}	*/
	return 0;//len;
}

static int beat_len_release(struct inode *ino, struct file *file)
{
	module_put(THIS_MODULE);
	return 0;
}

static const struct file_operations beat_len_fops = {
 .read = beat_len_read,
 .write = beat_len_write,
 .open = beat_len_open,
 .release = beat_len_release,
};


static int srio_send_data(struct net_local *drvdata, u8 *data, unsigned int byte_count)
{

	dprintk("\n\n========================== SENDING ==========================\n");
	dprintk("\n\n then data@:0x%p\n\n", data);

    print_hex_buffer( data, byte_count);

    drvdata -> tx_dma_count = byte_count;

    //
    // Get the address seen from the device for this buffer
    //
	drvdata -> tx_dma_addr = dma_map_single(
            drvdata -> ndev->dev.parent, data,
			drvdata -> tx_dma_count, DMA_TO_DEVICE);

    //
    // Start the transmission of the buffer
    //
	writel(drvdata -> tx_dma_addr,  drvdata->base_addr + SRIO_SOURCEADDR_OFFSET);
	writel(drvdata -> tx_dma_count, drvdata->base_addr + SRIO_XFERSIZE_OFFSET);

	dprintk("=========================================================\n");

	return 0;
}
void srio_tx_handler(unsigned long data)
{	
	unsigned long flags;
    struct net_local * lp = (struct net_local *)data;

	lp->ndev->stats.tx_packets++;
    dprintk("1 packet successfully sent\n");
    //
    // Mark when we have transmitted the data
    //
    skb_tx_timestamp(lp->tx_skb);

    //
    // Update the number of transmitted bytes
    //
    lp->ndev->stats.tx_bytes += lp->tx_skb->len;

    lp->tx_intr++;

    //
    // Job done !
    //
    spin_lock_irqsave(&lp->reset_lock, flags);
    dma_unmap_single(lp -> ndev->dev.parent, lp->tx_dma_addr,
			lp->tx_dma_count, DMA_TO_DEVICE);
    dev_kfree_skb(lp->tx_skb);
	lp->tx_skb = NULL;
    lp->tx_dma_addr = 0x0;
    lp->tx_dma_count = 0x0;
	spin_unlock_irqrestore(&lp->reset_lock, flags);

    // 
    // Is there any packet pending ? meaning the queue has been stopped
    //
    if (lp -> deferred_skb != NULL ) {

        //
        // Transfert the packet already correctly fill and aligned to the hardware
        //
        lp->tx_skb = lp -> deferred_skb;

        //
        // Transmit the buffer to the hardware
        //
        srio_send_data(lp, (u8 *) lp->tx_skb->data, lp->tx_skb->len);

        lp -> deferred_skb = NULL;

	    netif_wake_queue(lp -> ndev);


    }
}

static inline bool is_valid_size( u32 len)
{
    //return len != SRIO_INVALID_MSG_SIZE;
    return (len & 0xFFFF0000 ) != SRIO_INVALID_MSG_SIZE;
}


static u32 read_mbox_current_msg_size( struct net_local *plp )
{
    u32 len;

    len = readl(plp->mbox_ptr + plp->pos * SRIO_MSG_SIZE);

    return len;
}

void srio_rx_handler(unsigned long lp)
{
	struct net_local *plp = (struct net_local *)lp;
	u32 len;
	struct sk_buff *skb;
	struct net_device *dev = plp->ndev;
	unsigned char align = 0, end = 0;

	dprintk("\n*************************1 packet received***********************\n");

    plp->rx_intr++;

    atomic_inc( &plp->pending_intr);

    //
    // Check the len of the message we are supposed to process
    //
    len = read_mbox_current_msg_size(plp);
    

	dprintk("read@rx_handler : len=%08x\n", len);
    //
    // Process while there is valid message written inside the
    //  mailbox
    //
    while (is_valid_size( len ) ) {

        if ( atomic_read(&plp->pending_intr) > 0 ) {
            atomic_dec(&plp->pending_intr);
        }

        //
        // Allow skb to push up into the stack
        //
        skb = netdev_alloc_skb(dev, len+2);//skb à usage unique ?, len +2 ?

        if (skb) {

            volatile void * ethernet_payload;

            //
            // allocated enough space, even for useless data
            //
            skb_put(skb, len);
	   

            //
            // The ethernet paylod is at the next expected position, just after the SRIO Mbox header
            //
            ethernet_payload = plp->mbox_ptr + plp->pos * SRIO_MSG_SIZE  + SRIO_MBOX_HEADER_SIZE;




            // ASA /memcpy(skb->data, ethernet_payload, len);
            memcpy(skb->data, (void*)ethernet_payload, len);


            //
            // How many byte do we need to remove at the beginning to have the real start of
            //  the ethernet payload (added for dealing for 32 bits alignement requirement of
            //  the SRIO device)
            //
            align = *(skb->data);

            //
            // Remove at the begining
            //
            skb_pull(skb, align);

            //
            // How many bytes do we need to remove at the end of the buffer ?
            //
            end = *(skb->data);
            skb_trim(skb, skb->len - end);

            //
            // Remove the byte used to store the alignement on the tail
            skb_pull(skb, 1);

            dprintk("\n\nRECEIVED !! : %d / 0x%02x bytes at pos=%u\n", skb->len, skb->len, plp->pos);
            print_hex_buffer( skb->data, skb->len);

            skb->protocol = eth_type_trans(skb, dev);
            skb_checksum_none_assert(skb);

            dev->stats.rx_packets++;
            dev->stats.rx_bytes += len;


            netif_rx(skb);

//#           ifdef DEBUG_SRIO
            //memset( (void*)plp->mbox_ptr + plp->pos * SRIO_MSG_SIZE, 0xCA, SRIO_MSG_SIZE);
//#           endif


        } else {

            //printk(KERN_NOTICE "snull rx: low on mem - packet dropped\n");
            dev->stats.rx_dropped++;

        }


        //
        // mark we have read the message whatever if we push it into the stack
        // or just dropped it
        //
        // Write counter for debug
        SRIO_WRITE_MBOX_CURRENT_MSG_SIZE(plp, (SRIO_INVALID_MSG_SIZE & 0xFFFF0000) | ((dev->stats.rx_packets+dev->stats.rx_dropped) & 0x0000FFFF) );
        //SRIO_WRITE_MBOX_CURRENT_MSG_SIZE(plp, SRIO_INVALID_MSG_SIZE);


        plp -> pos++;
	    dprintk("Inc pos=%u\n",plp -> pos);
        plp -> pos %= plp -> mbox_entry_count;
	    dprintk("Inc mbox_entry_count=%lu\n",plp -> mbox_entry_count);

        //
        // Check the next entry, in case of the hardware has written a new message
        //  during the current processing
        //
        len = read_mbox_current_msg_size(plp);
    } 


    // 
    // If at the end of the processing we have many more interrut non
    // processed, it proably means we are not seeing the messages
    //
    if ( atomic_read(&plp->pending_intr) > 1 ) {

        // Disable mailbox
        writel(0xFFFFFFFF, plp->base_addr + SRIO_MAILBOXBASEADDR_OFFSET);
        printk("Disabling mailbox\n");
    }





    dprintk("Ending with pos=%u\n",plp -> pos);
    dprintk("*********************************************************************\n");


    return;


}



static int srio_send(struct sk_buff *skb, struct net_device *dev)	//starts data transfert
{
    int rc = 0;
    unsigned long flags;
	struct net_local *lp = netdev_priv(dev);
	unsigned char end;
    unsigned char* tail_size_ptr;
    unsigned char* head_size_ptr;
    unsigned char* tail_ptr;

    int missing_byte_for_32bits_alignement;
    //
    // The SRIO hardare is only able to transfer data 32 bits aligned
    //  and multiple of 32 bits. So we need to align both the start
    //  address and the length.
    //  We store at the beginning of the packet the information necessary
    //      on the receiver side to retrieve the original data
    //  | head_size | tail_size | data
    //      - Head size is made of 1 to 4 bytes. The first byte of the buffer
    //          contains the number of byte to be strip at the head
    //      - tail_size is made of 1 byte. It contains the number
    //          of bytes to be removed at the end
    //
	dprintk("\ninitial lenght : 0x%02x bytes\n", skb->len);

			
    //
    // Reserve one byte to store how many bytes we need to strip at the end
    //  of the buffer
    //
	tail_size_ptr = skb_push(skb, 1);


    //
    // Once data has been reserved, check, what alignement, we have to
    //  do to get an aligned pointer
    //
	missing_byte_for_32bits_alignement = (phys_addr_t)skb->data & (phys_addr_t)(0x3);

	dprintk("\nooooooooooooooo\n adding 0x%02x at the beginning \nooooooooooo\n", missing_byte_for_32bits_alignement);

    //
    // The address is already align. Ideally nothing to do,
    //  but as the receiver is expecting a size, we add a complete 32 bits
    //  word
	if(unlikely(missing_byte_for_32bits_alignement == 0))
	{
        missing_byte_for_32bits_alignement = 4;

	} 

    //
    // Add at the begining, the number of bytes to get the address aligned
    //
    head_size_ptr = skb_push(skb, missing_byte_for_32bits_alignement);

    //
    // Clear the area, then mark the number of bytes we just added
    //
    memset(head_size_ptr, 0, missing_byte_for_32bits_alignement);
    *head_size_ptr = missing_byte_for_32bits_alignement;


    //
    // Now: how is the size multiple of 32 bits ?
    //
	end = skb->len & 0x3;

	dprintk("\n========\n adding 0x%02x at the end \n========\n", end);

	if(unlikely(end > 0))
	{
		tail_ptr = skb_put(skb, 4 - end);
		memset(tail_ptr, 0, end);
		*tail_size_ptr = 4 -end;
	}
	else
	{
		*tail_size_ptr = 0;
	}

	dprintk("\n\ndata@:0x%p\n\n", skb->data);

	
    // 
    // Ensure the xmitting buffer won't be marked free during checking
    //
	spin_lock_irqsave(&lp->reset_lock, flags);

    //
    // We transmit the buffer only if the device is not already processing a buffer
    //
    if ( lp->tx_skb == NULL ) {

        //
        // We are sure we are able to transmit the buffer
        //
        spin_unlock_irqrestore(&lp->reset_lock, flags);

        //
        // Here we are sure the tx_skb is NULL, it means the hardware already free it, so
        //  no one can touch it, we can access it freely
        //
        lp->tx_skb = skb;

        //
        // Transmit the buffer to the hardware
        //
        srio_send_data(lp, (u8 *) skb->data, skb->len);

        //
        // Problem during hardware transmission, let's retry later
        //
    } else {


        //
        // Store the buffer to be transmitted later
        //
        lp->deferred_skb = skb;

        dprintk("srio_send_data != 0 !");

        netif_stop_queue(dev);

        /* Take the time stamp now, since we can't do this in an ISR. */
        skb_tx_timestamp(skb);


        //
        // After having stop the queue and store the buffer
        //  if a completion interrupt is raised now, it will send the pending
        //  buffer
        //
        spin_unlock_irqrestore(&lp->reset_lock, flags);
    }


	return rc;
}

static int srio_set_mac_address(struct net_device *dev, void *address)
{
    int rc = 0;

	struct sockaddr *addr = address;

	if ( ! netif_running(dev)) {

        memcpy(dev->dev_addr, addr->sa_data, dev->addr_len);

    } else {

		rc = -EBUSY;

	}


	return rc;
}

static void srio_tx_timeout(struct net_device *dev)	//function  executed if transmission has failed
{
	struct net_local *lp = netdev_priv(dev);
	unsigned long flags;

	dev_err(&lp->ndev->dev, "Exceeded transmit timeout of %lu ms\n",
		TX_TIMEOUT * 1000UL / HZ);

	dev->stats.tx_errors++;

	/* Reset the device */
	spin_lock_irqsave(&lp->reset_lock, flags);

	/* Shouldn't really be necessary, but shouldn't hurt */
	netif_stop_queue(dev);


	if (lp->deferred_skb)
       	{
		dev_kfree_skb(lp->deferred_skb);
		lp->deferred_skb = NULL;
		dev->stats.tx_errors++;
	}

	/* To exclude tx timeout */
	dev->trans_start = jiffies; /* prevent tx timeout */

	/* We're all ready to go. Start the queue */
	netif_wake_queue(dev);
	spin_unlock_irqrestore(&lp->reset_lock, flags);
}

#ifdef CONFIG_NET_POLL_CONTROLLER
static void srio_poll_controller(struct net_device *ndev)	//dunno
{
	disable_irq(ndev->irq);
	srio_interrupt(ndev->irq, ndev);
	enable_irq(ndev->irq);
}
#endif



int axienet_probe(struct platform_device *pdev)

{
	int rc = 0, j;
	struct net_device *ndev = NULL;
	u32 reg_data, max_len;
	int index;
	uint16_t sum1 = 0;
	uint16_t sum2 = 0;
	uint16_t id;
    u32 maint_data;
	
	struct net_local *lp = NULL;

    unsigned bytes[6];
    unsigned i_byte;

	sscanf(macaddr, "%02x:%02x:%02x:%02x:%02x:%02x", 
            &(bytes[0]),
            &(bytes[1]),
            &(bytes[2]),
            &(bytes[3]),
			&(bytes[4]),
            &(bytes[5])
            );


	dprintk(KERN_ALERT "Initiliazing test module...\n");

	ndev = alloc_etherdev(sizeof(struct net_local));	//allocation memory for a netdevice + our private data
	if (!ndev)
	{
		return -ENOMEM;
	}

    //
    // Default configuration for an ethernet device
    //
	ether_setup(ndev);

    //
    // Define the netdevice as the payload of the platform device
    //
	platform_set_drvdata(pdev, ndev);
    SET_NETDEV_DEV(ndev, &pdev->dev);

    //
    // Set device name
    //
	memcpy(ndev->name, DRIVER_NAME, sizeof(DRIVER_NAME)+1);	

    //
    // Set device's mac adress
    //
    for (i_byte = 0; i_byte < ETH_ALEN; i_byte++ ) {
        ndev->dev_addr[i_byte] = (u8)bytes[i_byte];  
    }

	lp = netdev_priv(ndev);	//puts the datas in our device
	lp->ndev = ndev;
    lp->dev = &pdev->dev;
	lp -> base_addr = ioremap(SRIO_REGS_BASEADDR , SRIO_REGS_SIZE);

    //
    // No pending interrupt upon start
    //
    atomic_set( &lp->pending_intr, 0);

	pdevice = lp;

	if(lp -> base_addr == NULL)
	{
		goto fail_ioremap;
	}	

	reg_data = readl(lp->base_addr + SRIO_MAGIC_OFFSET); //looks if everything is correct
	if(reg_data != SRIO_MAGIC)
	{
		goto fail_magic;
	}
    //
    // Check FPGA revision
    //
	reg_data = __raw_readl(lp->base_addr + SRIO_REVISION_OFFSET); 
    printk("Find SRIO adapter magic in revision %02d.%02d.%02d\n", (reg_data >> 16) & 0xFF, (reg_data >> 8) & 0xFF, reg_data & 0xFF );

    //
    // Looking for the Swtich magic
    //
#if 1
    maint_data = read_maintenance( lp, 0x0, 0x0);
    printk("Magic from SRIO switch 0x%08x\n", maint_data);

    //
    // Enable input and output port 1 (0 is reserved to the CPU)
    //
    write_maintenance(lp,  0x0, 0x17C, 0x00600000);

	//reg_data = __raw_readl(lp->base_addr + SRIO_XFER_ID); 

    //
    // Define we have 0 id, and we are targeting 1, as there is no check on reception
    // it will go through the switch.
    // FIXME: should be computed from the mac address
    //
	__raw_writel(0x00000001,lp->base_addr + SRIO_XFER_ID);

    //
    // Say we are configuring the route to reach id=1...
    //
    write_maintenance(lp,  0x0, 0x70, 0x00000001);

    //
    // We are going through port 1;
    //
    write_maintenance(lp,  0x0, 0x74, 0x00000001);

    //printk("Route to 0x%08x\n", read_maintenance( lp, 0x3, 0x74));
    printk("Route to 0x%08x set to port 0x%08x\n", read_maintenance( lp, 0x0, 0x70), read_maintenance( lp, 0x0, 0x74));
    printk("Port 0 status= 0x%08x\n", read_maintenance( lp, 0x0, 0x158));
    printk("Port 1 status= 0x%08x\n", read_maintenance( lp, 0x0, 0x178));
    printk("Port 2 status= 0x%08x\n", read_maintenance( lp, 0x0, 0x198));

#endif
	spin_lock_init(&lp->reset_lock);	//init spinlock

	ndev->netdev_ops = &srio_netdev_ops;	//telling netdevice to use our functions
	ndev->flags &= ~IFF_MULTICAST;
	ndev->watchdog_timeo = TX_TIMEOUT;
//	ndev->features |= NETIF_F_NO_CSUM;

	rc = register_netdev(ndev); //telling the kernel that our device exists
	if (rc)
       	{
		dprintk("Cannot register \n");
		goto fail_register;
	}	


	srio_tx_tasklet = kmalloc(sizeof(struct tasklet_struct),GFP_KERNEL);// FIXME handle errors

	srio_rx_tasklet = kmalloc(sizeof(struct tasklet_struct),GFP_KERNEL);// FIXME handle errors


	tasklet_init(srio_tx_tasklet, srio_tx_handler, (unsigned long)lp);

	tasklet_init(srio_rx_tasklet, srio_rx_handler, (unsigned long)lp);



	for( index = 0; index < 6; ++index )

	{
		sum1 = (sum1 + bytes[index]) % 255;
		sum2 = (sum2 + sum1) % 255;
	}


	id =  (sum2 << 8) | sum1;

	dprintk("ID : 0x%08x\n", id);
	

	max_len = readl(lp->base_addr + SRIO_MAXBEATLEN_OFFSET);

	/*max_len |= 0xFF;

	__raw_writel(max_len, lp->base_addr + SRIO_MAXBEATLEN_OFFSET);
    */


	for(j = 0; j < 0x50; j += 4)
	{
		dprintk("reg@0x%08x : 0x%08x\n", j, readl(lp->base_addr + j));
	}
	dprintk("max_len=0x%08x \n", max_len);



	lp->trt_srio_dir = proc_mkdir(DRIVER_NAME, NULL);


	if(lp->trt_srio_dir == NULL) {
		
		rc = -ENOMEM;
		goto fail_mkdir;
	}
		

	lp->beat_len_file = proc_create("beat_len", 0644, lp->trt_srio_dir, &beat_len_fops);

	if(lp->beat_len_file == NULL) {
		rc = -ENOMEM;
		goto no_beat_len;
	}
	

	return rc;

no_beat_len:

fail_mkdir:

	
	/*memset(&(lp->kobj), 0, sizeof(struct kobject));

	kobject_init(&(lp->kobj)); device_model en cours

	kobject_set_name(&(lp->kobj), DRIVER_NAME);*/



fail_register:

fail_magic:
	iounmap(lp -> base_addr);

fail_ioremap:
	free_netdev(lp->ndev);

	return rc;
}

static int axienet_remove(struct platform_device *pdev)
{
    int rc = 0;

	struct net_device *ndev = platform_get_drvdata(pdev);
	struct net_local *lp = netdev_priv(ndev);

	dprintk("End of the module...\n");
	tasklet_kill(srio_rx_tasklet);
	tasklet_kill(srio_tx_tasklet);
	iounmap(lp -> base_addr);
	unregister_netdev(lp->ndev);
	free_netdev(lp->ndev);
	remove_proc_entry("beat_len", lp->trt_srio_dir);
	remove_proc_entry("DRIVER_NAME", NULL);

    return rc;

}

static irqreturn_t srio_interrupt(int irq, void *dev_id)
{
	struct net_device *dev = dev_id;
	struct net_local *lp = netdev_priv(dev);

	void __iomem *base_addr = lp -> base_addr;

	u32 int_status;
	

    lp->all_intr++;
    // 
    // Checking the reason for being interrupted
    //
	int_status = readl(base_addr + SRIO_INTERRUPTSTATUS_OFFSET);

	dprintk("\nxxxxxxxxxxxxxxxxxxxxxxxxx INTERRUPT OCCURED xxxxxxxxxxxxxxxxxxxxxxxx\n");

	dprintk("int_status : 0x%08x\n", int_status);
   
    //
    // Let's first inform the device we have processed its interrupt,
    // So if a new interrut is raised during the processing, we will enter
    // again the interrupt
    //
	writel(int_status, base_addr + SRIO_INTERRUPTSTATUS_OFFSET);

    //
    // Do we have received at least one packet ?
    //
	if( (int_status & SRIO_INTERRUPTSTATUS_RX) == SRIO_INTERRUPTSTATUS_RX) {

		dprintk("received at position : %u\n", lp -> pos * SRIO_MSG_SIZE );

        // 
        // Schedule the tasklet for processing the message in non interrupt context
        //
        //atomic_inc( &lp->pending_intr);

   
        tasklet_schedule(srio_rx_tasklet);

		dprintk("\n Mailbox Content:\n");

        //print_hex_buffer( lp->mbox_ptr+(lp->pos*SRIO_MSG_SIZE), 16*10 ) ;

	}

    //
    // Has the device completed the transfer of the last buffer we have to transmit ?
    //
	if((int_status & SRIO_INTERRUPTSTATUS_TX_DONE) == SRIO_INTERRUPTSTATUS_TX_DONE)
	{
        // 
        // Schedule the tasklet for processing the buffer freeing in non interrupt context
        //
		tasklet_schedule(srio_tx_tasklet);
	}


	dprintk("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n");

	return IRQ_HANDLED;
}

static int srio_open(struct net_device *dev)
{
	int retval = 0;

	struct net_local *lp = netdev_priv(dev);
    struct device* pdev = &lp -> ndev -> dev;

    // 
    // Byte size of the mailbox, based on module configuration
    //
    lp -> mbox_entry_count  = (1 << mbox_size_order);
    lp -> mbox_size         = lp -> mbox_entry_count * SRIO_MSG_SIZE;

    //
    // No memory allocated for now
    //
    lp -> mbox_ptr      = NULL;
    lp -> mbox_bus_addr = 0;

    dev_info( pdev, "allocating mailbox of 0x%lx bytes\n", lp -> mbox_size);

    /*lp -> mbox_ptr = dma_alloc_noncoherent(  lp -> ndev->dev.parent, lp->mbox_size, 
            & lp -> mbox_bus_addr,  GFP_KERNEL);
    if ( lp -> mbox_ptr != NULL ) {*/


    dprintk("Allocating DMA pages...\n");
    //lp->mbox_ptr = (void*)__get_dma_pages(GFP_KERNEL, mbox_size_order);
    lp -> mbox_ptr = dma_alloc_coherent(  lp -> ndev->dev.parent, lp->mbox_size, 
            & lp -> mbox_bus_addr,  GFP_KERNEL);
    dprintk("Done, mbox_ptr = 0x%p\n", lp->mbox_ptr);

    if ( lp -> mbox_ptr != NULL ) {

	    u32 entry_count_mask;
	    dma_addr_t mbox_bus_addr;
        unsigned i_byte;


	    //
	    // An entry is detected having been written if the 4 first bytes different of 0xFF 
	    // At initilization time, we fill the whole mailbox with 0xFF
	    //
	    memset_io( lp->mbox_ptr, 0xCA, lp -> mbox_size );

        for(i_byte = 0; i_byte < lp->mbox_entry_count; i_byte++) {
            *((unsigned int*)(lp->mbox_ptr + i_byte * SRIO_MSG_SIZE)) = SRIO_INVALID_MSG_SIZE;
        }

	    dprintk("DMA mapping...\n");

	    dprintk("Done, mbox_bus_addr = %08x\n", lp->mbox_bus_addr);

	    if(lp->mbox_bus_addr == 0)  dev_err(pdev, "DMA mapping failed\n");

	    dprintk("P : %lu vs M : %d", PAGE_SIZE, SRIO_MSG_SIZE);

	    //
	    // Time to inform the device of the address it has to use
	    //
	    writel(lp -> mbox_bus_addr, lp->base_addr + SRIO_MAILBOXBASEADDR_OFFSET);

	    //
	    // Compute the mask defining how many entries are available inside the mailbox
	    //
	    entry_count_mask = (1 << mbox_size_order) -1;
	    writel(entry_count_mask, lp->base_addr + SRIO_MAILBOXSIZE_OFFSET);

	    //
	    // Sanity checking
	    mbox_bus_addr = readl(lp->base_addr + SRIO_MAILBOXBASEADDR_OFFSET);

	    if ( mbox_bus_addr == lp -> mbox_bus_addr ) {


		    dev_info(pdev, "Device mailbox address set to: 0x%08x\n", mbox_bus_addr);

		    //
		    // Attach to IRQ
		    //FIXME use dev->irq when device tree updated
		    //
		    retval = request_irq(SRIO_INTERRUPT, srio_interrupt, 0, dev -> name, dev);
		    if (retval == 0 )
		    {

			    dev->irq = SRIO_INTERRUPT;
                dev_info(pdev, "Driver attached to interrupt %d \n", dev->irq);

			    lp->pos = 0;

			    //
			    // Ready to receive data packet to be transmitted
			    //
			    netif_start_queue(dev);	

                dev_info(pdev, "network queue started \n");



		    } else {

			    dev_err( pdev,  "Unable to attach to interrupt %d\n",  dev -> irq );
		    }

	    } else {

		    dev_err( pdev,  "Unable to read back from device the mailbox base address 0x%p != Ox%08x \n", lp->base_addr + SRIO_MAILBOXBASEADDR_OFFSET, mbox_bus_addr);
		    retval = -EIO;
	    }
    } else {

	    dev_err( pdev,  "Unable to allocate mailbox coherent buffer \n");
	    retval = -ENOMEM;

    } 

    //
    // Cleaning up in case of error
    //
    if ( retval != 0 ) {

	    //
	    // Free DMA buffer
	    // 
	    if (  lp -> mbox_ptr != NULL ) {

		    free_pages((unsigned long)(lp->mbox_ptr), mbox_size_order);

		    lp -> mbox_ptr      = NULL;
		    lp -> mbox_bus_addr = 0;
	    }
    }

    return retval;	
}

static int srio_close(struct net_device *dev)
{
	struct net_local *lp = netdev_priv(dev);

	dprintk("closing module...\n");

    //
    // Stopping the reception of the packet
    //
	netif_stop_queue(dev);	//we don't want any more data

    //
    // Detach from the interruption
    //
	free_irq(dev->irq, dev);	

    /*dma_unmap_single(lp -> ndev->dev.parent, lp->mbox_bus_addr,
			lp->mbox_size, DMA_BIDIRECTIONAL);*/

    dma_free_coherent(&lp -> ndev->dev,
				lp->mbox_size,
				(void*)lp -> mbox_ptr,
				lp->mbox_bus_addr);;

    //
    // Free the mailbox memory
    //
    //free_pages((unsigned long)(lp->mbox_ptr), mbox_size_order);

	return 0;
}


static struct net_device_ops srio_netdev_ops = {
	.ndo_open		= srio_open,
	.ndo_stop		= srio_close,
	.ndo_start_xmit		= srio_send,
	.ndo_set_mac_address	= srio_set_mac_address,
	.ndo_tx_timeout		= srio_tx_timeout,
#ifdef CONFIG_NET_POLL_CONTROLLER
	.ndo_poll_controller = srio_poll_controller,
#endif
};
static struct platform_driver axienet_driver = {
	.probe = axienet_probe,
	.remove = axienet_remove,
	.driver = {
		 .owner = THIS_MODULE,
		 .name = "trt_srio",
		 .of_match_table = axienet_of_match,
	},
};


module_platform_driver(axienet_driver);


MODULE_AUTHOR("THALES TRT FRANCE");
MODULE_DESCRIPTION("RapidIO Ethernet driver");
MODULE_LICENSE("GPL");
