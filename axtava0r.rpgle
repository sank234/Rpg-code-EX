     h dftactgrp(*no)                                                                               //D10055
     h actgrp(*CALLER)                                                                              //D10055
     h Copyright('Copyright © 2023 Infor. All rights reserved. +
     h www.infor.com.')                                                                               S42163
     F***************************************************************** *                             S42163
     F*                                                                 *                             S42163
     F*                           NOTICE                                *                             S42163
     F*                                                                 *                             S42163
     F*   THIS SOFTWARE IS THE PROPERTY OF AND CONTAINS                 *                             S42163
     F*   CONFIDENTIAL INFORMATION OF INFOR AND/OR ITS AFFILIATES       *                             S42163
     F*   OR SUBSIDIARIES AND SHALL NOT BE DISCLOSED WITHOUT PRIOR      *                             S42163
     F*   WRITTEN PERMISSION. LICENSED CUSTOMERS MAY COPY AND           *                             S42163
     F*   ADAPT THIS SOFTWARE FOR THEIR OWN USE IN ACCORDANCE WITH      *                             S42163
     F*   THE TERMS OF THEIR SOFTWARE LICENSE AGREEMENT.                *                             S42163
     F*   ALL OTHER RIGHTS RESERVED.                                    *                             S42163
     F*                                                                 *                             S42163
     F*   (c) COPYRIGHT 2023 INFOR.  ALL RIGHTS RESERVED.               *                           //S73152
     F*   THE WORD AND DESIGN MARKS SET FORTH HEREIN ARE                *                             S42163
     F*   TRADEMARKS AND/OR REGISTERED TRADEMARKS OF INFOR              *                             S42163
     F*   AND/OR ITS AFFILIATES AND SUBSIDIARIES. ALL RIGHTS            *                             S42163
     F*   RESERVED.  ALL OTHER TRADEMARKS LISTED HEREIN ARE             *                             S42163
     F*   TRADEMARKS AND/OR REGISTERED TRADEMARKS OF INFOR              *                             S42163
     F*   AND/OR ITS AFFILIATES AND SUBSIDIARIES. ALL RIGHTS            *                             S42163
     F*   RESERVED.  ALL OTHER TRADEMARKS LISTED HEREIN ARE             *                             S42163
     F*   THE PROPERTY OF THEIR RESPECTIVE OWNERS.                      *                             S42163
     F*                                                                 *                             S42163
     F*******************************************************************
     F* MODULE NAME - AXTAVA0R                                          *
     F* OBJECT NAME - Inventory Transaction History                     *
     F* DESCRIPTION - Client maintenance interface                      *
     F* TEMPLATE VERSION - 01.27                                        *                           //S73152
     F*
     F*  01.06 changes:
     F*  ==============
     F*  Enable SOA replication.
     F*                                                                                               S42163
     F*  01.07 changes:                                                                               S42163
     F*  ==============                                                                               S42163
     F*  Avoid calling M user exit when the program is running as a                                   S42163
     F*  child transaction.                                                                           S42163
     F*                                                                 *
     F*******************************************************************
     F* Purpose                                                         *
     F*******************************************************************
     F*                                                                 *
     F* This program creates, edits and applies maintenance             *
     F* transactions to the object's master file.                       *
     F*                                                                 *
     F*---------------------------------------------------------------- *
     F* @@@@ Compile overrides                                          *
     F*---------------------------------------------------------------- *
     F* START USER PROTECTED SECTION  - CMPOVR 0001.000 / *USR
     F*
     F* Add compile parameters as shown in below sample.
     F* You need to change position 6 to 'Z' for Edison to recognize.
     F*
CRTP F* COMMIT(*NONE) USRPRF(*OWNER) DYNUSRPRF(*OWNER)
     F*
     F* FINISH USER PROTECTED SECTION - CMPOVR 0001.000
     F*---------------------------------------------------------------- *
     F*                                                                 *
     F*---------------------------------------------------------------- *
     F* @@@@ Maintenance history                                        *
     F*---------------------------------------------------------------- *
     F* START USER PROTECTED SECTION  - SECA0R 0001.000 / *USR
     F* VERSION 02/RELEASE 10           PTF 1000907                                                 //S73152
     F* DATE 08/17/10                                                   *                             S42163
     F* APARS/PTFS APPLIED                                              *
     F*              XAR09 - S42163/0901471,S45151/0902150              *                             S45151
      *                      S47026/0902628,S48144/0902923                                          //S48144
      *                      S48721/0903062                                                         //S48721
      *                      S49743/0903147,S51642/0903614                                          //S51642
      *                      S55862/0904621,S56880/0904885                                          //S56670
      *                      S56878/0904886,S56881/0904887                                          //S56670
      *                      S60956/0920962                                                         //S60956
      *                      S65516/0922359                                                         //S65516
      *              XAR10 - S73152/1000907                                                         //S73152
                                                                                                      S47026
     F* FINISH USER PROTECTED SECTION - SECA0R 0001.000
     F*---------------------------------------------------------------- *
     F*******************************************************************
     F* Files                                                           *
     F*******************************************************************
     F*---------------------------------------------------------------- *
     F* @@@@ Additional file definitions                                *
     F*---------------------------------------------------------------- *
     F* START USER PROTECTED SECTION  - SECA0R 0002.000 / *USR
                                                                                                      S47026
     fpoitmxl0  if   e           k disk    usropn                                                     S47026
     fpomasxl0  if   e           k disk    usropn                                                   //S48144
     finvstsl0  if   e           k disk    usropn                                                   //S56670
     fitembxl0  if   e           k disk    usropn                                                   //S48144
     ftrnstsl0  if   e           k disk    usropn                                                   //S47026
     ftrnstsl1  if   e           k disk    usropn                                                   //S47026
                                                                                                      S47026
     F* FINISH USER PROTECTED SECTION - SECA0R 0002.000
     F*---------------------------------------------------------------- *
     F*******************************************************************
     D/EJECT
     D*---------------------------------------------------------------- *
     D* @@@@ Additional user data definitions.                          *
     D*---------------------------------------------------------------- *
     D* START USER PROTECTED SECTION  - SECA0R 0003.000 / *USR
     D* FINISH USER PROTECTED SECTION - SECA0R 0003.000
     D*---------------------------------------------------------------- *
     D*******************************************************************
     D* Arrays                                                          *
     D*******************************************************************
     D COPSI           S             80    DIM(2) CTDATA PERRCD(1)
     D*******************************************************************
     D* Data structures                                                 *
     D*******************************************************************
     D*
     D* Messaging data structures.
     D*
     D* For retrieving messages - PSXRTM1C
     D RTMSG           DS           512
     D RTSCLV          DS          3000
     D*
     D* Program status.
     D*
     D PGMSTS         SDS
     D* Program Name
     D  $PPGNM                       10A
     D* Current Status Code
     D  $PSTSC                        5S 0
     D* Previous Status Code
     D  $PSTSP                        5S 0
     D* SEU Sequence Number
     D  $PSEUN                        8A
     D* Subroutine Name
     D  $PSUBR                        8A
     D* Number of Parameters
     D  $PPCNT                        3S 0
     D* Exception Message ID
     D  $PMSID                        7A
     D* MI/ODT Number
     D  $PMINB                        4A
     D* Work Area for Messages
     D  $PMSWK                       30A
     D* Program's Library Name
     D  $PPLIB                       10A
     D* Retrieved Exception Message Data
     D  $PMSDT                       80A
     D* Called Program Exception Message ID
     D  $PCPMS                        4A
     D* Filler
     D  $PFLR1                       26A
     D* Name of File in Error
     D  $PERRF                        8A
     D* Status of Last File
     D  $PSTSF                       35A
     D* Job Name
     D  $PJBNM                       10A
     D* User ID
     D  $PUSER                       10A
     D* Job Number
     D  $PJBNO                        6S 0
     D* Job Entry Date - yymmdd
     D  $PJEDT                        6S 0
     D* Job Start Date - yymmdd
     D  $PJSDT                        6S 0
     D* Job Start Time - hhmmss
     D  $PJSTM                        6S 0
     D* Compile Date
     D  $PCPDT                        6A
     D* Compile Time
     D  $PCPTM                        6A
     D* Compiler Level
     D  $PCPLV                        4A
     D* Source File Name
     D  $PSRCF                       10A
     D* Source File's Library Name
     D  $PSRCL                       10A
     D* Source File's Member Name
     D  $PSRCM                       10A
     D* Program containg procedure
     D  $PPRCP                       10A
     D* Module containg procedure
     D  $PMDCP                       10A
     D* Source Id matching stmt number 21 - 28
     D  $PSID1                        2B 0
     D* Source Id matching stmt number 225 - 235
     D  $PSID2                        2B 0
     D* Current user profile name.
     D  $PCRUS                       10A
     D* Filler
     D  $PFLR2                       62A
     D*
     D* Mapics transaction buffer
     D*
     D P#RC1T          DS
     D* Transaction Reason code
     D  mRSCD                        10A
     D* Transaction task token
     D  mTSTK                        10A
     D* Transaction ID
     D  mTRID                        10A
     D* Mapics object Class
     D  mOBCL                        10A
     D* Copy transaction flag
     D  mCOPY                         1A
     D* Copy attachment token
     D  mATTK                        10A
     D* Originating process
     D  mORPR                        10A
     D* Bypass maintenance history?                                                                   S46409
     D  mBPMH                         1A                                                              S46409
     D* Logical Transaction ID                                                                        S46409
     D  mLTRN                        10A                                                              S46409
     D* Originating program                                                                           S46409
     D  mOPGM                        10A                                                              S46409
     D* Delete related data ?
     D  mDLTR                         1A
     D* Auto key gen status
     D  mAOIG                         1A
     D* Auto key gen validation status
     D  mAOIV                         1A
     D* CrtDlg key is user entered ?
     D  mUSKY                         1A
     D* CrtDlg key is changed ?
     D  mGKYC                         1A
     D* SOA replication destinations
     D  mSOAD                       128A
     D* Maintenance control
     D  mMCTL                         1A
     D* Posted date
     D  mUPDTAV                   00007S 0
     D* Copy from key value - Posted date
     D  cUPDTAV                   00007S 0
     D* Auto key gen put back value - Posted date
     D  pUPDTAV                   00007S 0
     D* Posted time
     D  mUPTMAV                   00006S 0
     D* Copy from key value - Posted time
     D  cUPTMAV                   00006S 0
     D* Auto key gen put back value - Posted time
     D  pUPTMAV                   00006S 0
     D* Order
     D  mORDRAV                   00007A
     D* Copy from key value - Order
     D  cORDRAV                   00007A
     D* Auto key gen put back value - Order
     D  pORDRAV                   00007A
     D* Item
     D  mITNOAV                   00015A
     D* Copy from key value - Item
     D  cITNOAV                   00015A
     D* Auto key gen put back value - Item
     D  pITNOAV                   00015A
     D* Warehouse
     D  mWHIDAV                   00003A
     D* Copy from key value - Warehouse
     D  cWHIDAV                   00003A
     D* Auto key gen put back value - Warehouse
     D  pWHIDAV                   00003A
     D* Token
     D  mTKENAV                   00015A
     D* Copy from key value - Token
     D  cTKENAV                   00015A
     D* Auto key gen put back value - Token
     D  pTKENAV                   00015A
     D* Number of inventory trans GL records                                                        //D10055
     D  mNBRIAV                   00011S 0                                                            S47026
     D* Active record code
     D  mACRCAV                   00001A
     D* Average unit cost
     D  mAVCSAV                   00019S 8
     D* Batch type
     D  mBCHTAV                   00001A
     D* Badge number
     D  mBDGEAV                   00005S 0
     D* Batch number
     D  mBHNOAV                   00003S 0
     D* Release
     D  mBKSQAV                   00004S 0
     D* Carrier (derived)                                                                           //S59459
     D  mCARRAV                   00010A                                                            //S59459
     D* Component item description
     D  mCDSCAV                   00030A
     D* Charge line number
     D  mCGLNAV                   00005S 0
     D* Child transaction
     D  mCHTNAV                   00001A
     D* Completion code
     D  mCMPLAV                   00001A
     D* Country of origin
     D  mCOFOAV                   00003A
     D* Company
     D  mCONOAV                   00002S 0
     D* Cost code
     D  mCOSCAV                   00006A
     D* Crew number or crew size
     D  mCREWAV                   00003A
     D* Current Item Revision
     D  mCURVAV                   00006A
     D* Customer allowance
     D  mCUSAAV                   00001A
     D* Quantity on-hand last affected date
     D  mDTLOAV                   00007S 0
     D* Administrative division ID
     D  mDVIDAV                   00010A
     D* Drawing number
     D  mEGNOAV                   00015A
     D* Unit of measure
     D  mENTUAV                   00002A
     D* FIFO date
     D  mFFDTAV                   00007S 0
     D* Freight terms (derived)                                                                     //S59459
     D  mFRATAV                   00040A                                                            //S59459
     D* General ledger interface
     D  mGLICAV                   00001A
     D* GRN invoice number
     D  mGRNIAV                   00001A
     D* Goods received note
     D  mGRNNAV                   00026A
     D* Inventory code
     D  mINVFAV                   00002S 0
     D* Transaction matched to invoice
     D  mINVMAV                   00001A
     D* Item sequence
     D  mITMSAV                   00007S 0
     D* Customer order kit release number
     D  mKTRLAV                   00005S 0
     D* Batch/lot
     D  mLBHNAV                   00010A
     D* Local Data Area 1 - 256
     D  mLDA1AV                   00256A
     D* Local data area 257 - 512
     D  mLDA2AV                   00256A
     D* Local data area 513 - 768
     D  mLDA3AV                   00256A
     D* Local data area 769 - 1024
     D  mLDA4AV                   00256A
     D* Production line started
     D  mLICDAV                   00001A
     D* Last physical/cycle inventory date
     D  mLPHDAV                   00007S 0
     D* Miscellaneous/service item sequence
     D  mLSEQAV                   00003S 0
     D* MMS costing method
     D  mMMCMAV                   00001A
     D* Manufacturing order status
     D  mMOSTAV                   00002A
     D* MRO item
     D  mMROIAV                   00001A
     D* New quantity allocated
     D  mNALCAV                   00010S 3
     D* New batch/lot
     D  mNLBHAV                   00010A
     D* New location
     D  mNLLOAV                   00007A
     D* New quantity on-hand
     D  mNQOHAV                   00010S 3
     D* New quantity on-order
     D  mNQOOAV                   00010S 3
     D* Order due date
     D  mODDTAV                   00007S 0
     D* Operation where used
     D  mOPRUAV                   00004A
     D* Operation sequence
     D  mOPSQAV                   00004A
     D* Order (formatted)
     D  mORDXAV                   00012A
     D* Order type
     D  mORTPAV                   00001A
     D* Originating transaction code
     D  mORTRAV                   00002A
     D* Previous quantity allocated
     D  mPALCAV                   00010S 3
     D* Previous unit cost
     D  mPCSOAV                   00019S 8
     D* P.O. item sequence
     D  mPISQAV                   00005S 0
     D* Planner
     D  mPLANAV                   00005S 0
     D* Production line
     D  mPLINAV                   00005A
     D* Purchase overhead (local currency)
     D  mPOVLAV                   00015S 4
     D* Stocking purchase price (local currency)
     D  mPPSLAV                   00015S 4
     D* Previous quantity on-order
     D  mPQOOAV                   00010S 3
     D* Previous location quantity allocated
     D  mPRLAAV                   00010S 3
     D* Previous location quantity on-hand
     D  mPRLQAV                   00010S 3
     D* Previous quantity on-hand
     D  mPRQHAV                   00010S 3
     D* Register printed
     D  mPRTNAV                   00001A
     D* Posted timestamp
     D  mPSTMAV                   00026A
     D* Purge date
     D  mPUDTAV                   00007S 0
     D* QC status
     D  mQCFGAV                   00001A
     D* Location quantity allocated
     D  mQLALAV                   00010S 3
     D* Location quantity on-hand
     D  mQLOHAV                   00010S 3
     D* Quantity per unit expanded
     D  mQPREAV                   00015S 7
     D* Total quantity required
     D  mQTEQAV                   00010S 3
     D* Reason
     D  mRECDAV                   00006A
     D* Reference
     D  mRENOAV                   00010A
     D* Required date
     D  mREQTAV                   00007S 0
     D* Reversed token
     D  mRKENAV                   00015A
     D* Customer order release
     D  mRLNBAV                   00005S 0
     D* Restored transaction
     D  mRSTNAV                   00001A
     D* Resupply requested
     D  mRSUPAV                   00001S 0
     D* Reversal code
     D  mRVCDAV                   00001A
     D* Reversing transaction
     D  mRVNGAV                   00001A
     D* Reversed transaction
     D  mRVTNAV                   00001A
     D* Ship-to contact (derived                                                                    //S59459
     D  mS2CTAV                   00025A                                                            //S59459
     D* Ship-to dock (derived)                                                                      //S59459
     D  mS2DKAV                   00030A                                                            //S59459
     D* Ship-to name (derived)                                                                      //S59459
     D  mS2NMAV                   00035A                                                            //S59459
     D* Sales cost (derived)
     D  mSACSAV                   00015S 4
     D* Sales analysis tracking
     D  mSANFAV                   00001S 0
     D* Shutdown input parm from AMI3H
     D  mSHDNAV                   00001A
     D* Ship-from name (derived)                                                                    //S59459
     D  mSHFNAV                   00050A                                                            //S59459
     D* Employee shift override
     D  mSHFTAV                   00001A
     D* Shipment (derived)                                                                          //S59459
     D  mSHPMAV                   00030A                                                            //S59459
     D* S-number
     D  mSNMRAV                   00020A
     D* New unit cost
     D  mSTPCAV                   00019S 8
     D* Tag number
     D  mTAGNAV                   00030A
     D* Transaction ID
     D  mTAIDAV                   00011S 0
     D* Transaction amount (keyed)
     D  mTAMTAV                   00015S 4
     D* Transaction amount (calculated)
     D  mTAMXAV                   00015S 4
     D* Transaction code
     D  mTCDEAV                   00002A
     D* Clock time
     D  mTMCDAV                   00004A
     D* Transaction date
     D  mTRDAAV                   00007S 0
     D* Their reference invoice number
     D  mTRINAV                   00026A
     D* Work station ID
     D  mTRMIAV                   00010A
     D* Transaction quantity
     D  mTRQTAV                   00010S 3
     D* Transfer warehouse
     D  mTRWHAV                   00003A
     D* Top parent token
     D  mTTKNAV                   00015A
     D* Reversed update period date (calculated)
     D  mUPDRAV                   00007S 0
     D* Reversed posted time (calculated)
     D  mUPTRAV                   00006S 0
     D* M.O. User sequence or P.O. line
     D  mUSRQAV                   00004A
     D* User field - text 40
     D  mUU40AV                   00040A
     D* User field - amount 1
     D  mUUA1AV                   00015S 2
     D* User field - code a
     D  mUUCAAV                   00005A
     D* User field - code b                                                                         //S56670
     D  mUUCBAV                   00005A                                                            //S56670
     D* User field - code c                                                                         //S56670
     D  mUUCCAV                   00005A                                                            //S56670
     D* User field - date 1
     D  mUUD1AV                   00007S 0
     D* User field - currency a
     D  mUUIAAV                   00003A
     D* User field - quantity 1
     D  mUUQ1AV                   00011S 3
     D* User field - switch a
     D  mUUSAAV                   00001A
     D* User field - switch b                                                                       //S56670
     D  mUUSBAV                   00001A                                                            //S56670
     D* User field - switch c                                                                       //S56670
     D  mUUSCAV                   00001A                                                            //S56670
     D* Alternate transaction type - SP reserved
     D  mUVATAV                   00005A
     D* Receiver number - SP reserved
     D  mUVRNAV                   00010A
     D* Vendor catalog number
     D  mVCLNAV                   00025A
     D* Vendor name (derived)                                                                       //S59459
     D  mVNDNAV                   00035A                                                            //S59459
     D* Vendor
     D  mVNDRAV                   00006A
     D* Location
     D  mWHLCAV                   00007A
     D* Work order
     D  mWONBAV                   00006A
     D* Work order task
     D  mWTSKAV                   00002S 0
     D* Internal token                                                                                S46409
     D  mZTKNAV                   00015A                                                              S46409
     D* Bill of lading (derived)                                                                    //S59459
     D  mBILLAV                   00030A                                                            //S59459
     D* Ship-to ID (derived)                                                                        //S59459
     D  mS2IDAV                   00003S 0                                                          //S59459
     D* LIFITR Token                                                                                //D10055
     D  mLFTK3N                   00032A                                                            //D10055
     D* New average unit cost                                                                       //D10055
     D  mNAUC3N                   00019S 8                                                          //D10055
     D* New default unit cost                                                                       //D10055
     D  mNDUC3N                   00019S 8                                                          //D10055
     D* New last unit cost                                                                          //D10055
     D  mNLUC3N                   00019S 8                                                          //D10055
     D* New standard unit cost                                                                      //D10055
     D  mNSUC3N                   00019S 8                                                          //D10055
     D* Previous average unit cost                                                                  //D10055
     D  mPAUC3N                   00019S 8                                                          //D10055
     D* Previous default unit cost                                                                  //D10055
     D  mPDUC3N                   00019S 8                                                          //D10055
     D* Previous last unit cost                                                                     //D10055
     D  mPLUC3N                   00019S 8                                                          //D10055
     D* Previous standard unit cost                                                                 //D10055
     D  mPSUC3N                   00019S 8                                                          //D10055
     D* Batch/lot (extended)
     D  mBBLNQC                   00030A
     D* Serial
     D  mBSRLQC                   00030A
     D* Container
     D  mCNIDQC                   00030A
     D* Parent token
     D  mPTKNQC                   00015A
     D* Receipt token
     D  mSCTKQC                   00032A
     D* Item token
     D  mSITKQC                   00032A
     D* Shipment
     D  mSPIDQC                   00030A
     D* Print code
     D  mPRCDA3                   00001A
     D* History archive log sequence
     D  mHKSQNP                   00005S 0
     D* Reversed warehouse
     D  mRHIDE9                   00003A
     D* Reversed token
     D  mRKENE9                   00015A
     D* Reversed posted date
     D  mRPDTE9                   00007S 0
     D* Reversed posted time
     D  mRPTME9                   00006S 0
     D* Reversed order
     D  mRRDRE9                   00007A
     D* Reversed item
     D  mRTNOE9                   00015A
     D* ERP qty received negative (order UM)                                                        //S62268
     D  mRNOM8Q                   00015S 3                                                          //S62532
     D* ERP qty received positive (order UM)                                                        //S62268
     D  mRPOM8Q                   00015S 3                                                          //S62532
     D* ERP qty received negative (purchase UM)                                                     //D10055
     D  mRNPM0D                   00015S 3                                                          //S62532
     D* ERP qty received negative (stocking UM)                                                     //D10055
     D  mRNSM0D                   00015S 3                                                          //S62532
     D* ERP qty received negative (vendor UM)                                                       //D10055
     D  mRNVM0D                   00015S 3                                                          //S62532
     D* ERP qty received positive (purchase UM)                                                     //D10055
     D  mRPPM0D                   00015S 3                                                          //S62532
     D* ERP qty received positive (stocking UM)                                                     //D10055
     D  mRPSM0D                   00015S 3                                                          //S62532
     D* ERP qty received positive (vendor UM)                                                       //D10055
     D  mRPVM0D                   00015S 3                                                          //S62532
     D* Goods received note                                                                         //D10055
     D  mCEE40D                   00026A                                                            //D10055
     D* Line                                                                                        //D10055
     D  mARNB0D                   00005S 0                                                          //D10055
     D* Purchase order                                                                              //D10055
     D  mNCMF0D                   00007A                                                            //D10055
     D* ERP qty received negative (order UM)                                                        //S62268
     D  mRNOM8S                   00015S 3                                                          //S62532
     D* ERP qty received positive (order UM)                                                        //S62268
     D  mRPOM8S                   00015S 3                                                          //S62532
     D* ERP qty received negative (purchase UM)                                                     //D10055
     D  mRNPM0F                   00015S 3                                                          //S62532
     D* ERP qty received negative (stocking UM)                                                     //D10055
     D  mRNSM0F                   00015S 3                                                          //S62532
     D* ERP qty received negative (vendor UM)                                                       //D10055
     D  mRNVM0F                   00015S 3                                                          //S62532
     D* ERP qty received positive (purchase UM)                                                     //D10055
     D  mRPPM0F                   00015S 3                                                          //S62532
     D* ERP qty received positive (stocking UM)                                                     //D10055
     D  mRPSM0F                   00015S 3                                                          //S62532
     D* ERP qty received positive (vendor UM)                                                       //D10055
     D  mRPVM0F                   00015S 3                                                          //S62532
     D* Goods received note                                                                         //D10055
     D  mCEE40F                   00026A                                                            //D10055
     D* Line                                                                                        //D10055
     D  mARNB0F                   00005S 0                                                          //D10055
     D* Purchase order                                                                              //D10055
     D  mNCMF0F                   00007A                                                            //D10055
     D* Release                                                                                     //D10055
     D  mRLSE0F                   00004S 0                                                          //D10055
     D* ERP qty app invoiced not matched (order UM)                                                 //S62268
     D  mAIOM8P                   00015S 4                                                          //S62268
     D* ERP qty matched (order UM)                                                                  //S62268
     D  mQMOM8P                   00015S 4                                                          //S62268
     D* ERP qty received negative (order UM)                                                        //S62268
     D  mRNOM8P                   00015S 3                                                          //S62532
     D* ERP qty received positive (order UM)                                                        //S62268
     D  mRPOM8P                   00015S 3                                                          //S62532
     D* ERP qty user invoiced not matched (order UM)                                                //S62268
     D  mUIOM8P                   00015S 4                                                          //S62268
     D* Actual freight amount                                                                       //D10055
     D  mVAGC0C                   00013S 2                                                          //D10055
     D* Actual freight amount LC                                                                    //D10055
     D  mVAGD0C                   00013S 2                                                          //D10055
     D* Actual price for order                                                                      //D10055
     D  mVAFV0C                   00013S 2                                                          //D10055
     D* Actual price for order LC                                                                   //D10055
     D  mVAFW0C                   00013S 2                                                          //D10055
     D* Actual quantity for order                                                                   //D10055
     D  mVAFX0C                   00010S 3                                                          //D10055
     D* Actual via code                                                                             //D10055
     D  mCEDJ0C                   00003A                                                            //D10055
     D* Date goods first received                                                                   //D10055
     D  mNCD60C                   00007S 0                                                          //D10055
     D* Date last receipt                                                                           //D10055
     D  mNCEG0C                   00007S 0                                                          //D10055
     D* Date order completed                                                                        //D10055
     D  mNCEF0C                   00007S 0                                                          //D10055
     D* Date received to stock                                                                      //D10055
     D  mNCD80C                   00007S 0                                                          //D10055
     D* Delivered qty (stocking UM)                                                                 //D10055
     D  mDLQS0C                   00010S 3                                                          //D10055
     D* Deviation qty (stocking UM)                                                                 //D10055
     D  mDVQS0C                   00010S 3                                                          //D10055
     D* Dock qty (stocking UM)                                                                      //D10055
     D  mDCQS0C                   00010S 3                                                          //D10055
     D* ERP qty app invoiced not matched (purchase UM)                                              //D10055
     D  mAIPM0C                   00015S 4                                                          //S56670
     D* ERP qty app invoiced not matched (stocking UM)                                              //D10055
     D  mAISM0C                   00015S 4                                                          //S56670
     D* ERP qty app invoiced not matched (vendor UM)                                                //D10055
     D  mAIVM0C                   00015S 4                                                          //S56670
     D* ERP qty matched (purchase UM)                                                               //D10055
     D  mQMPM0C                   00015S 4                                                          //S56670
     D* ERP qty matched (stocking UM)                                                               //D10055
     D  mQMSM0C                   00015S 4                                                          //S56670
     D* ERP qty matched (vendor UM)                                                                 //D10055
     D  mQMVM0C                   00015S 4                                                          //S56670
     D* ERP qty received negative (purchase UM)                                                     //D10055
     D  mRNPM0C                   00015S 3                                                          //S62532
     D* ERP qty received negative (stocking UM)                                                     //D10055
     D  mRNSM0C                   00015S 3                                                          //S62532
     D* ERP qty received negative (vendor UM)                                                       //D10055
     D  mRNVM0C                   00015S 3                                                          //S62532
     D* ERP qty received positive (purchase UM)                                                     //D10055
     D  mRPPM0C                   00015S 3                                                          //S62532
     D* ERP qty received positive (stocking UM)                                                     //D10055
     D  mRPSM0C                   00015S 3                                                          //S62532
     D* ERP qty received positive (vendor UM)                                                       //D10055
     D  mRPVM0C                   00015S 3                                                          //S62532
     D* ERP qty user invoiced not matched (purchase UM)                                             //D10055
     D  mUIPM0C                   00015S 4                                                          //S56670
     D* ERP qty user invoiced not matched (stocking UM)                                             //D10055
     D  mUISM0C                   00015S 4                                                          //S56670
     D* ERP qty user invoiced not matched (vendor UM)                                               //D10055
     D  mUIVM0C                   00015S 4                                                          //S56670
     D* Ext qty invoiced not matched (purchase UM)                                                  //D10055
     D  mEQPM0C                   00015S 4                                                          //S56670
     D* Ext qty invoiced not matched (stocking UM)                                                  //D10055
     D  mEQSM0C                   00015S 4                                                          //S56670
     D* Ext qty invoiced not matched (vendor)                                                       //D10055
     D  mEQVM0C                   00015S 4                                                          //S56670
     D* Ext qty matched (purchase UM)                                                               //D10055
     D  mEXPM0C                   00015S 4                                                          //S56670
     D* Ext qty matched (stocking UM)                                                               //D10055
     D  mEXSM0C                   00015S 4                                                          //S56670
     D* Ext qty matched (vendor UM)                                                                 //D10055
     D  mEXVM0C                   00015S 4                                                          //S56670
     D* Ext qty received negative (purchase UM)                                                     //D10055
     D  mEQNP0C                   00010S 3                                                          //D10055
     D* Ext qty received negative (stocking UM)                                                     //D10055
     D  mEQNS0C                   00010S 3                                                          //D10055
     D* Ext qty received negative (vendor UM)                                                       //D10055
     D  mEQNV0C                   00010S 3                                                          //D10055
     D* Ext qty received positive (purchase UM)                                                     //D10055
     D  mEQPP0C                   00010S 3                                                          //D10055
     D* Ext qty received positive (stocking UM)                                                     //D10055
     D  mEQPS0C                   00010S 3                                                          //D10055
     D* Ext qty received positive (vendor UM)                                                       //D10055
     D  mEQPV0C                   00010S 3                                                          //D10055
     D* Inspection quantity                                                                         //D10055
     D  mVAFQ0C                   00010S 3                                                          //D10055
     D* Invoice flag                                                                                //D10055
     D  mSSDO0C                   00001A                                                            //D10055
     D* Last activity date                                                                          //D10055
     D  mNCB60C                   00007S 0                                                          //D10055
     D* Lead time actual                                                                            //D10055
     D  mNCEJ0C                   00003S 0                                                          //D10055
     D* Line                                                                                        //D10055
     D  mARNB0C                   00005S 0                                                          //D10055
     D* Number of days early                                                                        //D10055
     D  mNCEA0C                   00005S 0                                                          //D10055
     D* Number of days late                                                                         //D10055
     D  mNCEB0C                   00005S 0                                                          //D10055
     D* Percent remaining on blankets                                                               //D10055
     D  mPROB0C                   00003S 2                                                          //D10055
     D* Purchase order                                                                              //D10055
     D  mNCMF0C                   00007A                                                            //D10055
     D* Quantity delivered                                                                          //D10055
     D  mVAFY0C                   00010S 3                                                          //D10055
     D* Quantity deviation                                                                          //D10055
     D  mVACU0C                   00010S 3                                                          //D10055
     D* Quantity last receipt                                                                       //D10055
     D  mVAF60C                   00010S 3                                                          //D10055
     D* Quantity received at dock                                                                   //D10055
     D  mVAFP0C                   00010S 3                                                          //D10055
     D* Quantity received to stock                                                                  //D10055
     D  mVAFR0C                   00010S 3                                                          //D10055
     D* Quantity returned                                                                           //D10055
     D  mVAF20C                   00010S 3                                                          //D10055
     D* Resupply no return quantity                                                                 //D10055
     D  mVAF30C                   00010S 3                                                          //D10055
     D* RP cost amounts                                                                             //D10055
     D  mVAH70C                   00015S 4                                                          //D10055
     D* Scrap quantity                                                                              //D10055
     D  mVAC10C                   00010S 3                                                          //D10055
     D* Ship via actual description                                                                 //D10055
     D  mTXBM0C                   00025A                                                            //D10055
     D* Transaction activity code                                                                   //D10055
     D  mSSDE0C                   00001A                                                            //D10055
     D* Transaction amount                                                                          //D10055
     D  mVAA90C                   00015S 4                                                          //D10055
     D* Warehouse stock location                                                                    //D10055
     D  mCEB40C                   00007A                                                            //D10055
     D* ERP qty app invoiced not matched (order UM)                                                 //S62268
     D  mAIOM8R                   00015S 4                                                          //S62268
     D* ERP qty matched (order UM)                                                                  //S62268
     D  mQMOM8R                   00015S 4                                                          //S62268
     D* ERP qty received negative (order UM)                                                        //S62268
     D  mRNOM8R                   00015S 3                                                          //S62532
     D* ERP qty received positive (order UM)                                                        //S62268
     D  mRPOM8R                   00015S 3                                                          //S62532
     D* ERP qty user invoiced not matched (order UM)                                                //S62268
     D  mUIOM8R                   00015S 4                                                          //S62268
     D* Delivered qty (stocking UM)                                                                 //D10055
     D  mDLQS0E                   00010S 3                                                          //D10055
     D* Deviation qty (stocking UM)                                                                 //D10055
     D  mDVQS0E                   00010S 3                                                          //D10055
     D* Dock qty (stocking UM)                                                                      //D10055
     D  mDCQS0E                   00010S 3                                                          //D10055
     D* ERP qty app invoiced not matched (purchase UM)                                              //D10055
     D  mAIPM0E                   00015S 4                                                          //S56670
     D* ERP qty app invoiced not matched (stocking UM)                                              //D10055
     D  mAISM0E                   00015S 4                                                          //S56670
     D* ERP qty app invoiced not matched (vendor UM)                                                //D10055
     D  mAIVM0E                   00015S 4                                                          //S56670
     D* ERP qty matched (purchase UM)                                                               //D10055
     D  mQMPM0E                   00015S 4                                                          //S56670
     D* ERP qty matched (stocking UM)                                                               //D10055
     D  mQMSM0E                   00015S 4                                                          //S56670
     D* ERP qty matched (vendor UM)                                                                 //D10055
     D  mQMVM0E                   00015S 4                                                          //S56670
     D* ERP qty received negative (purchase UM)                                                     //D10055
     D  mRNPM0E                   00015S 3                                                          //S62532
     D* ERP qty received negative (stocking UM)                                                     //D10055
     D  mRNSM0E                   00015S 3                                                          //S62532
     D* ERP qty received negative (vendor UM)                                                       //D10055
     D  mRNVM0E                   00015S 3                                                          //S62532
     D* ERP qty received positive (purchase UM)                                                     //D10055
     D  mRPPM0E                   00015S 3                                                          //S62532
     D* ERP qty received positive (stocking UM)                                                     //D10055
     D  mRPSM0E                   00015S 3                                                          //S62532
     D* ERP qty received positive (vendor UM)                                                       //D10055
     D  mRPVM0E                   00015S 3                                                          //S62532
     D* ERP qty user invoiced not matched (purchase UM)                                             //D10055
     D  mUIPM0E                   00015S 4                                                          //S56670
     D* ERP qty user invoiced not matched (stocking UM)                                             //D10055
     D  mUISM0E                   00015S 4                                                          //S56670
     D* ERP qty user invoiced not matched (vendor UM)                                               //D10055
     D  mUIVM0E                   00015S 4                                                          //S56670
     D* Ext qty invoiced not matched (purchase UM)                                                  //D10055
     D  mEQPM0E                   00015S 4                                                          //S56670
     D* Ext qty invoiced not matched (stocking UM)                                                  //D10055
     D  mEQSM0E                   00015S 4                                                          //S56670
     D* Ext qty invoiced not matched (vendor)                                                       //D10055
     D  mEQVM0E                   00015S 4                                                          //S56670
     D* Ext qty matched (purchase UM)                                                               //D10055
     D  mEXPM0E                   00015S 4                                                          //S56670
     D* Ext qty matched (stocking UM)                                                               //D10055
     D  mEXSM0E                   00015S 4                                                          //S56670
     D* Ext qty matched (vendor UM)                                                                 //D10055
     D  mEXVM0E                   00015S 4                                                          //S56670
     D* Ext qty received negative (purchase UM)                                                     //D10055
     D  mEQNP0E                   00010S 3                                                          //D10055
     D* Ext qty received negative (stocking UM)                                                     //D10055
     D  mEQNS0E                   00010S 3                                                          //D10055
     D* Ext qty received negative (vendor UM)                                                       //D10055
     D  mEQNV0E                   00010S 3                                                          //D10055
     D* Ext qty received positive (purchase UM)                                                     //D10055
     D  mEQPP0E                   00010S 3                                                          //D10055
     D* Ext qty received positive (stocking UM)                                                     //D10055
     D  mEQPS0E                   00010S 3                                                          //D10055
     D* Ext qty received positive (vendor UM)                                                       //D10055
     D  mEQPV0E                   00010S 3                                                          //D10055
     D* Line                                                                                        //D10055
     D  mARNB0E                   00005S 0                                                          //D10055
     D* Purchase order                                                                              //D10055
     D  mNCMF0E                   00007A                                                            //D10055
     D* Release                                                                                     //D10055
     D  mRLSE0E                   00004S 0                                                          //D10055
     D* Purchase order                                                                              //D10055
     D  mNCMFXL                   00007A                                                            //D10055
     D* Purchase order debit memo                                                                   //D10055
     D  mPODMXL                   00010A                                                            //D10055
     D* Purchase order line                                                                         //D10055
     D  mARNBXL                   00005S 0                                                          //D10055
     D* Purchase order release                                                                      //D10055
     D  mRLSEXL                   00004S 0                                                          //D10055
     D* Transaction date                                                                            //D10055
     D  mABVHXL                   00007S 0                                                          //D10055
     D* Transaction time                                                                            //D10055
     D  mTIMEXL                   00006S 0                                                          //D10055
     D* Inventory token                                                                             //D10055
     D  mINTKXL                   00015A                                                            //D10055
     D* Type                                                                                        //D10055
     D  mTYPEXL                   00002A                                                            //D10055
     D* Token                                                                                       //D10055
     D  mTKENXL                   00015A                                                            //D10055
     D*
     D* Mapics transaction buffer DS definition
     D*
     D P$RC1TDF        DS
     D                               28    INZ('RSCD1T         000010001000A')
     D                               28    INZ('TSTK1T         000110001000A')
     D                               28    INZ('TRID1T         000210001000A')
     D                               28    INZ('OBCL1T         000310001000A')
     D                               28    INZ('COPY4T         000410000100A')
     D                               28    INZ('ATTK6T         000420001000A')
     D                               28    INZ('ORPR1T         000520001000A')
     D                               28    INZ('mBPMH          000620000100A')                        S46409
     D                               28    INZ('mLTRN          000630001000A')                        S46409
     D                               28    INZ('mOPGM          000730001000A')                        S46409
     D                               28    INZ('DLTR6T         000830000100A')                        S46409
     D                               28    INZ('AOIG5T         000840000100A')                        S46409
     D                               28    INZ('AOIV5T         000850000100A')                        S46409
     D                               28    INZ('USKY5T         000860000100A')                        S46409
     D                               28    INZ('GKYC5T         000870000100A')                        S46409
     D                               28    INZ('mSOAD          000880012800A')                        S46409
     D                               28    INZ('mMCTL          002160000100A')                        S46409
     D                               28    INZ('UPDTAV         002170000700S')                        S46409
     D                               28    INZ('cUPDTAV        002240000700S')                        S46409
     D                               28    INZ('pUPDTAV        002310000700S')                        S46409
     D                               28    INZ('UPTMAV         002380000600S')                        S46409
     D                               28    INZ('cUPTMAV        002440000600S')                        S46409
     D                               28    INZ('pUPTMAV        002500000600S')                        S46409
     D                               28    INZ('ORDRAV         002560000700A')                        S46409
     D                               28    INZ('cORDRAV        002630000700A')                        S46409
     D                               28    INZ('pORDRAV        002700000700A')                        S46409
     D                               28    INZ('ITNOAV         002770001500A')                        S46409
     D                               28    INZ('cITNOAV        002920001500A')                        S46409
     D                               28    INZ('pITNOAV        003070001500A')                        S46409
     D                               28    INZ('WHIDAV         003220000300A')                        S46409
     D                               28    INZ('cWHIDAV        003250000300A')                        S46409
     D                               28    INZ('pWHIDAV        003280000300A')                        S46409
     D                               28    INZ('TKENAV         003310001500A')                        S46409
     D                               28    INZ('cTKENAV        003460001500A')                        S46409
     D                               28    INZ('pTKENAV        003610001500A')                        S46409
     D                               28    INZ('NBRIAV         003760001100S')                        S47026
     D                               28    INZ('ACRCAV         003870000100A')                        S47026
     D                               28    INZ('AVCSAV         003880001908S')                        S47026
     D                               28    INZ('BCHTAV         004070000100A')                        S47026
     D                               28    INZ('BDGEAV         004080000500S')                        S47026
     D                               28    INZ('BHNOAV         004130000300S')                        S47026
     D                               28    INZ('BKSQAV         004160000400S')                        S47026
     D                               28    INZ('CARRAV         004200001000A')                      //S59459
     D                               28    INZ('CDSCAV         004300003000A')                      //S59459
     D                               28    INZ('CGLNAV         004600000500S')                      //S59459
     D                               28    INZ('CHTNAV         004650000100A')                      //S59459
     D                               28    INZ('CMPLAV         004660000100A')                      //S59459
     D                               28    INZ('COFOAV         004670000300A')                      //S59459
     D                               28    INZ('CONOAV         004700000200S')                      //S59459
     D                               28    INZ('COSCAV         004720000600A')                      //S59459
     D                               28    INZ('CREWAV         004780000300A')                      //S59459
     D                               28    INZ('CURVAV         004810000600A')                      //S59459
     D                               28    INZ('CUSAAV         004870000100A')                      //S59459
     D                               28    INZ('DTLOAV         004880000700S')                      //S59459
     D                               28    INZ('DVIDAV         004950001000A')                      //S59459
     D                               28    INZ('EGNOAV         005050001500A')                      //S59459
     D                               28    INZ('ENTUAV         005200000200A')                      //S59459
     D                               28    INZ('FFDTAV         005220000700S')                      //S59459
     D                               28    INZ('FRATAV         005290004000A')                      //S59459
     D                               28    INZ('GLICAV         005690000100A')                      //S59459
     D                               28    INZ('GRNIAV         005700000100A')                      //S59459
     D                               28    INZ('GRNNAV         005710002600A')                      //S59459
     D                               28    INZ('INVFAV         005970000200S')                      //S59459
     D                               28    INZ('INVMAV         005990000100A')                      //S59459
     D                               28    INZ('ITMSAV         006000000700S')                      //S59459
     D                               28    INZ('KTRLAV         006070000500S')                      //S59459
     D                               28    INZ('LBHNAV         006120001000A')                      //S59459
     D                               28    INZ('LDA1AV         006220025600A')                      //S59459
     D                               28    INZ('LDA2AV         008780025600A')                      //S59459
     D                               28    INZ('LDA3AV         011340025600A')                      //S59459
     D                               28    INZ('LDA4AV         013900025600A')                      //S59459
     D                               28    INZ('LICDAV         016460000100A')                      //S59459
     D                               28    INZ('LPHDAV         016470000700S')                      //S59459
     D                               28    INZ('LSEQAV         016540000300S')                      //S59459
     D                               28    INZ('MMCMAV         016570000100A')                      //S59459
     D                               28    INZ('MOSTAV         016580000200A')                      //S59459
     D                               28    INZ('MROIAV         016600000100A')                      //S59459
     D                               28    INZ('NALCAV         016610001003S')                      //S59459
     D                               28    INZ('NLBHAV         016710001000A')                      //S59459
     D                               28    INZ('NLLOAV         016810000700A')                      //S59459
     D                               28    INZ('NQOHAV         016880001003S')                      //S59459
     D                               28    INZ('NQOOAV         016980001003S')                      //S59459
     D                               28    INZ('ODDTAV         017080000700S')                      //S59459
     D                               28    INZ('OPRUAV         017150000400A')                      //S59459
     D                               28    INZ('OPSQAV         017190000400A')                      //S59459
     D                               28    INZ('ORDXAV         017230001200A')                      //S59459
     D                               28    INZ('ORTPAV         017350000100A')                      //S59459
     D                               28    INZ('ORTRAV         017360000200A')                      //S59459
     D                               28    INZ('PALCAV         017380001003S')                      //S59459
     D                               28    INZ('PCSOAV         017480001908S')                      //S59459
     D                               28    INZ('PISQAV         017670000500S')                      //S59459
     D                               28    INZ('PLANAV         017720000500S')                      //S59459
     D                               28    INZ('PLINAV         017770000500A')                      //S59459
     D                               28    INZ('POVLAV         017820001504S')                      //S59459
     D                               28    INZ('PPSLAV         017970001504S')                      //S59459
     D                               28    INZ('PQOOAV         018120001003S')                      //S59459
     D                               28    INZ('PRLAAV         018220001003S')                      //S59459
     D                               28    INZ('PRLQAV         018320001003S')                      //S59459
     D                               28    INZ('PRQHAV         018420001003S')                      //S59459
     D                               28    INZ('PRTNAV         018520000100A')                      //S59459
     D                               28    INZ('PSTMAV         018530002600A')                      //S59459
     D                               28    INZ('PUDTAV         018790000700S')                      //S59459
     D                               28    INZ('QCFGAV         018860000100A')                      //S59459
     D                               28    INZ('QLALAV         018870001003S')                      //S59459
     D                               28    INZ('QLOHAV         018970001003S')                      //S59459
     D                               28    INZ('QPREAV         019070001507S')                      //S59459
     D                               28    INZ('QTEQAV         019220001003S')                      //S59459
     D                               28    INZ('RECDAV         019320000600A')                      //S59459
     D                               28    INZ('RENOAV         019380001000A')                      //S59459
     D                               28    INZ('REQTAV         019480000700S')                      //S59459
     D                               28    INZ('RKENAV         019550001500A')                      //S59459
     D                               28    INZ('RLNBAV         019700000500S')                      //S59459
     D                               28    INZ('RSTNAV         019750000100A')                      //S59459
     D                               28    INZ('RSUPAV         019760000100S')                      //S59459
     D                               28    INZ('RVCDAV         019770000100A')                      //S59459
     D                               28    INZ('RVNGAV         019780000100A')                      //S59459
     D                               28    INZ('RVTNAV         019790000100A')                      //S59459
     D                               28    INZ('S2CTAV         019800002500A')                      //S59459
     D                               28    INZ('S2DKAV         020050003000A')                      //S59459
     D                               28    INZ('S2NMAV         020350003500A')                      //S59459
     D                               28    INZ('SACSAV         020700001504S')                      //S59459
     D                               28    INZ('SANFAV         020850000100S')                      //S59459
     D                               28    INZ('SHDNAV         020860000100A')                      //S59459
     D                               28    INZ('SHFNAV         020870005000A')                      //S59459
     D                               28    INZ('SHFTAV         021370000100A')                      //S59459
     D                               28    INZ('SHPMAV         021380003000A')                      //S59459
     D                               28    INZ('SNMRAV         021680002000A')                      //S59459
     D                               28    INZ('STPCAV         021880001908S')                      //S59459
     D                               28    INZ('TAGNAV         022070003000A')                      //S59459
     D                               28    INZ('TAIDAV         022370001100S')                      //S59459
     D                               28    INZ('TAMTAV         022480001504S')                      //S59459
     D                               28    INZ('TAMXAV         022630001504S')                      //S59459
     D                               28    INZ('TCDEAV         022780000200A')                      //S59459
     D                               28    INZ('TMCDAV         022800000400A')                      //S59459
     D                               28    INZ('TRDAAV         022840000700S')                      //S59459
     D                               28    INZ('TRINAV         022910002600A')                      //S59459
     D                               28    INZ('TRMIAV         023170001000A')                      //S59459
     D                               28    INZ('TRQTAV         023270001003S')                      //S59459
     D                               28    INZ('TRWHAV         023370000300A')                      //S59459
     D                               28    INZ('TTKNAV         023400001500A')                      //S59459
     D                               28    INZ('UPDRAV         023550000700S')                      //S59459
     D                               28    INZ('UPTRAV         023620000600S')                      //S59459
     D                               28    INZ('USRQAV         023680000400A')                      //S59459
     D                               28    INZ('UU40AV         023720004000A')                      //S59459
     D                               28    INZ('UUA1AV         024120001502S')                      //S59459
     D                               28    INZ('UUCAAV         024270000500A')                      //S59459
     D                               28    INZ('UUCBAV         024320000500A')                      //S59459
     D                               28    INZ('UUCCAV         024370000500A')                      //S59459
     D                               28    INZ('UUD1AV         024420000700S')                      //S59459
     D                               28    INZ('UUIAAV         024490000300A')                      //S59459
     D                               28    INZ('UUQ1AV         024520001103S')                      //S59459
     D                               28    INZ('UUSAAV         024630000100A')                      //S59459
     D                               28    INZ('UUSBAV         024640000100A')                      //S59459
     D                               28    INZ('UUSCAV         024650000100A')                      //S59459
     D                               28    INZ('UVATAV         024660000500A')                      //S59459
     D                               28    INZ('UVRNAV         024710001000A')                      //S59459
     D                               28    INZ('VCLNAV         024810002500A')                      //S59459
     D                               28    INZ('VNDNAV         025060003500A')                      //S59459
     D                               28    INZ('VNDRAV         025410000600A')                      //S59459
     D                               28    INZ('WHLCAV         025470000700A')                      //S59459
     D                               28    INZ('WONBAV         025540000600A')                      //S59459
     D                               28    INZ('WTSKAV         025600000200S')                      //S59459
     D                               28    INZ('ZTKNAV         025620001500A')                      //S59459
     D                               28    INZ('BILLAV         025770003000A')                      //S59459
     D                               28    INZ('S2IDAV         026070000300S')                      //S59459
     D                               28    INZ('LFTK3N         026100003200A')                      //S59459
     D                               28    INZ('NAUC3N         026420001908S')                      //S59459
     D                               28    INZ('NDUC3N         026610001908S')                      //S59459
     D                               28    INZ('NLUC3N         026800001908S')                      //S59459
     D                               28    INZ('NSUC3N         026990001908S')                      //S59459
     D                               28    INZ('PAUC3N         027180001908S')                      //S59459
     D                               28    INZ('PDUC3N         027370001908S')                      //S59459
     D                               28    INZ('PLUC3N         027560001908S')                      //S59459
     D                               28    INZ('PSUC3N         027750001908S')                      //S59459
     D                               28    INZ('BBLNQC         027940003000A')                      //S59459
     D                               28    INZ('BSRLQC         028240003000A')                      //S59459
     D                               28    INZ('CNIDQC         028540003000A')                      //S59459
     D                               28    INZ('PTKNQC         028840001500A')                      //S59459
     D                               28    INZ('SCTKQC         028990003200A')                      //S59459
     D                               28    INZ('SITKQC         029310003200A')                      //S59459
     D                               28    INZ('SPIDQC         029630003000A')                      //S59459
     D                               28    INZ('PRCDA3         029930000100A')                      //S59459
     D                               28    INZ('HKSQNP         029940000500S')                      //S59459
     D                               28    INZ('RHIDE9         029990000300A')                      //S59459
     D                               28    INZ('RKENE9         030020001500A')                      //S59459
     D                               28    INZ('RPDTE9         030170000700S')                      //S59459
     D                               28    INZ('RPTME9         030240000600S')                      //S59459
     D                               28    INZ('RRDRE9         030300000700A')                      //S59459
     D                               28    INZ('RTNOE9         030370001500A')                      //S59459
     D                               28    INZ('RNOM8Q         030520001503S')                      //S62532
     D                               28    INZ('RPOM8Q         030670001503S')                      //S62532
     D                               28    INZ('RNPM0D         030820001503S')                      //S62532
     D                               28    INZ('RNSM0D         030970001503S')                      //S62532
     D                               28    INZ('RNVM0D         031120001503S')                      //S62532
     D                               28    INZ('RPPM0D         031270001503S')                      //S62532
     D                               28    INZ('RPSM0D         031420001503S')                      //S62532
     D                               28    INZ('RPVM0D         031570001503S')                      //S62532
     D                               28    INZ('CEE40D         031720002600A')                      //S62532
     D                               28    INZ('ARNB0D         031980000500S')                      //S62532
     D                               28    INZ('NCMF0D         032030000700A')                      //S62532
     D                               28    INZ('RNOM8S         032100001503S')                      //S62532
     D                               28    INZ('RPOM8S         032250001503S')                      //S62532
     D                               28    INZ('RNPM0F         032400001503S')                      //S62532
     D                               28    INZ('RNSM0F         032550001503S')                      //S62532
     D                               28    INZ('RNVM0F         032700001503S')                      //S62532
     D                               28    INZ('RPPM0F         032850001503S')                      //S62532
     D                               28    INZ('RPSM0F         033000001503S')                      //S62532
     D                               28    INZ('RPVM0F         033150001503S')                      //S62532
     D                               28    INZ('CEE40F         033300002600A')                      //S62532
     D                               28    INZ('ARNB0F         033560000500S')                      //S62532
     D                               28    INZ('NCMF0F         033610000700A')                      //S62532
     D                               28    INZ('RLSE0F         033680000400S')                      //S62532
     D                               28    INZ('AIOM8P         033720001504S')                      //S62532
     D                               28    INZ('QMOM8P         033870001504S')                      //S62532
     D                               28    INZ('RNOM8P         034020001503S')                      //S62532
     D                               28    INZ('RPOM8P         034170001503S')                      //S62532
     D                               28    INZ('UIOM8P         034320001504S')                      //S62532
     D                               28    INZ('VAGC0C         034470001302S')                      //S62532
     D                               28    INZ('VAGD0C         034600001302S')                      //S62532
     D                               28    INZ('VAFV0C         034730001302S')                      //S62532
     D                               28    INZ('VAFW0C         034860001302S')                      //S62532
     D                               28    INZ('VAFX0C         034990001003S')                      //S62532
     D                               28    INZ('CEDJ0C         035090000300A')                      //S62532
     D                               28    INZ('NCD60C         035120000700S')                      //S62532
     D                               28    INZ('NCEG0C         035190000700S')                      //S62532
     D                               28    INZ('NCEF0C         035260000700S')                      //S62532
     D                               28    INZ('NCD80C         035330000700S')                      //S62532
     D                               28    INZ('DLQS0C         035400001003S')                      //S62532
     D                               28    INZ('DVQS0C         035500001003S')                      //S62532
     D                               28    INZ('DCQS0C         035600001003S')                      //S62532
     D                               28    INZ('AIPM0C         035700001504S')                      //S62532
     D                               28    INZ('AISM0C         035850001504S')                      //S62532
     D                               28    INZ('AIVM0C         036000001504S')                      //S62532
     D                               28    INZ('QMPM0C         036150001504S')                      //S62532
     D                               28    INZ('QMSM0C         036300001504S')                      //S62532
     D                               28    INZ('QMVM0C         036450001504S')                      //S62532
     D                               28    INZ('RNPM0C         036600001503S')                      //S62532
     D                               28    INZ('RNSM0C         036750001503S')                      //S62532
     D                               28    INZ('RNVM0C         036900001503S')                      //S62532
     D                               28    INZ('RPPM0C         037050001503S')                      //S62532
     D                               28    INZ('RPSM0C         037200001503S')                      //S62532
     D                               28    INZ('RPVM0C         037350001503S')                      //S62532
     D                               28    INZ('UIPM0C         037500001504S')                      //S62532
     D                               28    INZ('UISM0C         037650001504S')                      //S62532
     D                               28    INZ('UIVM0C         037800001504S')                      //S62532
     D                               28    INZ('EQPM0C         037950001504S')                      //S62532
     D                               28    INZ('EQSM0C         038100001504S')                      //S62532
     D                               28    INZ('EQVM0C         038250001504S')                      //S62532
     D                               28    INZ('EXPM0C         038400001504S')                      //S62532
     D                               28    INZ('EXSM0C         038550001504S')                      //S62532
     D                               28    INZ('EXVM0C         038700001504S')                      //S62532
     D                               28    INZ('EQNP0C         038850001003S')                      //S62532
     D                               28    INZ('EQNS0C         038950001003S')                      //S62532
     D                               28    INZ('EQNV0C         039050001003S')                      //S62532
     D                               28    INZ('EQPP0C         039150001003S')                      //S62532
     D                               28    INZ('EQPS0C         039250001003S')                      //S62532
     D                               28    INZ('EQPV0C         039350001003S')                      //S62532
     D                               28    INZ('VAFQ0C         039450001003S')                      //S62532
     D                               28    INZ('SSDO0C         039550000100A')                      //S62532
     D                               28    INZ('NCB60C         039560000700S')                      //S62532
     D                               28    INZ('NCEJ0C         039630000300S')                      //S62532
     D                               28    INZ('ARNB0C         039660000500S')                      //S62532
     D                               28    INZ('NCEA0C         039710000500S')                      //S62532
     D                               28    INZ('NCEB0C         039760000500S')                      //S62532
     D                               28    INZ('PROB0C         039810000302S')                      //S62532
     D                               28    INZ('NCMF0C         039840000700A')                      //S62532
     D                               28    INZ('VAFY0C         039910001003S')                      //S62532
     D                               28    INZ('VACU0C         040010001003S')                      //S62532
     D                               28    INZ('VAF60C         040110001003S')                      //S62532
     D                               28    INZ('VAFP0C         040210001003S')                      //S62532
     D                               28    INZ('VAFR0C         040310001003S')                      //S62532
     D                               28    INZ('VAF20C         040410001003S')                      //S62532
     D                               28    INZ('VAF30C         040510001003S')                      //S62532
     D                               28    INZ('VAH70C         040610001504S')                      //S62532
     D                               28    INZ('VAC10C         040760001003S')                      //S62532
     D                               28    INZ('TXBM0C         040860002500A')                      //S62532
     D                               28    INZ('SSDE0C         041110000100A')                      //S62532
     D                               28    INZ('VAA90C         041120001504S')                      //S62532
     D                               28    INZ('CEB40C         041270000700A')                      //S62532
     D                               28    INZ('AIOM8R         041340001504S')                      //S62532
     D                               28    INZ('QMOM8R         041490001504S')                      //S62532
     D                               28    INZ('RNOM8R         041640001503S')                      //S62532
     D                               28    INZ('RPOM8R         041790001503S')                      //S62532
     D                               28    INZ('UIOM8R         041940001504S')                      //S62532
     D                               28    INZ('DLQS0E         042090001003S')                      //S62532
     D                               28    INZ('DVQS0E         042190001003S')                      //S62532
     D                               28    INZ('DCQS0E         042290001003S')                      //S62532
     D                               28    INZ('AIPM0E         042390001504S')                      //S62532
     D                               28    INZ('AISM0E         042540001504S')                      //S62532
     D                               28    INZ('AIVM0E         042690001504S')                      //S62532
     D                               28    INZ('QMPM0E         042840001504S')                      //S62532
     D                               28    INZ('QMSM0E         042990001504S')                      //S62532
     D                               28    INZ('QMVM0E         043140001504S')                      //S62532
     D                               28    INZ('RNPM0E         043290001503S')                      //S62532
     D                               28    INZ('RNSM0E         043440001503S')                      //S62532
     D                               28    INZ('RNVM0E         043590001503S')                      //S62532
     D                               28    INZ('RPPM0E         043740001503S')                      //S62532
     D                               28    INZ('RPSM0E         043890001503S')                      //S62532
     D                               28    INZ('RPVM0E         044040001503S')                      //S62532
     D                               28    INZ('UIPM0E         044190001504S')                      //S62532
     D                               28    INZ('UISM0E         044340001504S')                      //S62532
     D                               28    INZ('UIVM0E         044490001504S')                      //S62532
     D                               28    INZ('EQPM0E         044640001504S')                      //S62532
     D                               28    INZ('EQSM0E         044790001504S')                      //S62532
     D                               28    INZ('EQVM0E         044940001504S')                      //S62532
     D                               28    INZ('EXPM0E         045090001504S')                      //S62532
     D                               28    INZ('EXSM0E         045240001504S')                      //S62532
     D                               28    INZ('EXVM0E         045390001504S')                      //S62532
     D                               28    INZ('EQNP0E         045540001003S')                      //S62532
     D                               28    INZ('EQNS0E         045640001003S')                      //S62532
     D                               28    INZ('EQNV0E         045740001003S')                      //S62532
     D                               28    INZ('EQPP0E         045840001003S')                      //S62532
     D                               28    INZ('EQPS0E         045940001003S')                      //S62532
     D                               28    INZ('EQPV0E         046040001003S')                      //S62532
     D                               28    INZ('ARNB0E         046140000500S')                      //S62532
     D                               28    INZ('NCMF0E         046190000700A')                      //S62532
     D                               28    INZ('RLSE0E         046260000400S')                      //S62532
     D                               28    INZ('NCMFXL         046300000700A')                      //S62532
     D                               28    INZ('PODMXL         046370001000A')                      //S62532
     D                               28    INZ('ARNBXL         046470000500S')                      //S62532
     D                               28    INZ('RLSEXL         046520000400S')                      //S62532
     D                               28    INZ('ABVHXL         046560000700S')                      //S62532
     D                               28    INZ('TIMEXL         046630000600S')                      //S62532
     D                               28    INZ('INTKXL         046690001500A')                      //S62532
     D                               28    INZ('TYPEXL         046840000200A')                      //S62532
     D                               28    INZ('TKENXL         046860001500A')                      //S62532
     D*
     D* Mapics file buffer(s)
     D*
     D mRecIMHISTL5  E DS                  EXTNAME(IMHISTL5  )
     D mRecIMHISC    E DS                  EXTNAME(IMHISC    )                                      //D10055
     D mRecIMHISXL2  E DS                  EXTNAME(IMHISXL2  )
     D mRecIMHPRTL1  E DS                  EXTNAME(IMHPRTL1  )
     D mRecIMHAXFL1  E DS                  EXTNAME(IMHAXFL1  )
     D mRecIMRVSLL2  E DS                  EXTNAME(IMRVSLL2  )
     D*
     D* Business object token.
     D*
     D M_W#BOTK        C                   CONST('Z#1026MHM00793619729-
     D                                     6074817AAAAH')
     D*
     D* Object class.
     D*
     D M_OBJCLS        C                   CONST('INVTXN    ')
     D*
     D* Transactions.
     D*
     D*   Change
     D #TxINVTXNCHG    C                   CONST('INVTXNCHG ')
     D*   Create
     D #TxINVTXNCRT    C                   CONST('INVTXNCRT ')
     D* Locking error message ID
     D #LMSID          C                   CONST('PSX0025')
     D* Locking error message file name
     D #LMSFN          C                   CONST('PSIMSGF   ')
     I*---------------------------------------------------------------- *
     I* @@@@ Input specifications.                                      *
     I*---------------------------------------------------------------- *
     I* START USER PROTECTED SECTION  - SECA0R 0004.000 / *USR
     I* FINISH USER PROTECTED SECTION - SECA0R 0004.000
     I*---------------------------------------------------------------- *
     C*******************************************************************
     C*:STRPARM                                                         *
     C*                                                                 *
     C* USE FIELD    DESCRIPTION                                        *
     C* +++ ++++++   ++++++++++++++++++++++++++++++++++++++++++++++++++ *
     C*   P#PFPR      Perform process?                     I       8    *
     C*   P#SHDN      Shutdown program?                    I       8    *
     C*   P#TSTK      Task token                           I      10    *
     C*   P#MSID      Message ID                           O       7    *
     C*                                                                 *
     C*:ENDPARM                                                         *
     C*******************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    P#PFPR            8
     C                   PARM                    P#SHDN            8
     C                   PARM                    P#TSTK           10
     C                   PARM                    P#MSID            7
     C/EJECT
     C*******************************************************************
     C* Mainline.                                                       *
     C*******************************************************************
     C*
     C* Perform program open considerations.
     C*
     C     W#PGOP        IFNE      *ON
     C                   EXSR      PGMOPN
     C                   ENDIF
     C*
     C* Perform program initialization.
     C*
     C                   EXSR      PGMINZ
     C*
     C* If requested, perform the process.
     C*
     C     P#PFPR        IFEQ      '*YES    '
     C                   EXSR      PGMPRC
     C                   ENDIF
     C*
     C* If requested, perform program close considerations.
     C*
     C     P#SHDN        IFEQ      '*YES    '
     C                   EXSR      PGMCLS
     C                   ENDIF
     C*
     C* Return to caller.
     C*
     C                   RETURN
     C/EJECT
     C*******************************************************************
     C* AAADEF - Program declarations.                                  *
     C*******************************************************************
     C     AAADEF        BEGSR
     C*
     C* Copyright statement.
     C*
     C                   MOVE      COPSI         COPSI
     C*
     C*    Work context.
     C*
     C                   Z-ADD     *ZEROS        #X                5 0
     C                   Z-ADD     *ZEROS        #Y                5 0
     C                   MOVEL     *BLANKS       W#SBTG            8
     C                   MOVEL     *BLANKS       W#MDRD            8
     C                   MOVEL     *BLANKS       W#WA10           10
     C                   MOVEL     *OFF          W#RCLK            1
     C                   MOVEL     *OFF          M_P$EEUF          1
     C                   MOVEL     *OFF          W#ERED            1
     C*---------------------------------------------------------------- *
     C* @@@@ Additional variables.                                      *
     C*---------------------------------------------------------------- *
     C* START USER PROTECTED SECTION  - SECA0R 0005.000 / *USR
                                                                                                      S47026
      * Key lists                                                                                     S47026
      *---------------                                                                                S47026
                                                                                                    //S51642
     c     itbxkey       klist                                                                      //S51642
     c                   kfld                    mwhidav                                            //S51642
     c                   kfld                    mitnoav                                            //S51642
                                                                                                    //S51642
                                                                                                      S47026
     c     poikey        klist                                                                        S47026
     c                   kfld                    mordrav                                              S47026
     c                   kfld                    mpisqav                                              S47026
                                                                                                      S47026
                                                                                                      S47026
                                                                                                      S47026
                                                                                                      S47026
     C* FINISH USER PROTECTED SECTION - SECA0R 0005.000
     C*---------------------------------------------------------------- *
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* CLOFIL - Close open files.                                      *
     C*******************************************************************
     C     CLOFIL        BEGSR
     C*
     C                   MOVEL     'CLOFIL01'    W#SBTG
     C*---------------------------------------------------------------- *
     C* @@@@ Close files.                                               *
     C*---------------------------------------------------------------- *
     C* START USER PROTECTED SECTION  - SECA0R 0006.000 / *USR
                                                                                                    //S48144
     c                   close     *all                                                             //S48144
                                                                                                    //S48144
                                                                                                    //S48144
     C* FINISH USER PROTECTED SECTION - SECA0R 0006.000
     C*---------------------------------------------------------------- *
     C*
     C     ZCLFIL        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* CLOPGM - Close open programs.                                   *
     C*******************************************************************
     C     CLOPGM        BEGSR
     C*
     C                   MOVEL     'CLOPGM01'    W#SBTG
     C                   EXSR      SVMHG9                                                           //S65516
     C                   EXSR      SVRMH0
     C                   EXSR      CLSP0R
     C                   EXSR      CLSM0R
     C                   EXSR      CLSN0R
     C                   EXSR      CLSO0R
     C                   EXSR      CLSQ0R
     C*
     C* If active user exit calls, shutdown
     C*
     C                   EXSR      M_CL$$M0
     C                   EXSR      M_CL$$N0
     C                   EXSR      M_CL$$O0
     C                   EXSR      M_CL$$Q0
     C*
     C* Close perform function point call API.
     C*
     C                   EXSR      M_SVPFC0
     C*
     C* Close user exit checking API
     C*
     C                   EXSR      M_SVRFP0
     C                   EXSR      M_SVSFD0
     C*
     C*---------------------------------------------------------------- *
     C* @@@@ Shutdown programs.                                         *
     C*---------------------------------------------------------------- *
     C* START USER PROTECTED SECTION  - SECA0R 0007.000 / *USR
     C* FINISH USER PROTECTED SECTION - SECA0R 0007.000
     C*---------------------------------------------------------------- *
     C*
     C     ZCLPGM        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* PGMABT - Program abort logic.                                   *
     C*******************************************************************
     C     PGMABT        BEGSR
     C*
     C* Make sure the lock has been released.
     C*
     C                   IF        W#SBTG<>'EXCLCK01'
     C* Release Object lock                                                                         //D10055
     C                   MOVEL     '*REL    '    W#RQFN
     C                   EXSR      EXCLCK
     C* Release Physical lock                                                                       //D10055
     C                   MOVEL     '*YES    '    W#PFPR                                             //D10055
     C                   MOVEL     '*NO     '    W#SHDN                                             //D10055
     C                   MOVEL     '*RLSLOCK'    W#UPFC                                             //D10055
     C                   EXSR      EXCO0R                                                           //D10055
     C                   ENDIF                                                                      //D10055
     C*
     C* If message needs to be sent, send it.
     C*
     C     W#MSID        IFGT      *BLANKS
     C                   MOVEL     W#MSID        W#MSID
     C                   MOVEL     W#MSDT        W#MSDT
     C                   EXSR      PGMERR
     C                   END
     C*
     C* Relay messages to caller.
     C*
     C                   EXSR      RLYMSG
     C*
     C* Load the output parameters and end the program.
     C*
     C                   MOVEL     W#MSID        P#MSID
     C                   EXSR      PGMCLS
     C*
     C     ZPGABT        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* PGMCLS - Program close considerations.                          *
     C*******************************************************************
     C     PGMCLS        BEGSR
     C*
     C* Close open files.
     C*
     C                   EXSR      CLOFIL
     C*
     C* Close open programs.
     C*
     C                   EXSR      CLOPGM
     C*
     C* End the program.
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C     ZPGCLS        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* PGMERR - Send error message to current programs message queue.  *
     C*******************************************************************
     C     PGMERR        BEGSR
     C*
     C* Input:
     C* ------
     C* W#MSID - Message ID to be sent.
     C* W#MSDT - Message data.
     C*
     C*-----------------------------------------------------------------*
     C*
     C                   MOVEL     W#MSID        W#MSID            7
     C                   MOVEL     W#MSDT        W#MSDT          256
     C*
     C                   CALL      'PSXSPM1C'                                   Send message
     C                   PARM      W#MSID        SMMSID            7            Message ID
     C                   PARM      *BLANKS       SMMFNM           10            Message file
     C                   PARM      W#MSDT        SMMSDT          256            Message data
     C                   PARM      *BLANKS       SMPQRL            5            PGMQ relation
     C                   PARM      *BLANKS       SMPQNM           10            PGMQ name
     C*
     C     ZPGERR        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* RLYMSG - Relay program messages.                                *
     C*******************************************************************
     C     RLYMSG        BEGSR
     C*
     C* Dependencies:
     C* -------------
     C* Requires program name out of the program status data structure.
     C*   PGMSTS    ESDSPGMSTS
     C*
     C* Requires data structures to define length of 1st and 2nd level
     C* message text.
     C*   /COPY $PSICOPY,DS_RTM
     C*
     C*-----------------------------------------------------------------*
     C*
     C*  Assemble command string - RLYRPGMSG PGMQ()
     C*
     C                   CALL      'PSXRTM1C'                           90      Retrieve msg
     C                   PARM      'CMD0002'     RTMSID            7            Message ID
     C                   PARM      'PSIMSGF'     RTMFNM           10            Message file
     C                   PARM      *BLANKS       RTMFLB           10            Msg file lib
     C                   PARM      $PPGNM        RTMSDT          256            Message data
     C                   PARM      *BLANKS       RTMSG                          DS 1st level
     C                   PARM      *BLANKS       RTSCLV                         DS 2nd level
     C*
     C*  Execute command
     C*
     C                   CALL      'QCMDEXC'                            90
     C                   PARM                    RTSCLV
     C                   PARM      512           QCLENG           15 5
     C*
     C     ZRLYMS        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* XXEMP1 - Execute program - PSXEMP1R                             *
     C*******************************************************************
     C     XXEMP1        BEGSR
     C*
     C                   MOVEL     W#PFPR        W#PFPR            8
     C                   MOVEL     W#SHDN        W#SHDN            8
     C                   MOVEL     W#TSTK        W#TSTK           10
     C                   MOVEL     W1MSID        W1MSID            7
     C                   MOVEL     W#MSDT        W#MSDT          256
     C                   MOVEL     W#MSFN        W#MSFN           10
     C                   MOVEL     W#MSCL        W#MSCL            8
     C                   MOVEL     W#MSFL        W#MSFL           65
     C*
     C                   CALL      'PSXEMP1R'                           90
     C                   PARM      W#PFPR        P$PFPR            8
     C                   PARM      W#SHDN        P$SHDN            8
     C                   PARM      W#TSTK        P$TSTK           10
     C                   PARM      W1MSID        P1MSID            7
     C                   PARM      W#MSDT        P$MSDT          256
     C                   PARM      W#MSFN        P$MSFN           10
     C                   PARM      W#MSCL        P$MSCL            8
     C                   PARM      W#MSFL        P$MSFL           65
     C                   PARM      *BLANKS       P$MSID            7
     C*
     C     ZXEMP1        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* PGMINZ - Program initialization.                                *
     C*******************************************************************
     C     PGMINZ        BEGSR
     C*
     C                   MOVEL     'PGMINZ01'    W#SBTG
     C*
     C* Initialize variables.
     C*
     C                   MOVEL     *BLANKS       P#MSID
     C*---------------------------------------------------------------- *
     C* @@@@ Additional intialization considerations.                   *
     C*---------------------------------------------------------------- *
     C* START USER PROTECTED SECTION  - SECA0R 0008.000 / *USR
     C* FINISH USER PROTECTED SECTION - SECA0R 0008.000
     C*---------------------------------------------------------------- *
     C*
     C     ZPGINZ        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* PGMOPN - Program open considerations.                           *
     C*******************************************************************
     C     PGMOPN        BEGSR
     C*
     C                   MOVEL     'PGMOPN01'    W#SBTG
     C*
     C                   MOVEL     *ON           W#PGOP            1
     C                   MOVEL     *OFF          *IN91
     C*
     C* See if we have any user extension function point calls.
     C*
     C                   EXSR      M_SF$$M0
     C                   EXSR      M_SF$$N0
     C                   EXSR      M_SF$$O0
     C                   EXSR      M_SF$$Q0
     C*
     C* Get object name from NLSTXT.
     C*
     C                   EVAL      P$FNC$='AXTAVN0R'                                                //S62268
     C                   CALL      'AXZFNT0R'                           90
     C                   PARM                    P$FNC$           10                                //S62268
     C                   PARM      1             P$SEQ$            3 0
     C                   PARM      *ALL'?'       P$TEX$           80
     C                   MOVEL     P$TEX$        W$OBJN           50
     C*
     C*---------------------------------------------------------------- *
     C* @@@@ Additional open considerations.                            *
     C*---------------------------------------------------------------- *
     C* START USER PROTECTED SECTION  - SECA0R 0009.000 / *USR
                                                                                                      S47026
     c                   open      pomasxl0                             90                          //S48144
     c                   open      poitmxl0                             90                          //S47026
     c                   if        *in90 = *off                                                     //S47026
     c                   open      trnstsl0                             90                          //S47026
     c                   open      trnstsl1                             90                          //S47026
     c                   movel     '0'           nopo1w            1                                //S47026
     c                   else                                                                       //S47026
     c                   movel     '1'           nopo1w            1                                //S47026
     c                   endif                                                                      //S47026
     c                   open      invstsl0                             90                          //S56670
     c                   open      itembxl0                             90                          //S48144
                                                                                                      S47026
     C* FINISH USER PROTECTED SECTION - SECA0R 0009.000
     C*---------------------------------------------------------------- *
     C*
     C     ZPGOPN        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* PGMPRC - Program processing logic.                              *
     C*******************************************************************
     C     PGMPRC        BEGSR
     C*
     C                   MOVEL     'PGMPRC01'    W#SBTG
     C*
     C* Clear lock flag.
     C*
     C                   MOVEL     *OFF          W#RCLK
     C*
     C* Call process to convert string transaction data to
     C*  template transaction record format.
     C*
     C                   MOVEL     '*YES    '    W#PFPR
     C                   MOVEL     '*NO     '    W#SHDN
     C                   MOVEL     P#TSTK        W#TSTK
     C                   EXSR      EXCK0R
     C*
     C* Avoid any further processing, if the transaction ID is blank
     C*
     C     mTRID         CABEQ     *BLANKS       ZPGPRC
     C*
     C* Avoid transaction when needed                                                               //S65516
     C*                                                                                             //S65516
     C                   EXSR      AVDTRN                                                           //S65516
     C*                                                                                             //S65516
     C* If update/delete or special trans requires lock,  attempt
     C*  to lock the record.  After lock attempt, interpret
     C*  any messages issued by locking program.
     C*  by locking program.
     C*
     C     mORPR         IFEQ      *BLANKS                                                          //D10055
     C     mTRID         IFEQ      #TxINVTXNCHG
     C                   EXSR      LCKSTD
     C                   ENDIF                                                                      //D10055
     C                   ENDIF                                                                      //D10055
     C*
     C* Load current data base values to transaction template and
     C*  convert special null values (9- and ***) to blanks and
     C*  zeros.
     C*
     C                   MOVEL     '*YES    '    W#PFPR
     C                   MOVEL     '*NO     '    W#SHDN
     C                   EXSR      EXCM0R
     C*
     C* Load current user data base values to transaction template.
     C*
     C     M_XA0002      IFEQ      *ON
     C                   EXSR      M_XU$$M0
     C                   END
     C*
     C* If add/update is requested or special trans needs default
     C*  logic.  Call default calculation program.
     C*
     C     mTRID         IFEQ      #TxINVTXNCHG
     C     mTRID         OREQ      #TxINVTXNCRT
     C                   MOVEL     '*YES    '    W#PFPR
     C                   MOVEL     '*NO     '    W#SHDN
     C                   MOVEL     '*COMMON '    W#SCDF
     C                   MOVEL     '*TRANS  '    W#CLPR
     C                   EXSR      EXCQ0R
     C                   END
     C*
     C* Load user defaults to template.
     C*
     C     M_XA0008      IFEQ      *ON
     C                   MOVEL     '*ALL    '    W#SCDF
     C                   MOVEL     '*TRANS  '    W#CLPR
     C                   EXSR      M_XU$$Q0
     C                   END
     C*
     C* Call edit user exit to establish and edit overrides.
     C*
     C     M_XA0003      IFEQ      *ON
     C                   MOVEL     '*ESTOVR '    W#SCED
     C                   EXSR      M_XU$$N0
     C                   END
     C*
     C* Edit template transaction.
     C*
     C                   EXSR      EDTTMP
     C*
     C* Edit user template transaction.
     C*
     C     M_XA0003      IFEQ      *ON
     C                   MOVEL     '*FULL   '    W#SCED
     C                   EXSR      M_XU$$N0
     C                   END
     C*
     C* If edit errors were not encountered, call process to
     C*  to perform the transaction update.
     C*
     C     W#ERED        IFEQ      *OFF
     C                   EXSR      UPDTMP
     C                   ELSE                                                                       //S65516
     C                   EXSR      APPERR                                                           //S65516
     C                   END
     C*
     C* If edit errors were encountered, or the generated key has been
     C*  changed by the user, call cancel program.
     C*
     C     W#ERED        IFEQ      *ON
     C                   MOVEL     '*YES    '    W#PFPR
     C                   MOVEL     '*NO     '    W#SHDN
     C                   MOVEL     mTSTK         W#TSTK
     C                   MOVEL     '*TRNINT '    W#SCCN
     C                   EXSR      EXCF0R
     C                   END
     C*
     C* Execute SOA replication program, if needed.
     C*
     C     W#ERED        IFEQ      *OFF
     C     mTRID         IFEQ      #TxINVTXNCHG
     C     mTRID         OREQ      #TxINVTXNCRT
     C                   EXSR      EXCP2R
     C                   ENDIF
     C                   ENDIF
     C*
     C* If errors found, release Physical lock                                                      //D10055
     C*                                                                                             //D10055
     C                   IF        W#ERED = *ON                                                     //D10055
     C     mTRID         IFEQ      #TxINVTXNCHG                                                     //D10055
     C                   MOVEL     '*YES    '    W#PFPR                                             //D10055
     C                   MOVEL     '*NO     '    W#SHDN                                             //D10055
     C                   MOVEL     '*RLSLOCK'    W#UPFC                                             //D10055
     C                   EXSR      EXCO0R                                                           //D10055
     C                   ENDIF                                                                      //D10055
     C                   ENDIF                                                                      //D10055
     C*                                                                                             //D10055
     C* Always make sure the lock has been released.
     C*
     C     mORPR         IFEQ      *BLANKS                                                          //D10055
     C                   MOVEL     '*REL    '    W#RQFN
     C                   EXSR      EXCLCK
     C                   EndIf                                                                      //D10055
     C*
     C* If errors were encountered issue appropriate completion
     C*  message.
     C*
     C                   EXSR      SNDCMS
     C*
     C     ZPGPRC        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* EXCP2R - Execute SOA replication program                        *
     C*******************************************************************
     C     EXCP2R        BEGSR
     C*
     C                   MOVEL     'EXCP2R01'    W#SBTG
     C*                                                                                               S47026
     C                   MOVEL     *BLANKS       W#RQID                                               S47026
     C*---------------------------------------------------------------- *                             S46409
     C* @@@@ Custom replication logic                                   *                             S46409
     C*---------------------------------------------------------------- *                             S46409
     C* START USER PROTECTED SECTION  - SECA0R 0012.200 / *USR                                        S46409
                                                                                                      S47026
      /free
                                                                                                      S47026
        // get msoad                                                                                //S48144
                                                                                                    //S48144
           if msoad = *blanks;                                                                      //S48144
                                                                                                      S47026
             if nopo1W = '0';   // if po files exist                                                //S48144
                                                                                                      S47026
        // getting parent transaction                                                               //S47026
                                                                                                    //S48144
        //     if mtcdeav = 'RP'                                                                    //S55862
        //      And  mordrav <> *blanks;                                                            //S55862
                                                                                                    //S55862
                                                                                                    //S55862
               if mordrav <> *blanks;                                                               //S55862
                                                                                                    //S48144
                 chain mtstk trnstsl1;                                                              //S48144
                  if %found(trnstsl1);                                                              //S48144
                   chain (tstsgp:tstsgp) trnstsl0;                                                  //S48144
                   if %found(trnstsl0);                                                             //S48144
                                                                                                      S47026
                   // if the orginating transaction if external                                     //S48144
                   // item create / then do not publish                                             //S48144
                                                                                                      S47026
                     if tstrid <> 'EXTRCLCRT';                                                      //S48144
                                                                                                    //S47026
                       if mpisqav <> *zeros;                                                        //S48144
                         chain poikey poitmxl0;                                                     //S48144
                                                                                                    //S48144
                           if %found(poitmxl0);                                                     //S48144
                             msoad = repdrk;                                                        //S48144
                           endif;                                                                   //S48144
                                                                                                    //S48144
                       endif;                                                                       //S48144
                                                                                                    //S47026
                     endif;                                                                         //S48144
                                                                                                    //S47026
                   endif;                                                                           //S48144
                  endif;                                                                            //S48144
               endif;                                                                               //S48144
             endif;                                                                                 //S48144
           endif;                                                                                   //S48144
                                                                                                    //S48144
         if mordrav <> *blanks                                                                      //S48144
           And  msoad = *blanks;                                                                    //S48144
             chain (mordrav) pomasxl0;                                                              //S48144
             if %found;                                                                             //S48144
               msoad = repdrj;                                                                      //S48144
             endif;                                                                                 //S48144
         endif;                                                                                     //S48144
                                                                                                    //S48144
         if msoad = *blanks;                                                                        //S48144
           //  chain (whidm5:itnom5) itembxl0;                                                      //S51642
           chain itbxkey itembxl0;                                                                  //S51642
           if %found;                                                                               //S48144
             msoad=repdm5;                                                                          //S48144
           endif;                                                                                   //S48144
         endif;                                                                                     //S48144
                                                                                                    //S47026
         if   msoad=*blanks;                                                                        //S48144
           leavesr;                                                                                 //S47026
         endif;                                                                                     //S47026
                                                                                                    //S47026
          //  Check to make sure item Inventory Status not set to external                          //S56670
                                                                                                    //S56670
           chain itbxkey itembxl0;                                                                  //S56670
           if %found;                                                                               //S56670
              chain istsm5 invstsl0;                                                                //S56670
                if %found;                                                                          //S56670
                   if excnrf = '1';                                                                 //S56670
                     leavesr;                                                                       //S56670
                   endif;                                                                           //S56670
                endif;                                                                              //S56670
           endif;                                                                                   //S56670
                                                                                                    //S56670
                                                                                                    //S56670
         if mmctl = *blanks                                                                         //S47026
           Or mmctl = '0';                                                                          //S47026
              mmctl = '1';                                                                          //S47026
         endif;                                                                                     //S47026
                                                                                                    //S47026
      /end-free
                                                                                                      S47026
     c                   eval      w#rqid = *blanks                                                   S47026
                                                                                                      S47026
     c                   select                                                                       S47026
                                                                                                      S47026
      *SyncInventoryAdjustment                                                                      //S48144
     c                   when      mtcdeav = 'IA'                                                     S47026
     c                               Or  mtcdeav = 'RQ'                                             //S48144
     c                               Or  mtcdeav = 'SS'                                             //S48144
                                                                                                    //S48144
     c                   eval      w#rqid =                                                           S47026
     c                                'XA_Replicate_InventoryAdjustment'                              S47026
                                                                                                      S47026
      *SyncInventoryConsumption                                                                     //S48144
     c                   when      mtcdeav = 'IP'                                                   //S48144
     c                               Or  mtcdeav = 'IS'                                             //S48144
     c                               Or  mtcdeav = 'IU'                                               S47026
     c                               Or  mtcdeav = 'IW'                                             //S56670
                                                                                                    //S48144
     c                   eval      w#rqid =                                                           S47026
     c                                'XA_Replicate_InventoryConsumption'                           //S48144
                                                                                                    //S48144
      *SyncInventoryCount                                                                           //S48144
     c                   when      mtcdeav = 'PH'                                                   //S48144
                                                                                                    //S48144
     c                   eval      w#rqid =                                                         //S48144
     c                                'XA_Replicate_InventoryCount'                                 //S60956
                                                                                                      S47026
                                                                                                    //S48144
      *SyncReceiveDelivery                                                                          //S48144
     c                   when      mtcdeav = 'RC'                                                     S47026
     c                               Or  mtcdeav = 'RD'                                             //S48144
     c                               Or  mtcdeav = 'RM'                                               S47026
     c                               Or  mtcdeav = 'RP'                                               S47026
     c                               Or  mtcdeav = 'RS'                                               S47026
     c                               Or  mtcdeav = 'RW'                                             //S56670
     c                               Or  mtcdeav = 'PQ'                                             //S48144
     c                               Or  mtcdeav = 'MQ'                                             //S48144
     c                   eval      w#rqid = 'XA_Replicate_InventoryReceipt'                           S47026
                                                                                                    //S48144
      *SyncInspectDelivery                                                                          //S48144
     c                   when      mtcdeav = 'RI'                                                   //S48144
     c                   eval      w#rqid = 'XA_Replicate_InspectDelivery'                          //S48144
                                                                                                    //S48144
      *SyncSupplierRMA                                                                              //S48144
     c                   when      mtcdeav = 'VR'                                                   //S48144
     c                   eval      w#rqid = 'XA_Replicate_SupplierRMA'                              //S48144
                                                                                                    //S56670
                                                                                                    //S56670
      * If not specified - do not publish                                                           //S56670
                                                                                                    //S56670
     c                   other                                                                      //S56670
     c                   goto      zexcp2r                                                          //S56670
                                                                                                    //S56670
                                                                                                      S47026
     c                   endsl                                                                        S47026
                                                                                                      S47026
                                                                                                      S47026
     C* FINISH USER PROTECTED SECTION - SECA0R 0012.200                                               S46409
     C*---------------------------------------------------------------- *                             S46409
     C*
     C                   MOVEL     '*YES    '    W#PFPR
     C                   MOVEL     '*NO     '    W#SHDN
     C                   MOVEL     mTRID         W#TRID           10
     C                   MOVEL     P#TSTK        W#TSTK           10
     C                   MOVEL     W#RQID        W#RQID          256                                  S47026
     C                   MOVEL     mSOAD         W#SOAD          128
     C*
     C                   MOVEL     *OFF          W#RTVD            1
     C*
     C*  Call Publish pgm only when maintenance control is Local
     C*
     C                   IF        mMCTL='1' or mMCTL='3'
     C                   MOVEL     *ON           wCallP2R          1
     C                   ELSE
     C                   MOVEL     *OFF          wCallP2R
     C                   END
     C     *LIKE         DEFINE    mUPDTAV       p$KEY01
     C                   EVAL      p$Key01 =mUPDTAV
     C     *LIKE         DEFINE    mUPTMAV       p$KEY02
     C                   EVAL      p$Key02 =mUPTMAV
     C     *LIKE         DEFINE    mORDRAV       p$KEY03
     C                   EVAL      p$Key03 =mORDRAV
     C     *LIKE         DEFINE    mITNOAV       p$KEY04
     C                   EVAL      p$Key04 =mITNOAV
     C     *LIKE         DEFINE    mWHIDAV       p$KEY05
     C                   EVAL      p$Key05 =mWHIDAV
     C     *LIKE         DEFINE    mTKENAV       p$KEY06
     C                   EVAL      p$Key06 =mTKENAV
     C                   IF        wCallP2R = *on
     C                   CALL      'AXTAVP2R'                           90
     C                   PARM      W#PFPR        P$PFPR            8
     C                   PARM      W#SHDN        P$SHDN            8
     C                   PARM      W#TSTK        P$TSTK           10
     C                   PARM      W#TRID        P$TRID           10
     C                   PARM      W#RTVD        P$RTVD            1
     C                   PARM      W#RQID        P$RQID          256                                  S46409
     C                   PARM      W#SOAD        P$SOAD          128
     C                   PARM                    p$KEY01
     C                   PARM                    p$KEY02
     C                   PARM                    p$KEY03
     C                   PARM                    p$KEY04
     C                   PARM                    p$KEY05
     C                   PARM                    p$KEY06
     C                   PARM      *BLANKS       P$MSID            7
     C*
     C* If there is an error on the call to this program,
     C*  report it.
     C*
     C     *IN90         IFEQ      *ON
     C     W#PFPR        ANDEQ     '*YES    '
     C     P$MSID        ORGT      *BLANKS
     C     W#PFPR        ANDEQ     '*YES    '
     C                   MOVEL     'PSX0001'     W#MSID
     C                   MOVE      *BLANKS       W#MSDT
     C                   MOVEL     'AXTAVP2R'    W#WA10           10
     C     W#SBTG        CAT       W#MSDT        W#MSDT
     C     $PPGNM        CAT       W#MSDT        W#MSDT
     C     W#WA10        CAT       W#MSDT        W#MSDT
     C                   EXSR      PGMABT
     C                   ENDIF
     C                   ENDIF
     C*
     C     ZEXCP2R       ENDSR                                                                        S46409
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* LCKSTD - Standard locking process.                              *
     C*******************************************************************
     C     LCKSTD        BEGSR
     C*
     C                   MOVEL     'LCKSTD01'    W#SBTG
     C*
     C                   MOVEL     '*LOCK   '    W#RQFN
     C                   EXSR      EXCLCK
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* EXCLCK - Execute lock program                                   *
     C*******************************************************************
     C     EXCLCK        BEGSR
     C*
     C                   MOVEL     'EXCLCK01'    W#SBTG
     C*
     C                   MOVEL     '*YES    '    W#PFPR
     C                   MOVEL     '*NO     '    W#SHDN
     C                   MOVEL     W#RQFN        W#RQFN            8
     C*
     C     *LIKE         DEFINE    mUPDTAV       p$KEY01
     C                   EVAL      p$Key01 =mUPDTAV
     C     *LIKE         DEFINE    mUPTMAV       p$KEY02
     C                   EVAL      p$Key02 =mUPTMAV
     C     *LIKE         DEFINE    mORDRAV       p$KEY03
     C                   EVAL      p$Key03 =mORDRAV
     C     *LIKE         DEFINE    mITNOAV       p$KEY04
     C                   EVAL      p$Key04 =mITNOAV
     C     *LIKE         DEFINE    mWHIDAV       p$KEY05
     C                   EVAL      p$Key05 =mWHIDAV
     C     *LIKE         DEFINE    mTKENAV       p$KEY06
     C                   EVAL      p$Key06 =mTKENAV
     C                   CALL      'AXTAVL2R'                           90
     C                   PARM      W#PFPR        P$PFPR            8
     C                   PARM      W#SHDN        P$SHDN            8
     C                   PARM      W#RQFN        P$RQFN            8
     C                   PARM                    p$KEY01
     C                   PARM                    p$KEY02
     C                   PARM                    p$KEY03
     C                   PARM                    p$KEY04
     C                   PARM                    p$KEY05
     C                   PARM                    p$KEY06
     C*---------------------------------------------------------------- *                           //S62268
     C* Additional parameters for L2R call                              *                           //S62268
     C*---------------------------------------------------------------- *                           //S62268
     C* START USER PROTECTED SECTION  - SECA0R 0012.400 / *USR                                      //S62268
     C* FINISH USER PROTECTED SECTION - SECA0R 0012.400                                             //S62268
     C*---------------------------------------------------------------- *                           //S62268
     C                   PARM      *BLANKS       P$MSEX            7
     C                   PARM      *BLANKS       P$MSID            7
     C*
     C* If there is an error on the call to this program,
     C*  report it.
     C*
     C     *IN90         IFEQ      *ON
     C     W#PFPR        ANDEQ     '*YES    '
     C     P$MSID        ORGT      *BLANKS
     C     W#PFPR        ANDEQ     '*YES    '
     C                   MOVEL     'PSX0001'     W#MSID
     C                   MOVE      *BLANKS       W#MSDT
     C                   MOVEL     'AXTAVL2R'    W#WA10           10
     C     W#SBTG        CAT       W#MSDT        W#MSDT
     C     $PPGNM        CAT       W#MSDT        W#MSDT
     C     W#WA10        CAT       W#MSDT        W#MSDT
     C                   EXSR      PGMABT
     C                   ENDIF
     C*
     C                   EXSR      ITRLCK
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* ITRLCK - Interpret messages from locking program.               *
     C*******************************************************************
     C     ITRLCK        BEGSR
     C*
     C                   MOVEL     'ITRLCK01'    W#SBTG
     C*
     C* If there is a message and the message is not that the
     C*  record is already locked.
     C*
     C     P$MSEX        IFNE      *BLANKS
     C     P$MSEX        ANDNE     'PSX0004'
     C*
     C*   Relay lock messages.
     C*
     C                   MOVEL     $PPGNM        W#MSQU
     C                   MOVEL     P#TSTK        W#MSTK
     C                   EXSR      XXRAM1
     C*
     C*   Send "lock not acquired" message.
     C*
     C                   MOVEL     '*YES    '    W#PFPR
     C                   MOVEL     '*NO     '    W#SHDN
     C                   MOVEL     P#TSTK        W#TSTK
     C                   MOVEL     #LMSID        W1MSID
     C                   MOVEL     *BLANKS       W#MSDT
     C                   MOVEL     #LMSFN        W#MSFN
     C                   MOVEL     '*ERROR  '    W#MSCL
     C                   MOVEL     *BLANKS       W#MSST
     C                   MOVEL     *BLANKS       W#OBCL
     C                   MOVEL     *BLANKS       W#MSFL
     C                   EXSR      XXEMP2
     C*
     C*   If errors were encountered issue appropriate completion
     C*    message.
     C*
     C                   EXSR      SNDCMS
     C*
     C* Release Physical lock                                                                       //S65516
     C*                                                                                             //S65516
     C                   MOVEL     '*YES    '    W#PFPR                                             //S65516
     C                   MOVEL     '*NO     '    W#SHDN                                             //S65516
     C                   MOVEL     '*RLSLOCK'    W#UPFC                                             //S65516
     C                   EXSR      EXCO0R                                                           //S65516
     C*                                                                                             //S65516
     C*   Return to caller.
     C*
     C                   RETURN
     C*
     C                   END
     C*
     C* If there is no message, then set as locked by this
     C*  program.
     C*
     C     P$MSEX        IFEQ      *BLANKS
     C                   MOVEL     *ON           W#RCLK
     C                   END
     C*
     C     XITLCK        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* EDTTMP - Edit transaction template.                             *
     C*******************************************************************
     C     EDTTMP        BEGSR
     C*
     C                   MOVEL     'EDTTMP01'    W#SBTG
     C*
     C* Set error condition off.
     C*
     C                   MOVEL     *OFF          W#ERED
     C*
     C* Start the edit process.
     C*
     C                   MOVEL     '*YES    '    W#PFPR
     C                   MOVEL     '*NO     '    W#SHDN
     C                   MOVEL     '*FULL   '    W#SCED
     C                   EXSR      EXCN0R
     C*
     C* If error occurred set error condition.
     C*
     C     P$EEUF        IFEQ      *ON
     C                   MOVEL     *ON           W#ERED
     C                   END
     C*
     C     ZEDTMP        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* GENHST - History maintenance generation.                        *
     C*******************************************************************
     C     GENHST        BEGSR
     C*
     C                   MOVEL     'GENHST01'    W#SBTG
     C*
     C* Determine if transaction is flagged for history
     C*  considerations.
     C*
     C                   MOVEL     '*YES    '    W#PFPR
     C                   MOVEL     '*NO     '    W#SHDN
     C                   MOVEL     mTRID         W#TRID
     C                   EXSR      XVRMH0
     C*
     C* If transaction is set for history reporting.
     C*
     C     P$MTHS        IFEQ      '*YES    '
     C*                                                                                               S46409
     C* Determine if transaction is flagged for publish & subscribe                                   S46409
     C*                                                                                               S46409
     C                   MOVEL     '*YES    '    W#PFPR                                               S46409
     C                   MOVEL     '*NO     '    W#SHDN                                               S46409
     C                   MOVEL     mTRID         W#TRID                                               S46409
     C                   EXSR      XVRPS0                                                             S46409
     C*                                                                                               S46409
     C* If P&S is active OR History bypass is not requested                                           S46409
     C*                                                                                               S46409
     C     P$PSAN        IFEQ      '*YES'                                                             S46409
     C     P$PSAN        OREQ      '*NO'                                                              S46409
     C     mBPMH         ANDNE     *OFF                                                               S46409
     C*
     C* Call process to generate and store maintenance string.
     C*
     C                   MOVEL     '*YES    '    W#PFPR
     C                   MOVEL     '*NO     '    W#SHDN
     C                   MOVEL     P#TSTK        W#TSTK
     C                   EXSR      EXCP0R
     C*
     C* Call program to generate maintenance history records.
     C*
     C                   MOVEL     '*YES    '    W#PFPR
     C                   MOVEL     '*NO     '    W#SHDN
     C                   MOVEL     P#TSTK        W#TSTK
     C                   MOVEL     *BLANKS       W#TRID                                             //S65516
     C                   MOVEL     *BLANKS       W#OBKY                                             //S65516
     C                   MOVEL     *BLANKS       W#TRKD                                             //S65516
     C                   MOVEL     *BLANKS       W#OBCL                                             //S65516
     C*---------------------------------------------------------------- *                           //S65516
     C* Override maintenance history header data                        *                           //S65516
     C*---------------------------------------------------------------- *                           //S65516
     C* START USER PROTECTED SECTION  - SECA0R 0012.450 / *USR                                      //S65516
     C                   eval      P$IN=mTKENAV                                                     //S65516
     C                   eval      P$LEN=15                                                         //S65516
     C                   Exsr      XVCHS0                                                           //S65516
     C                   eval      w#obky='*KEY(' + %char(mUPDTAV)                                  //S65516
     C                                   +','+%char(mUptmav)                                        //S65516
     C                                   +','''+ %trimr(mORDRAV) +''''                              //S65516
     C                                   +','''+ %trimr(mITNOAV) +''''                              //S65516
     C                                   +','''+ %trimr(mWHIDAV) +''''                              //S65516
     C                                   +',X'''+ %trimr(p$OUT) +''')'                              //S65516
     C* FINISH USER PROTECTED SECTION - SECA0R 0012.450                                             //S65516
     C*---------------------------------------------------------------- *                           //S65516
     C                   EXSR      XVMHG9                                                           //S65516
     C*
     C                   END
     C*
     C                   END                                                                          S46409
     C*                                                                                               S46409
     C     XGEHST        ENDSR
     C*******************************************************************
     C/EJECT
     C/COPY $PSICOPYLE,SVMHG9                                                                       //S65516
     C/COPY $PSICOPYLE,SVRMH0
     C/COPY $PSICOPYLE,XVMHG9                                                                       //S65516
     C/COPY $PSICOPYLE,XVRMH0
     C*******************************************************************
     C* EXCP0R - Execute program - AXTAVP0R                             *
     C*******************************************************************
     C     EXCP0R        BEGSR
     C*
     C                   MOVEL     W#PFPR        W#PFPR            8
     C                   MOVEL     W#SHDN        W#SHDN            8
     C*
     C                   CALL      'AXTAVP0R'                           90
     C                   PARM      W#PFPR        P$PFPR            8
     C                   PARM      W#SHDN        P$SHDN            8
     C                   PARM                    P#RC1T
     C                   PARM      *BLANKS       P$MSID            7
     C*
     C* If there is an error on the call to this program,
     C*  report it.
     C*
     C     *IN90         IFEQ      *ON
     C     W#PFPR        ANDEQ     '*YES    '
     C     P$MSID        ORGT      *BLANKS
     C     W#PFPR        ANDEQ     '*YES    '
     C                   MOVEL     'PSX0001'     W#MSID
     C                   MOVE      *BLANKS       W#MSDT
     C                   MOVEL     'AXTAVP0R'    W#WA10           10
     C     W#SBTG        CAT       W#MSDT        W#MSDT
     C     $PPGNM        CAT       W#MSDT        W#MSDT
     C     W#WA10        CAT       W#MSDT        W#MSDT
     C                   EXSR      PGMABT
     C                   ENDIF
     C*
     C     ZEXCP0R       ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* CLSP0R - Shutdown program - AXTAVP0R                            *
     C*******************************************************************
     C     CLSP0R        BEGSR
     C*
     C                   MOVEL     '*NO     '    W#PFPR
     C                   MOVEL     '*YES    '    W#SHDN
     C                   EXSR      EXCP0R
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* SNDCMS - Send completion message.                               *
     C*******************************************************************
     C     SNDCMS        BEGSR
     C*
     C                   MOVEL     'SNDCMS01'    W#SBTG
     C*
     C* Retrieve completion message from storage.
     C*
     C                   MOVEL     '*YES    '    W#PFPR
     C                   MOVEL     '*NO     '    W#SHDN
     C                   MOVEL     '*RTV    '    W#STRQ
     C                   MOVEL     *BLANKS       W1MSID
     C                   EXSR      XXEMS0
     C*
     C* If message exists, send completion message.
     C*
     C     P1MSID        IFNE      *BLANKS
     C                   MOVEL     '*YES    '    W#PFPR
     C                   MOVEL     '*NO     '    W#SHDN
     C                   MOVEL     P#TSTK        W#TSTK
     C                   MOVEL     P1MSID        W1MSID
     C                   MOVEL     *BLANKS       W#MSDT
     C                   MOVEL     *BLANKS       W#MSFN
     C                   MOVEL     *BLANKS       W#MSCL
     C                   MOVEL     *BLANKS       W#MSST
     C                   MOVEL     *BLANKS       W#OBCL
     C                   MOVEL     *BLANKS       W#MSFL
     C                   EXSR      XXEMP2
     C                   END
     C*
     C     ZSNCMS        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* UPDTMP - Update template processing.                            *
     C*******************************************************************
     C     UPDTMP        BEGSR
     C*
     C                   MOVEL     'UPDTMP01'    W#SBTG
     C*
     C* Call update process.                                                                        //S73152
     C*
     C                   MOVEL     '*YES    '    W#PFPR
     C                   MOVEL     '*NO     '    W#SHDN
     C                   MOVEL     '*PROCESS'    W#UPFC                                             //D10055
     C                   EXSR      EXCO0R
     C*---------------------------------------------------------------- *
     C* R6 User exit call considerations                                *
     C*---------------------------------------------------------------- *
     C* START USER PROTECTED SECTION  - SECA0R 0012.500 / *USR
     C* FINISH USER PROTECTED SECTION - SECA0R 0012.500
     C*---------------------------------------------------------------- *
     C*
     C* If Mapics update completed succesfully and there are active user exits.
     C*
     C     M_XA0004      IFEQ      *ON
     C     P$UPCM        ANDEQ     '*YES    '
     C*
     C*   Call user update process.
     C*
     C                   EXSR      M_XU$$O0
     C                   END
     C*
     C* Make sure the lock has been released.
     C*
     C     mORPR         IFEQ      *BLANKS                                                          //D10055
     C                   MOVEL     '*REL    '    W#RQFN
     C                   EXSR      EXCLCK
     C                   ENDIF                                                                      //D10055
     C*
     C* If update occurred, generate maintenance history.
     C*
     C     P$UPCM        IFEQ      '*YES    '
     C                   EXSR      GENHST
     C                   END
     C*
     C     ZUPTMP        ENDSR
     C*******************************************************************                           //S65516
     C/EJECT                                                                                        //S65516
     C*******************************************************************                           //S65516
     C* APPERR - Application Error Logic                                *                           //S65516
     C*******************************************************************                           //S65516
     C     APPERR        BEGSR                                                                      //S65516
     C*                                                                                             //S65516
     C                   MOVEL     'APPERR01'    W#SBTG                                             //S65516
     C*                                                                                             //S65516
     C                   MOVEL     *OFF          wAPPERR           1                                //S65516
     C*                                                                                             //S65516
     C*---------------------------------------------------------------- *                           //S65516
     C* Turn on *APPERR call flag, if required.                         *                           //S65516
     C*---------------------------------------------------------------- *                           //S65516
     C* START USER PROTECTED SECTION  - SECA0R 0012.600 / *USR                                      //S65516
     C* FINISH USER PROTECTED SECTION - SECA0R 0012.600                                             //S65516
     C*---------------------------------------------------------------- *                           //S65516
     C                   IF        wAPPERR = *ON                                                    //S65516
     C                   MOVEL     '*YES    '    W#PFPR                                             //S65516
     C                   MOVEL     '*NO     '    W#SHDN                                             //S65516
     C                   MOVEL     '*APPERR '    W#UPFC                                             //S65516
     C                   EXSR      EXCO0R                                                           //S65516
     C                   ENDIF                                                                      //S65516
     C*                                                                                             //S65516
     C     ZAPERR        ENDSR                                                                      //S65516
     C*******************************************************************                           //S65516
     C/EJECT                                                                                        //S65516
     C*******************************************************************                           //S65516
     C* AVDTRN - Avoid Transaction Logic                                *                           //S65516
     C*******************************************************************                           //S65516
     C     AVDTRN        BEGSR                                                                      //S65516
     C*                                                                                             //S65516
     C                   MOVEL     'AVDTRN01'    W#SBTG                                             //S65516
     C*                                                                                             //S65516
     C*---------------------------------------------------------------- *                           //S65516
     C* Avoid transaction logic.                                        *                           //S65516
     C*---------------------------------------------------------------- *                           //S65516
     C* START USER PROTECTED SECTION  - SECA0R 0012.700 / *USR                                      //S65516
     C* FINISH USER PROTECTED SECTION - SECA0R 0012.700                                             //S65516
     C*---------------------------------------------------------------- *                           //S65516
     C*                                                                                             //S65516
     C     ZAVTRN        ENDSR                                                                      //S65516
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* EXCF0R - Execute program - AXTAVF0R                             *
     C*******************************************************************
     C     EXCF0R        BEGSR
     C*
     C                   MOVEL     W#PFPR        W#PFPR            8
     C                   MOVEL     W#SHDN        W#SHDN            8
     C                   MOVEL     W#TSTK        W#TSTK           10
     C                   MOVEL     W#SCCN        W#SCCN            8
     C*
     C                   CALL      'AXTAVF0R'                           90
     C                   PARM      W#PFPR        P$PFPR            8
     C                   PARM      W#SHDN        P$SHDN            8
     C                   PARM      W#TSTK        P$TSTK           10
     C                   PARM      W#SCCN        P$SCCN            8
     C                   PARM      *BLANKS       P$MSID            7
     C*
     C* If there is an error on the call to this program,
     C*  report it.
     C*
     C     *IN90         IFEQ      *ON
     C     W#PFPR        ANDEQ     '*YES    '
     C     P$MSID        ORGT      *BLANKS
     C     W#PFPR        ANDEQ     '*YES    '
     C                   MOVEL     'PSX0001'     W#MSID
     C                   MOVE      *BLANKS       W#MSDT
     C                   MOVEL     'AXTAVF0R'    W#WA10           10
     C     W#SBTG        CAT       W#MSDT        W#MSDT
     C     $PPGNM        CAT       W#MSDT        W#MSDT
     C     W#WA10        CAT       W#MSDT        W#MSDT
     C                   EXSR      PGMABT
     C                   ENDIF
     C*
     C     ZEXCF0R       ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* CLSF0R - Shutdown program - AXTAVF0R                            *
     C*******************************************************************
     C     CLSF0R        BEGSR
     C*
     C                   MOVEL     '*NO     '    W#PFPR
     C                   MOVEL     '*YES    '    W#SHDN
     C                   EXSR      EXCF0R
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* EXCK0R - Execute program - AXTAVK0R                             *
     C*******************************************************************
     C     EXCK0R        BEGSR
     C*
     C                   MOVEL     W#PFPR        W#PFPR            8
     C                   MOVEL     W#SHDN        W#SHDN            8
     C                   MOVEL     W#TSTK        W#TSTK           10
     C*
     C                   CALL      'AXTAVK0R'                           90
     C                   PARM      W#PFPR        P$PFPR            8
     C                   PARM      W#SHDN        P$SHDN            8
     C                   PARM      W#TSTK        P$TSTK           10
     C                   PARM                    P#RC1T
     C                   PARM      *BLANKS       P$MSID            7
     C*
     C* If there is an error on the call to this program,
     C*  report it.
     C*
     C     *IN90         IFEQ      *ON
     C     W#PFPR        ANDEQ     '*YES    '
     C     P$MSID        ORGT      *BLANKS
     C     W#PFPR        ANDEQ     '*YES    '
     C                   MOVEL     'PSX0001'     W#MSID
     C                   MOVE      *BLANKS       W#MSDT
     C                   MOVEL     'AXTAVK0R'    W#WA10           10
     C     W#SBTG        CAT       W#MSDT        W#MSDT
     C     $PPGNM        CAT       W#MSDT        W#MSDT
     C     W#WA10        CAT       W#MSDT        W#MSDT
     C                   EXSR      PGMABT
     C                   ENDIF
     C*
     C     ZEXCK0R       ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* CLSK0R - Shutdown program - AXTAVK0R                            *
     C*******************************************************************
     C     CLSK0R        BEGSR
     C*
     C                   MOVEL     '*NO     '    W#PFPR
     C                   MOVEL     '*YES    '    W#SHDN
     C                   EXSR      EXCK0R
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* EXCM0R - Execute program - AXTAVM0R                             *
     C*******************************************************************
     C     EXCM0R        BEGSR
     C*
     C                   MOVEL     W#PFPR        W#PFPR            8
     C                   MOVEL     W#SHDN        W#SHDN            8
     C*
     C                   CALL      'AXTAVM0R'                           90
     C                   PARM      W#PFPR        P$PFPR            8
     C                   PARM      W#SHDN        P$SHDN            8
     C                   PARM                    P#RC1T
     C                   PARM      *BLANKS       P$MSID            7
     C*
     C* If there is an error on the call to this program,
     C*  report it.
     C*
     C     *IN90         IFEQ      *ON
     C     W#PFPR        ANDEQ     '*YES    '
     C     P$MSID        ORGT      *BLANKS
     C     W#PFPR        ANDEQ     '*YES    '
     C                   MOVEL     'PSX0001'     W#MSID
     C                   MOVE      *BLANKS       W#MSDT
     C                   MOVEL     'AXTAVM0R'    W#WA10           10
     C     W#SBTG        CAT       W#MSDT        W#MSDT
     C     $PPGNM        CAT       W#MSDT        W#MSDT
     C     W#WA10        CAT       W#MSDT        W#MSDT
     C                   EXSR      PGMABT
     C                   ENDIF
     C*
     C     ZEXCM0R       ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* CLSM0R - Shutdown program - AXTAVM0R                            *
     C*******************************************************************
     C     CLSM0R        BEGSR
     C*
     C                   MOVEL     '*NO     '    W#PFPR
     C                   MOVEL     '*YES    '    W#SHDN
     C                   EXSR      EXCM0R
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* EXCN0R - Execute program - AXTAVN0R                             *
     C*******************************************************************
     C     EXCN0R        BEGSR
     C*
     C                   MOVEL     W#PFPR        W#PFPR            8
     C                   MOVEL     W#SHDN        W#SHDN            8
     C                   MOVEL     W#SCED        W#SCED            8
     C*
     C                   CALL      'AXTAVN0R'                           90
     C                   PARM      W#PFPR        P$PFPR            8
     C                   PARM      W#SHDN        P$SHDN            8
     C                   PARM      W#SCED        P$SCED            8
     C                   PARM                    P#RC1T
     C                   PARM      *BLANKS       P$EEUF            1
     C                   PARM      *BLANKS       P$MSID            7
     C*
     C* If there is an error on the call to this program,
     C*  report it.
     C*
     C     *IN90         IFEQ      *ON
     C     W#PFPR        ANDEQ     '*YES    '
     C     P$MSID        ORGT      *BLANKS
     C     W#PFPR        ANDEQ     '*YES    '
     C                   MOVEL     'PSX0001'     W#MSID
     C                   MOVE      *BLANKS       W#MSDT
     C                   MOVEL     'AXTAVN0R'    W#WA10           10
     C     W#SBTG        CAT       W#MSDT        W#MSDT
     C     $PPGNM        CAT       W#MSDT        W#MSDT
     C     W#WA10        CAT       W#MSDT        W#MSDT
     C                   EXSR      PGMABT
     C                   ENDIF
     C*
     C     ZEXCN0R       ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* CLSN0R - Shutdown program - AXTAVN0R                            *
     C*******************************************************************
     C     CLSN0R        BEGSR
     C*
     C                   MOVEL     '*NO     '    W#PFPR
     C                   MOVEL     '*YES    '    W#SHDN
     C                   EXSR      EXCN0R
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* EXCO0R - Execute program - AXTAVO0R                             *
     C*******************************************************************
     C     EXCO0R        BEGSR
     C*
     C                   MOVEL     W#PFPR        W#PFPR            8
     C                   MOVEL     W#SHDN        W#SHDN            8
     C                   MOVEL     W#UPFC        W#UPFC            8                                //D10055
     C*
     C                   CALL      'AXTAVO0R'                           90
     C                   PARM      W#PFPR        P$PFPR            8
     C                   PARM      W#SHDN        P$SHDN            8
     C                   PARM                    P#RC1T
     C                   PARM      W#UPFC        P$UPFC            8                                //D10055
     C                   PARM                    P$UPCM            8
     C                   PARM      *BLANKS       P$MSID            7
     C*
     C* If there is an error on the call to this program,
     C*  report it.
     C*
     C     *IN90         IFEQ      *ON
     C     W#PFPR        ANDEQ     '*YES    '
     C     P$MSID        ORGT      *BLANKS
     C     W#PFPR        ANDEQ     '*YES    '
     C                   MOVEL     'PSX0001'     W#MSID
     C                   MOVE      *BLANKS       W#MSDT
     C                   MOVEL     'AXTAVO0R'    W#WA10           10
     C     W#SBTG        CAT       W#MSDT        W#MSDT
     C     $PPGNM        CAT       W#MSDT        W#MSDT
     C     W#WA10        CAT       W#MSDT        W#MSDT
     C                   EXSR      PGMABT
     C                   ENDIF
     C*
     C     ZEXCO0R       ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* CLSO0R - Shutdown program - AXTAVO0R                            *
     C*******************************************************************
     C     CLSO0R        BEGSR
     C*
     C                   MOVEL     '*NO     '    W#PFPR
     C                   MOVEL     '*YES    '    W#SHDN
     C                   EXSR      EXCO0R
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* EXCQ0R - Execute program - AXTAVQ0R                             *
     C*******************************************************************
     C     EXCQ0R        BEGSR
     C*
     C                   MOVEL     W#PFPR        W#PFPR            8
     C                   MOVEL     W#SHDN        W#SHDN            8
     C                   MOVEL     W#SCDF        W#SCDF            8
     C                   MOVEL     W#CLPR        W#CLPR            8
     C*
     C                   CALL      'AXTAVQ0R'                           90
     C                   PARM      W#PFPR        P$PFPR            8
     C                   PARM      W#SHDN        P$SHDN            8
     C                   PARM      W#SCDF        P$SCDF            8
     C                   PARM      W#CLPR        P$CLPR            8
     C                   PARM                    P#RC1T
     C                   PARM      *BLANKS       P$MSID            7
     C*
     C* If there is an error on the call to this program,
     C*  report it.
     C*
     C     *IN90         IFEQ      *ON
     C     W#PFPR        ANDEQ     '*YES    '
     C     P$MSID        ORGT      *BLANKS
     C     W#PFPR        ANDEQ     '*YES    '
     C                   MOVEL     'PSX0001'     W#MSID
     C                   MOVE      *BLANKS       W#MSDT
     C                   MOVEL     'AXTAVQ0R'    W#WA10           10
     C     W#SBTG        CAT       W#MSDT        W#MSDT
     C     $PPGNM        CAT       W#MSDT        W#MSDT
     C     W#WA10        CAT       W#MSDT        W#MSDT
     C                   EXSR      PGMABT
     C                   ENDIF
     C*
     C     ZEXCQ0R       ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* CLSQ0R - Shutdown program - AXTAVQ0R                            *
     C*******************************************************************
     C     CLSQ0R        BEGSR
     C*
     C                   MOVEL     '*NO     '    W#PFPR
     C                   MOVEL     '*YES    '    W#SHDN
     C                   EXSR      EXCQ0R
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_CVPFC0 - Shutdowm user exit programs.                         *
     C*******************************************************************
     C     M_CVPFC0      BEGSR
     C*
     C* Shutdown function point calls.
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     mTSTK         M_W#TSTK
     C***                MOVEL     M_P$BOTK      M_P$BOTK
     C***                MOVEL     M_W#FNPT      M_W#FNPT
     C                   MOVEL     '*SHUTSTR'    M_W#FNCR
     C                   MOVEL     '*ALL    '    M_W#CLSL
     C                   CLEAR                   M_W#SLLV
     C                   MOVEL     '*NO    '     M_W#SPIN
     C                   EXSR      M_XVPFC0
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_SVPFC0 - Shutdown program - PSVPFC0R.                         *
     C*******************************************************************
     C     M_SVPFC0      BEGSR
     C*
     C                   MOVEL     '*NO     '    M_W#PFPR
     C                   MOVEL     '*YES    '    M_W#SHDN
     C                   EXSR      M_XVPFC0
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_XVPFC0 - Execute program - PSVPFC0R                           *
     C*******************************************************************
     C     M_XVPFC0      BEGSR
     C*
     F*---------------------------------------------------------------- *
     F* @@@@ Additional file definitions                                *
     F*---------------------------------------------------------------- *
     F* START USER PROTECTED SECTION  - XVPFC0 0001.000 / *USR
     C* Call user exits in child mode too.                                                          //S49743
     C                   MOVEL     '*ALL    '    M_W#CLSL                                           //S49743
     C                   CLEAR                   M_W#SLLV                                           //S49743
     F* FINISH USER PROTECTED SECTION - XVPFC0 0001.000
     F*---------------------------------------------------------------- *
     C*
     C                   MOVEL     M_W#PFPR      M_W#PFPR          8
     C                   MOVEL     M_W#SHDN      M_W#SHDN          8
     C                   MOVEL     M_W#TSTK      M_W#TSTK         10
     C                   MOVEL     M_W#FNPT      M_W#FNPT          6
     C                   MOVEL     M_W#FNCR      M_W#FNCR          8
     C                   MOVEL     M_W#CLSL      M_W#CLSL          8
     C                   MOVEL     M_W#SLLV      M_W#SLLV          6
     C                   MOVEL     M_W#SPIN      M_W#SPIN          8
     C*
     C                   CALL      'PSVPFC0R'                           90
     C                   PARM      M_W#PFPR      M_P$PFPR          8
     C                   PARM      M_W#SHDN      M_P$SHDN          8
     C                   PARM      M_W#TSTK      M_P$TSTK         10
     C                   PARM                    M_P$BOTK         32
     C                   PARM      M_W#FNPT      M_P$FNPT          6
     C                   PARM      M_W#FNCR      M_P$FNCR          8
     C                   PARM      M_W#CLSL      M_P$CLSL          8
     C                   PARM      M_W#SLLV      M_P$SLLV          6
     C                   PARM      M_W#SPIN      M_P$SPIN          8
     C                   PARM      *BLANKS       M_P$FNCM          8
     C                   PARM      *BLANKS       M_P$SHCL          8
     C                   PARM      *BLANKS       M_P$ITIN          8
     C                   PARM      *BLANKS       M_P$CLPT        256
     C                   PARM      *BLANKS       M_P$CLPG        256
     C                   PARM      *BLANKS       M_P$CLLV          6
     C                   PARM      *BLANKS       M_P$CLDS         40
     C                   PARM      *BLANKS       M_P$DBOP          2
     C                   PARM      *BLANKS       M_P$ABTR          2
     C                   PARM      *BLANKS       M_P$MSID          7
     C*
     C* If there is an error on the call to this program,
     C*  report it.
     C*
     C     *IN90         IFEQ      *ON
     C     M_W#PFPR      ANDEQ     '*YES    '
     C     M_P$MSID      ORNE      *BLANKS
     C     M_W#PFPR      ANDEQ     '*YES    '
     C                   MOVEL     'PSX0001'     W#MSID
     C                   MOVE      *BLANKS       W#MSDT
     C                   MOVEL     'PSVPFC0R'    W#WA10           10
     C     W#SBTG        CAT       W#MSDT        W#MSDT
     C     $PPGNM        CAT       W#MSDT        W#MSDT
     C     W#WA10        CAT       W#MSDT        W#MSDT
     C                   EXSR      PGMABT
     C                   ENDIF
     C*
     C     ZXVPFC        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_XVSFD0 - Execute program - PSVSFD0R                           *
     C*******************************************************************
     C     M_XVSFD0      BEGSR
     C*
     C                   MOVEL     M_W#PFPR      M_W#PFPR          8
     C                   MOVEL     M_W#SHDN      M_W#SHDN          8
     C                   MOVEL     M_W#FNPT      M_W#FNPT          6
     C                   MOVEL     M_W#RQDF      M_W#RQDF          8
     C                   MOVEL     M_W#RQDN      M_W#RQDN        256
     C                   MOVEL     M_W#RQDT      M_W#RQDT          8
     C                   Z-ADD     M_W#NMDV      M_W#NMDV         30 9
     C*
     C                   CALL      'PSVSFD0R'                           90
     C                   PARM      M_W#PFPR      M_P$PFPR          8
     C                   PARM      M_W#SHDN      M_P$SHDN          8
     C                   PARM      M_W#FNPT      M_P$FNPT          6
     C                   PARM      M_W#RQDF      M_P$RQDF          8
     C                   PARM      M_W#RQDN      M_P$RQDN        256
     C                   PARM      M_W#RQDT      M_P$RQDT          8
     C                   PARM                    M_P$CHDV       7680                                //S62268
     C                   PARM      M_W#NMDV      M_P$NMDV         30 9
     C                   PARM                    M_P$DSDF      15456                                //S62268
     C                   PARM      *BLANKS       M_P$MSID          7
     C*
     C* If there is an error on the call to this program,
     C*  report it.
     C*
     C     *IN90         IFEQ      *ON
     C     M_W#PFPR      ANDEQ     '*YES    '
     C     M_P$MSID      ORNE      *BLANKS
     C     M_W#PFPR      ANDEQ     '*YES    '
     C                   MOVEL     'PSX0001'     W#MSID
     C                   MOVE      *BLANKS       W#MSDT
     C                   MOVEL     'PSVSFD0R'    W#WA10           10
     C     W#SBTG        CAT       W#MSDT        W#MSDT
     C     $PPGNM        CAT       W#MSDT        W#MSDT
     C     W#WA10        CAT       W#MSDT        W#MSDT
     C                   EXSR      PGMABT
     C                   ENDIF
     C*
     C     M_ZVSFD0      ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_SVSFD0 - Shutdown program - PSVSFD0R                          *
     C*******************************************************************
     C     M_SVSFD0      BEGSR
     C*
     C                   MOVEL     '*NO     '    M_W#PFPR
     C                   MOVEL     '*YES    '    M_W#SHDN
     C                   EXSR      M_XVSFD0
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_XVRFP0 - Execute program - PSVRFP0R                           *
     C*******************************************************************
     C     M_XVRFP0      BEGSR
     C*
     C                   MOVEL     M_W#PFPR      M_W#PFPR          8
     C                   MOVEL     M_W#SHDN      M_W#SHDN          8
     C                   MOVEL     M_W#FNPT      M_W#FNPT          6
     C                   MOVEL     M_W#FNRR      M_W#FNRR          8
     C                   MOVEL     M_W#FNSL      M_W#FNSL          8
     C                   MOVEL     M_W#CLLV      M_W#CLLV          6
     C*
     F*---------------------------------------------------------------- *
     F* @@@@ Pre-call operations                                        *
     F*---------------------------------------------------------------- *
     F* START USER PROTECTED SECTION  - XVRFP0 0001.000 / *USR
     F* FINISH USER PROTECTED SECTION - XVRFP0 0001.000
     F*---------------------------------------------------------------- *
     C*
     C                   CALL      'PSVRFP0R'                           90
     C                   PARM      M_W#PFPR      M_P$PFPR          8
     C                   PARM      M_W#SHDN      M_P$SHDN          8
     C                   PARM                    M_P$BOTK         32
     C                   PARM      M_W#FNPT      M_P$FNPT          6
     C                   PARM      M_W#FNRR      M_P$FNRR          8
     C                   PARM      M_W#FNSL      M_P$FNSL          8
     C                   PARM      *BLANKS       M_P$CREE          8
     C                   PARM      *BLANKS       M_P$CLPT        256
     C                   PARM      *BLANKS       M_P$CLPG        256
     C                   PARM      M_W#CLLV      M_P$CLLV          6
     C                   PARM      *BLANKS       M_P$CLDS         40
     C                   PARM      *BLANKS       M_P$CLST          2
     C                   PARM      *BLANKS       M_P$DBOP          2
     C                   PARM      *BLANKS       M_P$ABTR          2
     C                   PARM      *BLANKS       M_P$MSID          7
     C*
     C* If there is an error on the call to this program,
     C*  report it.
     C*
     C     *IN90         IFEQ      *ON
     C     M_W#PFPR      ANDEQ     '*YES    '
     C     M_P$MSID      ORNE      *BLANKS
     C     M_W#PFPR      ANDEQ     '*YES    '
     C                   MOVEL     'PSX0001'     W#MSID
     C                   MOVE      *BLANKS       W#MSDT
     C                   MOVEL     'PSVRFP0R'    W#WA10           10
     C     W#SBTG        CAT       W#MSDT        W#MSDT
     C     $PPGNM        CAT       W#MSDT        W#MSDT
     C     W#WA10        CAT       W#MSDT        W#MSDT
     C                   EXSR      PGMABT
     C                   ENDIF
     C*
     C     ZXVRFP        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_SVRFP0 - Shutdown program - PSVRFP0R.                         *
     C*******************************************************************
     C     M_SVRFP0      BEGSR
     C*
     C                   MOVEL     '*NO     '    M_W#PFPR
     C                   MOVEL     '*YES    '    M_W#SHDN
     C                   EXSR      M_XVRFP0
     C*
     C                   ENDSR
     C*******************************************************************                             S47026
     C/EJECT                                                                                          S47026
     C*******************************************************************                             S47026
     C* XVORS0 - Execute program - PSVORS0R                             *                             S47026
     C*******************************************************************                             S47026
     C     XVORS0        BEGSR                                                                        S47026
     C*                                                                                               S47026
     C                   MOVEL     W#PFPR        W#PFPR            8                                  S47026
     C                   MOVEL     W#SHDN        W#SHDN            8                                  S47026
     C                   MOVEL     W#OBCL        W#OBCL           10                                  S47026
     C*                                                                                               S47026
     C                   CALL      'PSVORS0R'                           90                            S47026
     C                   PARM      W#PFPR        P$PFPR            8                                  S47026
     C                   PARM      W#SHDN        P$SHDN            8                                  S47026
     C                   PARM      W#OBCL        P$OBCL           10                                  S47026
     C                   PARM      *BLANKS       P$DCTL            1                                  S47026
     C                   PARM      *BLANKS       P$MCTL            1                                  S47026
     C                   PARM      *BLANKS       P$REPD           32                                  S47026
     C                   PARM      *BLANKS       P$MSID            7                                  S47026
     C*                                                                                               S47026
     C* If there is an error on the call to this program,                                             S47026
     C*  report it.                                                                                   S47026
     C*                                                                                               S47026
     C     *IN90         IFEQ      *ON                                                                S47026
     C     W#PFPR        ANDEQ     '*YES    '                                                         S47026
     C     P$MSID        ORGT      *BLANKS                                                            S47026
     C     W#PFPR        ANDEQ     '*YES    '                                                         S47026
     C                   MOVEL     'PSX0001'     W#MSID                                               S47026
     C                   MOVE      *BLANKS       W#MSDT                                               S47026
     C                   MOVEL     'PSVORS0R'    W#WA10           10                                  S47026
     C     W#SBTG        CAT       W#MSDT        W#MSDT                                               S47026
     C     $PPGNM        CAT       W#MSDT        W#MSDT                                               S47026
     C     W#WA10        CAT       W#MSDT        W#MSDT                                               S47026
     C                   EXSR      PGMABT                                                             S47026
     C                   END                                                                          S47026
     C*                                                                                               S47026
     C     ZVORS0        ENDSR                                                                        S47026
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_CL$$M0 - If XA0002 function point call is active shutdown     *
     C*            all XA0002 function point called programs.           *
     C*******************************************************************
     C     M_CL$$M0      BEGSR
     C*
     C     M_XA0002      IFEQ      *ON
     C                   MOVEL     M_W#BOTK      M_P$BOTK
     C                   MOVEL     'XA0002'      M_W#FNPT
     C                   EXSR      M_CVPFC0
     C                   END
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_SF$$M0 - Set active user exits exist flag                     *
     C*******************************************************************
     C     M_SF$$M0      BEGSR
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     M_W#BOTK      M_P$BOTK
     C                   MOVEL     'XA0002'      M_W#FNPT
     C                   MOVEL     '*INITIAL'    M_W#FNRR
     C                   MOVEL     '*ACTIVE '    M_W#FNSL
     C                   MOVEL     '*ALL    '    M_W#CLLV
     C                   EXSR      M_XVRFP0
     C     M_P$CREE      IFEQ      '*YES    '
     C                   MOVEL     *ON           M_XA0002          1
     C                   ELSE
     C                   MOVEL     *OFF          M_XA0002
     C                   END
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_XU$$M0 - Execute program - User ext - Load database values    *
     C*******************************************************************
     C     M_XU$$M0      BEGSR
     C*
     C* Load application parms to storage program.
     C*
     C                   EXSR      M_ST$$M0
     C*
     C* Perform function point calls.
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     mTSTK         M_W#TSTK
     C                   MOVEL     M_W#BOTK      M_P$BOTK
     C                   MOVEL     'XA0002'      M_W#FNPT
     C                   MOVEL     '*START  '    M_W#FNCR
     C                   MOVEL     '*ALL    '    M_W#CLSL
     C                   CLEAR                   M_W#SLLV
     C                   SELECT                                                                       S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'ITMLOCCHG    '                                            S42163
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'ITMLOCCRT    '                                            S42163
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'ITMLOCDLT    '                                            S42163
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'MORCMPCHG    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'MORCMPCRT    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'MORCMPDLT    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'INVTXN_LQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'INVTXN_LA    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'INVTXN_SQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'INVTXN_MQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'MORCMPPB     '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'INVTXN_PQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'INVTXN_RM    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_SS    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_IU    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_RS    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_SM    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S47026
     C                   MOVEL     '010200'      M_W#SLLV                                             S47026
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S47026
     C                                  AND                                                           S47026
     C                             mORPR = 'INVTXN_IX    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SC    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CR    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CS    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SP    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CQ    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RQ    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IP    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IW    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IS    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RW    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RC    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_TW    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RP    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RD    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RI    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_VR    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_VA    '                                            S42163
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   OTHER                                                                        S42163
     C                   MOVEL     '*ALL    '    M_W#CLSL                                             S42163
     C                   CLEAR                   M_W#SLLV                                             S42163
     C                   ENDSL                                                                        S42163
     C                   MOVEL     '*NO    '     M_W#SPIN
     C                   EXSR      M_XVPFC0
     C*
     C* Retrieve application parms from storage.
     C*
     C                   EXSR      M_RT$$M0
     C*
     C* Process until all function point calls have been accomplished.
     C*
     C     M_P$FNCM      DOWEQ     '*NO     '
     C*
     C*   If we are being told to execute the call then setup and
     C*    call the appropriate program.
     C*
     C     M_P$SHCL      IFEQ      '*YES    '
     F*---------------------------------------------------------------- *
     F* @@@@ XAR6 User exit support considerations                      *
     F*---------------------------------------------------------------- *
     F* START USER PROTECTED SECTION  - XUSRM0 0001.000 / *USR
     F* FINISH USER PROTECTED SECTION - XUSRM0 0001.000
     F*---------------------------------------------------------------- *
     C                   END
     C*
     C*   If this is an interrupt between function point calls then
     C*    do any necessary application logic.
     C*
     C     M_P$ITIN      IFEQ      '*YES    '
     C*
     C                   END
     C*
     C*   Load application parms to storage program.
     C*
     C                   EXSR      M_ST$$M0
     C*
     C*   Continue with function point calls.
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     mTSTK         M_W#TSTK
     C                   MOVEL     M_W#BOTK      M_P$BOTK
     C                   MOVEL     'XA0002'      M_W#FNPT
     C                   MOVEL     '*PROCESS'    M_W#FNCR
     C                   MOVEL     '*ALL    '    M_W#CLSL
     C                   CLEAR                   M_W#SLLV
     C                   SELECT                                                                       S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'ITMLOCCHG    '                                            S42163
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'ITMLOCCRT    '                                            S42163
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'ITMLOCDLT    '                                            S42163
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'MORCMPCHG    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'MORCMPCRT    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'MORCMPDLT    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_LQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_LA    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_MQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'MORCMPPB     '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_PQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RM    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SS    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IU    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RS    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SM    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IX    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SC    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CR    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CS    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SP    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CQ    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'INVTXN_RQ    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'INVTXN_IA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'INVTXN_IP    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'INVTXN_SA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'INVTXN_IW    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'INVTXN_IS    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'INVTXN_RW    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'INVTXN_RC    '                                            S42163
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_TW    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RP    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RD    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'INVTXN_RI    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S47026
     C                   MOVEL     '010200'      M_W#SLLV                                             S47026
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S47026
     C                                  AND                                                           S47026
     C                             mORPR = 'INVTXN_VR    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S42163
     C                                  AND                                                           S42163
     C                             mORPR = 'INVTXN_VA    '                                            S42163
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S42163
     C                   MOVEL     '010200'      M_W#SLLV                                             S42163
     C                   OTHER                                                                        S42163
     C                   MOVEL     '*ALL    '    M_W#CLSL                                             S42163
     C                   CLEAR                   M_W#SLLV                                             S42163
     C                   ENDSL                                                                        S42163
     C                   MOVEL     '*NO    '     M_W#SPIN
     C                   EXSR      M_XVPFC0
     C*
     C*   Retrieve application parms from storage.
     C*
     C                   EXSR      M_RT$$M0
     C*
     C                   ENDDO
     C*
     C     ZU$$M0        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_RT$$M0 - Retrieve load database from buffer data.             *
     C*******************************************************************
     C     M_RT$$M0      BEGSR
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0002'      M_W#FNPT
     C                   MOVEL     '*RTVDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   MOVEL     'P$RC1T  '    M_W#RQDN
     C                   MOVEL     '*DATAS  '    M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   MOVEL     P#RC1T        M_P$CHDV
     C                   CLEAR                   M_P$DSDF
     C                   MOVEL     P$RC1TDF      M_P$DSDF
     C                   EXSR      M_XVSFD0
     C                   MOVEL     M_P$CHDV      P#RC1T
     C*
     C     ZR$$M0        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_ST$$M0 - Set load database from buffer data.                  *
     C*******************************************************************
     C     M_ST$$M0      BEGSR
     C*
     C* Clear application parms.
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0002'      M_W#FNPT
     C                   MOVEL     '*CLRDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   CLEAR                   M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   EXSR      M_XVSFD0
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0002'      M_W#FNPT
     C                   MOVEL     '*SETDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   MOVEL     'P$RC1T  '    M_W#RQDN
     C                   MOVEL     '*DATAS  '    M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   MOVEL     P#RC1T        M_P$CHDV
     C                   MOVEL     P$RC1TDF      M_P$DSDF
     C                   EXSR      M_XVSFD0
     C     ZS$$M0        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_CL$$N0 - If XA0003 function point call is active shutdown     *
     C*            all XA0003 function point called programs.           *
     C*******************************************************************
     C     M_CL$$N0      BEGSR
     C*
     C     M_XA0003      IFEQ      *ON
     C                   MOVEL     M_W#BOTK      M_P$BOTK
     C                   MOVEL     'XA0003'      M_W#FNPT
     C                   EXSR      M_CVPFC0
     C                   END
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_SF$$N0 - Set active user exits exist flag                     *
     C*******************************************************************
     C     M_SF$$N0      BEGSR
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     M_W#BOTK      M_P$BOTK
     C                   MOVEL     'XA0003'      M_W#FNPT
     C                   MOVEL     '*INITIAL'    M_W#FNRR
     C                   MOVEL     '*ACTIVE '    M_W#FNSL
     C                   MOVEL     '*ALL    '    M_W#CLLV
     C                   EXSR      M_XVRFP0
     C     M_P$CREE      IFEQ      '*YES    '
     C                   MOVEL     *ON           M_XA0003          1
     C                   ELSE
     C                   MOVEL     *OFF          M_XA0003
     C                   END
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_XU$$N0 - Execute program - User exit - Edit                   *
     C*******************************************************************
     C     M_XU$$N0      BEGSR
     C*
     C* Load application parms to storage program.
     C*
     C                   EXSR      M_ST$$N0
     C*
     C* Perform function point calls.
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     mTSTK         M_W#TSTK
     C                   MOVEL     M_W#BOTK      M_P$BOTK
     C                   MOVEL     'XA0003'      M_W#FNPT
     C                   MOVEL     '*START  '    M_W#FNCR
     C                   SELECT
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCCHG    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCCRT    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCDLT    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'MORCMPCHG    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'MORCMPCRT    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'MORCMPDLT    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_LQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_LA    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_SQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_MQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'MORCMPPB     '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_PQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_RM    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_SS    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_IU    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_RS    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_SM    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S47026
     C                   MOVEL     '010200'      M_W#SLLV                                             S47026
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S47026
     C                                  AND                                                           S47026
     C                             mORPR = 'INVTXN_IX    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SC    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CR    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CS    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SP    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CQ    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RQ    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IP    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IW    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IS    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RW    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RC    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_TW    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RP    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RD    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RI    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_VR    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_VA    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   OTHER
     C                   MOVEL     '*ALL    '    M_W#CLSL
     C                   CLEAR                   M_W#SLLV
     C                   ENDSL
     C                   MOVEL     '*YES   '     M_W#SPIN
     C                   EXSR      M_XVPFC0
     C*
     C* Retrieve application parms from storage.
     C*
     C                   EXSR      M_RT$$N0
     C*
     C* Process until all function point calls have been accomplished.
     C*
     C     M_P$FNCM      DOWEQ     '*NO     '
     C*
     C*   If we are being told to execute the call then setup and
     C*    call the appropriate program.
     C*
     C     M_P$SHCL      IFEQ      '*YES    '
     F*---------------------------------------------------------------- *
     F* @@@@ XAR6 User exit support considerations                      *
     F*---------------------------------------------------------------- *
     F* START USER PROTECTED SECTION  - XUSRN0 0001.000 / *USR
     F* FINISH USER PROTECTED SECTION - XUSRN0 0001.000
     F*---------------------------------------------------------------- *
     C                   END
     C*
     C*   If this is an interrupt between function point calls then
     C*    do any necessary application logic.
     C*
     C     M_P$ITIN      IFEQ      '*YES    '
     C*
     C                   END
     C*
     C*   Load application parms to storage program.
     C*
     C     M_P$EEUF      IFEQ      *ON
     C                   MOVEL     *ON           W#ERED
     C                   END
     C*
     C*   Load application parms to storage program.
     C*
     C                   EXSR      M_ST$$N0
     C*
     C*   Continue with function point calls.
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     mTSTK         M_W#TSTK
     C                   MOVEL     M_W#BOTK      M_P$BOTK
     C                   MOVEL     'XA0003'      M_W#FNPT
     C                   MOVEL     '*PROCESS'    M_W#FNCR
     C                   SELECT
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCCHG    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCCRT    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCDLT    '
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'MORCMPCHG    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'MORCMPCRT    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'MORCMPDLT    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_LQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_LA    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_MQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'MORCMPPB     '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_PQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RM    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SS    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IU    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RS    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SM    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IX    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SC    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CR    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CS    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SP    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CQ    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_RQ    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_IA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_IP    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_SA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_IW    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_IS    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_RW    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_RC    '
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_TW    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RP    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RD    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_RI    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S47026
     C                   MOVEL     '010200'      M_W#SLLV                                             S47026
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S47026
     C                                  AND                                                           S47026
     C                             mORPR = 'INVTXN_VR    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_VA    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   OTHER
     C                   MOVEL     '*ALL    '    M_W#CLSL
     C                   CLEAR                   M_W#SLLV
     C                   ENDSL
     C                   MOVEL     '*YES   '     M_W#SPIN
     C                   EXSR      M_XVPFC0
     C*
     C*   Retrieve application parms from storage.
     C*
     C                   EXSR      M_RT$$N0
     C*
     C                   ENDDO
     C*
     C     ZU$$N0        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_RT$$N0 - Retrieve edit data.                                  *
     C*******************************************************************
     C     M_RT$$N0      BEGSR
     C*
     C* Retrieve application parms from storage.
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0003'      M_W#FNPT
     C                   MOVEL     '*RTVDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   MOVEL     'P$SCED  '    M_W#RQDN
     C                   MOVEL     '*CHAR   '    M_W#RQDT
     C                   EXSR      M_XVSFD0
     C                   MOVEL     M_P$CHDV      P$SCED
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0003'      M_W#FNPT
     C                   MOVEL     '*RTVDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   MOVEL     'P$RC1T  '    M_W#RQDN
     C                   MOVEL     '*DATAS  '    M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   MOVEL     P#RC1T        M_P$CHDV
     C                   CLEAR                   M_P$DSDF
     C                   MOVEL     P$RC1TDF      M_P$DSDF
     C                   EXSR      M_XVSFD0
     C                   MOVEL     M_P$CHDV      P#RC1T
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0003'      M_W#FNPT
     C                   MOVEL     '*RTVDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   MOVEL     'M_P$EEUF'    M_W#RQDN
     C                   MOVEL     '*CHAR   '    M_W#RQDT
     C                   EXSR      M_XVSFD0
     C                   MOVEL     M_P$CHDV      M_P$EEUF
     C*
     C     ZR$$N0        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_ST$$N0 - Set edit data.                                       *
     C*******************************************************************
     C     M_ST$$N0      BEGSR
     C*
     C* Clear application parms.
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0003'      M_W#FNPT
     C                   MOVEL     '*CLRDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   CLEAR                   M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   EXSR      M_XVSFD0
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0003'      M_W#FNPT
     C                   MOVEL     '*SETDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   MOVEL     'P$RC1T  '    M_W#RQDN
     C                   MOVEL     '*DATAS  '    M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   MOVEL     P#RC1T        M_P$CHDV
     C                   MOVEL     P$RC1TDF      M_P$DSDF
     C                   EXSR      M_XVSFD0
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0003'      M_W#FNPT
     C                   MOVEL     '*SETDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   MOVEL     'P$SCED  '    M_W#RQDN
     C                   MOVEL     '*CHAR   '    M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   MOVEL     W#SCED        M_P$CHDV
     C                   EXSR      M_XVSFD0
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0003'      M_W#FNPT
     C                   MOVEL     '*SETDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   MOVEL     'M_P$EEUF'    M_W#RQDN
     C                   MOVEL     '*CHAR   '    M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   MOVEL     M_P$EEUF      M_P$CHDV
     C                   EXSR      M_XVSFD0
     C*
     C     ZS$$N0        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_CL$$O0 - If XA0004 function point call is active shutdown     *
     C*            all XA0004 function point called programs.           *
     C*******************************************************************
     C     M_CL$$O0      BEGSR
     C*
     C     M_XA0004      IFEQ      *ON
     C                   MOVEL     M_W#BOTK      M_P$BOTK
     C                   MOVEL     'XA0004'      M_W#FNPT
     C                   EXSR      M_CVPFC0
     C                   END
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_SF$$O0 - Set active user exits exist flag                     *
     C*******************************************************************
     C     M_SF$$O0      BEGSR
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     M_W#BOTK      M_P$BOTK
     C                   MOVEL     'XA0004'      M_W#FNPT
     C                   MOVEL     '*INITIAL'    M_W#FNRR
     C                   MOVEL     '*ACTIVE '    M_W#FNSL
     C                   MOVEL     '*ALL    '    M_W#CLLV
     C                   EXSR      M_XVRFP0
     C     M_P$CREE      IFEQ      '*YES    '
     C                   MOVEL     *ON           M_XA0004          1
     C                   ELSE
     C                   MOVEL     *OFF          M_XA0004
     C                   END
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_XU$$O0 - Execute program - User exit - Update process         *
     C*******************************************************************
     C     M_XU$$O0      BEGSR
     C*
     C* Load application parms to storage program.
     C*
     C                   EXSR      M_ST$$O0
     C*
     C* Perform function point calls.
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     mTSTK         M_W#TSTK
     C                   MOVEL     M_W#BOTK      M_P$BOTK
     C                   MOVEL     'XA0004'      M_W#FNPT
     C                   MOVEL     '*START  '    M_W#FNCR
     C                   SELECT
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCCHG    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCCRT    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCDLT    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'MORCMPCHG    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'MORCMPCRT    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'MORCMPDLT    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_LQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_LA    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_SQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_MQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'MORCMPPB     '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_PQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_RM    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_SS    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_IU    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_RS    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_SM    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S47026
     C                   MOVEL     '010200'      M_W#SLLV                                             S47026
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S47026
     C                                  AND                                                           S47026
     C                             mORPR = 'INVTXN_IX    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SC    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CR    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CS    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SP    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CQ    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RQ    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IP    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IW    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IS    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RW    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RC    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_TW    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RP    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RD    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RI    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_VR    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_VA    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   OTHER
     C                   MOVEL     '*ALL    '    M_W#CLSL
     C                   CLEAR                   M_W#SLLV
     C                   ENDSL
     C                   MOVEL     '*NO    '     M_W#SPIN
     C                   EXSR      M_XVPFC0
     C*
     C* Retrieve application parms from storage.
     C*
     C                   EXSR      M_RT$$O0
     C*
     C* Process until all function point calls have been accomplished.
     C*
     C     M_P$FNCM      DOWEQ     '*NO     '
     C*
     C*   If we are being told to execute the call then setup and
     C*    call the appropriate program.
     C*
     C     M_P$SHCL      IFEQ      '*YES    '
     F*---------------------------------------------------------------- *
     F* @@@@ XAR6 User exit support considerations                      *
     F*---------------------------------------------------------------- *
     F* START USER PROTECTED SECTION  - XUSRO0 0001.000 / *USR
     F* FINISH USER PROTECTED SECTION - XUSRO0 0001.000
     F*---------------------------------------------------------------- *
     C                   END
     C*
     C*   If this is an interrupt between function point calls then
     C*    do any necessary application logic.
     C*
     C     M_P$ITIN      IFEQ      '*YES    '
     C*
     C                   END
     C*
     C*   Do not let user program control MAPICS update process.
     C*
     C                   MOVEL     '*YES    '    P$UPCM
     C*
     C*   Load application parms to storage program.
     C*
     C                   EXSR      M_ST$$O0
     C*
     C*   Continue with function point calls.
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     mTSTK         M_W#TSTK
     C                   MOVEL     M_W#BOTK      M_P$BOTK
     C                   MOVEL     'XA0004'      M_W#FNPT
     C                   MOVEL     '*PROCESS'    M_W#FNCR
     C                   SELECT
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCCHG    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCCRT    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCDLT    '
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'MORCMPCHG    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'MORCMPCRT    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'MORCMPDLT    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_LQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_LA    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_MQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'MORCMPPB     '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_PQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RM    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SS    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IU    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RS    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SM    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IX    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SC    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CR    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CS    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SP    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CQ    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_RQ    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_IA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_IP    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_SA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_IW    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_IS    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_RW    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_RC    '
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_TW    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RP    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RD    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_RI    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S47026
     C                   MOVEL     '010200'      M_W#SLLV                                             S47026
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S47026
     C                                  AND                                                           S47026
     C                             mORPR = 'INVTXN_VR    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_VA    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   OTHER
     C                   MOVEL     '*ALL    '    M_W#CLSL
     C                   CLEAR                   M_W#SLLV
     C                   ENDSL
     C                   EXSR      M_XVPFC0
     C*
     C*   Retrieve application parms from storage.
     C*
     C                   EXSR      M_RT$$O0
     C*
     C                   ENDDO
     C*
     C     ZU$$O0        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_RT$$O0 - Retrieve update process data.                        *
     C*******************************************************************
     C     M_RT$$O0      BEGSR
     C*
     C* Retrieve application parms from storage.
     C*
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0004'      M_W#FNPT
     C                   MOVEL     '*RTVDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   MOVEL     'P$RC1T  '    M_W#RQDN
     C                   MOVEL     '*DATAS  '    M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   MOVEL     P#RC1T        M_P$CHDV
     C                   CLEAR                   M_P$DSDF
     C                   MOVEL     P$RC1TDF      M_P$DSDF
     C                   EXSR      M_XVSFD0
     C                   MOVEL     M_P$CHDV      P#RC1T
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0004'      M_W#FNPT
     C                   MOVEL     '*RTVDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   MOVEL     'P$UPCM  '    M_W#RQDN
     C                   MOVEL     '*CHAR   '    M_W#RQDT
     C                   EXSR      M_XVSFD0
     C                   MOVEL     M_P$CHDV      P$UPCM            8
     C*
     C     ZR$$O0        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_ST$$O0 - Set update process data.                             *
     C*******************************************************************
     C     M_ST$$O0      BEGSR
     C*
     C* Clear application parms.
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0004'      M_W#FNPT
     C                   MOVEL     '*CLRDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   CLEAR                   M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   EXSR      M_XVSFD0
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0004'      M_W#FNPT
     C                   MOVEL     '*SETDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   MOVEL     'P$RC1T  '    M_W#RQDN
     C                   MOVEL     '*DATAS  '    M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   MOVEL     P#RC1T        M_P$CHDV
     C                   MOVEL     P$RC1TDF      M_P$DSDF
     C                   EXSR      M_XVSFD0
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0004'      M_W#FNPT
     C                   MOVEL     '*SETDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   MOVEL     'P$UPCM  '    M_W#RQDN
     C                   MOVEL     '*CHAR   '    M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   MOVEL     P$UPCM        M_P$CHDV
     C                   EXSR      M_XVSFD0
     C*
     C     ZS$$O0        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_CL$$Q0 - If XA0008 function point call is active shutdown     *
     C*            all XA0008 function point called programs.           *
     C*******************************************************************
     C     M_CL$$Q0      BEGSR
     C*
     C     M_XA0008      IFEQ      *ON
     C                   MOVEL     M_W#BOTK      M_P$BOTK
     C                   MOVEL     'XA0008'      M_W#FNPT
     C                   EXSR      M_CVPFC0
     C                   END
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_SF$$Q0 - Set active user exits exist flag                     *
     C*******************************************************************
     C     M_SF$$Q0      BEGSR
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     M_W#BOTK      M_P$BOTK
     C                   MOVEL     'XA0008'      M_W#FNPT
     C                   MOVEL     '*INITIAL'    M_W#FNRR
     C                   MOVEL     '*ACTIVE '    M_W#FNSL
     C                   MOVEL     '*ALL    '    M_W#CLLV
     C                   EXSR      M_XVRFP0
     C     M_P$CREE      IFEQ      '*YES    '
     C                   MOVEL     *ON           M_XA0008          1
     C                   ELSE
     C                   MOVEL     *OFF          M_XA0008
     C                   END
     C*
     C                   ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_XU$$Q0 - Execute program - User exit - Defaults               *
     C*******************************************************************
     C     M_XU$$Q0      BEGSR
     C*
     C* Load application parms to storage program.
     C*
     C                   EXSR      M_ST$$Q0
     C*
     C* Perform function point calls.
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     mTSTK         M_W#TSTK
     C                   MOVEL     M_W#BOTK      M_P$BOTK
     C                   MOVEL     'XA0008'      M_W#FNPT
     C                   MOVEL     '*START  '    M_W#FNCR
     C                   SELECT
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCCHG    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCCRT    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCDLT    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'MORCMPCHG    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'MORCMPCRT    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'MORCMPDLT    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_LQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_LA    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_SQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_MQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'MORCMPPB     '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_PQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_RM    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_SS    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_IU    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_RS    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_SM    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S47026
     C                   MOVEL     '010200'      M_W#SLLV                                             S47026
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S47026
     C                                  AND                                                           S47026
     C                             mORPR = 'INVTXN_IX    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SC    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CR    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CS    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SP    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CQ    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RQ    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IP    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IW    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IS    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RW    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RC    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_TW    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RP    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RD    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RI    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_VR    '                                          //S62268
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_VA    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   OTHER
     C                   MOVEL     '*ALL    '    M_W#CLSL
     C                   CLEAR                   M_W#SLLV
     C                   ENDSL
     C                   MOVEL     '*NO    '     M_W#SPIN
     C                   EXSR      M_XVPFC0
     C*
     C* Retrieve application parms from storage.
     C*
     C                   EXSR      M_RT$$Q0
     C*
     C* Process until all function point calls have been accomplished.
     C*
     C     M_P$FNCM      DOWEQ     '*NO     '
     C*
     C*   If we are being told to execute the call then setup and
     C*    call the appropriate program.
     C*
     C     M_P$SHCL      IFEQ      '*YES    '
     F*---------------------------------------------------------------- *
     F* @@@@ XAR6 User exit support considerations                      *
     F*---------------------------------------------------------------- *
     F* START USER PROTECTED SECTION  - XUSRQ0 0001.000 / *USR
     F* FINISH USER PROTECTED SECTION - XUSRQ0 0001.000
     F*---------------------------------------------------------------- *
     C                   END
     C*
     C*   If this is an interrupt between function point calls then
     C*    do any necessary application logic.
     C*
     C     M_P$ITIN      IFEQ      '*YES    '
     C*
     C                   END
     C*
     C*   Load application parms to storage program.
     C*
     C                   EXSR      M_ST$$Q0
     C*
     C*   Continue with function point calls.
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     mTSTK         M_W#TSTK
     C                   MOVEL     M_W#BOTK      M_P$BOTK
     C                   MOVEL     'XA0008'      M_W#FNPT
     C                   MOVEL     '*PROCESS'    M_W#FNCR
     C                   SELECT
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCCHG    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCCRT    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'ITMLOCDLT    '
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'MORCMPCHG    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'MORCMPCRT    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'MORCMPDLT    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S46409
     C                   MOVEL     '010200'      M_W#SLLV                                             S46409
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S46409
     C                                  AND                                                           S46409
     C                             mORPR = 'INVTXN_LQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_LA    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_MQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'MORCMPPB     '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_PQ    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RM    '                                            S46409
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SS    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IU    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RS    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SM    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_IX    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SC    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CR    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CS    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_SP    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_CQ    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_RQ    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_IA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_IP    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_SA    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_IW    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_IS    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_RW    '                                          //S73152
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_RC    '
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_TW    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RP    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                           //D10055
     C                   MOVEL     '010200'      M_W#SLLV                                           //D10055
     C                   WHEN      mTRID = 'INVTXNCRT    '                                          //D10055
     C                                  AND                                                         //D10055
     C                             mORPR = 'INVTXN_RD    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_RI    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL                                             S47026
     C                   MOVEL     '010200'      M_W#SLLV                                             S47026
     C                   WHEN      mTRID = 'INVTXNCRT    '                                            S47026
     C                                  AND                                                           S47026
     C                             mORPR = 'INVTXN_VR    '                                          //D10055
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   WHEN      mTRID = 'INVTXNCRT    '
     C                                  AND
     C                             mORPR = 'INVTXN_VA    '
     C                   MOVEL     '*GE     '    M_W#CLSL
     C                   MOVEL     '010200'      M_W#SLLV
     C                   OTHER
     C                   MOVEL     '*ALL    '    M_W#CLSL
     C                   CLEAR                   M_W#SLLV
     C                   ENDSL
     C                   MOVEL     '*NO    '     M_W#SPIN
     C                   EXSR      M_XVPFC0
     C*
     C*   Retrieve application parms from storage.
     C*
     C                   EXSR      M_RT$$Q0
     C*
     C                   ENDDO
     C*
     C     ZU$$Q0        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_RT$$Q0 - Retrieve default generation data.                    *
     C*******************************************************************
     C     M_RT$$Q0      BEGSR
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0008'      M_W#FNPT
     C                   MOVEL     '*RTVDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   MOVEL     'P$RC1T  '    M_W#RQDN
     C                   MOVEL     '*DATAS  '    M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   MOVEL     P#RC1T        M_P$CHDV
     C                   CLEAR                   M_P$DSDF
     C                   MOVEL     P$RC1TDF      M_P$DSDF
     C                   EXSR      M_XVSFD0
     C                   MOVEL     M_P$CHDV      P#RC1T
     C*
     C     ZR$$Q0        ENDSR
     C*******************************************************************
     C/EJECT
     C*******************************************************************
     C* M_ST$$Q0 - Set default generation data.                         *
     C*******************************************************************
     C     M_ST$$Q0      BEGSR
     C*
     C* Clear application parms.
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0008'      M_W#FNPT
     C                   MOVEL     '*CLRDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   CLEAR                   M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   EXSR      M_XVSFD0
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0008'      M_W#FNPT
     C                   MOVEL     '*SETDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   MOVEL     'P$RC1T  '    M_W#RQDN
     C                   MOVEL     '*DATAS  '    M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   MOVEL     P#RC1T        M_P$CHDV
     C                   MOVEL     P$RC1TDF      M_P$DSDF
     C                   EXSR      M_XVSFD0
     C                   MOVEL     W#SCDF        W#SCDF            8
     C                   MOVEL     W#CLPR        W#CLPR            8
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0008'      M_W#FNPT
     C                   MOVEL     '*SETDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   MOVEL     'P$SCDF  '    M_W#RQDN
     C                   MOVEL     '*CHAR   '    M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   MOVEL     W#SCDF        M_P$CHDV
     C                   EXSR      M_XVSFD0
     C*
     C                   MOVEL     '*YES    '    M_W#PFPR
     C                   MOVEL     '*NO     '    M_W#SHDN
     C                   MOVEL     'XA0008'      M_W#FNPT
     C                   MOVEL     '*SETDTA '    M_W#RQDF
     C                   CLEAR                   M_W#RQDN
     C                   MOVEL     'P$CLPR  '    M_W#RQDN
     C                   MOVEL     '*CHAR   '    M_W#RQDT
     C                   CLEAR                   M_P$CHDV
     C                   MOVEL     W#CLPR        M_P$CHDV
     C                   EXSR      M_XVSFD0
     C*
     C     ZS$$Q0        ENDSR
     C*******************************************************************
     C/EJECT
     C/COPY $PSICOPYLE,XXEMP2
     C/COPY $PSICOPYLE,XXEMS0
     C/COPY $PSICOPYLE,XXRAM1
     C*******************************************************************                             S46409
     C* XVGGT0 - Execute program - PSVGGT0R                             *                           //S65516
     C*******************************************************************                           //S65516
     C     XVGGT0        BEGSR                                                                      //S65516
     C*                                                                                             //S65516
     C                   MOVEL     W#PFPR        W#PFPR            8                                //S65516
     C                   MOVEL     W#SHDN        W#SHDN            8                                //S65516
     C                   MOVEL     W#TSTK        W#TSTK           10                                //S65516
     C*                                                                                             //S65516
     C                   CALL      'PSVGGT0R'                           90                          //S65516
     C                   PARM      W#PFPR        P$PFPR            8                                //S65516
     C                   PARM      W#SHDN        P$SHDN            8                                //S65516
     C                   PARM      W#TSTK        P$TSTK           10                                //S65516
     C                   PARM      *BLANKS       P$TSGP           10                                //S65516
     C                   PARM      *BLANKS       P$MSID            7                                //S65516
     C*                                                                                             //S65516
     C* If there is an error on the call to this program,                                           //S65516
     C*  report it.                                                                                 //S65516
     C*                                                                                             //S65516
     C     *IN90         IFEQ      *ON                                                              //S65516
     C     W#PFPR        ANDEQ     '*YES    '                                                       //S65516
     C     P$MSID        ORNE      *BLANKS                                                          //S65516
     C     W#PFPR        ANDEQ     '*YES    '                                                       //S65516
     C                   MOVEL     'PSX0001'     W#MSID                                             //S65516
     C                   MOVE      *BLANKS       W#MSDT                                             //S65516
     C                   MOVEL     'PSVGGT0R'    W#WA10           10                                //S65516
     C     W#SBTG        CAT       W#MSDT        W#MSDT                                             //S65516
     C     $PPGNM        CAT       W#MSDT        W#MSDT                                             //S65516
     C     W#WA10        CAT       W#MSDT        W#MSDT                                             //S65516
     C                   EXSR      PGMABT                                                           //S65516
     C                   ENDIf                                                                      //S65516
     C*                                                                                             //S65516
     C     ZVGGT0        ENDSR                                                                      //S65516
     C*******************************************************************                           //S65516
     C/EJECT                                                                                        //S65516
      *****************************************************************                             //S65516
     C* XVRTC0 - Execute program - PSVRTC0R                             *                           //S65516
     C*******************************************************************                           //S65516
     C     XVRTC0        Begsr                                                                      //S65516
     C                   CALL      'PSVRTC0R'                           90                          //S65516
     C                   PARM      W#PFPR        P$PFPR            8                                //S65516
     C                   PARM      W#SHDN        P$SHDN            8                                //S65516
     C                   Parm      W#TSTK        P$TSTK           10                                //S65516
     C                   Parm      *blanks       P$APER            8                                //S65516
     C                   Parm      *blanks       P$MSID            7                                //S65516
     C*                                                                                             //S65516
     C* If there is an error on the call to this program,                                           //S65516
     C*  report it.                                                                                 //S65516
     C*                                                                                             //S65516
     C     *IN90         IFEQ      *ON                                                              //S65516
     C     W#PFPR        ANDEQ     '*YES    '                                                       //S65516
     C     P$MSID        ORNE      *BLANKS                                                          //S65516
     C     W#PFPR        ANDEQ     '*YES    '                                                       //S65516
     C                   MOVEL     'PSX0001'     W#MSID                                             //S65516
     C                   MOVE      *BLANKS       W#MSDT                                             //S65516
     C                   MOVEL     'PSVRTC0R'    W#WA10           10                                //S65516
     C     W#SBTG        CAT       W#MSDT        W#MSDT                                             //S65516
     C     $PPGNM        CAT       W#MSDT        W#MSDT                                             //S65516
     C     W#WA10        CAT       W#MSDT        W#MSDT                                             //S65516
     C                   EXSR      PGMABT                                                           //S65516
     C                   ENDIf                                                                      //S65516
     C     ZVRTC0        ENDSR                                                                      //S65516
     C*******************************************************************                           //S65516
     C/EJECT                                                                                        //S65516
     c*******************************************************************                           //S65516
     C* ChkTrnErr: Check for Transaction Errors within group            *                           //S65516
     C*******************************************************************                           //S65516
     c     ChkTrnErr     BEGSR                                                                      //S65516
      *                                                                                             //S65516
      * Assume no errors                                                                            //S65516
     C                   MOVEL     *OFF          W#ERFD            1                                //S65516
      * Get transaction group token                                                                 //S65516
     C                   MOVEL     '*YES     '   W#PFPR                                             //S65516
     C                   MOVEL     '*NO      '   W#SHDN                                             //S65516
     C                   MOVEL     P#TSTK        W#TSTK                                             //S65516
     C                   EXSR      XVGGT0                                                           //S65516
     C                   MOVEL     P$TSGP        W_P$TSGP         10                                //S65516
     C*  Check for transactions errors                                                              //S65516
     C                   Eval      W#TSTK = W_P$TSGP                                                //S65516
     C                   ExSr      XVRTC0                                                           //S65516
     C                   If        P$APER = '*YES'                                                  //S65516
     C                   Eval      w#erfd = *On                                                     //S65516
     C                   EndIf                                                                      //S65516
      *                                                                                             //S65516
     C                   Endsr                                                                      //S65516
     C*******************************************************************                           //S65516
     C/EJECT                                                                                        //S65516
     C*******************************************************************                           //S65516
     C* XVRPS0 - Execute program - PSVRPS0R                             *                             S46409
     C*******************************************************************                             S46409
     C     XVRPS0        BEGSR                                                                        S46409
     C*                                                                                               S46409
     C                   MOVEL     W#PFPR        W#PFPR            8                                  S46409
     C                   MOVEL     W#SHDN        W#SHDN            8                                  S46409
     C                   MOVEL     W#TRID        W#TRID           10                                  S46409
     C*                                                                                               S46409
     C                   CALL      'PSVRPS0R'                           90                            S46409
     C                   PARM      W#PFPR        P$PFPR            8                                  S46409
     C                   PARM      W#SHDN        P$SHDN            8                                  S46409
     C                   PARM      W#TRID        P$TRID           10                                  S46409
     C                   PARM                    P$PSAN            8                                  S46409
     C                   PARM      *BLANKS       P$MSID            7                                  S46409
     C*                                                                                               S46409
     C* If there is an error on the call to this program,                                             S46409
     C*  report it.                                                                                   S46409
     C*                                                                                               S46409
     C     *IN90         IFEQ      *ON                                                                S46409
     C     W#PFPR        ANDEQ     '*YES    '                                                         S46409
     C     P$MSID        ORNE      *BLANKS                                                            S46409
     C     W#PFPR        ANDEQ     '*YES    '                                                         S46409
     C                   MOVEL     'PSX0001'     W#MSID                                               S46409
     C                   MOVE      *BLANKS       W#MSDT                                               S46409
     C                   MOVEL     'PSVRPS0R'    W#WA10           10                                  S46409
     C     W#SBTG        CAT       W#MSDT        W#MSDT                                               S46409
     C     $PPGNM        CAT       W#MSDT        W#MSDT                                               S46409
     C     W#WA10        CAT       W#MSDT        W#MSDT                                               S46409
     C                   EXSR      PGMABT                                                             S46409
     C                   END                                                                          S46409
     C*                                                                                               S46409
     C     ZVRPS0        ENDSR                                                                        S46409
     C*******************************************************************                             S46409
     C/EJECT                                                                                          S46409
     C*---------------------------------------------------------------- *
     C* @@@@ Additional subrotines and compile time array data          *
     C*---------------------------------------------------------------- *
     C* START USER PROTECTED SECTION  - SECA0R 0013.000 / *USR
     C*******************************************************************                           //S65516
     C* XVCHS0 - Execute program - PSVCHS0R                             *                           //S65516
     C*******************************************************************                           //S65516
     C     XVCHS0        BEGSR                                                                      //S65516
     C*                                                                                             //S65516
     C                   CALL      'PSVCHS0R'                           90                          //S65516
     C                   PARM                    P$IN            128                                //S65516
     C                   PARM                    P$LEN             3 0                              //S65516
     C                   PARM      *BLANKS       P$OUT           256                                //S65516
     C*                                                                                             //S65516
     C* If there is an error on the call to this program,                                           //S65516
     C*  report it.                                                                                 //S65516
     C*                                                                                             //S65516
     C     *IN90         IFEQ      *ON                                                              //S65516
     C                   MOVEL     'PSX0001'     W#MSID                                             //S65516
     C                   MOVE      *BLANKS       W#MSDT                                             //S65516
     C                   MOVEL     'PSVCHS0R'    W#WA10           10                                //S65516
     C     W#SBTG        CAT       W#MSDT        W#MSDT                                             //S65516
     C     $PPGNM        CAT       W#MSDT        W#MSDT                                             //S65516
     C     W#WA10        CAT       W#MSDT        W#MSDT                                             //S65516
     C                   EXSR      PGMABT                                                           //S65516
     C                   END                                                                        //S65516
     C*                                                                                             //S65516
     C     ZVCHS0        ENDSR                                                                      //S65516
     C*******************************************************************                           //S65516
     C/EJECT                                                                                        //S65516
**      FINISH USER PROTECTED SECTION - SECA0R 0013.000
LICENSED MATERIALS - PROPERTY OF INFOR VERSION 02/RELEASE 10 PTF 1000907                            //S73152
