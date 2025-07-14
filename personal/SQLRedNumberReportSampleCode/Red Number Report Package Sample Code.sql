SELECT
ITM_CODE,
Description,
LCTN_NM,
BKNG_STRT_DT,
BKNG_END_DT,
min(Ad_Short_Flag) as Ad_First_Out_Qty,
min(Ad_First_Out_Dt) as Ad_First_Out_Dt,
Action_Item_Appoint,
Action_Item_Rush,
BOH,
OSS,
BKNG_TYPE,
Booking_Memo,
Shipper_Flag,
min(CSTMR_ITM_ID) as Cust_ID,
BYR_NUM,
LCTN_NUM,
VNDR_NUM,
VNDR_NM,
min(Appointment_Date) as OO_Appt_Dt,
--Cumulative_BOO,
--OO.BOO,
--Lead_Days,
--TTL_Cumulative_Forecast,
--AD_Cumulative_Forecast,
--FC.Total_Forecast,
--FC.Ad_Forecast,
--BKNG_ORDR_QTY,
--AD.BLD_QTY,
--AD.RMNG_QTY,
RO_TRIGGER
--TMLN_DT,
--RANK() OVER (ORDER BY a.) AS Rank
--Late_PO_Qty,
--min(TTL_First_Out_Dt),
--min(TTL_Short_Flag) as TTL_First_Out_Qty

FROM (


SELECT
B.ITM_NUM as ITM_CODE,
--max(E.CSTMR_ITM_NBR),
I.CSTMR_ITM_ID,
F.SNGL_SYS_ITM_DESC as Description, 
B.DVSN_BYR_NUM as BYR_NUM,
G.LCTN_NUM,
G.LCTN_NM,
H.VNDR_NUM,
H.VNDR_NM,
AD.BYR_CMNTS as Booking_Memo,
F.SHPR_IND as Shipper_Flag,
OO.Appointment_Date,
OH.BOH,
OH.OSS,
--OO.DLVRY_DT,
--Cumulative_BOO,
--OO.BOO,
OH.Lead_Dys_Cnt as Lead_Days,
AD.BKNG_STRT_DT,
AD.BKNG_END_DT,
FC.TTL_Cumulative_Forecast,
FC.AD_Cumulative_Forecast,
--FC.Total_Forecast,
--FC.Ad_Forecast,
AD.BKNG_ORDR_QTY,
--AD.BLD_QTY,
--AD.RMNG_QTY,
RO.RO_TRIGGER,
FC.TMLN_DT,
AD.BKNG_TYPE,
--OO.DLVRY_DT,
--OO.PO_DLVRY_DT,
--AD.BYR_CMNTS,
--AD.Ad_Length,
OO.Late_PO_Qty,
Sum(OO.Appt_OO+OO.Unappt_OO) as BOO,
sum(BOO) over (Partition by b.itm_id,fc.chn_prty_id Order by FC.TMLN_DT Asc Rows Unbounded preceding) as Cumulative_BOO,

--rush PO or within leadtime PO, less than lead time days--
Case when FC. AD_Cumulative_Forecast> OH.BOH + OH.OSS+ zeroifnull(Cumulative_BOO) then FC. AD_Cumulative_Forecast-(OH.BOH + OH.OSS+ zeroifnull(Cumulative_BOO))  Else null end as Ad_Short_Flag,
--Case when FC. TTL_Cumulative_Forecast> OH.BOH + OH.OSS+ zeroifnull(Cumulative_BOO) then FC.TTL_Cumulative_Forecast- (OH.BOH + OH.OSS+ zeroifnull(Cumulative_BOO))  Else null end as TTL_Short_Flag,
--Case when TTL_Short_Flag>0  then min(FC.TMLN_DT) else null end as TTL_First_Out_Dt,
Case when Ad_Short_Flag>0 then min(FC.TMLN_DT) else AD.BKNG_STRT_DT end as Ad_First_Out_Dt,
Case when Ad_Short_Flag>0 and OO.Unappt_OO>0 then 'Appoint On Orders' end as Action_Item_Appoint,
/*Case when Ad_Short_Flag>0 and OO.Rush_Needed=1 then 'Rush PO' end as Action_Item_Rush*/
Case When Ad_Short_Flag>0 and  ((abs(OO.DLVRY_DT-current_date) > OO.Lead_Dys_Cnt)) then 'Rush PO' end as Action_Item_Rush
--Case when Ad_First_Out_Dt then min(Ad_Short_Flag) end as Ad_First_Out_Qty

		  		  	
FROM
--EDW_VM.CHN_ADS_VB as A
EDW_VM.ITM_VB as B
--inner join EDW_VM.CHN_VB as C on A.CHN_PRTY_ID=C.CHN_PRTY_ID
inner join EDW_VM.SNGL_SYS_ITM_VB as F on B.SNGL_SYS_ITM_ID_SEQ=F.SNGL_SYS_ITM_ID_SEQ
inner join EDW_VM.LCTN_VB as G on B.ITM_LCTN_NUM=G.LCTN_NUM
inner join EDW_VM.VNDR_VB as H on B.VNDR_PRTY_ID=H.VNDR_PRTY_ID
inner join EDW_VM.CSTMR_CS_ITM_XREF_VB as I on B.ITM_ID=I.ITM_ID
inner join EDW_VM.CSTMR_ITM_VB as J on I.CSTMR_ITM_ID=J.CSTMR_ITM_ID

--recommended order--
inner join (

	SELECT
	B.ITM_ID,
	max(A.RO_TRIGGER_QTY) as RO_TRIGGER
	
	FROM
	EDW_VM.RCMND_ORDR_LN_VB as A
	inner join EDW_VM.ITM_VB as B on A.ITM_ID=B.ITM_ID
	inner join EDW_VM.RCMND_ORDR_VB as C on A.RCMND_ORDR_ID=C.RCMND_ORDR_ID	
	
	WHERE
	RO_CRT_DT=current_date
	
	GROUP BY
	B.ITM_ID
	--A.RO_TRIGGER_QTY

) as RO on B.ITM_ID=RO.ITM_ID

	--On Hand--
	
	inner join (
	
		SELECT
		A.ITM_ID,
		A.BOH1 as BOH, 
		Case When C.LDTM_UNT_CD='B' then C.VNDR_LD_DYS_CNT*1.4 else C.VNDR_LD_DYS_CNT end as Lead_Dys_Cnt,	
		SUM(Case When D.ITM_DSTNTN_LCTN_ID<>B.ITM_LCTN_NUM then A.BOH1 else 0 end) as OSS 
		
		FROM
		EDW_VM.ITM_VB as B
		inner join EDW_VM.LOT_LN_VB as D on B.ITM_ID=D.ITM_ID
		left join (SELECT a.itm_id 

, CASE WHEN b.itm_lctn_num IN (61) THEN oms_boh.oms_Boh_qty ELSE SUM(trn_cases_blnc_qty + prm_cases_blnc_qty + invstmnt_cases_blnc_qty) END AS BOH1

FROM lot_ln_vb a

INNER JOIN itm_vb b ON a.itm_id = b.itm_id

LEFT JOIN (

SELECT itm_id

,oms_boh_qty 

FROM intgr_vm.itm_vb a   

INNER JOIN intgr_vm.sngl_sys_itm_vb b ON a.sngl_sys_itm_id_seq = b.sngl_sys_itm_id_seq             

INNER JOIN intgr_vm.oms_boh_inv_vb c ON CAST(b.sngl_sys_itm_id AS INT) = CAST(c.global_itm_cd AS INT)    

WHERE sub_inv_desc = 'AVAILABLE'        

AND a.itm_lctn_num in (24)                

AND c.cs_whs_nb in (24)

) oms_boh ON a.itm_id = oms_boh.itm_id

WHERE a.itm_dstntn_lctn_id NOT IN (10,12)

GROUP BY a.itm_id, oms_boh.oms_Boh_qty, b.itm_lctn_num) as A on B.ITM_ID=A.ITM_ID

		/*EDW_VM.LOT_LN_VB as A*/
		inner join INTGR_VM.VNDR_LD_TIM_VB as C on B.VNDR_PRTY_ID=C.VNDR_PRTY_ID	
		--inner join EDW_VM.DLY_CHN_ITM_MVMNT_SMRY_VB as D on B.ITM_ID=D.ITM_ID
		
		WHERE 
		B.END_EFCTV_DT is Null
		AND D.itm_dstntn_lctn_id NOT IN (10,12)
		--AND D.CLNDR_DT between current_date and current_date + Lead_Dys_Cnt +1
		
		GROUP BY
		A.ITM_ID,
		Lead_Dys_Cnt,
		A.BOH1	
	
	) as OH on B.ITM_ID=OH.ITM_ID
	
--Forecasts--
inner join (

	SELECT
	--B.ITM_NUM,
	A.ITM_ID,
	A.TMLN_DT,
	--VNDR_LD_DYS_CNT
	Case When C.LDTM_UNT_CD='B' then C.VNDR_LD_DYS_CNT*1.4 else C.VNDR_LD_DYS_CNT end as Lead_Dys_Cnt,	 
    sum(A.TTL_FRCST_QTY) as Total_Forecast,
	sum(Total_Forecast) over (Partition by A.itm_id Order by A.Tmln_dt Asc Rows Unbounded preceding) as TTL_Cumulative_Forecast,
	sum(A.AD_FRCST_QTY) as Ad_Forecast,
	sum(Ad_Forecast) over (Partition by A.itm_id Order by A.Tmln_dt Asc Rows Unbounded preceding) as AD_Cumulative_Forecast,
	A.CHN_PRTY_ID
		
	FROM
	INTGR_VM.FACT_ITM_TMLN_PRJCTN_BV as A
	inner join INTGR_VM.ITM_VB as B on  A.ITM_ID= B.ITM_ID
	inner join INTGR_VM.VNDR_LD_TIM_VB as C on  B.VNDR_PRTY_ID= C.VNDR_PRTY_ID
		
	WHERE
	B.END_EFCTV_DT is Null
	AND A.TMLN_DT between current_date and current_date + Lead_Dys_Cnt +1
	--AND ITM_NUM= 113712
			
	GROUP BY
	--B.ITM_NUM,
	A.ITM_ID,
	--Forecast_within_LT
	Lead_Dys_Cnt,
	A.CHN_PRTY_ID,
	A.TMLN_DT
	
/*	HAVING
	Ad_Forecast>0
*/	
) as FC on B.ITM_ID=FC.ITM_ID and FC.CHN_PRTY_ID=J.CHN_PRTY_ID


	--On Order--

		left join (
	
		SELECT
		A.ITM_ID,
		Late_PO_Dt as DLVRY_DT,
		--sum(Appt_OO+Unappt_OO) as BOO, --appointed BOO--
		--BYN_DIVERT_IND, --There is divert included--
		Sum(Case When APPTMNT_DTTM is not null then A.ORDRD_QTY else 0 end) as Appt_OO, --appointed on orders--
		Sum(Case When APPTMNT_DTTM is null and BYN_DIVERT_IND<>'D' then A.ORDRD_QTY else 0 end) as Unappt_OO, --unappointed on orders- and  excluding divert
		Cast(APPTMNT_DTTM as Date) as Appointment_Date,
		Case When C.LDTM_UNT_CD='B' then C.VNDR_LD_DYS_CNT*1.4 else C.VNDR_LD_DYS_CNT end as Lead_Dys_Cnt,
		sum(Case When D.PO_DLVRY_DT<current_date then A.ORDRD_QTY else 0 end) as Late_PO_Qty,
		(Case when PO_DLVRY_DT<current_date then current_date else PO_DLVRY_DT end) +1 as Late_PO_Dt
			
		FROM
		EDW_VM.VNDR_PO_LN_VB as A
		inner join EDW_VM.VNDR_PO_VB as D on A.VNDR_PO_ID=D.VNDR_PO_ID
		inner join EDW_VM.ITM_VB as B on A.ITM_ID=B.ITM_ID
		inner join INTGR_VM.VNDR_LD_TIM_VB as C on B.VNDR_PRTY_ID=C.VNDR_PRTY_ID
		--inner join EDW_VM.DLY_CHN_ITM_MVMNT_SMRY_VB as D on B.ITM_ID=D.ITM_ID
		
		WHERE
		B.END_EFCTV_DT is Null
		AND D.VNDR_PO_NUM/1000 not in (621,935) --Taking out outside storage out of 
		--AND D.CLNDR_DT between current_date and current_date + Lead_Dys_Cnt +1
		AND D.PO_STS_CD= 'O'
		--AND D.PO_DLVRY_DT between current_date and current_date + Lead_Dys_Cnt +1
		--AND ITM_NUM= 113712 
								
		GROUP BY
		A.ITM_ID,
		DLVRY_DT,
		Lead_Dys_Cnt,
		D.PO_DLVRY_DT,
		A.ORDRD_QTY,		
		BYN_DIVERT_IND,
		Appointment_Date
		--Late_PO_Qty,
		--Late_PO_Dt		
		--Appointment_Date
	
	) as OO on B.ITM_ID=OO.ITM_ID and FC.TMLN_DT=OO.DLVRY_DT 
	
--Ad information--

	inner join (
	
		SELECT
		A.ITM_ID,		
		A.BKNG_STRT_DT,
		A.BKNG_END_DT,
		A.BKNG_ORDR_QTY,
		A.BLD_QTY,
		A.RMNG_QTY,
		A.BYR_CMNTS,
		A.BKNG_END_DT-A.BKNG_STRT_DT as Ad_Length,
		A.CHN_PRTY_ID,
		A.RECAP_IND||A.AD_TYP_CD||A.AD_TYP2_CD as BKNG_TYPE,
		
		Case When C.LDTM_UNT_CD='B' then C.VNDR_LD_DYS_CNT*1.4 else C.VNDR_LD_DYS_CNT end as Lead_Dys_Cnt	 
			
		FROM
		EDW_VM.CHN_ADS_VB as A
		inner join EDW_VM.ITM_VB as B on A.ITM_ID=B.ITM_ID
		inner join INTGR_VM.VNDR_LD_TIM_VB as C on B.VNDR_PRTY_ID=C.VNDR_PRTY_ID 
		
				
		WHERE
		B.END_EFCTV_DT is Null
		AND A.BKNG_STRT_DT<A.BKNG_END_DT
		AND A.BKNG_STRT_DT between current_date and current_date + Lead_Dys_Cnt +1
		--AND A.BKNG_END_DT<current_date + Lead_Dys_Cnt +1						
		/*AND (A.BKNG_END_DT between current_date-2*7 and current_date-1 
OR A.BKNG_STRT_DT between current_date and current_date+2*7)*/
 		
		GROUP BY
		A.ITM_ID,
		A.BKNG_STRT_DT,
		A.BKNG_END_DT,
		A.BKNG_ORDR_QTY,
		A.BLD_QTY,
		A.RMNG_QTY,
		A.BYR_CMNTS,
		Lead_Dys_Cnt	,
		A.AD_TYP_CD,
		A.CHN_PRTY_ID,
		BKNG_TYPE 
					
	) as AD on B.ITM_ID=AD.ITM_ID and Ad.CHN_PRTY_ID=FC.CHN_PRTY_ID

/*WHERE
--G.LCTN_NUM in (61,95)
G.LCTN_NUM in (24)*/


GROUP BY
B.ITM_NUM,
--max(E.CSTMR_ITM_NBR),
I.CSTMR_ITM_ID,
F.SNGL_SYS_ITM_DESC, 
B.DVSN_BYR_NUM,
G.LCTN_NUM,
G.LCTN_NM,
H.VNDR_NUM,
H.VNDR_NM,
AD.BYR_CMNTS,
F.SHPR_IND,
OO.Appointment_Date,
OH.BOH,
OH.OSS,
--Cumulative_BOO,
--OO.BOO,
OH.Lead_Dys_Cnt,
AD.BKNG_STRT_DT,
AD.BKNG_END_DT,
FC.TTL_Cumulative_Forecast,
FC.AD_Cumulative_Forecast,
--FC.Total_Forecast,
--FC.Ad_Forecast,
AD.BKNG_ORDR_QTY,
--AD.BLD_QTY,
--AD.RMNG_QTY,
RO.RO_TRIGGER,
--FC.TMLN_DT,
--AD.BYR_CMNTS,
--AD.Ad_Length,
--Ad_Short_Flag,
--TTL_Short_Flag,
Appt_OO,
Unappt_OO,
 b.itm_id,
 FC.TMLN_DT,
--OO.DLVRY_DT,
fc.chn_prty_id,
AD.BKNG_TYPE,
--OO.Rush_Needed,
OO.Late_PO_Qty,
OO.DLVRY_DT,
OO.Lead_Dys_Cnt
/*OO.DLVRY_DT*/
--OO.PO_DLVRY_DT

) as A

GROUP BY
ITM_CODE,
--CSTMR_ITM_ID,
Description, 
BYR_NUM,
LCTN_NUM,
LCTN_NM,
VNDR_NUM,
VNDR_NM,
Booking_Memo,
Shipper_Flag,
OSS,
Action_Item_Appoint,
Action_Item_Rush,
--Appointment_Date,
BOH,
--OH.OSS,
--Cumulative_BOO,
--OO.BOO,
BKNG_TYPE,
--Lead_Days,
BKNG_STRT_DT,
BKNG_END_DT,
--TTL_Cumulative_Forecast,
--AD_Cumulative_Forecast,
--FC.Total_Forecast,
--FC.Ad_Forecast,
--BKNG_ORDR_QTY,
--AD.BLD_QTY,
--AD.RMNG_QTY
RO_TRIGGER
--Late_PO_Qty
--TMLN_DT,
--RANK() OVER (ORDER BY a.) AS Rank
--Ad_First_Out_Dt,
--TTL_First_Out_Dt
