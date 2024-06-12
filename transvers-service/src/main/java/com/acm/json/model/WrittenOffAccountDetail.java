/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link WrittenOffAccountDetail} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class WrittenOffAccountDetail {

	/** The s ANCTIO N DATE. */
	@JsonProperty("SANCTION_DATE")
	public String sANCTION_DATE;

	/** The acility nature indicator desc. */
	@JsonProperty("FACILITY_NATURE_INDICATOR_DESC")
	public String fACILITY_NATURE_INDICATOR_DESC;

	/** The acility nature indicator. */
	@JsonProperty("FACILITY_NATURE_INDICATOR")
	public String fACILITY_NATURE_INDICATOR;

	/** The c URRENC Y DESC. */
	@JsonProperty("CURRENCY_DESC")
	public String cURRENCY_DESC;

	/** The c URRENCY. */
	@JsonProperty("CURRENCY")
	public String cURRENCY;

	/** The c URRENC Y CODE. */
	@JsonProperty("CURRENCY_CODE")
	public String cURRENCY_CODE;

	/** The s ANCTIO N AMT. */
	@JsonProperty("SANCTION_AMT")
	public String sANCTION_AMT;

	/** The l OA N TERM. */
	@JsonProperty("LOAN_TERM")
	public String lOAN_TERM;

	/** The a M T O F INSTALMENT. */
	@JsonProperty("AMT_OF_INSTALMENT")
	public String aMT_OF_INSTALMENT;

	/** The r EPAYMEN T TYP E DESC. */
	@JsonProperty("REPAYMENT_TYPE_DESC")
	public String rEPAYMENT_TYPE_DESC;

	/** The r EPAYMEN T TYPE. */
	@JsonProperty("REPAYMENT_TYPE")
	public String rEPAYMENT_TYPE;

	/** The c F AMENDMEN T DATE. */
	@JsonProperty("CF_AMENDMENT_DATE")
	public String cF_AMENDMENT_DATE;

	/** The a C C STATU S DESC. */
	@JsonProperty("ACC_STATUS_DESC")
	public String aCC_STATUS_DESC;

	/** The a C C STATUS. */
	@JsonProperty("ACC_STATUS")
	public String aCC_STATUS;

	/** The c F SETTLEMEN T DATE. */
	@JsonProperty("CF_SETTLEMENT_DATE")
	public String cF_SETTLEMENT_DATE;

	/** The a M T WRITTE N OFF. */
	@JsonProperty("AMT_WRITTEN_OFF")
	public String aMT_WRITTEN_OFF;

	/** The r EASO N AM T WRITTE N OF F DESC. */
	@JsonProperty("REASON_AMT_WRITTEN_OFF_DESC")
	public String rEASON_AMT_WRITTEN_OFF_DESC;

	/** The r EASO N AM T WRITTE N OFF. */
	@JsonProperty("REASON_AMT_WRITTEN_OFF")
	public String rEASON_AMT_WRITTEN_OFF;

	/** The a MOUN T FORGIVEN. */
	@JsonProperty("AMOUNT_FORGIVEN")
	public String aMOUNT_FORGIVEN;

	/** The r EASO N AM T FORGIVE N DESC. */
	@JsonProperty("REASON_AMT_FORGIVEN_DESC")
	public String rEASON_AMT_FORGIVEN_DESC;

	/** The r EASO N AM T FORGIVEN. */
	@JsonProperty("REASON_AMT_FORGIVEN")
	public String rEASON_AMT_FORGIVEN;

	/** The d AT E AC C CLOSE. */
	@JsonProperty("DATE_ACC_CLOSE")
	public String dATE_ACC_CLOSE;

	/** The s PECIA L COMMENT S DESC. */
	@JsonProperty("SPECIAL_COMMENTS_DESC")
	public String sPECIAL_COMMENTS_DESC;

	/** The s PECIA L COMMENTS. */
	@JsonProperty("SPECIAL_COMMENTS")
	public String sPECIAL_COMMENTS;

	/** The irst disburse date. */
	@JsonProperty("FIRST_DISBURSE_DATE")
	public String fIRST_DISBURSE_DATE;

	/** The s ECURIT Y INDICATO R DESC. */
	@JsonProperty("SECURITY_INDICATOR_DESC")
	public String sECURITY_INDICATOR_DESC;

	/** The s ECURIT Y INDICATOR. */
	@JsonProperty("SECURITY_INDICATOR")
	public String sECURITY_INDICATOR;

	/** The l IABILIT Y INDICATO R DESC. */
	@JsonProperty("LIABILITY_INDICATOR_DESC")
	public String lIABILITY_INDICATOR_DESC;

	/** The l IABILIT Y INDICATOR. */
	@JsonProperty("LIABILITY_INDICATOR")
	public String lIABILITY_INDICATOR;

	/** The n O O F INSTALMENTS. */
	@JsonProperty("NO_OF_INSTALMENTS")
	public String nO_OF_INSTALMENTS;

	/** The l OA N TYP E DESC. */
	@JsonProperty("LOAN_TYPE_DESC")
	public String lOAN_TYPE_DESC;

	/** The l OA N TYPE. */
	@JsonProperty("LOAN_TYPE")
	public String lOAN_TYPE;

	/** The r EPORTE D DATE. */
	@JsonProperty("REPORTED_DATE")
	public String rEPORTED_DATE;

	/** The d PD history 18 months. */
	@JsonProperty("DPDHistory18Months")
	public List<DPDHistory18Months> dPDHistory18Months;

	/** The legal actions. */
	@JsonProperty("LegalActions")
	public List<Object> legalActions;

	/** The dishonor cheque. */
	@JsonProperty("DishonorCheque")
	public List<Object> dishonorCheque;

	/** The dispute ref. */
	@JsonProperty("DisputeRef")
	public List<Object> disputeRef;

	/** The d AY S DUE. */
	@JsonProperty("DAYS_DUE")
	public String dAYS_DUE;

	/** The l AS T AMOUN T PAID. */
	@JsonProperty("LAST_AMOUNT_PAID")
	public String lAST_AMOUNT_PAID;

	/** The d AT E LATES T PA Y RECEIVED. */
	@JsonProperty("DATE_LATEST_PAY_RECEIVED")
	public String dATE_LATEST_PAY_RECEIVED;

	/** The a SSE T CLASSIFICATIO N DESC. */
	@JsonProperty("ASSET_CLASSIFICATION_DESC")
	public String aSSET_CLASSIFICATION_DESC;

	/** The a SSE T CLASSIFICATION. */
	@JsonProperty("ASSET_CLASSIFICATION")
	public String aSSET_CLASSIFICATION;

	/** The h IGHES T CREDIT. */
	@JsonProperty("HIGHEST_CREDIT")
	public String hIGHEST_CREDIT;

	/** The c URREN T BA L SV. */
	@JsonProperty("CURRENT_BAL_SV")
	public String cURRENT_BAL_SV;

	/** The o VE R DU E AMT. */
	@JsonProperty("OVER_DUE_AMT")
	public String oVER_DUE_AMT;

	/** The a M T O F UNEARNE D INT. */
	@JsonProperty("AMT_OF_UNEARNED_INT")
	public String aMT_OF_UNEARNED_INT;

	/** The n O O F INSTALLMENT S OVERDUE. */
	@JsonProperty("NO_OF_INSTALLMENTS_OVERDUE")
	public String nO_OF_INSTALLMENTS_OVERDUE;

	/** The d AT A PRD R ID. */
	@JsonProperty("DATA_PRDR_ID")
	public String dATA_PRDR_ID;

	/** The m A X NU M DAY S DUE. */
	@JsonProperty("MAX_NUM_DAYS_DUE")
	public String mAX_NUM_DAYS_DUE;

	/** The d AT E REPORTE D NDPD. */
	@JsonProperty("DATE_REPORTED_NDPD")
	public String dATE_REPORTED_NDPD;

	/** The s M E CB E INITIATIV E STATU S S V DESC. */
	@JsonProperty("SME_CBE_INITIATIVE_STATUS_SV_DESC")
	public String sME_CBE_INITIATIVE_STATUS_SV_DESC;

	/** The s M E CB E INITIATIV E STATU S SV. */
	@JsonProperty("SME_CBE_INITIATIVE_STATUS_SV")
	public String sME_CBE_INITIATIVE_STATUS_SV;

	/**
	 * Instantiates a new written off account detail.
	 */
	public WrittenOffAccountDetail() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "WrittenOffAccountDetail ["
				+ (sANCTION_DATE != null ? "sANCTION_DATE=" + sANCTION_DATE + ", " : "")
				+ (fACILITY_NATURE_INDICATOR_DESC != null
						? "fACILITY_NATURE_INDICATOR_DESC=" + fACILITY_NATURE_INDICATOR_DESC + ", "
						: "")
				+ (fACILITY_NATURE_INDICATOR != null
						? "fACILITY_NATURE_INDICATOR=" + fACILITY_NATURE_INDICATOR + ", "
						: "")
				+ (cURRENCY_DESC != null ? "cURRENCY_DESC=" + cURRENCY_DESC + ", " : "")
				+ (cURRENCY != null ? "cURRENCY=" + cURRENCY + ", " : "")
				+ (cURRENCY_CODE != null ? "cURRENCY_CODE=" + cURRENCY_CODE + ", " : "")
				+ (sANCTION_AMT != null ? "sANCTION_AMT=" + sANCTION_AMT + ", " : "")
				+ (lOAN_TERM != null ? "lOAN_TERM=" + lOAN_TERM + ", " : "")
				+ (aMT_OF_INSTALMENT != null ? "aMT_OF_INSTALMENT=" + aMT_OF_INSTALMENT + ", " : "")
				+ (rEPAYMENT_TYPE_DESC != null ? "rEPAYMENT_TYPE_DESC=" + rEPAYMENT_TYPE_DESC + ", "
						: "")
				+ (rEPAYMENT_TYPE != null ? "rEPAYMENT_TYPE=" + rEPAYMENT_TYPE + ", " : "")
				+ (cF_AMENDMENT_DATE != null ? "cF_AMENDMENT_DATE=" + cF_AMENDMENT_DATE + ", " : "")
				+ (aCC_STATUS_DESC != null ? "aCC_STATUS_DESC=" + aCC_STATUS_DESC + ", " : "")
				+ (aCC_STATUS != null ? "aCC_STATUS=" + aCC_STATUS + ", " : "")
				+ (cF_SETTLEMENT_DATE != null ? "cF_SETTLEMENT_DATE=" + cF_SETTLEMENT_DATE + ", "
						: "")
				+ (aMT_WRITTEN_OFF != null ? "aMT_WRITTEN_OFF=" + aMT_WRITTEN_OFF + ", " : "")
				+ (rEASON_AMT_WRITTEN_OFF_DESC != null
						? "rEASON_AMT_WRITTEN_OFF_DESC=" + rEASON_AMT_WRITTEN_OFF_DESC + ", "
						: "")
				+ (rEASON_AMT_WRITTEN_OFF != null
						? "rEASON_AMT_WRITTEN_OFF=" + rEASON_AMT_WRITTEN_OFF + ", "
						: "")
				+ (aMOUNT_FORGIVEN != null ? "aMOUNT_FORGIVEN=" + aMOUNT_FORGIVEN + ", " : "")
				+ (rEASON_AMT_FORGIVEN_DESC != null
						? "rEASON_AMT_FORGIVEN_DESC=" + rEASON_AMT_FORGIVEN_DESC + ", "
						: "")
				+ (rEASON_AMT_FORGIVEN != null ? "rEASON_AMT_FORGIVEN=" + rEASON_AMT_FORGIVEN + ", "
						: "")
				+ (dATE_ACC_CLOSE != null ? "dATE_ACC_CLOSE=" + dATE_ACC_CLOSE + ", " : "")
				+ (sPECIAL_COMMENTS_DESC != null
						? "sPECIAL_COMMENTS_DESC=" + sPECIAL_COMMENTS_DESC + ", "
						: "")
				+ (sPECIAL_COMMENTS != null ? "sPECIAL_COMMENTS=" + sPECIAL_COMMENTS + ", " : "")
				+ (fIRST_DISBURSE_DATE != null ? "fIRST_DISBURSE_DATE=" + fIRST_DISBURSE_DATE + ", "
						: "")
				+ (sECURITY_INDICATOR_DESC != null
						? "sECURITY_INDICATOR_DESC=" + sECURITY_INDICATOR_DESC + ", "
						: "")
				+ (sECURITY_INDICATOR != null ? "sECURITY_INDICATOR=" + sECURITY_INDICATOR + ", "
						: "")
				+ (lIABILITY_INDICATOR_DESC != null
						? "lIABILITY_INDICATOR_DESC=" + lIABILITY_INDICATOR_DESC + ", "
						: "")
				+ (lIABILITY_INDICATOR != null ? "lIABILITY_INDICATOR=" + lIABILITY_INDICATOR + ", "
						: "")
				+ (nO_OF_INSTALMENTS != null ? "nO_OF_INSTALMENTS=" + nO_OF_INSTALMENTS + ", " : "")
				+ (lOAN_TYPE_DESC != null ? "lOAN_TYPE_DESC=" + lOAN_TYPE_DESC + ", " : "")
				+ (lOAN_TYPE != null ? "lOAN_TYPE=" + lOAN_TYPE + ", " : "")
				+ (rEPORTED_DATE != null ? "rEPORTED_DATE=" + rEPORTED_DATE + ", " : "")
				+ (dPDHistory18Months != null ? "dPDHistory18Months=" + dPDHistory18Months + ", "
						: "")
				+ (legalActions != null ? "legalActions=" + legalActions + ", " : "")
				+ (dishonorCheque != null ? "dishonorCheque=" + dishonorCheque + ", " : "")
				+ (disputeRef != null ? "disputeRef=" + disputeRef + ", " : "")
				+ (dAYS_DUE != null ? "dAYS_DUE=" + dAYS_DUE + ", " : "")
				+ (lAST_AMOUNT_PAID != null ? "lAST_AMOUNT_PAID=" + lAST_AMOUNT_PAID + ", " : "")
				+ (dATE_LATEST_PAY_RECEIVED != null
						? "dATE_LATEST_PAY_RECEIVED=" + dATE_LATEST_PAY_RECEIVED + ", "
						: "")
				+ (aSSET_CLASSIFICATION_DESC != null
						? "aSSET_CLASSIFICATION_DESC=" + aSSET_CLASSIFICATION_DESC + ", "
						: "")
				+ (aSSET_CLASSIFICATION != null
						? "aSSET_CLASSIFICATION=" + aSSET_CLASSIFICATION + ", "
						: "")
				+ (hIGHEST_CREDIT != null ? "hIGHEST_CREDIT=" + hIGHEST_CREDIT + ", " : "")
				+ (cURRENT_BAL_SV != null ? "cURRENT_BAL_SV=" + cURRENT_BAL_SV + ", " : "")
				+ (oVER_DUE_AMT != null ? "oVER_DUE_AMT=" + oVER_DUE_AMT + ", " : "")
				+ (aMT_OF_UNEARNED_INT != null ? "aMT_OF_UNEARNED_INT=" + aMT_OF_UNEARNED_INT + ", "
						: "")
				+ (nO_OF_INSTALLMENTS_OVERDUE != null
						? "nO_OF_INSTALLMENTS_OVERDUE=" + nO_OF_INSTALLMENTS_OVERDUE + ", "
						: "")
				+ (dATA_PRDR_ID != null ? "dATA_PRDR_ID=" + dATA_PRDR_ID + ", " : "")
				+ (mAX_NUM_DAYS_DUE != null ? "mAX_NUM_DAYS_DUE=" + mAX_NUM_DAYS_DUE + ", " : "")
				+ (dATE_REPORTED_NDPD != null ? "dATE_REPORTED_NDPD=" + dATE_REPORTED_NDPD + ", "
						: "")
				+ (sME_CBE_INITIATIVE_STATUS_SV_DESC != null
						? "sME_CBE_INITIATIVE_STATUS_SV_DESC=" + sME_CBE_INITIATIVE_STATUS_SV_DESC
								+ ", "
						: "")
				+ (sME_CBE_INITIATIVE_STATUS_SV != null
						? "sME_CBE_INITIATIVE_STATUS_SV=" + sME_CBE_INITIATIVE_STATUS_SV
						: "")
				+ "]";
	}

}
