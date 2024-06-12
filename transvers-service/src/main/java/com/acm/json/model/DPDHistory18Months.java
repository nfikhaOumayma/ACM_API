/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link DPDHistory18Months} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class DPDHistory18Months {

	/** The m A X NU M DAY S DUE. */
	@JsonProperty("MAX_NUM_DAYS_DUE")
	public String mAX_NUM_DAYS_DUE;

	/** The o VE R DU E AMT. */
	@JsonProperty("OVER_DUE_AMT")
	public String oVER_DUE_AMT;

	/** The c URREN T BAL. */
	@JsonProperty("CURRENT_BAL")
	public String cURRENT_BAL;

	/** The p ERIO D ID. */
	@JsonProperty("PERIOD_ID")
	public String pERIOD_ID;

	/**
	 * Instantiates a new DPD history 18 months.
	 */
	public DPDHistory18Months() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "DPDHistory18Months ["
				+ (mAX_NUM_DAYS_DUE != null ? "mAX_NUM_DAYS_DUE=" + mAX_NUM_DAYS_DUE + ", " : "")
				+ (oVER_DUE_AMT != null ? "oVER_DUE_AMT=" + oVER_DUE_AMT + ", " : "")
				+ (cURRENT_BAL != null ? "cURRENT_BAL=" + cURRENT_BAL + ", " : "")
				+ (pERIOD_ID != null ? "pERIOD_ID=" + pERIOD_ID : "") + "]";
	}

}
