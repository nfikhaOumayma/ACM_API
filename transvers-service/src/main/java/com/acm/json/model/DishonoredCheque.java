/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link DishonoredCheque}.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class DishonoredCheque {

	/** The currency. */
	@JsonProperty("Currency")
	public String currency;

	/** The no of cheques. */
	@JsonProperty("NoOfCheques")
	public String noOfCheques;

	/** The total disohonor amt. */
	@JsonProperty("TotalDisohonorAmt")
	public String totalDisohonorAmt;

	/** The last cheque dis honored date. */
	@JsonProperty("LastChequeDisHonoredDate")
	public String lastChequeDisHonoredDate;

	/**
	 * Instantiates a new dishonored cheque.
	 */
	public DishonoredCheque() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "DishonoredCheque [" + (currency != null ? "currency=" + currency + ", " : "")
				+ (noOfCheques != null ? "noOfCheques=" + noOfCheques + ", " : "")
				+ (totalDisohonorAmt != null ? "totalDisohonorAmt=" + totalDisohonorAmt + ", " : "")
				+ (lastChequeDisHonoredDate != null
						? "lastChequeDisHonoredDate=" + lastChequeDisHonoredDate
						: "")
				+ "]";
	}

}
