/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link DishonorCheque} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class DishonorCheque {

	/** The cheque number. */
	@JsonProperty("ChequeNumber")
	public String chequeNumber;

	/** The currency DESC. */
	@JsonProperty("Currency_DESC")
	public String currency_DESC;

	/** The currency. */
	@JsonProperty("Currency")
	public String currency;

	/** The amount. */
	@JsonProperty("Amount")
	public String amount;

	/** The date dishonored. */
	@JsonProperty("DateDishonored")
	public String dateDishonored;

	/** The reason for dishonor DESC. */
	@JsonProperty("ReasonForDishonor_DESC")
	public String reasonForDishonor_DESC;

	/** The reason for dishonor. */
	@JsonProperty("ReasonForDishonor")
	public String reasonForDishonor;

	/** The reporting date. */
	@JsonProperty("ReportingDate")
	public String reportingDate;

	/**
	 * Instantiates a new dishonor cheque.
	 */
	public DishonorCheque() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "DishonorCheque ["
				+ (chequeNumber != null ? "chequeNumber=" + chequeNumber + ", " : "")
				+ (currency_DESC != null ? "currency_DESC=" + currency_DESC + ", " : "")
				+ (currency != null ? "currency=" + currency + ", " : "")
				+ (amount != null ? "amount=" + amount + ", " : "")
				+ (dateDishonored != null ? "dateDishonored=" + dateDishonored + ", " : "")
				+ (reasonForDishonor_DESC != null
						? "reasonForDishonor_DESC=" + reasonForDishonor_DESC + ", "
						: "")
				+ (reasonForDishonor != null ? "reasonForDishonor=" + reasonForDishonor + ", " : "")
				+ (reportingDate != null ? "reportingDate=" + reportingDate : "") + "]";
	}

}
