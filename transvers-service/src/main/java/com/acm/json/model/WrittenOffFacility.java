/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link WrittenOffFacility}.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class WrittenOffFacility {

	/** The currency. */
	@JsonProperty("Currency")
	public String currency;

	/** The no of written off accounts. */
	@JsonProperty("NoOfWrittenOffAccounts")
	public String noOfWrittenOffAccounts;

	/** The total approval amt. */
	@JsonProperty("TotalApprovalAmt")
	public String totalApprovalAmt;

	/** The total written off amt. */
	@JsonProperty("TotalWrittenOffAmt")
	public String totalWrittenOffAmt;

	/**
	 * Instantiates a new written off facility.
	 */
	public WrittenOffFacility() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "WrittenOffFacility [" + (currency != null ? "currency=" + currency + ", " : "")
				+ (noOfWrittenOffAccounts != null
						? "noOfWrittenOffAccounts=" + noOfWrittenOffAccounts + ", "
						: "")
				+ (totalApprovalAmt != null ? "totalApprovalAmt=" + totalApprovalAmt + ", " : "")
				+ (totalWrittenOffAmt != null ? "totalWrittenOffAmt=" + totalWrittenOffAmt : "")
				+ "]";
	}

}
