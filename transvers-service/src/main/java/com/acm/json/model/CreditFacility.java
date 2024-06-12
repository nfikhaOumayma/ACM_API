/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link CreditFacility}.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class CreditFacility {

	/** The currency. */
	@JsonProperty("Currency")
	public String currency;

	/** The no of accounts. */
	@JsonProperty("NoOfAccounts")
	public String noOfAccounts;

	/** The total approval amt. */
	@JsonProperty("TotalApprovalAmt")
	public String totalApprovalAmt;

	/** The total monthly installment amt. */
	@JsonProperty("TotalMonthlyInstallmentAmt")
	public String totalMonthlyInstallmentAmt;

	/** The total balance amount. */
	@JsonProperty("TotalBalanceAmount")
	public String totalBalanceAmount;

	/**
	 * Instantiates a new credit facility.
	 */
	public CreditFacility() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CreditFacility [" + (currency != null ? "currency=" + currency + ", " : "")
				+ (noOfAccounts != null ? "noOfAccounts=" + noOfAccounts + ", " : "")
				+ (totalApprovalAmt != null ? "totalApprovalAmt=" + totalApprovalAmt + ", " : "")
				+ (totalMonthlyInstallmentAmt != null
						? "totalMonthlyInstallmentAmt=" + totalMonthlyInstallmentAmt + ", "
						: "")
				+ (totalBalanceAmount != null ? "totalBalanceAmount=" + totalBalanceAmount : "")
				+ "]";
	}

}
