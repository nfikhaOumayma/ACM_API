/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link UnearnedInterest}.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class UnearnedInterest {

	/** The currency. */
	@JsonProperty("Currency")
	public String currency;

	/** The no of accounts. */
	@JsonProperty("NoOfAccounts")
	public String noOfAccounts;

	/** The total un earned interest amount. */
	@JsonProperty("TotalUnEarnedInterestAmount")
	public String totalUnEarnedInterestAmount;

	/**
	 * Instantiates a new unearned interest.
	 */
	public UnearnedInterest() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "UnearnedInterest [" + (currency != null ? "currency=" + currency + ", " : "")
				+ (noOfAccounts != null ? "noOfAccounts=" + noOfAccounts + ", " : "")
				+ (totalUnEarnedInterestAmount != null
						? "totalUnEarnedInterestAmount=" + totalUnEarnedInterestAmount
						: "")
				+ "]";
	}

}
