/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link PerfOverdueGraph}.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class PerfOverdueGraph {

	/** The account type DESC. */
	@JsonProperty("AccountType_DESC")
	public String accountType_DESC;

	/** The account type. */
	@JsonProperty("AccountType")
	public String accountType;

	/** The performing accounts. */
	@JsonProperty("PerformingAccounts")
	public String performingAccounts;

	/** The overdue accounts. */
	@JsonProperty("OverdueAccounts")
	public String overdueAccounts;

	/**
	 * Instantiates a new perf overdue graph.
	 */
	public PerfOverdueGraph() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "PerfOverdueGraph ["
				+ (accountType_DESC != null ? "accountType_DESC=" + accountType_DESC + ", " : "")
				+ (accountType != null ? "accountType=" + accountType + ", " : "")
				+ (performingAccounts != null ? "performingAccounts=" + performingAccounts + ", "
						: "")
				+ (overdueAccounts != null ? "overdueAccounts=" + overdueAccounts : "") + "]";
	}

}
