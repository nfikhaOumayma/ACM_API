/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link UtilizationGraph}.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class UtilizationGraph {

	/** The account type DESC. */
	@JsonProperty("AccountType_DESC")
	public String accountType_DESC;

	/** The account type. */
	@JsonProperty("AccountType")
	public String accountType;

	/** The utilized amount. */
	@JsonProperty("UtilizedAmount")
	public String utilizedAmount;

	/** The un utilized amount. */
	@JsonProperty("UnUtilizedAmount")
	public String unUtilizedAmount;

	/**
	 * Instantiates a new utilization graph.
	 */
	public UtilizationGraph() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "UtilizationGraph ["
				+ (accountType_DESC != null ? "accountType_DESC=" + accountType_DESC + ", " : "")
				+ (accountType != null ? "accountType=" + accountType + ", " : "")
				+ (utilizedAmount != null ? "utilizedAmount=" + utilizedAmount + ", " : "")
				+ (unUtilizedAmount != null ? "unUtilizedAmount=" + unUtilizedAmount : "") + "]";
	}

}
