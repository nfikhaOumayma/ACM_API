/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link RequestApplicationDetails} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class RequestApplicationDetails {

	/** The in quiry reason. */
	@JsonProperty("INQUIRY_REASON")
	public String inQuiryReason;

	/** The in quiry loan type. */
	@JsonProperty("INQUIRY_LOAN_TYPE")
	public String inQuiryLoanType;

	/** The in quiry amount. */
	@JsonProperty("INQUIRY_AMOUNT")
	public String inQuiryAmount;

	/** The reference no. */
	@JsonProperty("REFERENCE_NO")
	public String referenceNo;

	/**
	 * Instantiates a new request application details.
	 */
	public RequestApplicationDetails() {

		// Empty
	}

	/**
	 * Instantiates a new request application details.
	 *
	 * @param inQuiryReason the in quiry reason
	 * @param inQuiryLoanType the in quiry loan type
	 * @param inQuiryAmount the in quiry amount
	 * @param referenceNo the reference no
	 */
	public RequestApplicationDetails(String inQuiryReason, String inQuiryLoanType,
			String inQuiryAmount, String referenceNo) {

		this.inQuiryReason = inQuiryReason;
		this.inQuiryLoanType = inQuiryLoanType;
		this.inQuiryAmount = inQuiryAmount;
		this.referenceNo = referenceNo;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "RequestApplicationDetails ["
				+ (inQuiryReason != null ? "inQuiryReason=" + inQuiryReason + ", " : "")
				+ (inQuiryLoanType != null ? "inQuiryLoanType=" + inQuiryLoanType + ", " : "")
				+ (inQuiryAmount != null ? "inQuiryAmount=" + inQuiryAmount + ", " : "")
				+ (referenceNo != null ? "referenceNo=" + referenceNo : "") + "]";
	}

}
