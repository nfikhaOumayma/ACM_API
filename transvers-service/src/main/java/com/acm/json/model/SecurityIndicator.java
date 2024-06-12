/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link SecurityIndicator}.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class SecurityIndicator {

	/** The no of fully secured facilities. */
	@JsonProperty("NoOfFullySecuredFacilities")
	public String noOfFullySecuredFacilities;

	/** The no of partial secured facilities. */
	@JsonProperty("NoOfPartialSecuredFacilities")
	public String noOfPartialSecuredFacilities;

	/** The no of no secured facilities. */
	@JsonProperty("NoOfNoSecuredFacilities")
	public String noOfNoSecuredFacilities;

	/**
	 * Instantiates a new security indicator.
	 */
	public SecurityIndicator() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "SecurityIndicator ["
				+ (noOfFullySecuredFacilities != null
						? "noOfFullySecuredFacilities=" + noOfFullySecuredFacilities + ", "
						: "")
				+ (noOfPartialSecuredFacilities != null
						? "noOfPartialSecuredFacilities=" + noOfPartialSecuredFacilities + ", "
						: "")
				+ (noOfNoSecuredFacilities != null
						? "noOfNoSecuredFacilities=" + noOfNoSecuredFacilities
						: "")
				+ "]";
	}

}
