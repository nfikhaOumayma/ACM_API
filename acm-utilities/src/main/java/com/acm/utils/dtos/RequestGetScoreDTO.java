/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class RequestGetScoreDTO.
 */
public class RequestGetScoreDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The identity info. */
	@JsonProperty("identityInfo")
	private IdentityInfoDTO identityInfo;

	/** The application details. */
	@JsonProperty("applicationDetails")
	private ApplicationDetailsDTO applicationDetails;

	/** The demographic info. */
	@JsonProperty("demographicInfo")
	private DemographicInfoDTO demographicInfo;

	/** The accept. */
	@JsonProperty("accept")
	private boolean accept;

	/** The reference number. */
	@JsonProperty("referenceNumber")
	private String referenceNumber;

	/**
	 * Instantiates a new request get score DTO.
	 */
	public RequestGetScoreDTO() {

		super();
	}

	/**
	 * Gets the identity info.
	 *
	 * @return the identity info
	 */
	public IdentityInfoDTO getIdentityInfo() {

		return identityInfo;
	}

	/**
	 * Sets the identity info.
	 *
	 * @param identityInfo the new identity info
	 */
	public void setIdentityInfo(IdentityInfoDTO identityInfo) {

		this.identityInfo = identityInfo;
	}

	/**
	 * Gets the application details.
	 *
	 * @return the application details
	 */
	public ApplicationDetailsDTO getApplicationDetails() {

		return applicationDetails;
	}

	/**
	 * Sets the application details.
	 *
	 * @param applicationDetails the new application details
	 */
	public void setApplicationDetails(ApplicationDetailsDTO applicationDetails) {

		this.applicationDetails = applicationDetails;
	}

	/**
	 * Gets the demographic info.
	 *
	 * @return the demographic info
	 */
	public DemographicInfoDTO getDemographicInfo() {

		return demographicInfo;
	}

	/**
	 * Sets the demographic info.
	 *
	 * @param demographicInfo the new demographic info
	 */
	public void setDemographicInfo(DemographicInfoDTO demographicInfo) {

		this.demographicInfo = demographicInfo;
	}

	/**
	 * Checks if is accept.
	 *
	 * @return true, if is accept
	 */
	public boolean isAccept() {

		return accept;
	}

	/**
	 * Sets the accept.
	 *
	 * @param accept the new accept
	 */
	public void setAccept(boolean accept) {

		this.accept = accept;
	}

	/**
	 * Gets the reference number.
	 *
	 * @return the reference number
	 */
	public String getReferenceNumber() {

		return referenceNumber;
	}

	/**
	 * Sets the reference number.
	 *
	 * @param referenceNumber the new reference number
	 */
	public void setReferenceNumber(String referenceNumber) {

		this.referenceNumber = referenceNumber;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "RequestGetScoreDTO [identityInfo=" + identityInfo + ", applicationDetails="
				+ applicationDetails + ", demographicInfo=" + demographicInfo + ", accept=" + accept
				+ ", referenceNumber=" + referenceNumber + "]";
	}

}
