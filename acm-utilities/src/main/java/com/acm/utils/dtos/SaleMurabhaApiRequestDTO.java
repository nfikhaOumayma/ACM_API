/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class SaleMurabhaApiRequestDTO.
 */
public class SaleMurabhaApiRequestDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -8554636470407224805L;

	/** The reference id. */
	@JsonProperty("Reference ID")
	private String referenceId;

	/** The client code. */
	@JsonProperty("Client Code")
	private String clientCode;

	/** The third party buyer. */
	@JsonProperty("Third-Party Buyer")
	private String thirdPartyBuyer;

	// Getters and setters for the fields

	/**
	 * Gets the reference id.
	 *
	 * @return the reference id
	 */
	public String getReferenceId() {

		return referenceId;
	}

	/**
	 * Sets the reference id.
	 *
	 * @param referenceId the new reference id
	 */
	public void setReferenceId(String referenceId) {

		this.referenceId = referenceId;
	}

	/**
	 * Gets the client code.
	 *
	 * @return the client code
	 */
	public String getClientCode() {

		return clientCode;
	}

	/**
	 * Sets the client code.
	 *
	 * @param clientCode the new client code
	 */
	public void setClientCode(String clientCode) {

		this.clientCode = clientCode;
	}

	/**
	 * Gets the third party buyer.
	 *
	 * @return the third party buyer
	 */
	public String getThirdPartyBuyer() {

		return thirdPartyBuyer;
	}

	/**
	 * Sets the third party buyer.
	 *
	 * @param thirdPartyBuyer the new third party buyer
	 */
	public void setThirdPartyBuyer(String thirdPartyBuyer) {

		this.thirdPartyBuyer = thirdPartyBuyer;
	}

	/**
	 * Instantiates a new sale murabha api request DTO.
	 */
	public SaleMurabhaApiRequestDTO() {

		super();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "SaleMurabhaApiRequestDTO [referenceId=" + referenceId + ", clientCode=" + clientCode
				+ ", thirdPartyBuyer=" + thirdPartyBuyer + "]";
	}

}
