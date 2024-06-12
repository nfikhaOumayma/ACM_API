/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class ConfirmPurchaseOrSaleRequestApiDTO.
 */
public class ConfirmPurchaseOrSaleRequestApiDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2059985935099571737L;

	/** The reference id. */
	@JsonProperty("Reference ID")
	private String referenceId;

	/** The client code. */
	@JsonProperty("Client Code")
	private String clientCode;

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

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ConfirmPurchaseRequestApiDTO [referenceId=" + referenceId + ", clientCode="
				+ clientCode + "]";
	}

	/**
	 * Instantiates a new confirm purchase request api DTO.
	 */
	public ConfirmPurchaseOrSaleRequestApiDTO() {

		super();
	}

}
