/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class ConfirmPurchaseResponseApiDTO.
 */
public class ConfirmPurchaseResponseApiDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3796724314499180272L;

	/** The result. */
	@JsonProperty("result")
	private String result;

	/** The result description. */
	@JsonProperty("resultDescription")
	private String resultDescription;

	/** The eiger result code. */
	@JsonProperty("eigerResultCode")
	private int eigerResultCode;

	// Getters and setters for the fields

	/**
	 * Gets the result.
	 *
	 * @return the result
	 */
	public String getResult() {

		return result;
	}

	/**
	 * Sets the result.
	 *
	 * @param result the new result
	 */
	public void setResult(String result) {

		this.result = result;
	}

	/**
	 * Gets the result description.
	 *
	 * @return the result description
	 */
	public String getResultDescription() {

		return resultDescription;
	}

	/**
	 * Sets the result description.
	 *
	 * @param resultDescription the new result description
	 */
	public void setResultDescription(String resultDescription) {

		this.resultDescription = resultDescription;
	}

	/**
	 * Instantiates a new confirm purchase response api DTO.
	 */
	public ConfirmPurchaseResponseApiDTO() {

		super();
	}

	/**
	 * Gets the eiger result code.
	 *
	 * @return the eiger result code
	 */
	public int getEigerResultCode() {

		return eigerResultCode;
	}

	/**
	 * Sets the eiger result code.
	 *
	 * @param eigerResultCode the new eiger result code
	 */
	public void setEigerResultCode(int eigerResultCode) {

		this.eigerResultCode = eigerResultCode;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ConfirmPurchaseResponseApiDTO [result=" + result + ", resultDescription="
				+ resultDescription + ", eigerResultCode=" + eigerResultCode + "]";
	}

}
