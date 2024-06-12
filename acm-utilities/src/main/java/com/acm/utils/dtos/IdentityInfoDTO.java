/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class IdentityInfoDTO.
 */
public class IdentityInfoDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The id type. */
	@JsonProperty("idType")
	private int idType;

	/** The id number. */
	@JsonProperty("idNumber")
	private String idNumber;

	/**
	 * Instantiates a new identity info DTO.
	 */
	public IdentityInfoDTO() {

		super();
	}

	/**
	 * Gets the id type.
	 *
	 * @return the id type
	 */
	public int getIdType() {

		return idType;
	}

	/**
	 * Sets the id type.
	 *
	 * @param idType the new id type
	 */
	public void setIdType(int idType) {

		this.idType = idType;
	}

	/**
	 * Gets the id number.
	 *
	 * @return the id number
	 */
	public String getIdNumber() {

		return idNumber;
	}

	/**
	 * Sets the id number.
	 *
	 * @param idNumber the new id number
	 */
	public void setIdNumber(String idNumber) {

		this.idNumber = idNumber;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "IdentityInfoDTO [idType=" + idType + ", idNumber=" + idNumber + "]";
	}

}
