/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.utils.dtos;

import java.io.Serializable;

import org.codehaus.jackson.annotate.JsonProperty;

/**
 * The Class ContactRequestSimahApiDTO.
 */
public class ContactRequestSimahApiDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1029485701875829320L;

	/** The contact type. */
	@JsonProperty("contactType")
	private String contactType;

	/** The area code. */
	@JsonProperty("areaCode")
	private String areaCode;

	/** The phone number. */
	@JsonProperty("phoneNumber")
	private String phoneNumber;

	/** The extension. */
	@JsonProperty("extension")
	private String extension;

	/** The country code. */
	@JsonProperty("countryCode")
	private int countryCode;

	/**
	 * Instantiates a new contact request simah api DTO.
	 */
	public ContactRequestSimahApiDTO() {

		super();
	}

	/**
	 * Gets the contact type.
	 *
	 * @return the contact type
	 */
	public String getContactType() {

		return contactType;
	}

	/**
	 * Sets the contact type.
	 *
	 * @param contactType the new contact type
	 */
	public void setContactType(String contactType) {

		this.contactType = contactType;
	}

	/**
	 * Gets the area code.
	 *
	 * @return the area code
	 */
	public String getAreaCode() {

		return areaCode;
	}

	/**
	 * Sets the area code.
	 *
	 * @param areaCode the new area code
	 */
	public void setAreaCode(String areaCode) {

		this.areaCode = areaCode;
	}

	/**
	 * Gets the phone number.
	 *
	 * @return the phone number
	 */
	public String getPhoneNumber() {

		return phoneNumber;
	}

	/**
	 * Sets the phone number.
	 *
	 * @param phoneNumber the new phone number
	 */
	public void setPhoneNumber(String phoneNumber) {

		this.phoneNumber = phoneNumber;
	}

	/**
	 * Gets the extension.
	 *
	 * @return the extension
	 */
	public String getExtension() {

		return extension;
	}

	/**
	 * Sets the extension.
	 *
	 * @param extension the new extension
	 */
	public void setExtension(String extension) {

		this.extension = extension;
	}

	/**
	 * Gets the country code.
	 *
	 * @return the country code
	 */
	public int getCountryCode() {

		return countryCode;
	}

	/**
	 * Sets the country code.
	 *
	 * @param countryCode the new country code
	 */
	public void setCountryCode(int countryCode) {

		this.countryCode = countryCode;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ContactRequestSimahApiDTO [contactType=" + contactType + ", areaCode=" + areaCode
				+ ", phoneNumber=" + phoneNumber + ", extension=" + extension + ", countryCode="
				+ countryCode + "]";
	}

}
