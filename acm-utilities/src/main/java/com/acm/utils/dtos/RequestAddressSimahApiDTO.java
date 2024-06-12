/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.utils.dtos;

import java.io.Serializable;

import org.codehaus.jackson.annotate.JsonProperty;

/**
 * The Class RequestAddressSimahApiDTO.
 */
public class RequestAddressSimahApiDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2994646291924976570L;

	/** The address type. */
	@JsonProperty("addressType")
	private int addressType;

	/** The zip code. */
	@JsonProperty("zipCode")
	private int zipCode;

	/** The city. */
	@JsonProperty("city")
	private int city;

	/** The building number. */
	@JsonProperty("buildingNumber")
	private int buildingNumber;

	/** The street. */
	@JsonProperty("street")
	private String street;

	/** The district. */
	@JsonProperty("district")
	private String district;

	/** The unit number. */
	@JsonProperty("unitNumber")
	private String unitNumber;

	/** The additional number. */
	@JsonProperty("additionalNumber")
	private String additionalNumber;

	/**
	 * Instantiates a new request address simah api DTO.
	 */
	public RequestAddressSimahApiDTO() {

		super();
	}

	/**
	 * Gets the address type.
	 *
	 * @return the address type
	 */
	public int getAddressType() {

		return addressType;
	}

	/**
	 * Sets the address type.
	 *
	 * @param addressType the new address type
	 */
	public void setAddressType(int addressType) {

		this.addressType = addressType;
	}

	/**
	 * Gets the zip code.
	 *
	 * @return the zip code
	 */
	public int getZipCode() {

		return zipCode;
	}

	/**
	 * Sets the zip code.
	 *
	 * @param zipCode the new zip code
	 */
	public void setZipCode(int zipCode) {

		this.zipCode = zipCode;
	}

	/**
	 * Gets the city.
	 *
	 * @return the city
	 */
	public int getCity() {

		return city;
	}

	/**
	 * Sets the city.
	 *
	 * @param city the new city
	 */
	public void setCity(int city) {

		this.city = city;
	}

	/**
	 * Gets the building number.
	 *
	 * @return the building number
	 */
	public int getBuildingNumber() {

		return buildingNumber;
	}

	/**
	 * Sets the building number.
	 *
	 * @param buildingNumber the new building number
	 */
	public void setBuildingNumber(int buildingNumber) {

		this.buildingNumber = buildingNumber;
	}

	/**
	 * Gets the street.
	 *
	 * @return the street
	 */
	public String getStreet() {

		return street;
	}

	/**
	 * Sets the street.
	 *
	 * @param street the new street
	 */
	public void setStreet(String street) {

		this.street = street;
	}

	/**
	 * Gets the district.
	 *
	 * @return the district
	 */
	public String getDistrict() {

		return district;
	}

	/**
	 * Sets the district.
	 *
	 * @param district the new district
	 */
	public void setDistrict(String district) {

		this.district = district;
	}

	/**
	 * Gets the unit number.
	 *
	 * @return the unit number
	 */
	public String getUnitNumber() {

		return unitNumber;
	}

	/**
	 * Sets the unit number.
	 *
	 * @param unitNumber the new unit number
	 */
	public void setUnitNumber(String unitNumber) {

		this.unitNumber = unitNumber;
	}

	/**
	 * Gets the additional number.
	 *
	 * @return the additional number
	 */
	public String getAdditionalNumber() {

		return additionalNumber;
	}

	/**
	 * Sets the additional number.
	 *
	 * @param additionalNumber the new additional number
	 */
	public void setAdditionalNumber(String additionalNumber) {

		this.additionalNumber = additionalNumber;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "RequestAddressSimahApiDTO [addressType=" + addressType + ", zipCode=" + zipCode
				+ ", city=" + city + ", buildingNumber=" + buildingNumber + ", street=" + street
				+ ", district=" + district + ", unitNumber=" + unitNumber + ", additionalNumber="
				+ additionalNumber + "]";
	}

}
