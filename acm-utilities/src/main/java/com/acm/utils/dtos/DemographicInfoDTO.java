/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class DemographicInfoDTO.
 */
public class DemographicInfoDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The is hijri ID expiry date. */
	@JsonProperty("isHijriIDExpiryDate")
	private boolean isHijriIDExpiryDate;

	/** The id expiry date. */
	@JsonProperty("idExpiryDate")
	private String idExpiryDate;

	/** The nationality. */
	@JsonProperty("nationality")
	private int nationality;

	/** The marital status. */
	@JsonProperty("maritalStatus")
	private int maritalStatus;

	/** The is hijri date of birth. */
	@JsonProperty("isHijriDateOfBirth")
	private boolean isHijriDateOfBirth;

	/** The date of birth. */
	@JsonProperty("dateOfBirth")
	private String dateOfBirth;

	/** The first name. */
	@JsonProperty("firstName")
	private String firstName;

	/** The gender. */
	@JsonProperty("gender")
	private int gender;

	/** The second name. */
	@JsonProperty("secondName")
	private String secondName;

	/** The third name. */
	@JsonProperty("thirdName")
	private String thirdName;

	/** The family name. */
	@JsonProperty("familyName")
	private String familyName;

	/**
	 * Instantiates a new demographic info DTO.
	 */
	public DemographicInfoDTO() {

		super();
	}

	/**
	 * Checks if is hijri ID expiry date.
	 *
	 * @return true, if is hijri ID expiry date
	 */
	public boolean isHijriIDExpiryDate() {

		return isHijriIDExpiryDate;
	}

	/**
	 * Sets the hijri ID expiry date.
	 *
	 * @param isHijriIDExpiryDate the new hijri ID expiry date
	 */
	public void setHijriIDExpiryDate(boolean isHijriIDExpiryDate) {

		this.isHijriIDExpiryDate = isHijriIDExpiryDate;
	}

	/**
	 * Gets the id expiry date.
	 *
	 * @return the id expiry date
	 */
	public String getIdExpiryDate() {

		return idExpiryDate;
	}

	/**
	 * Sets the id expiry date.
	 *
	 * @param idExpiryDate the new id expiry date
	 */
	public void setIdExpiryDate(String idExpiryDate) {

		this.idExpiryDate = idExpiryDate;
	}

	/**
	 * Gets the nationality.
	 *
	 * @return the nationality
	 */
	public int getNationality() {

		return nationality;
	}

	/**
	 * Sets the nationality.
	 *
	 * @param nationality the new nationality
	 */
	public void setNationality(int nationality) {

		this.nationality = nationality;
	}

	/**
	 * Gets the marital status.
	 *
	 * @return the marital status
	 */
	public int getMaritalStatus() {

		return maritalStatus;
	}

	/**
	 * Sets the marital status.
	 *
	 * @param maritalStatus the new marital status
	 */
	public void setMaritalStatus(int maritalStatus) {

		this.maritalStatus = maritalStatus;
	}

	/**
	 * Gets the hijri date of birth.
	 *
	 * @return the hijri date of birth
	 */
	public boolean getHijriDateOfBirth() {

		return isHijriDateOfBirth;
	}

	/**
	 * Sets the hijri date of birth.
	 *
	 * @param isHijriDateOfBirth the new hijri date of birth
	 */
	public void setHijriDateOfBirth(boolean isHijriDateOfBirth) {

		this.isHijriDateOfBirth = isHijriDateOfBirth;
	}

	/**
	 * Gets the date of birth.
	 *
	 * @return the date of birth
	 */
	public String getDateOfBirth() {

		return dateOfBirth;
	}

	/**
	 * Sets the date of birth.
	 *
	 * @param dateOfBirth the new date of birth
	 */
	public void setDateOfBirth(String dateOfBirth) {

		this.dateOfBirth = dateOfBirth;
	}

	/**
	 * Gets the first name.
	 *
	 * @return the first name
	 */
	public String getFirstName() {

		return firstName;
	}

	/**
	 * Sets the first name.
	 *
	 * @param firstName the new first name
	 */
	public void setFirstName(String firstName) {

		this.firstName = firstName;
	}

	/**
	 * Gets the gender.
	 *
	 * @return the gender
	 */
	public int getGender() {

		return gender;
	}

	/**
	 * Sets the gender.
	 *
	 * @param gender the new gender
	 */
	public void setGender(int gender) {

		this.gender = gender;
	}

	/**
	 * Gets the second name.
	 *
	 * @return the second name
	 */
	public String getSecondName() {

		return secondName;
	}

	/**
	 * Sets the second name.
	 *
	 * @param secondName the new second name
	 */
	public void setSecondName(String secondName) {

		this.secondName = secondName;
	}

	/**
	 * Gets the third name.
	 *
	 * @return the third name
	 */
	public String getThirdName() {

		return thirdName;
	}

	/**
	 * Sets the third name.
	 *
	 * @param thirdName the new third name
	 */
	public void setThirdName(String thirdName) {

		this.thirdName = thirdName;
	}

	/**
	 * Gets the family name.
	 *
	 * @return the family name
	 */
	public String getFamilyName() {

		return familyName;
	}

	/**
	 * Sets the family name.
	 *
	 * @param familyName the new family name
	 */
	public void setFamilyName(String familyName) {

		this.familyName = familyName;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "DemographicInfoDTO [isHijriIDExpiryDate=" + isHijriIDExpiryDate + ", idExpiryDate="
				+ idExpiryDate + ", nationality=" + nationality + ", maritalStatus=" + maritalStatus
				+ ", isHijriDateOfBirth=" + isHijriDateOfBirth + ", dateOfBirth=" + dateOfBirth
				+ ", firstName=" + firstName + ", gender=" + gender + ", secondName=" + secondName
				+ ", thirdName=" + thirdName + ", familyName=" + familyName + "]";
	}

}
