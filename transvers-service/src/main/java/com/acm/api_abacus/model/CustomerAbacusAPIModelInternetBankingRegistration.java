/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;

/**
 * {@link CustomerAbacusAPIModelInternetBankingRegistration} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class CustomerAbacusAPIModelInternetBankingRegistration implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6052348275169762560L;

	/** The forename. */
	private String forename;

	/** The surname. */
	private String surname;

	/** The customer number. */
	private String customerNumber;

	/** The phone number. */
	private String phoneNumber;

	/** The email. */
	private String email;

	/** The dob. */
	private Object dob;

	/** The is register. */
	private boolean isRegister;

	/** The ib status. */
	private Object ibStatus;

	/** The ib communication type. */
	private Object ibCommunicationType;

	/** The skin name. */
	private String skinName;

	/** The ib comm consent. */
	private boolean ibCommConsent;

	/** The email details. */
	private String emailDetails;

	/** The custom user ID. */
	private String customUserID;

	/** The is valid user ID. */
	private int isValidUserID;

	/**
	 * Gets the forename.
	 *
	 * @return the forename
	 */
	public String getForename() {

		return forename;
	}

	/**
	 * Sets the forename.
	 *
	 * @param forename the forename to set
	 */
	public void setForename(String forename) {

		this.forename = forename;
	}

	/**
	 * Gets the surname.
	 *
	 * @return the surname
	 */
	public String getSurname() {

		return surname;
	}

	/**
	 * Sets the surname.
	 *
	 * @param surname the surname to set
	 */
	public void setSurname(String surname) {

		this.surname = surname;
	}

	/**
	 * Gets the customer number.
	 *
	 * @return the customerNumber
	 */
	public String getCustomerNumber() {

		return customerNumber;
	}

	/**
	 * Sets the customer number.
	 *
	 * @param customerNumber the customerNumber to set
	 */
	public void setCustomerNumber(String customerNumber) {

		this.customerNumber = customerNumber;
	}

	/**
	 * Gets the phone number.
	 *
	 * @return the phoneNumber
	 */
	public String getPhoneNumber() {

		return phoneNumber;
	}

	/**
	 * Sets the phone number.
	 *
	 * @param phoneNumber the phoneNumber to set
	 */
	public void setPhoneNumber(String phoneNumber) {

		this.phoneNumber = phoneNumber;
	}

	/**
	 * Gets the email.
	 *
	 * @return the email
	 */
	public String getEmail() {

		return email;
	}

	/**
	 * Sets the email.
	 *
	 * @param email the email to set
	 */
	public void setEmail(String email) {

		this.email = email;
	}

	/**
	 * Gets the dob.
	 *
	 * @return the dob
	 */
	public Object getDob() {

		return dob;
	}

	/**
	 * Sets the dob.
	 *
	 * @param dob the dob to set
	 */
	public void setDob(Object dob) {

		this.dob = dob;
	}

	/**
	 * Checks if is register.
	 *
	 * @return the isRegister
	 */
	public boolean isRegister() {

		return isRegister;
	}

	/**
	 * Sets the register.
	 *
	 * @param isRegister the isRegister to set
	 */
	public void setRegister(boolean isRegister) {

		this.isRegister = isRegister;
	}

	/**
	 * Gets the ib status.
	 *
	 * @return the ibStatus
	 */
	public Object getIbStatus() {

		return ibStatus;
	}

	/**
	 * Sets the ib status.
	 *
	 * @param ibStatus the ibStatus to set
	 */
	public void setIbStatus(Object ibStatus) {

		this.ibStatus = ibStatus;
	}

	/**
	 * Gets the ib communication type.
	 *
	 * @return the ibCommunicationType
	 */
	public Object getIbCommunicationType() {

		return ibCommunicationType;
	}

	/**
	 * Sets the ib communication type.
	 *
	 * @param ibCommunicationType the ibCommunicationType to set
	 */
	public void setIbCommunicationType(Object ibCommunicationType) {

		this.ibCommunicationType = ibCommunicationType;
	}

	/**
	 * Gets the skin name.
	 *
	 * @return the skinName
	 */
	public String getSkinName() {

		return skinName;
	}

	/**
	 * Sets the skin name.
	 *
	 * @param skinName the skinName to set
	 */
	public void setSkinName(String skinName) {

		this.skinName = skinName;
	}

	/**
	 * Checks if is ib comm consent.
	 *
	 * @return the ibCommConsent
	 */
	public boolean isIbCommConsent() {

		return ibCommConsent;
	}

	/**
	 * Sets the ib comm consent.
	 *
	 * @param ibCommConsent the ibCommConsent to set
	 */
	public void setIbCommConsent(boolean ibCommConsent) {

		this.ibCommConsent = ibCommConsent;
	}

	/**
	 * Gets the email details.
	 *
	 * @return the emailDetails
	 */
	public String getEmailDetails() {

		return emailDetails;
	}

	/**
	 * Sets the email details.
	 *
	 * @param emailDetails the emailDetails to set
	 */
	public void setEmailDetails(String emailDetails) {

		this.emailDetails = emailDetails;
	}

	/**
	 * Gets the custom user ID.
	 *
	 * @return the customUserID
	 */
	public String getCustomUserID() {

		return customUserID;
	}

	/**
	 * Sets the custom user ID.
	 *
	 * @param customUserID the customUserID to set
	 */
	public void setCustomUserID(String customUserID) {

		this.customUserID = customUserID;
	}

	/**
	 * Gets the checks if is valid user ID.
	 *
	 * @return the isValidUserID
	 */
	public int getIsValidUserID() {

		return isValidUserID;
	}

	/**
	 * Sets the checks if is valid user ID.
	 *
	 * @param isValidUserID the isValidUserID to set
	 */
	public void setIsValidUserID(int isValidUserID) {

		this.isValidUserID = isValidUserID;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CustomerAbacusAPIModelInternetBankingRegistration [forename=" + forename
				+ ", surname=" + surname + ", customerNumber=" + customerNumber + ", phoneNumber="
				+ phoneNumber + ", email=" + email + ", dob=" + dob + ", isRegister=" + isRegister
				+ ", ibStatus=" + ibStatus + ", ibCommunicationType=" + ibCommunicationType
				+ ", skinName=" + skinName + ", ibCommConsent=" + ibCommConsent + ", emailDetails="
				+ emailDetails + ", customUserID=" + customUserID + ", isValidUserID="
				+ isValidUserID + "]";
	}

}
