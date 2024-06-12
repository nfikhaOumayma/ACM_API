/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.List;

/**
 * The Class customerDetailsReportsDTO.
 */
public class CustomerDetailsReportsDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 114728182277019545L;

	/** The identity. */
	private String identity;

	/** The customer name. */
	private String customerName;

	/** The age. */
	private Long age;

	/** The mobile number. */
	private String mobileNumber;

	/** The place of issue. */
	private String placeOfIssue;

	/** The issue date. */
	private String issueDate;

	/** The family situation. */
	private String familySituation;
	/** The profession. */
	private String profession;
	/** The address DTO. */
	private List<AddressDTO> addressDTO;

	/** The phone number. */
	private String phoneNumber;

	/**
	 * Instantiates a new customer details reports DTO.
	 */
	public CustomerDetailsReportsDTO() {

	}

	/**
	 * Instantiates a new customer details reports DTO.
	 *
	 * @param identity the identity
	 * @param customerName the customer name
	 * @param age the age
	 * @param mobileNumber the mobile number
	 * @param placeOfIssue the place of issue
	 * @param issueDate the issue date
	 * @param familySituation the family situation
	 * @param profession the profession
	 * @param addressDTO the address DTO
	 */
	public CustomerDetailsReportsDTO(String identity, String customerName, Long age,
			String mobileNumber, String placeOfIssue, String issueDate, String familySituation,
			String profession, List<AddressDTO> addressDTO) {

		super();
		this.identity = identity;
		this.customerName = customerName;
		this.age = age;
		this.mobileNumber = mobileNumber;
		this.placeOfIssue = placeOfIssue;
		this.issueDate = issueDate;
		this.familySituation = familySituation;
		this.profession = profession;
		this.addressDTO = addressDTO;
	}

	/**
	 * Gets the identity.
	 *
	 * @return the identity
	 */
	public String getIdentity() {

		return identity;
	}

	/**
	 * Sets the identity.
	 *
	 * @param identity the new identity
	 */
	public void setIdentity(String identity) {

		this.identity = identity;
	}

	/**
	 * Gets the customer name.
	 *
	 * @return the customer name
	 */
	public String getCustomerName() {

		return customerName;
	}

	/**
	 * Sets the customer name.
	 *
	 * @param customerName the new customer name
	 */
	public void setCustomerName(String customerName) {

		this.customerName = customerName;
	}

	/**
	 * Gets the age.
	 *
	 * @return the age
	 */
	public Long getAge() {

		return age;
	}

	/**
	 * Sets the age.
	 *
	 * @param age the new age
	 */
	public void setAge(Long age) {

		this.age = age;
	}

	/**
	 * Gets the mobile number.
	 *
	 * @return the mobile number
	 */
	public String getMobileNumber() {

		return mobileNumber;
	}

	/**
	 * Sets the mobile number.
	 *
	 * @param mobileNumber the new mobile number
	 */
	public void setMobileNumber(String mobileNumber) {

		this.mobileNumber = mobileNumber;
	}

	/**
	 * Gets the place of issue.
	 *
	 * @return the place of issue
	 */
	public String getPlaceOfIssue() {

		return placeOfIssue;
	}

	/**
	 * Sets the place of issue.
	 *
	 * @param placeOfIssue the new place of issue
	 */
	public void setPlaceOfIssue(String placeOfIssue) {

		this.placeOfIssue = placeOfIssue;
	}

	/**
	 * Gets the issue date.
	 *
	 * @return the issue date
	 */
	public String getIssueDate() {

		return issueDate;
	}

	/**
	 * Sets the issue date.
	 *
	 * @param issueDate the new issue date
	 */
	public void setIssueDate(String issueDate) {

		this.issueDate = issueDate;
	}

	/**
	 * Gets the family situation.
	 *
	 * @return the family situation
	 */
	public String getFamilySituation() {

		return familySituation;
	}

	/**
	 * Sets the family situation.
	 *
	 * @param familySituation the new family situation
	 */
	public void setFamilySituation(String familySituation) {

		this.familySituation = familySituation;
	}

	/**
	 * Gets the profession.
	 *
	 * @return the profession
	 */
	public String getProfession() {

		return profession;
	}

	/**
	 * Sets the profession.
	 *
	 * @param profession the new profession
	 */
	public void setProfession(String profession) {

		this.profession = profession;
	}

	/**
	 * Gets the address DTO.
	 *
	 * @return the address DTO
	 */
	public List<AddressDTO> getAddressDTO() {

		return addressDTO;
	}

	/**
	 * Sets the address DTO.
	 *
	 * @param addressDTO the new address DTO
	 */
	public void setAddressDTO(List<AddressDTO> addressDTO) {

		this.addressDTO = addressDTO;
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

}
