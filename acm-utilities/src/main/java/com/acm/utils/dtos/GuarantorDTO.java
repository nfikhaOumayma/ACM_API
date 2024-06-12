/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * {@link GuarantorDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class GuarantorDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1118476312571340116L;

	/** The loan id. */
	private Long loanId;

	/** The loan id. */
	private Long accountId;

	/** The customer id. */
	private Long customerId;

	/** The description. */
	private String description;

	/** The full name. */
	private String fullName;

	/** The registration date. */
	private Date registrationDate;

	/** The agence. */
	private String agence;

	/** The age. */
	private Integer age;

	/** The adresse 1. */
	private String adresse1;

	/** The adresse 2. */
	private String adresse2;

	/** The adresse 3. */
	private String adresse3;

	/** The city. */
	private String city;

	/** The county. */
	private String county;

	/** The state. */
	private String state;

	/** The postal code. */
	private String postalCode;

	/** The country. */
	private String country;

	/** The guarantor type. */
	private String guarantorType;

	/** The currency. */
	private String currency;

	/** The currency id. */
	private Long currencyId;

	/** The outstanding balance. */
	private BigDecimal outstandingBalance;

	/** The amount. */
	private BigDecimal amount;

	/** The loan guarantor type id. */
	private Long loanGuarantorTypeId;

	/**
	 * Instantiates a new guarantor DTO.
	 */
	public GuarantorDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new guarantor DTO (USED in transvers-service in LOAD DATA method (FROM
	 * ABACUS)).
	 *
	 * @param fullName the full name
	 * @param registrationDate the registration date
	 * @param agence the agence
	 * @param age the age
	 * @param adresse1 the adresse 1
	 * @param adresse2 the adresse 2
	 * @param adresse3 the adresse 3
	 * @param city the city
	 * @param county the county
	 * @param state the state
	 * @param postalCode the postal code
	 * @param country the country
	 * @param guarantorType the guarantor type
	 * @param currency the currency
	 * @param outstandingBalance the outstanding balance
	 * @param amount the amount
	 * @param loanId the loan id
	 * @param customerId the customer id
	 */
	public GuarantorDTO(String fullName, Date registrationDate, String agence, Integer age,
			String adresse1, String adresse2, String adresse3, String city, String county,
			String state, String postalCode, String country, String guarantorType, String currency,
			BigDecimal outstandingBalance, BigDecimal amount, Long loanId, Long customerId) {

		this.fullName = fullName;
		this.registrationDate = registrationDate;
		this.agence = agence;
		this.age = age;
		this.adresse1 = adresse1;
		this.adresse2 = adresse2;
		this.adresse3 = adresse3;
		this.city = city;
		this.county = county;
		this.state = state;
		this.postalCode = postalCode;
		this.country = country;
		this.guarantorType = guarantorType;
		this.currency = currency;
		this.outstandingBalance = outstandingBalance;
		this.amount = amount;
		this.loanId = loanId;
		this.customerId = customerId;
	}

	/**
	 * Instantiates a new guarantor DTO (required data USED to CREATE GUARANTOR in ABACUS via API).
	 *
	 * @param loanId the loan id
	 * @param accountId the account id
	 * @param customerId the customer id
	 * @param description the description
	 * @param guarantorType the guarantor type
	 * @param currencyId the currency id
	 * @param amount the amount
	 * @param loanGuarantorTypeId the loan guarantor type id
	 */
	public GuarantorDTO(Long loanId, Long accountId, Long customerId, String description,
			String guarantorType, Long currencyId, BigDecimal amount, Long loanGuarantorTypeId) {

		this.loanId = loanId;
		this.accountId = accountId;
		this.customerId = customerId;
		this.description = description;
		this.guarantorType = guarantorType;
		this.currencyId = currencyId;
		this.amount = amount;
		this.loanGuarantorTypeId = loanGuarantorTypeId;
	}

	/**
	 * Gets the loan id.
	 *
	 * @return the loanId
	 */
	public Long getLoanId() {

		return loanId;
	}

	/**
	 * Sets the loan id.
	 *
	 * @param loanId the loanId to set
	 */
	public void setLoanId(Long loanId) {

		this.loanId = loanId;
	}

	/**
	 * Gets the account id.
	 *
	 * @return the account id
	 */
	public Long getAccountId() {

		return accountId;
	}

	/**
	 * Sets the account id.
	 *
	 * @param accountId the new account id
	 */
	public void setAccountId(Long accountId) {

		this.accountId = accountId;
	}

	/**
	 * Gets the currency id.
	 *
	 * @return the currencyId
	 */
	public Long getCurrencyId() {

		return currencyId;
	}

	/**
	 * Sets the currency id.
	 *
	 * @param currencyId the currencyId to set
	 */
	public void setCurrencyId(Long currencyId) {

		this.currencyId = currencyId;
	}

	/**
	 * Gets the loan guarantor type id.
	 *
	 * @return the loanGuarantorTypeId
	 */
	public Long getLoanGuarantorTypeId() {

		return loanGuarantorTypeId;
	}

	/**
	 * Sets the loan guarantor type id.
	 *
	 * @param loanGuarantorTypeId the loanGuarantorTypeId to set
	 */
	public void setLoanGuarantorTypeId(Long loanGuarantorTypeId) {

		this.loanGuarantorTypeId = loanGuarantorTypeId;
	}

	/**
	 * Gets the full name.
	 *
	 * @return the fullName
	 */
	public String getFullName() {

		return fullName;
	}

	/**
	 * Sets the full name.
	 *
	 * @param fullName the fullName to set
	 */
	public void setFullName(String fullName) {

		this.fullName = fullName;
	}

	/**
	 * Gets the registration date.
	 *
	 * @return the registrationDate
	 */
	public Date getRegistrationDate() {

		return registrationDate;
	}

	/**
	 * Sets the registration date.
	 *
	 * @param registrationDate the registrationDate to set
	 */
	public void setRegistrationDate(Date registrationDate) {

		this.registrationDate = registrationDate;
	}

	/**
	 * Gets the agence.
	 *
	 * @return the agence
	 */
	public String getAgence() {

		return agence;
	}

	/**
	 * Sets the agence.
	 *
	 * @param agence the agence to set
	 */
	public void setAgence(String agence) {

		this.agence = agence;
	}

	/**
	 * Gets the age.
	 *
	 * @return the age
	 */
	public Integer getAge() {

		return age;
	}

	/**
	 * Sets the age.
	 *
	 * @param age the age to set
	 */
	public void setAge(Integer age) {

		this.age = age;
	}

	/**
	 * Gets the adresse 1.
	 *
	 * @return the adresse1
	 */
	public String getAdresse1() {

		return adresse1;
	}

	/**
	 * Sets the adresse 1.
	 *
	 * @param adresse1 the adresse1 to set
	 */
	public void setAdresse1(String adresse1) {

		this.adresse1 = adresse1;
	}

	/**
	 * Gets the adresse 2.
	 *
	 * @return the adresse2
	 */
	public String getAdresse2() {

		return adresse2;
	}

	/**
	 * Sets the adresse 2.
	 *
	 * @param adresse2 the adresse2 to set
	 */
	public void setAdresse2(String adresse2) {

		this.adresse2 = adresse2;
	}

	/**
	 * Gets the adresse 3.
	 *
	 * @return the adresse3
	 */
	public String getAdresse3() {

		return adresse3;
	}

	/**
	 * Sets the adresse 3.
	 *
	 * @param adresse3 the adresse3 to set
	 */
	public void setAdresse3(String adresse3) {

		this.adresse3 = adresse3;
	}

	/**
	 * Gets the city.
	 *
	 * @return the city
	 */
	public String getCity() {

		return city;
	}

	/**
	 * Sets the city.
	 *
	 * @param city the city to set
	 */
	public void setCity(String city) {

		this.city = city;
	}

	/**
	 * Gets the county.
	 *
	 * @return the county
	 */
	public String getCounty() {

		return county;
	}

	/**
	 * Sets the county.
	 *
	 * @param county the county to set
	 */
	public void setCounty(String county) {

		this.county = county;
	}

	/**
	 * Gets the state.
	 *
	 * @return the state
	 */
	public String getState() {

		return state;
	}

	/**
	 * Sets the state.
	 *
	 * @param state the state to set
	 */
	public void setState(String state) {

		this.state = state;
	}

	/**
	 * Gets the postal code.
	 *
	 * @return the postalCode
	 */
	public String getPostalCode() {

		return postalCode;
	}

	/**
	 * Sets the postal code.
	 *
	 * @param postalCode the postalCode to set
	 */
	public void setPostalCode(String postalCode) {

		this.postalCode = postalCode;
	}

	/**
	 * Gets the country.
	 *
	 * @return the country
	 */
	public String getCountry() {

		return country;
	}

	/**
	 * Sets the country.
	 *
	 * @param country the country to set
	 */
	public void setCountry(String country) {

		this.country = country;
	}

	/**
	 * Gets the guarantor type.
	 *
	 * @return the guarantorType
	 */
	public String getGuarantorType() {

		return guarantorType;
	}

	/**
	 * Sets the guarantor type.
	 *
	 * @param guarantorType the guarantorType to set
	 */
	public void setGuarantorType(String guarantorType) {

		this.guarantorType = guarantorType;
	}

	/**
	 * Gets the currency.
	 *
	 * @return the currency
	 */
	public String getCurrency() {

		return currency;
	}

	/**
	 * Sets the currency.
	 *
	 * @param currency the currency to set
	 */
	public void setCurrency(String currency) {

		this.currency = currency;
	}

	/**
	 * Gets the outstanding balance.
	 *
	 * @return the outstandingBalance
	 */
	public BigDecimal getOutstandingBalance() {

		return outstandingBalance;
	}

	/**
	 * Sets the outstanding balance.
	 *
	 * @param outstandingBalance the outstandingBalance to set
	 */
	public void setOutstandingBalance(BigDecimal outstandingBalance) {

		this.outstandingBalance = outstandingBalance;
	}

	/**
	 * Gets the amount.
	 *
	 * @return the amount
	 */
	public BigDecimal getAmount() {

		return amount;
	}

	/**
	 * Sets the amount.
	 *
	 * @param amount the amount to set
	 */
	public void setAmount(BigDecimal amount) {

		this.amount = amount;
	}

	/**
	 * Gets the customer id.
	 *
	 * @return the customerId
	 */
	public Long getCustomerId() {

		return customerId;
	}

	/**
	 * Sets the customer id.
	 *
	 * @param customerId the customerId to set
	 */
	public void setCustomerId(Long customerId) {

		this.customerId = customerId;
	}

	/**
	 * Gets the description.
	 *
	 * @return the description
	 */
	public String getDescription() {

		return description;
	}

	/**
	 * Sets the description.
	 *
	 * @param description the description to set
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "GuarantorDTO [loanId=" + loanId + ", customerId=" + customerId + ", description="
				+ description + ", fullName=" + fullName + ", registrationDate=" + registrationDate
				+ ", agence=" + agence + ", age=" + age + ", adresse1=" + adresse1 + ", adresse2="
				+ adresse2 + ", adresse3=" + adresse3 + ", city=" + city + ", county=" + county
				+ ", state=" + state + ", postalCode=" + postalCode + ", country=" + country
				+ ", guarantorType=" + guarantorType + ", currency=" + currency + ", currencyId="
				+ currencyId + ", outstandingBalance=" + outstandingBalance + ", amount=" + amount
				+ ", loanGuarantorTypeId=" + loanGuarantorTypeId + "]";
	}

}
