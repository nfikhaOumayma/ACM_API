/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.math.BigDecimal;
import java.util.Date;

/**
 * {@link CollaterolDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class CollaterolDTO extends GenericDTO implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 76311120030892307L;

	/** The loan id. */
	private Integer loanId;

	/** The account number extern. */
	private String accountNumberExtern;

	/** The full name. */
	private String fullName;

	/** The registration date. */
	private Date registrationDate;

	/** The agence. */
	private String branch;

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

	/** The reference. */
	private String reference;

	/** The description. */
	private String description;

	/** The collateral type. */
	private String collateralType;

	/** The Original cross value. */
	private BigDecimal originalCrossValue;

	/** The gross value. */
	private BigDecimal grossValue;

	/** The realised value. */
	private BigDecimal realisedValue;

	/** The fixed cost. */
	private BigDecimal fixedCost;

	/** The net value. */
	private BigDecimal netValue;

	/**
	 * Instantiates a new collaterol.
	 */
	public CollaterolDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new collaterol DTO.
	 *
	 * @param loanId the loan id
	 * @param fullName the full name
	 * @param registrationDate the registration date
	 * @param branch the branch
	 * @param age the age
	 * @param adresse1 the adresse 1
	 * @param adresse2 the adresse 2
	 * @param adresse3 the adresse 3
	 * @param city the city
	 * @param county the county
	 * @param state the state
	 * @param postalCode the postal code
	 * @param country the country
	 * @param reference the reference
	 * @param description the description
	 * @param collateralType the collateral type
	 * @param originalCrossValue the original cross value
	 * @param grossValue the gross value
	 * @param realisedValue the realised value
	 * @param fixedCost the fixed cost
	 * @param netValue the net value
	 */
	public CollaterolDTO(Integer loanId, String fullName, Date registrationDate, String branch,
			Integer age, String adresse1, String adresse2, String adresse3, String city,
			String county, String state, String postalCode, String country, String reference,
			String description, String collateralType, BigDecimal originalCrossValue,
			BigDecimal grossValue, BigDecimal realisedValue, BigDecimal fixedCost,
			BigDecimal netValue) {

		this.loanId = loanId;
		this.fullName = fullName;
		this.registrationDate = registrationDate;
		this.branch = branch;
		this.age = age;
		this.adresse1 = adresse1;
		this.adresse2 = adresse2;
		this.adresse3 = adresse3;
		this.city = city;
		this.county = county;
		this.state = state;
		this.postalCode = postalCode;
		this.country = country;
		this.reference = reference;
		this.description = description;
		this.collateralType = collateralType;
		this.originalCrossValue = originalCrossValue;
		this.grossValue = grossValue;
		this.realisedValue = realisedValue;
		this.fixedCost = fixedCost;
		this.netValue = netValue;
	}

	/**
	 * Gets the loan id.
	 *
	 * @return the loanId
	 */
	public Integer getLoanId() {

		return loanId;
	}

	/**
	 * Sets the loan id.
	 *
	 * @param loanId the loanId to set
	 */
	public void setLoanId(Integer loanId) {

		this.loanId = loanId;
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
	 * Gets the branch.
	 *
	 * @return the branch
	 */
	public String getBranch() {

		return branch;
	}

	/**
	 * Sets the branch.
	 *
	 * @param branch the branch to set
	 */
	public void setBranch(String branch) {

		this.branch = branch;
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
	 * Gets the reference.
	 *
	 * @return the reference
	 */
	public String getReference() {

		return reference;
	}

	/**
	 * Sets the reference.
	 *
	 * @param reference the reference to set
	 */
	public void setReference(String reference) {

		this.reference = reference;
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

	/**
	 * Gets the collateral type.
	 *
	 * @return the collateralType
	 */
	public String getCollateralType() {

		return collateralType;
	}

	/**
	 * Sets the collateral type.
	 *
	 * @param collateralType the collateralType to set
	 */
	public void setCollateralType(String collateralType) {

		this.collateralType = collateralType;
	}

	/**
	 * Gets the original cross value.
	 *
	 * @return the originalCrossValue
	 */
	public BigDecimal getOriginalCrossValue() {

		return originalCrossValue;
	}

	/**
	 * Sets the original cross value.
	 *
	 * @param originalCrossValue the originalCrossValue to set
	 */
	public void setOriginalCrossValue(BigDecimal originalCrossValue) {

		this.originalCrossValue = originalCrossValue;
	}

	/**
	 * Gets the gross value.
	 *
	 * @return the grossValue
	 */
	public BigDecimal getGrossValue() {

		return grossValue;
	}

	/**
	 * Sets the gross value.
	 *
	 * @param grossValue the grossValue to set
	 */
	public void setGrossValue(BigDecimal grossValue) {

		this.grossValue = grossValue;
	}

	/**
	 * Gets the realised value.
	 *
	 * @return the realisedValue
	 */
	public BigDecimal getRealisedValue() {

		return realisedValue;
	}

	/**
	 * Sets the realised value.
	 *
	 * @param realisedValue the realisedValue to set
	 */
	public void setRealisedValue(BigDecimal realisedValue) {

		this.realisedValue = realisedValue;
	}

	/**
	 * Gets the fixed cost.
	 *
	 * @return the fixedCost
	 */
	public BigDecimal getFixedCost() {

		return fixedCost;
	}

	/**
	 * Sets the fixed cost.
	 *
	 * @param fixedCost the fixedCost to set
	 */
	public void setFixedCost(BigDecimal fixedCost) {

		this.fixedCost = fixedCost;
	}

	/**
	 * Gets the net value.
	 *
	 * @return the netValue
	 */
	public BigDecimal getNetValue() {

		return netValue;
	}

	/**
	 * Sets the net value.
	 *
	 * @param netValue the netValue to set
	 */
	public void setNetValue(BigDecimal netValue) {

		this.netValue = netValue;
	}

	/**
	 * Gets the account number extern.
	 *
	 * @return the account number extern
	 */
	public String getAccountNumberExtern() {

		return accountNumberExtern;
	}

	/**
	 * Sets the account number extern.
	 *
	 * @param accountNumberExtern the new account number extern
	 */
	public void setAccountNumberExtern(String accountNumberExtern) {

		this.accountNumberExtern = accountNumberExtern;
	}

}
