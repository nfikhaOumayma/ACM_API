/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class OccupationRequestSimahApiDTO.
 */
public class OccupationRequestSimahApiDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2171023956897580503L;

	/** The occupation. */
	@JsonProperty("occupation")
	private String occupation;

	/** The self employment. */
	@JsonProperty("selfEmployment")
	private boolean selfEmployment;

	/** The certificate reg no. */
	@JsonProperty("certificateRegNo")
	private String certificateRegNo;

	/** The business type. */
	@JsonProperty("businessType")
	private int businessType;

	/** The employer type. */
	@JsonProperty("employerType")
	private int employerType;

	/** The employer name. */
	@JsonProperty("employerName")
	private String employerName;

	/** The basic income. */
	@JsonProperty("basicIncome")
	private BigDecimal basicIncome;

	/** The total monthly income. */
	@JsonProperty("totalMonthlyIncome")
	private BigDecimal totalMonthlyIncome;

	/** The address. */
	@JsonProperty("address")
	private RequestAddressSimahApiDTO address;

	/**
	 * Instantiates a new occupation request simah api DTO.
	 */
	public OccupationRequestSimahApiDTO() {

		super();
	}

	/**
	 * Gets the occupation.
	 *
	 * @return the occupation
	 */
	public String getOccupation() {

		return occupation;
	}

	/**
	 * Sets the occupation.
	 *
	 * @param occupation the new occupation
	 */
	public void setOccupation(String occupation) {

		this.occupation = occupation;
	}

	/**
	 * Checks if is self employment.
	 *
	 * @return true, if is self employment
	 */
	public boolean isSelfEmployment() {

		return selfEmployment;
	}

	/**
	 * Sets the self employment.
	 *
	 * @param selfEmployment the new self employment
	 */
	public void setSelfEmployment(boolean selfEmployment) {

		this.selfEmployment = selfEmployment;
	}

	/**
	 * Gets the certificate reg no.
	 *
	 * @return the certificate reg no
	 */
	public String getCertificateRegNo() {

		return certificateRegNo;
	}

	/**
	 * Sets the certificate reg no.
	 *
	 * @param certificateRegNo the new certificate reg no
	 */
	public void setCertificateRegNo(String certificateRegNo) {

		this.certificateRegNo = certificateRegNo;
	}

	/**
	 * Gets the business type.
	 *
	 * @return the business type
	 */
	public int getBusinessType() {

		return businessType;
	}

	/**
	 * Sets the business type.
	 *
	 * @param businessType the new business type
	 */
	public void setBusinessType(int businessType) {

		this.businessType = businessType;
	}

	/**
	 * Gets the employer type.
	 *
	 * @return the employer type
	 */
	public int getEmployerType() {

		return employerType;
	}

	/**
	 * Sets the employer type.
	 *
	 * @param employerType the new employer type
	 */
	public void setEmployerType(int employerType) {

		this.employerType = employerType;
	}

	/**
	 * Gets the employer name.
	 *
	 * @return the employer name
	 */
	public String getEmployerName() {

		return employerName;
	}

	/**
	 * Sets the employer name.
	 *
	 * @param employerName the new employer name
	 */
	public void setEmployerName(String employerName) {

		this.employerName = employerName;
	}

	/**
	 * Gets the basic income.
	 *
	 * @return the basic income
	 */
	public BigDecimal getBasicIncome() {

		return basicIncome;
	}

	/**
	 * Sets the basic income.
	 *
	 * @param basicIncome the new basic income
	 */
	public void setBasicIncome(BigDecimal basicIncome) {

		this.basicIncome = basicIncome;
	}

	/**
	 * Gets the total monthly income.
	 *
	 * @return the total monthly income
	 */
	public BigDecimal getTotalMonthlyIncome() {

		return totalMonthlyIncome;
	}

	/**
	 * Sets the total monthly income.
	 *
	 * @param totalMonthlyIncome the new total monthly income
	 */
	public void setTotalMonthlyIncome(BigDecimal totalMonthlyIncome) {

		this.totalMonthlyIncome = totalMonthlyIncome;
	}

	/**
	 * Gets the address.
	 *
	 * @return the address
	 */
	public RequestAddressSimahApiDTO getAddress() {

		return address;
	}

	/**
	 * Sets the address.
	 *
	 * @param address the new address
	 */
	public void setAddress(RequestAddressSimahApiDTO address) {

		this.address = address;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "OccupationRequestSimahApiDTO [occupation=" + occupation + ", selfEmployment="
				+ selfEmployment + ", certificateRegNo=" + certificateRegNo + ", businessType="
				+ businessType + ", employerType=" + employerType + ", employerName=" + employerName
				+ ", basicIncome=" + basicIncome + ", totalMonthlyIncome=" + totalMonthlyIncome
				+ ", address=" + address + "]";
	}

}
