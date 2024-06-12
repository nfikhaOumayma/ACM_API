/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class RespInfoDakhliApiDTO.
 */
public class RespInfoDakhliApiDTO extends GenericDTO {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5959843300720894945L;

	/** The full name. */
	@JsonProperty("fullName")
	private String fullName;

	/** The basic wage. */
	@JsonProperty("basicWage")
	private int basicWage;

	/** The housing allowance. */
	@JsonProperty("housingAllowance")
	private int housingAllowance;

	/** The other allowance. */
	@JsonProperty("otherAllowance")
	private int otherAllowance;

	/** The employer name. */
	@JsonProperty("employerName")
	private String employerName;

	/** The working months. */
	@JsonProperty("workingMonths")
	private String workingMonths;

	/** The employment status. */
	@JsonProperty("employmentStatus")
	private String employmentStatus;

	/**
	 * Gets the full name.
	 *
	 * @return the full name
	 */
	public String getFullName() {

		return fullName;
	}

	/**
	 * Sets the full name.
	 *
	 * @param fullName the new full name
	 */
	public void setFullName(String fullName) {

		this.fullName = fullName;
	}

	/**
	 * Gets the basic wage.
	 *
	 * @return the basic wage
	 */
	public int getBasicWage() {

		return basicWage;
	}

	/**
	 * Sets the basic wage.
	 *
	 * @param basicWage the new basic wage
	 */
	public void setBasicWage(int basicWage) {

		this.basicWage = basicWage;
	}

	/**
	 * Gets the housing allowance.
	 *
	 * @return the housing allowance
	 */
	public int getHousingAllowance() {

		return housingAllowance;
	}

	/**
	 * Sets the housing allowance.
	 *
	 * @param housingAllowance the new housing allowance
	 */
	public void setHousingAllowance(int housingAllowance) {

		this.housingAllowance = housingAllowance;
	}

	/**
	 * Gets the other allowance.
	 *
	 * @return the other allowance
	 */
	public int getOtherAllowance() {

		return otherAllowance;
	}

	/**
	 * Sets the other allowance.
	 *
	 * @param otherAllowance the new other allowance
	 */
	public void setOtherAllowance(int otherAllowance) {

		this.otherAllowance = otherAllowance;
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
	 * Gets the working months.
	 *
	 * @return the working months
	 */
	public String getWorkingMonths() {

		return workingMonths;
	}

	/**
	 * Sets the working months.
	 *
	 * @param workingMonths the new working months
	 */
	public void setWorkingMonths(String workingMonths) {

		this.workingMonths = workingMonths;
	}

	/**
	 * Gets the employment status.
	 *
	 * @return the employment status
	 */
	public String getEmploymentStatus() {

		return employmentStatus;
	}

	/**
	 * Sets the employment status.
	 *
	 * @param employmentStatus the new employment status
	 */
	public void setEmploymentStatus(String employmentStatus) {

		this.employmentStatus = employmentStatus;
	}

	/**
	 * Instantiates a new resp info dakhli api DTO.
	 */
	public RespInfoDakhliApiDTO() {

		super();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "RespInfoDakhliApiDTO [fullName=" + fullName + ", basicWage=" + basicWage
				+ ", housingAllowance=" + housingAllowance + ", otherAllowance=" + otherAllowance
				+ ", employerName=" + employerName + ", workingMonths=" + workingMonths
				+ ", employmentStatus=" + employmentStatus + "]";
	}

}
