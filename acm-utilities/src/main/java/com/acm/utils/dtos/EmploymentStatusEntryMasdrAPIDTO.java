/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class EmploymentStatusEntryMasdrAPIDTO.
 */
public class EmploymentStatusEntryMasdrAPIDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1542797218877802223L;

	/** The id. */
	@JsonProperty("id")
	private String id;

	/** The full name. */
	@JsonProperty("fullName")
	private String fullName;

	/** The basic wage. */
	@JsonProperty("basicWage")
	private double basicWage;

	/** The housing allowance. */
	@JsonProperty("housingAllowance")
	private double housingAllowance;

	/** The other allowance. */
	@JsonProperty("otherAllowance")
	private double otherAllowance;

	/** The full wage. */
	@JsonProperty("fullWage")
	private double fullWage;

	/** The employer name. */
	@JsonProperty("employerName")
	private String employerName;

	/** The national unified no. */
	@JsonProperty("nationalUnifiedNo")
	private String nationalUnifiedNo;

	/** The date of joining. */
	@JsonProperty("dateOfJoining")
	private String dateOfJoining;

	/** The working months. */
	@JsonProperty("workingMonths")
	private String workingMonths;

	/** The employment status. */
	@JsonProperty("employmentStatus")
	private String employmentStatus;

	/** The salary starting date. */
	@JsonProperty("salaryStartingDate")
	private String salaryStartingDate;

	/** The establishment activity. */
	@JsonProperty("establishmentActivity")
	private String establishmentActivity;

	/** The commercial registration number. */
	@JsonProperty("commercialRegistrationNumber")
	private String commercialRegistrationNumber;

	/** The legal entity. */
	@JsonProperty("legalEntity")
	private String legalEntity;

	/** The date of birth. */
	@JsonProperty("dateOfBirth")
	private String dateOfBirth;

	/** The nationality. */
	@JsonProperty("nationality")
	private String nationality;

	/** The gosinumber. */
	@JsonProperty("gosinumber")
	private String gosinumber;

	/**
	 * Instantiates a new employment status entry masdr APIDTO.
	 */
	public EmploymentStatusEntryMasdrAPIDTO() {

		super();
	}

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public String getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the new id
	 */
	public void setId(String id) {

		this.id = id;
	}

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
	public double getBasicWage() {

		return basicWage;
	}

	/**
	 * Sets the basic wage.
	 *
	 * @param basicWage the new basic wage
	 */
	public void setBasicWage(double basicWage) {

		this.basicWage = basicWage;
	}

	/**
	 * Gets the housing allowance.
	 *
	 * @return the housing allowance
	 */
	public double getHousingAllowance() {

		return housingAllowance;
	}

	/**
	 * Sets the housing allowance.
	 *
	 * @param housingAllowance the new housing allowance
	 */
	public void setHousingAllowance(double housingAllowance) {

		this.housingAllowance = housingAllowance;
	}

	/**
	 * Gets the other allowance.
	 *
	 * @return the other allowance
	 */
	public double getOtherAllowance() {

		return otherAllowance;
	}

	/**
	 * Sets the other allowance.
	 *
	 * @param otherAllowance the new other allowance
	 */
	public void setOtherAllowance(double otherAllowance) {

		this.otherAllowance = otherAllowance;
	}

	/**
	 * Gets the full wage.
	 *
	 * @return the full wage
	 */
	public double getFullWage() {

		return fullWage;
	}

	/**
	 * Sets the full wage.
	 *
	 * @param fullWage the new full wage
	 */
	public void setFullWage(double fullWage) {

		this.fullWage = fullWage;
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
	 * Gets the national unified no.
	 *
	 * @return the national unified no
	 */
	public String getNationalUnifiedNo() {

		return nationalUnifiedNo;
	}

	/**
	 * Sets the national unified no.
	 *
	 * @param nationalUnifiedNo the new national unified no
	 */
	public void setNationalUnifiedNo(String nationalUnifiedNo) {

		this.nationalUnifiedNo = nationalUnifiedNo;
	}

	/**
	 * Gets the date of joining.
	 *
	 * @return the date of joining
	 */
	public String getDateOfJoining() {

		return dateOfJoining;
	}

	/**
	 * Sets the date of joining.
	 *
	 * @param dateOfJoining the new date of joining
	 */
	public void setDateOfJoining(String dateOfJoining) {

		this.dateOfJoining = dateOfJoining;
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
	 * Gets the salary starting date.
	 *
	 * @return the salary starting date
	 */
	public String getSalaryStartingDate() {

		return salaryStartingDate;
	}

	/**
	 * Sets the salary starting date.
	 *
	 * @param salaryStartingDate the new salary starting date
	 */
	public void setSalaryStartingDate(String salaryStartingDate) {

		this.salaryStartingDate = salaryStartingDate;
	}

	/**
	 * Gets the establishment activity.
	 *
	 * @return the establishment activity
	 */
	public String getEstablishmentActivity() {

		return establishmentActivity;
	}

	/**
	 * Sets the establishment activity.
	 *
	 * @param establishmentActivity the new establishment activity
	 */
	public void setEstablishmentActivity(String establishmentActivity) {

		this.establishmentActivity = establishmentActivity;
	}

	/**
	 * Gets the commercial registration number.
	 *
	 * @return the commercial registration number
	 */
	public String getCommercialRegistrationNumber() {

		return commercialRegistrationNumber;
	}

	/**
	 * Sets the commercial registration number.
	 *
	 * @param commercialRegistrationNumber the new commercial registration number
	 */
	public void setCommercialRegistrationNumber(String commercialRegistrationNumber) {

		this.commercialRegistrationNumber = commercialRegistrationNumber;
	}

	/**
	 * Gets the legal entity.
	 *
	 * @return the legal entity
	 */
	public String getLegalEntity() {

		return legalEntity;
	}

	/**
	 * Sets the legal entity.
	 *
	 * @param legalEntity the new legal entity
	 */
	public void setLegalEntity(String legalEntity) {

		this.legalEntity = legalEntity;
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
	 * Gets the nationality.
	 *
	 * @return the nationality
	 */
	public String getNationality() {

		return nationality;
	}

	/**
	 * Sets the nationality.
	 *
	 * @param nationality the new nationality
	 */
	public void setNationality(String nationality) {

		this.nationality = nationality;
	}

	/**
	 * Gets the gosinumber.
	 *
	 * @return the gosinumber
	 */
	public String getGosinumber() {

		return gosinumber;
	}

	/**
	 * Sets the gosinumber.
	 *
	 * @param gosinumber the new gosinumber
	 */
	public void setGosinumber(String gosinumber) {

		this.gosinumber = gosinumber;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "EmploymentStatusEntryMasdrAPIDTO [id=" + id + ", fullName=" + fullName
				+ ", basicWage=" + basicWage + ", housingAllowance=" + housingAllowance
				+ ", otherAllowance=" + otherAllowance + ", fullWage=" + fullWage
				+ ", employerName=" + employerName + ", nationalUnifiedNo=" + nationalUnifiedNo
				+ ", dateOfJoining=" + dateOfJoining + ", workingMonths=" + workingMonths
				+ ", employmentStatus=" + employmentStatus + ", salaryStartingDate="
				+ salaryStartingDate + ", establishmentActivity=" + establishmentActivity
				+ ", commercialRegistrationNumber=" + commercialRegistrationNumber
				+ ", legalEntity=" + legalEntity + ", dateOfBirth=" + dateOfBirth + ", nationality="
				+ nationality + ", gosinumber=" + gosinumber + "]";
	}

}
