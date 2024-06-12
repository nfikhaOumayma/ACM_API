/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * The persistent class for the ACM_AML_DATA database table. {@link AMLDate} class.
 * 
 * @author HaythemBenizid
 * @since 1.0.0
 */
@Entity
@Table(name = "ACM_AML_DATA")
public class AMLData extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3685732388507689372L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_AML_DATA", unique = true, nullable = false)
	private Long id;

	/** The reference case. */
	@Column(name = "REFERENCE_CASE")
	private String referenceCase;

	/** The reference in file. */
	@Column(name = "REFERENCE_IN_FILE")
	private Long referenceInFile;

	/** The name. */
	@Column(name = "NAME")
	private String name;

	/** The identity number. */
	@Column(name = "IDENTITY_NUMBER")
	private String identityNumber;

	/** The date of birth. */
	@Column(name = "DATE_OF_BIRTH")
	private String dateOfBirth;

	/** The updated data. */
	@Column(name = "UPDATED_DATA")
	private String updatedData;

	/**
	 * Instantiates a new AML data.
	 */
	public AMLData() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new AML data.
	 *
	 * @param id the id
	 * @param referenceCase the reference case
	 * @param referenceInFile the reference in file
	 * @param name the name
	 * @param identityNumber the identity number
	 * @param dateOfBirth the date of birth
	 * @param updatedData the updated data
	 */
	public AMLData(Long id, String referenceCase, Long referenceInFile, String name,
			String identityNumber, String dateOfBirth, String updatedData) {

		this.id = id;
		this.referenceCase = referenceCase;
		this.referenceInFile = referenceInFile;
		this.name = name;
		this.identityNumber = identityNumber;
		this.dateOfBirth = dateOfBirth;
		this.updatedData = updatedData;
	}

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the reference case.
	 *
	 * @return the referenceCase
	 */
	public String getReferenceCase() {

		return referenceCase;
	}

	/**
	 * Sets the reference case.
	 *
	 * @param referenceCase the referenceCase to set
	 */
	public void setReferenceCase(String referenceCase) {

		this.referenceCase = referenceCase;
	}

	/**
	 * Gets the reference in file.
	 *
	 * @return the referenceInFile
	 */
	public Long getReferenceInFile() {

		return referenceInFile;
	}

	/**
	 * Sets the reference in file.
	 *
	 * @param referenceInFile the referenceInFile to set
	 */
	public void setReferenceInFile(Long referenceInFile) {

		this.referenceInFile = referenceInFile;
	}

	/**
	 * Gets the name.
	 *
	 * @return the name
	 */
	public String getName() {

		return name;
	}

	/**
	 * Sets the name.
	 *
	 * @param name the name to set
	 */
	public void setName(String name) {

		this.name = name;
	}

	/**
	 * Gets the identity number.
	 *
	 * @return the identityNumber
	 */
	public String getIdentityNumber() {

		return identityNumber;
	}

	/**
	 * Sets the identity number.
	 *
	 * @param identityNumber the identityNumber to set
	 */
	public void setIdentityNumber(String identityNumber) {

		this.identityNumber = identityNumber;
	}

	/**
	 * Gets the date of birth.
	 *
	 * @return the dateOfBirth
	 */
	public String getDateOfBirth() {

		return dateOfBirth;
	}

	/**
	 * Sets the date of birth.
	 *
	 * @param dateOfBirth the dateOfBirth to set
	 */
	public void setDateOfBirth(String dateOfBirth) {

		this.dateOfBirth = dateOfBirth;
	}

	/**
	 * Gets the updated data.
	 *
	 * @return the updatedData
	 */
	public String getUpdatedData() {

		return updatedData;
	}

	/**
	 * Sets the updated data.
	 *
	 * @param updatedData the updatedData to set
	 */
	public void setUpdatedData(String updatedData) {

		this.updatedData = updatedData;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AMLData [id=" + id + ", referenceCase=" + referenceCase + ", referenceInFile="
				+ referenceInFile + ", name=" + name + ", identityNumber=" + identityNumber
				+ ", dateOfBirth=" + dateOfBirth + ", updatedData=" + updatedData + "]";
	}

}
