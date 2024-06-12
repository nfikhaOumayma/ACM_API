/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link AMLDataDTO} class.
 * 
 * @author HaythemBenizid
 * @since 1.0.0
 */
public class AMLDataDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -289224554708350840L;

	/** The id. */
	private Long id;

	/** The reference case. */
	private String referenceCase;

	/** The reference in file. */
	private Long referenceInFile;

	/** The name. */
	private String name;

	/** The identity number. */
	private String identityNumber;

	/** The date of birth. */
	private String dateOfBirth;

	/** The updated data. */
	private String updatedData;

	/**
	 * Instantiates a new AML data DTO.
	 */
	public AMLDataDTO() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new AML data DTO (used in SEARCH AML-CHECK method).
	 *
	 * @param name the name
	 * @param identityNumber the identity number
	 * @param dateOfBirth the date of birth
	 */
	public AMLDataDTO(String name, String identityNumber, String dateOfBirth) {

		this.name = name;
		this.identityNumber = identityNumber;
		this.dateOfBirth = dateOfBirth;
	}

	/**
	 * Instantiates a new AML data DTO.
	 *
	 * @param id the id
	 * @param referenceCase the reference case
	 * @param referenceInFile the reference in file
	 * @param name the name
	 * @param identityNumber the identity number
	 * @param dateOfBirth the date of birth
	 * @param updatedData the updated data
	 */
	public AMLDataDTO(Long id, String referenceCase, Long referenceInFile, String name,
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

		return "AMLDataDTO [id=" + id + ", referenceCase=" + referenceCase + ", referenceInFile="
				+ referenceInFile + ", name=" + name + ", identityNumber=" + identityNumber
				+ ", dateOfBirth=" + dateOfBirth + ", updatedData=" + updatedData + "]";
	}

}
