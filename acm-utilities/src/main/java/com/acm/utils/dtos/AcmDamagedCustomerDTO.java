/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link AcmDamagedCustomerDTO} class.
 *
 * @author idridi
 * @since 1.0.8
 */
public class AcmDamagedCustomerDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1043753459426593908L;

	/** The id. */
	private Long id;

	/** The id customer externe. */
	private Long idCustomerExterne;

	/** The damaged data. */
	private String damagedData;

	/** The exception details. */
	private String exceptionDetails;

	/**
	 * Instantiates a new acm damaged customer DTO.
	 */
	public AcmDamagedCustomerDTO() {

	}

	/**
	 * Instantiates a new acm damaged customer DTO.
	 *
	 * @param idCustomerExterne the id customer externe
	 * @param damagedData the damaged data
	 * @param exceptionDetails the exception details
	 */
	public AcmDamagedCustomerDTO(Long idCustomerExterne, String damagedData,
			String exceptionDetails) {

		this.idCustomerExterne = idCustomerExterne;
		this.damagedData = damagedData;
		this.exceptionDetails = exceptionDetails;
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
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the id customer externe.
	 *
	 * @return the id customer externe
	 */
	public Long getIdCustomerExterne() {

		return idCustomerExterne;
	}

	/**
	 * Sets the id customer externe.
	 *
	 * @param idCustomerExterne the new id customer externe
	 */
	public void setIdCustomerExterne(Long idCustomerExterne) {

		this.idCustomerExterne = idCustomerExterne;
	}

	/**
	 * Gets the damaged data.
	 *
	 * @return the damaged data
	 */
	public String getDamagedData() {

		return damagedData;
	}

	/**
	 * Sets the damaged data.
	 *
	 * @param damagedData the new damaged data
	 */
	public void setDamagedData(String damagedData) {

		this.damagedData = damagedData;
	}

	/**
	 * Gets the exception details.
	 *
	 * @return the exception details
	 */
	public String getExceptionDetails() {

		return exceptionDetails;
	}

	/**
	 * Sets the exception details.
	 *
	 * @param exceptionDetails the new exception details
	 */
	public void setExceptionDetails(String exceptionDetails) {

		this.exceptionDetails = exceptionDetails;
	}

}
