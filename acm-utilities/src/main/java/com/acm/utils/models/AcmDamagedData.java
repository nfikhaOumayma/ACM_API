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
 * {@link AcmDamagedData} class.
 *
 * @author idridi
 * @since 1.0.8
 */
@Entity
@Table(name = "ACM_DAMAGED_DATA")
public class AcmDamagedData extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8087505527109345743L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_DAMAGED_DATA", unique = true, nullable = false)
	private Long id;

	/** The id customer externe. */
	@Column(name = "ID_CUSTOMER_EXTERNE")
	private Long idCustomerExterne;

	/** The damaged data. */
	@Column(name = "DAMAGED_OBJECT_DATA")
	private String damagedData;

	/** The exception details. */
	@Column(name = "EXCEPTION_DETAILS")
	private String exceptionDetails;

	/** The id account extern. */
	@Column(name = "ID_ACCOUNT_EXTERN")
	private Long idAccountExtern;

	/**
	 * Instantiates a new acm damaged customer.
	 */
	public AcmDamagedData() {

	}

	/**
	 * Instantiates a new acm damaged customer.
	 *
	 * @param id the id
	 * @param idCustomerExterne the id customer externe
	 * @param damagedData the damaged data
	 * @param exceptionDetails the exception details
	 */
	public AcmDamagedData(Long id, Long idCustomerExterne, String damagedData,
			String exceptionDetails) {

		super();
		this.id = id;
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

	/**
	 * Gets the id account extern.
	 *
	 * @return the id account extern
	 */
	public Long getIdAccountExtern() {

		return idAccountExtern;
	}

	/**
	 * Sets the id account extern.
	 *
	 * @param idAccountExtern the new id account extern
	 */
	public void setIdAccountExtern(Long idAccountExtern) {

		this.idAccountExtern = idAccountExtern;
	}

}
