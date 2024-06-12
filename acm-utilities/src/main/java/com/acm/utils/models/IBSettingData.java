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
 * The persistent class for the IB_SETTING_DATA database table. {@link IBSettingData} class.
 * 
 * @author AbdelkarimTurki
 * @since 0.20.0
 */
@Entity
@Table(name = "IB_SETTING_DATA")
public class IBSettingData extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4789431309838536226L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

	/** The code. */
	@Column(name = "IB_CODE", length = 256)
	private String code;

	/** The value. */
	@Column(name = "IB_VALUE", length = 512)
	private String value;

	/** The parentId. */
	@Column(name = "IB_PARENT_ID", length = 512)
	private Long parentId;

	/** The location. */
	@Column(name = "IB_BRANCHE_LOCALISATION", length = 256)
	private String brancheLocalisation;

	/** branch phone number. */
	@Column(name = "PHONE_NUMBER", length = 256)
	private String phoneNumber;

	/**
	 * Instantiates a new IB setting data.
	 */
	public IBSettingData() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new IB setting data.
	 *
	 * @param id the id
	 * @param code the code
	 * @param value the value
	 * @param parentId the parent id
	 */
	public IBSettingData(Long id, String code, String value, Long parentId) {

		this.id = id;
		this.code = code;
		this.value = value;
		this.parentId = parentId;
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
	 * Gets the code.
	 *
	 * @return the code
	 */
	public String getCode() {

		return code;
	}

	/**
	 * Sets the code.
	 *
	 * @param code the new code
	 */
	public void setCode(String code) {

		this.code = code;
	}

	/**
	 * Gets the value.
	 *
	 * @return the value
	 */
	public String getValue() {

		return value;
	}

	/**
	 * Sets the value.
	 *
	 * @param value the new value
	 */
	public void setValue(String value) {

		this.value = value;
	}

	/**
	 * Gets the parent id.
	 *
	 * @return the parent id
	 */
	public Long getParentId() {

		return parentId;
	}

	/**
	 * Sets the parent id.
	 *
	 * @param parentId the new parent id
	 */
	public void setParentId(Long parentId) {

		this.parentId = parentId;
	}

	/**
	 * Gets the branche localisation.
	 *
	 * @return the brancheLocalisation
	 */
	public String getBrancheLocalisation() {

		return brancheLocalisation;
	}

	/**
	 * Sets the branche localisation.
	 *
	 * @param brancheLocalisation the brancheLocalisation to set
	 */
	public void setBrancheLocalisation(String brancheLocalisation) {

		this.brancheLocalisation = brancheLocalisation;
	}

	/**
	 * Gets the phone number.
	 *
	 * @return the phoneNumber
	 */
	public String getPhoneNumber() {

		return phoneNumber;
	}

	/**
	 * Sets the phone number.
	 *
	 * @param phoneNumber the phoneNumber to set
	 */
	public void setPhoneNumber(String phoneNumber) {

		this.phoneNumber = phoneNumber;
	}
}
