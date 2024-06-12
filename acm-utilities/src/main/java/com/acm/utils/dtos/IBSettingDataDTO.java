/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link IBSettingDataDTO} class.
 *
 * @author AbdelkarimTurki
 * @since 0.20.0
 */
public class IBSettingDataDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3241481868840051983L;

	/** The id. */
	private Long id;

	/** The date insertion. */
	private Date dateInsertion;

	/** The code. */
	private String code;

	/** The value. */
	private String value;

	/** The enabled. */
	private Boolean enabled;

	/** The parentId. */
	private Long parentId;

	/** The location. */
	private String brancheLocalisation;

	/** branch phone number. */
	private String phoneNumber;

	/**
	 * Instantiates a new IB setting data DTO.
	 */
	public IBSettingDataDTO() {

		/**
		 * EMPTY
		 */
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
	 * Gets the date insertion.
	 *
	 * @return the date insertion
	 */
	public Date getDateInsertion() {

		return dateInsertion;
	}

	/**
	 * Sets the date insertion.
	 *
	 * @param dateInsertion the new date insertion
	 */
	public void setDateInsertion(Date dateInsertion) {

		this.dateInsertion = dateInsertion;
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
	 * Gets the enabled.
	 *
	 * @return the enabled
	 */
	public Boolean getEnabled() {

		return enabled;
	}

	/**
	 * Sets the enabled.
	 *
	 * @param enabled the new enabled
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
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

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "IBSettingDataDTO [id=" + id + ", dateInsertion=" + dateInsertion + ", code=" + code
				+ ", value=" + value + ", enabled=" + enabled + ", parentId=" + parentId + "]";
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
