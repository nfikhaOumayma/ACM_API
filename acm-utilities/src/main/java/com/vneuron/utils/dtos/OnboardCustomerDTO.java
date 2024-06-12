/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.vneuron.utils.dtos;

/**
 * {@link OnboardCustomerDTO } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class OnboardCustomerDTO {

	/** The system name. */
	public String systemName;

	/** The system id. */
	public String systemId;

	/** The form id. */
	public int formId;

	/** The items. */
	public Items items;

	/**
	 * Gets the system name.
	 *
	 * @return the systemName
	 */
	public String getSystemName() {

		return systemName;
	}

	/**
	 * Sets the system name.
	 *
	 * @param systemName the systemName to set
	 */
	public void setSystemName(String systemName) {

		this.systemName = systemName;
	}

	/**
	 * Gets the system id.
	 *
	 * @return the systemId
	 */
	public String getSystemId() {

		return systemId;
	}

	/**
	 * Sets the system id.
	 *
	 * @param systemId the systemId to set
	 */
	public void setSystemId(String systemId) {

		this.systemId = systemId;
	}

	/**
	 * Gets the form id.
	 *
	 * @return the formId
	 */
	public int getFormId() {

		return formId;
	}

	/**
	 * Sets the form id.
	 *
	 * @param formId the formId to set
	 */
	public void setFormId(int formId) {

		this.formId = formId;
	}

	/**
	 * Gets the items.
	 *
	 * @return the items
	 */
	public Items getItems() {

		return items;
	}

	/**
	 * Sets the items.
	 *
	 * @param items the items to set
	 */
	public void setItems(Items items) {

		this.items = items;
	}

}
