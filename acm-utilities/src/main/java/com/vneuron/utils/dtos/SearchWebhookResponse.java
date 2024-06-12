/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.vneuron.utils.dtos;

/**
 * {@link SearchWebhookResponse } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class SearchWebhookResponse {

	/** The system id. */
	public String systemId;

	/** The customer id. */
	public Long customerId;

	/** The system name. */
	public String systemName;

	/** The service type. */
	public String serviceType;

	/** The search query id. */
	public Long searchQueryId;

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
	 * Gets the customer id.
	 *
	 * @return the customerId
	 */
	public Long getCustomerId() {

		return customerId;
	}

	/**
	 * Sets the customer id.
	 *
	 * @param customerId the customerId to set
	 */
	public void setCustomerId(Long customerId) {

		this.customerId = customerId;
	}

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
	 * Gets the service type.
	 *
	 * @return the serviceType
	 */
	public String getServiceType() {

		return serviceType;
	}

	/**
	 * Sets the service type.
	 *
	 * @param serviceType the serviceType to set
	 */
	public void setServiceType(String serviceType) {

		this.serviceType = serviceType;
	}

	/**
	 * Gets the search query id.
	 *
	 * @return the searchQueryId
	 */
	public Long getSearchQueryId() {

		return searchQueryId;
	}

	/**
	 * Sets the search query id.
	 *
	 * @param searchQueryId the searchQueryId to set
	 */
	public void setSearchQueryId(Long searchQueryId) {

		this.searchQueryId = searchQueryId;
	}

}
