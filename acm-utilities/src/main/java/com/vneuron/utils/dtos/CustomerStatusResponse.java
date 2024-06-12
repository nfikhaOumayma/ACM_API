/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.vneuron.utils.dtos;

/**
 * {@link CustomerStatusResponse} class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class CustomerStatusResponse {

	/** The statut. */
	public String status;

	/** The system id. */
	public String systemId;

	/** The customer id. */
	public Long customerId;

	/** The system name. */
	public String systemName;

	/** The service type. */
	public String ServiceType;

	/**
	 * Gets the status.
	 *
	 * @return the status
	 */
	public String getStatus() {

		return status;
	}

	/**
	 * Sets the status.
	 *
	 * @param status the new status
	 */
	public void setStatus(String status) {

		this.status = status;
	}

	/**
	 * Gets the service type.
	 *
	 * @return the service type
	 */
	public String getServiceType() {

		return ServiceType;
	}

	/**
	 * Sets the service type.
	 *
	 * @param serviceType the new service type
	 */
	public void setServiceType(String serviceType) {

		ServiceType = serviceType;
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

}
