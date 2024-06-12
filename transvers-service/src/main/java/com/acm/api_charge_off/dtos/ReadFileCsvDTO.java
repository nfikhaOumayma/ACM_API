/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_charge_off.dtos;

import java.io.Serializable;

import com.acm.utils.dtos.GenericDTO;

/**
 * The Class ReadFileCsvDTO.
 */
public class ReadFileCsvDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2776414596143183523L;

	/** The cu account id. */
	private String cuAccountId;

	/** The orginal product id. */
	private String orginalProductId;

	/** The charge off product id. */
	private String chargeOffProductId;

	/**
	 * Gets the cu account id.
	 *
	 * @return the cu account id
	 */
	public String getCuAccountId() {

		return cuAccountId;
	}

	/**
	 * Sets the cu account id.
	 *
	 * @param cuAccountId the new cu account id
	 */
	public void setCuAccountId(String cuAccountId) {

		this.cuAccountId = cuAccountId;
	}

	/**
	 * Gets the orginal product id.
	 *
	 * @return the orginal product id
	 */
	public String getOrginalProductId() {

		return orginalProductId;
	}

	/**
	 * Sets the orginal product id.
	 *
	 * @param orginalProductId the new orginal product id
	 */
	public void setOrginalProductId(String orginalProductId) {

		this.orginalProductId = orginalProductId;
	}

	/**
	 * Gets the charge off product id.
	 *
	 * @return the charge off product id
	 */
	public String getChargeOffProductId() {

		return chargeOffProductId;
	}

	/**
	 * Sets the charge off product id.
	 *
	 * @param chargeOffProductId the new charge off product id
	 */
	public void setChargeOffProductId(String chargeOffProductId) {

		this.chargeOffProductId = chargeOffProductId;
	}

	/**
	 * Instantiates a new read file csv DTO.
	 */
	public ReadFileCsvDTO() {

		super();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ReadFileCsvDTO [cuAccountId=" + cuAccountId + ", orginalProductId="
				+ orginalProductId + ", chargeOffProductId=" + chargeOffProductId + "]";
	}

}
