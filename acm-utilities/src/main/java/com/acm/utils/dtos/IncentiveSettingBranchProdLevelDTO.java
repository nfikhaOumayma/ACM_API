/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import org.dozer.Mapping;

/**
 * {@link IncentiveSettingBranchProdLevelDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class IncentiveSettingBranchProdLevelDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4249656469462260144L;

	/** The id. */
	private Long id;

	/** The product id. */
	private Long productId;

	/** The frequency. */
	@Mapping("frequency")
	private IncentiveSettingConstantDTO frequency;

	/** The role. */
	private String role;

	/** The min amount. */
	private Long minAmount;

	/** The min number customer. */
	private Long minNumberCustomer;

	/** The ordre. */
	private Long ordre;

	/**
	 * Instantiates a new incentive setting branch prod level DTO.
	 */
	public IncentiveSettingBranchProdLevelDTO() {

		/*
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
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the product id.
	 *
	 * @return the productId
	 */
	public Long getProductId() {

		return productId;
	}

	/**
	 * Sets the product id.
	 *
	 * @param productId the productId to set
	 */
	public void setProductId(Long productId) {

		this.productId = productId;
	}

	/**
	 * Gets the role.
	 *
	 * @return the role
	 */
	public String getRole() {

		return role;
	}

	/**
	 * Sets the role.
	 *
	 * @param role the role to set
	 */
	public void setRole(String role) {

		this.role = role;
	}

	/**
	 * Gets the min amount.
	 *
	 * @return the minAmount
	 */
	public Long getMinAmount() {

		return minAmount;
	}

	/**
	 * Sets the min amount.
	 *
	 * @param minAmount the minAmount to set
	 */
	public void setMinAmount(Long minAmount) {

		this.minAmount = minAmount;
	}

	/**
	 * Gets the min number customer.
	 *
	 * @return the minNumberCustomer
	 */
	public Long getMinNumberCustomer() {

		return minNumberCustomer;
	}

	/**
	 * Sets the min number customer.
	 *
	 * @param minNumberCustomer the minNumberCustomer to set
	 */
	public void setMinNumberCustomer(Long minNumberCustomer) {

		this.minNumberCustomer = minNumberCustomer;
	}

	/**
	 * Gets the ordre.
	 *
	 * @return the ordre
	 */
	public Long getOrdre() {

		return ordre;
	}

	/**
	 * Sets the ordre.
	 *
	 * @param ordre the ordre to set
	 */
	public void setOrdre(Long ordre) {

		this.ordre = ordre;
	}

	/**
	 * Gets the frequency.
	 *
	 * @return the frequency
	 */
	public IncentiveSettingConstantDTO getFrequency() {

		return frequency;
	}

	/**
	 * Sets the frequency.
	 *
	 * @param frequency the frequency to set
	 */
	public void setFrequency(IncentiveSettingConstantDTO frequency) {

		this.frequency = frequency;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "IncentiveSettingBranchProdLevel [" + (id != null ? "id=" + id + ", " : "")
				+ (productId != null ? "productId=" + productId + ", " : "")
				+ (frequency != null ? "frequency=" + frequency + ", " : "")
				+ (role != null ? "role=" + role + ", " : "")
				+ (minAmount != null ? "minAmount=" + minAmount + ", " : "")
				+ (minNumberCustomer != null ? "minNumberCustomer=" + minNumberCustomer + ", " : "")
				+ (ordre != null ? "ordre=" + ordre : "") + "]";
	}

}
