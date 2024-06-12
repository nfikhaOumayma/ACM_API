/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import org.dozer.Mapping;

import com.acm.utils.models.IncentiveSettingConstant;

/**
 * {@link IncentiveOperationDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class IncentiveOperationDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6048875374138178265L;

	/** The id. */
	private Long id;

	/** The product id. */
	private Long productId;

	/** The frequency. */
	@Mapping("frequency")
	private IncentiveSettingConstantDTO frequency;

	/** The incentive type. */
	@Mapping("incentiveType")
	private IncentiveSettingConstantDTO incentiveType;

	/** The based on id {@link IncentiveSettingConstant}. */
	@Mapping("basedOn")
	private IncentiveSettingConstantDTO basedOnId;

	/** The role. */
	private String role;

	/** The incentive value. */
	private String incentiveValue;

	/** The ordre. */
	private Long ordre;

	/** The enabled. */
	private Boolean enabled;

	/**
	 * Instantiates a new incentive operation DTO.
	 */
	public IncentiveOperationDTO() {

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
	 * Gets the incentive value.
	 *
	 * @return the incentiveValue
	 */
	public String getIncentiveValue() {

		return incentiveValue;
	}

	/**
	 * Sets the incentive value.
	 *
	 * @param incentiveValue the incentiveValue to set
	 */
	public void setIncentiveValue(String incentiveValue) {

		this.incentiveValue = incentiveValue;
	}

	/**
	 * Gets the based on id.
	 *
	 * @return the basedOnId
	 */
	public IncentiveSettingConstantDTO getBasedOnId() {

		return basedOnId;
	}

	/**
	 * Sets the based on id.
	 *
	 * @param basedOnId the basedOnId to set
	 */
	public void setBasedOnId(IncentiveSettingConstantDTO basedOnId) {

		this.basedOnId = basedOnId;
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

	/**
	 * Gets the incentive type.
	 *
	 * @return the incentiveType
	 */
	public IncentiveSettingConstantDTO getIncentiveType() {

		return incentiveType;
	}

	/**
	 * Sets the incentive type.
	 *
	 * @param incentiveType the incentiveType to set
	 */
	public void setIncentiveType(IncentiveSettingConstantDTO incentiveType) {

		this.incentiveType = incentiveType;
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

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "IncentiveOperation [" + (id != null ? "id=" + id + ", " : "")
				+ (productId != null ? "productId=" + productId + ", " : "")
				+ (frequency != null ? "frequency=" + frequency + ", " : "")
				+ (role != null ? "role=" + role + ", " : "")
				+ (incentiveType != null ? "incentiveType=" + incentiveType + ", " : "")
				+ (incentiveValue != null ? "incentiveValue=" + incentiveValue + ", " : "")
				+ (basedOnId != null ? "basedOnId=" + basedOnId + ", " : "")
				+ (ordre != null ? "ordre=" + ordre : "") + "]";
	}

}
