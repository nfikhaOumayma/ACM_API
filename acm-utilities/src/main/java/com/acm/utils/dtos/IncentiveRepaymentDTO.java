/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import org.dozer.Mapping;

import com.acm.utils.models.IncentiveSetting;
import com.acm.utils.models.IncentiveSettingConstant;

/**
 * {@link IncentiveRepaymentDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class IncentiveRepaymentDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3921922223030230480L;

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

	/** The active customer id {@link IncentiveSetting}. */
	private Long activeCustomerId;

	/** The productivity id {@link IncentiveSetting}. */
	private Long productivityId;

	/** The risk level id {@link IncentiveSetting}. */
	private Long riskLevelId;

	/** The incentive value. */
	private String incentiveValue;

	/** The ordre. */
	private Long ordre;

	/**
	 * Instantiates a new incentive repayment DTO.
	 */
	public IncentiveRepaymentDTO() {

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
	 * Gets the active customer id.
	 *
	 * @return the activeCustomerId
	 */
	public Long getActiveCustomerId() {

		return activeCustomerId;
	}

	/**
	 * Sets the active customer id.
	 *
	 * @param activeCustomerId the activeCustomerId to set
	 */
	public void setActiveCustomerId(Long activeCustomerId) {

		this.activeCustomerId = activeCustomerId;
	}

	/**
	 * Gets the productivity id.
	 *
	 * @return the productivityId
	 */
	public Long getProductivityId() {

		return productivityId;
	}

	/**
	 * Sets the productivity id.
	 *
	 * @param productivityId the productivityId to set
	 */
	public void setProductivityId(Long productivityId) {

		this.productivityId = productivityId;
	}

	/**
	 * Gets the risk level id.
	 *
	 * @return the riskLevelId
	 */
	public Long getRiskLevelId() {

		return riskLevelId;
	}

	/**
	 * Sets the risk level id.
	 *
	 * @param riskLevelId the riskLevelId to set
	 */
	public void setRiskLevelId(Long riskLevelId) {

		this.riskLevelId = riskLevelId;
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

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "IncentiveRepaymentDTO [" + (id != null ? "id=" + id + ", " : "")
				+ (productId != null ? "productId=" + productId + ", " : "")
				+ (frequency != null ? "frequency=" + frequency + ", " : "")
				+ (incentiveType != null ? "incentiveType=" + incentiveType + ", " : "")
				+ (basedOnId != null ? "basedOnId=" + basedOnId + ", " : "")
				+ (role != null ? "role=" + role + ", " : "")
				+ (activeCustomerId != null ? "activeCustomerId=" + activeCustomerId + ", " : "")
				+ (productivityId != null ? "productivityId=" + productivityId + ", " : "")
				+ (riskLevelId != null ? "riskLevelId=" + riskLevelId + ", " : "")
				+ (incentiveValue != null ? "incentiveValue=" + incentiveValue + ", " : "")
				+ (ordre != null ? "ordre=" + ordre : "") + "]";
	}

}
