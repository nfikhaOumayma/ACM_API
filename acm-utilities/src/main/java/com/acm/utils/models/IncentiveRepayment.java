/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * {@link IncentiveRepayment} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Entity
@Table(name = "ACM_INCENTIVE_REPAYMENT")
public class IncentiveRepayment extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2664979356737106627L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_INCENTIVE_REPAYMENT", unique = true, nullable = false)
	private Long id;

	/** The product id. */
	@Column(name = "PRODUCT_ID", nullable = false)
	private Long productId;

	/** The frequency. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "FREQUENCY_ID")
	private IncentiveSettingConstant frequency;

	/** The incentive type Fixed or Percentage. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "INCENTIVE_TYPE_ID")
	private IncentiveSettingConstant incentiveType;

	/** The based on id {@link IncentiveSettingConstant}. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "BASED_ON_ID")
	private IncentiveSettingConstant basedOn;

	/** The role. */
	@Column(name = "INCENTIVE_ROLE", nullable = false)
	private String role;

	/** The active customer id {@link IncentiveSetting}. */
	@Column(name = "ACTIVE_CUSTOMER_ID", nullable = false)
	private Long activeCustomerId;

	/** The productivity id {@link IncentiveSetting}. */
	@Column(name = "PRODUCTIVITY_ID", nullable = false)
	private Long productivityId;

	/** The risk level id {@link IncentiveSetting}. */
	@Column(name = "RISK_LEVEL_ID", nullable = false)
	private Long riskLevelId;

	/** The incentive value. */
	@Column(name = "INCENTIVE_VALUE", nullable = false)
	private String incentiveValue;

	/** The ordre. */
	@Column(name = "ORDRE", nullable = false)
	private Long ordre;

	/**
	 * Instantiates a new incentive repayment.
	 */
	public IncentiveRepayment() {

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
	public IncentiveSettingConstant getFrequency() {

		return frequency;
	}

	/**
	 * Sets the frequency.
	 *
	 * @param frequency the frequency to set
	 */
	public void setFrequency(IncentiveSettingConstant frequency) {

		this.frequency = frequency;
	}

	/**
	 * Gets the incentive type.
	 *
	 * @return the incentiveType
	 */
	public IncentiveSettingConstant getIncentiveType() {

		return incentiveType;
	}

	/**
	 * Sets the incentive type.
	 *
	 * @param incentiveType the incentiveType to set
	 */
	public void setIncentiveType(IncentiveSettingConstant incentiveType) {

		this.incentiveType = incentiveType;
	}

	/**
	 * Gets the based on.
	 *
	 * @return the basedOn
	 */
	public IncentiveSettingConstant getBasedOn() {

		return basedOn;
	}

	/**
	 * Sets the based on.
	 *
	 * @param basedOn the basedOn to set
	 */
	public void setBasedOn(IncentiveSettingConstant basedOn) {

		this.basedOn = basedOn;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "IncentiveRepayment [" + (id != null ? "id=" + id + ", " : "")
				+ (productId != null ? "productId=" + productId + ", " : "")
				+ (frequency != null ? "frequency=" + frequency + ", " : "")
				+ (incentiveType != null ? "incentiveType=" + incentiveType + ", " : "")
				+ (basedOn != null ? "basedOn=" + basedOn + ", " : "")
				+ (role != null ? "role=" + role + ", " : "")
				+ (activeCustomerId != null ? "activeCustomerId=" + activeCustomerId + ", " : "")
				+ (productivityId != null ? "productivityId=" + productivityId + ", " : "")
				+ (riskLevelId != null ? "riskLevelId=" + riskLevelId + ", " : "")
				+ (incentiveValue != null ? "incentiveValue=" + incentiveValue + ", " : "")
				+ (ordre != null ? "ordre=" + ordre : "") + "]";
	}

}
