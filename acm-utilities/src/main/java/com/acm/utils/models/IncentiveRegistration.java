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
 * {@link IncentiveRegistration} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Entity
@Table(name = "ACM_INCENTIVE_REGESTRATION")
public class IncentiveRegistration extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6867868528567856289L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_INCENTIVE_REGESTRATION", unique = true, nullable = false)
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

	/** The customer type. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "CUSTOMER_TYPE_ID")
	private IncentiveSettingConstant customerType;

	/** The role. */
	@Column(name = "INCENTIVE_ROLE", nullable = false)
	private String role;

	/** The incentive value. */
	@Column(name = "INCENTIVE_VALUE", nullable = false)
	private String incentiveValue;

	/** The ordre. */
	@Column(name = "ORDRE", nullable = false)
	private Long ordre;

	/**
	 * Instantiates a new incentive registration.
	 */
	public IncentiveRegistration() {

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

	/**
	 * Gets the customer type.
	 *
	 * @return the customerType
	 */
	public IncentiveSettingConstant getCustomerType() {

		return customerType;
	}

	/**
	 * Sets the customer type.
	 *
	 * @param customerType the customerType to set
	 */
	public void setCustomerType(IncentiveSettingConstant customerType) {

		this.customerType = customerType;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "IncentiveRegistration [" + (id != null ? "id=" + id + ", " : "")
				+ (productId != null ? "productId=" + productId + ", " : "")
				+ (frequency != null ? "frequency=" + frequency + ", " : "")
				+ (incentiveType != null ? "incentiveType=" + incentiveType + ", " : "")
				+ (basedOn != null ? "basedOn=" + basedOn + ", " : "")
				+ (customerType != null ? "customerType=" + customerType + ", " : "")
				+ (role != null ? "role=" + role + ", " : "")
				+ (incentiveValue != null ? "incentiveValue=" + incentiveValue + ", " : "")
				+ (ordre != null ? "ordre=" + ordre : "") + "]";
	}

}
