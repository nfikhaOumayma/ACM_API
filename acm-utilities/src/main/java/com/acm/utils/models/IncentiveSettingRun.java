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
 * {@link IncentiveSettingRun} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Entity
@Table(name = "ACM_INCENTIVE_SETTING_RUN")
public class IncentiveSettingRun extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8854654748805950331L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_INCENTIVE_SETTING_RUN", unique = true, nullable = false)
	private Long id;

	/** The code. */
	@Column(name = "CODE", nullable = false)
	private String code;

	/** The description. */
	@Column(name = "DESCRIPTION", nullable = false)
	private String description;

	/** The frequency. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "FREQUENCY_ID")
	private IncentiveSettingConstant frequency;

	/** The role. */
	@Column(name = "INCENTIVE_ROLE", nullable = false)
	private String role;

	/** The applay discount rule. */
	@Column(name = "APPLAY_DISCOUNT_RULE")
	private Boolean applayDiscountRule;

	/** The appaly branch prod level. */
	@Column(name = "APPLAY_BRANCH_PROD_LEVEL")
	private Boolean appalyBranchProdLevel;

	/**
	 * Instantiates a new incentive setting run.
	 */
	public IncentiveSettingRun() {

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
	 * @param code the code to set
	 */
	public void setCode(String code) {

		this.code = code;
	}

	/**
	 * Gets the description.
	 *
	 * @return the description
	 */
	public String getDescription() {

		return description;
	}

	/**
	 * Sets the description.
	 *
	 * @param description the description to set
	 */
	public void setDescription(String description) {

		this.description = description;
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
	 * Gets the applay discount rule.
	 *
	 * @return the applayDiscountRule
	 */
	public Boolean getApplayDiscountRule() {

		return applayDiscountRule;
	}

	/**
	 * Sets the applay discount rule.
	 *
	 * @param applayDiscountRule the applayDiscountRule to set
	 */
	public void setApplayDiscountRule(Boolean applayDiscountRule) {

		this.applayDiscountRule = applayDiscountRule;
	}

	/**
	 * Gets the appaly branch prod level.
	 *
	 * @return the appalyBranchProdLevel
	 */
	public Boolean getAppalyBranchProdLevel() {

		return appalyBranchProdLevel;
	}

	/**
	 * Sets the appaly branch prod level.
	 *
	 * @param appalyBranchProdLevel the appalyBranchProdLevel to set
	 */
	public void setAppalyBranchProdLevel(Boolean appalyBranchProdLevel) {

		this.appalyBranchProdLevel = appalyBranchProdLevel;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "IncentiveSettingRun [" + (id != null ? "id=" + id + ", " : "")
				+ (code != null ? "code=" + code + ", " : "")
				+ (description != null ? "description=" + description + ", " : "")
				+ (frequency != null ? "frequency=" + frequency + ", " : "")
				+ (role != null ? "role=" + role + ", " : "")
				+ (applayDiscountRule != null ? "applayDiscountRule=" + applayDiscountRule + ", "
						: "")
				+ (appalyBranchProdLevel != null ? "appalyBranchProdLevel=" + appalyBranchProdLevel
						: "")
				+ "]";
	}
}
