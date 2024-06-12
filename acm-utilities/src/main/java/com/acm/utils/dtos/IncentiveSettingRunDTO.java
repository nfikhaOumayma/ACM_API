/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import org.dozer.Mapping;

/**
 * {@link IncentiveSettingRunDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class IncentiveSettingRunDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -520264173010486659L;

	/** The id. */
	private Long id;

	/** The code. */
	private String code;

	/** The description. */
	private String description;

	/** The frequency. */
	@Mapping("frequency")
	private IncentiveSettingConstantDTO frequency;

	/** The role. */
	private String role;

	/** The applay discount rule. */
	private Boolean applayDiscountRule;

	/** The appaly branch prod level. */
	private Boolean appalyBranchProdLevel;

	/** The enabled. */
	private Boolean enabled;

	/**
	 * Instantiates a new incentive setting run DTO.
	 */
	public IncentiveSettingRunDTO() {

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
	 * @param enabled the enabled to set
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

		return "IncentiveSettingRunDTO [" + (id != null ? "id=" + id + ", " : "")
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
