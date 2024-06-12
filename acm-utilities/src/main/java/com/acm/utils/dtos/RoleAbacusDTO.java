/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link RoleAbacusDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class RoleAbacusDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4147153760789647318L;

	/** The role ID. */
	private Long roleID;

	/** The code. */
	private String code;

	/** The description. */
	private String description;

	/** The default item. */
	private Integer defaultItem;

	/** The enabled. */
	private Boolean enabled;

	/** The unique restriction. */
	private Boolean uniqueRestriction;

	/** The is group head. */
	private Boolean isGroupHead;

	/**
	 * Instantiates a new product loan reasons DTO.
	 */
	public RoleAbacusDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new role abacus DTO.
	 *
	 * @param roleID the role ID
	 * @param code the code
	 * @param description the description
	 * @param defaultItem the default item
	 * @param enabled the enabled
	 * @param uniqueRestriction the unique restriction
	 * @param isGroupHead the is group head
	 */
	public RoleAbacusDTO(Long roleID, String code, String description, Integer defaultItem,
			Boolean enabled, Boolean uniqueRestriction, Boolean isGroupHead) {

		this.roleID = roleID;
		this.code = code;
		this.description = description;
		this.defaultItem = defaultItem;
		this.enabled = enabled;
		this.uniqueRestriction = uniqueRestriction;
		this.isGroupHead = isGroupHead;
	}

	/**
	 * Gets the role ID.
	 *
	 * @return the roleID
	 */
	public Long getRoleID() {

		return roleID;
	}

	/**
	 * Sets the role ID.
	 *
	 * @param roleID the roleID to set
	 */
	public void setRoleID(Long roleID) {

		this.roleID = roleID;
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
	 * Gets the default item.
	 *
	 * @return the defaultItem
	 */
	public Integer getDefaultItem() {

		return defaultItem;
	}

	/**
	 * Sets the default item.
	 *
	 * @param defaultItem the defaultItem to set
	 */
	public void setDefaultItem(Integer defaultItem) {

		this.defaultItem = defaultItem;
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

	/**
	 * Gets the unique restriction.
	 *
	 * @return the uniqueRestriction
	 */
	public Boolean getUniqueRestriction() {

		return uniqueRestriction;
	}

	/**
	 * Sets the unique restriction.
	 *
	 * @param uniqueRestriction the uniqueRestriction to set
	 */
	public void setUniqueRestriction(Boolean uniqueRestriction) {

		this.uniqueRestriction = uniqueRestriction;
	}

	/**
	 * Gets the checks if is group head.
	 *
	 * @return the isGroupHead
	 */
	public Boolean getIsGroupHead() {

		return isGroupHead;
	}

	/**
	 * Sets the checks if is group head.
	 *
	 * @param isGroupHead the isGroupHead to set
	 */
	public void setIsGroupHead(Boolean isGroupHead) {

		this.isGroupHead = isGroupHead;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "RoleAbacusDTO [roleID=" + roleID + ", code=" + code + ", description=" + description
				+ ", defaultItem=" + defaultItem + ", enabled=" + enabled + ", uniqueRestriction="
				+ uniqueRestriction + ", isGroupHead=" + isGroupHead + "]";
	}

}
