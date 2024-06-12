/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link IndustryDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class IndustryDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8342139002736516478L;

	/** The industry ID. */
	private Long industryID;

	/** The name. */
	private String name;

	/** The default item. */
	private Integer defaultItem;

	/** The enabled. */
	private Boolean enabled;

	/**
	 * Instantiates a new industry DTO.
	 */
	public IndustryDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new industry DTO.
	 *
	 * @param industryID the industry ID
	 * @param name the name
	 * @param defaultItem the default item
	 * @param enabled the enabled
	 */
	public IndustryDTO(Long industryID, String name, Integer defaultItem, Boolean enabled) {

		this.industryID = industryID;
		this.name = name;
		this.defaultItem = defaultItem;
		this.enabled = enabled;
	}

	/**
	 * Gets the industry ID.
	 *
	 * @return the industryID
	 */
	public Long getIndustryID() {

		return industryID;
	}

	/**
	 * Sets the industry ID.
	 *
	 * @param industryID the industryID to set
	 */
	public void setIndustryID(Long industryID) {

		this.industryID = industryID;
	}

	/**
	 * Gets the name.
	 *
	 * @return the name
	 */
	public String getName() {

		return name;
	}

	/**
	 * Sets the name.
	 *
	 * @param name the name to set
	 */
	public void setName(String name) {

		this.name = name;
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

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "IndustryDTO [industryID=" + industryID + ", name=" + name + ", defaultItem="
				+ defaultItem + ", enabled=" + enabled + "]";
	}

}
