/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link SettingLevelDTO} class.
 *
 * @author YesserSomai
 * @since 0.3.0
 */
public class SettingLevelDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8405806313108249262L;

	/** The id. */
	private Long id;

	/** The code. */
	private String code;

	/** The title. */
	private String title;

	/** The description. */
	private String description;

	/** The enabled. */
	private Boolean enabled;

	/** The levelOrder. */
	private Integer levelOrder;

	/**
	 * Instantiates a new setting level.
	 */
	public SettingLevelDTO() {

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
	 * Gets the title.
	 *
	 * @return the title
	 */
	public String getTitle() {

		return title;
	}

	/**
	 * Sets the title.
	 *
	 * @param title the title to set
	 */
	public void setTitle(String title) {

		this.title = title;
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
	 * Gets the enable.
	 * 
	 * @return the enabled
	 */
	public Boolean getEnabled() {

		return enabled;
	}

	/**
	 * Sets the enable.
	 * 
	 * @param enabled the enabled to set
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the level order.
	 *
	 * @return the levelOrder
	 */
	public Integer getLevelOrder() {

		return levelOrder;
	}

	/**
	 * Sets the level order.
	 *
	 * @param levelOrder the levelOrder to set
	 */
	public void setLevelOrder(Integer levelOrder) {

		this.levelOrder = levelOrder;
	}

}
