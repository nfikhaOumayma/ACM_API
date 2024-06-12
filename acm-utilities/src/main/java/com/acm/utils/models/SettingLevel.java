/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * {@link SettingLevel} class.
 *
 * @author YesserSomai
 * @since 0.3.0
 */
@Entity
@Table(name = "ACM_SETTING_LEVEL")
public class SettingLevel extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 9007551373038610726L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_LEVEL", unique = true, nullable = false)
	private Long id;

	/** The code. */
	@Column(name = "CODE")
	private String code;

	/** The title. */
	@Column(name = "TITLE")
	private String title;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The setting level processes. */
	@OneToMany(mappedBy = "settingLevel")
	private Set<SettingLevelProcess> settingLevelProcesses = new HashSet<>();

	/** The levelOrder. */
	@Column(name = "LEVEL_ORDER")
	private Integer levelOrder;

	/**
	 * Instantiates a new setting level.
	 */
	public SettingLevel() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new setting level.
	 *
	 * @param id the id
	 */
	public SettingLevel(Long id) {

		this.id = id;
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
	 * Gets the setting level processes.
	 *
	 * @return the settingLevelProcesses
	 */
	public Set<SettingLevelProcess> getSettingLevelProcesses() {

		return settingLevelProcesses;
	}

	/**
	 * Sets the setting level processes.
	 *
	 * @param settingLevelProcesses the settingLevelProcesses to set
	 */
	public void setSettingLevelProcesses(Set<SettingLevelProcess> settingLevelProcesses) {

		this.settingLevelProcesses = settingLevelProcesses;
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
