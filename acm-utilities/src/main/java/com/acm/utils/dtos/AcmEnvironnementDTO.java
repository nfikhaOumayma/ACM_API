/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link AcmEnvironnementDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class AcmEnvironnementDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6191785098998691293L;

	/** The id. */
	private Long id;

	/** The date insertion. */
	private Date dateInsertion;

	/** The key. */
	private String key;

	/** The value. */
	private String value;

	/** The enabled. */
	private Boolean enabled;

	/** The value. */
	private String description;

	/** The category. */
	private String category;

	/** The search setting AML. */
	private Boolean searchSettingAML;

	/** The type. */
	private String type;

	/** The date last update. */
	private Date dateLastUpdate;

	/** The crepted key. */
	private String creptedKey;

	/**
	 * Instantiates a new acm environnement DTO.
	 */
	public AcmEnvironnementDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new acm environnement DTO.
	 *
	 * @param key the key
	 */
	public AcmEnvironnementDTO(String key) {

		this.key = key;
	}

	/**
	 * Instantiates a new acm environnement DTO.
	 *
	 * @param key the key
	 * @param value the value
	 */
	public AcmEnvironnementDTO(String key, String value) {

		this.key = key;
		this.value = value;
	}

	/**
	 * Instantiates a new acm environnement DTO.
	 *
	 * @param key the key
	 * @param searchSettingAML the search setting AML
	 */
	public AcmEnvironnementDTO(String key, Boolean searchSettingAML) {

		this.key = key;
		this.searchSettingAML = searchSettingAML;
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
	 * Gets the date insertion.
	 *
	 * @return the dateInsertion
	 */
	public Date getDateInsertion() {

		return dateInsertion;
	}

	/**
	 * Sets the date insertion.
	 *
	 * @param dateInsertion the dateInsertion to set
	 */
	public void setDateInsertion(Date dateInsertion) {

		this.dateInsertion = dateInsertion;
	}

	/**
	 * Gets the key.
	 *
	 * @return the key
	 */
	public String getKey() {

		return key;
	}

	/**
	 * Sets the key.
	 *
	 * @param key the key to set
	 */
	public void setKey(String key) {

		this.key = key;
	}

	/**
	 * Gets the value.
	 *
	 * @return the value
	 */
	public String getValue() {

		return value;
	}

	/**
	 * Sets the value.
	 *
	 * @param value the value to set
	 */
	public void setValue(String value) {

		this.value = value;
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
	 * Gets the category.
	 *
	 * @return the category
	 */
	public String getCategory() {

		return category;
	}

	/**
	 * Sets the category.
	 *
	 * @param category the new category
	 */
	public void setCategory(String category) {

		this.category = category;
	}

	/**
	 * Gets the search setting AML.
	 *
	 * @return the searchSettingAML
	 */
	public Boolean getSearchSettingAML() {

		return searchSettingAML;
	}

	/**
	 * Sets the search setting AML.
	 *
	 * @param searchSettingAML the searchSettingAML to set
	 */
	public void setSearchSettingAML(Boolean searchSettingAML) {

		this.searchSettingAML = searchSettingAML;
	}

	/**
	 * Gets the type.
	 *
	 * @return the type
	 */
	public String getType() {

		return type;
	}

	/**
	 * Sets the type.
	 *
	 * @param type the new type
	 */
	public void setType(String type) {

		this.type = type;
	}

	/**
	 * Gets the date last update.
	 *
	 * @return the date last update
	 */
	public Date getDateLastUpdate() {

		return dateLastUpdate;
	}

	/**
	 * Sets the date last update.
	 *
	 * @param dateLastUpdate the new date last update
	 */
	public void setDateLastUpdate(Date dateLastUpdate) {

		this.dateLastUpdate = dateLastUpdate;
	}

	/**
	 * Gets the crepted key.
	 *
	 * @return the creptedKey
	 */
	public String getCreptedKey() {

		return creptedKey;
	}

	/**
	 * Sets the crepted key.
	 *
	 * @param creptedKey the creptedKey to set
	 */
	public void setCreptedKey(String creptedKey) {

		this.creptedKey = creptedKey;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AcmEnvironnementDTO [" + (id != null ? "id=" + id + ", " : "")
				+ (dateInsertion != null ? "dateInsertion=" + dateInsertion + ", " : "")
				+ (key != null ? "key=" + key + ", " : "")
				+ (value != null ? "value=" + value + ", " : "")
				+ (enabled != null ? "enabled=" + enabled + ", " : "")
				+ (description != null ? "description=" + description + ", " : "")
				+ (category != null ? "category=" + category + ", " : "")
				+ (searchSettingAML != null ? "searchSettingAML=" + searchSettingAML + ", " : "")
				+ (type != null ? "type=" + type : "") + "]";
	}

}
