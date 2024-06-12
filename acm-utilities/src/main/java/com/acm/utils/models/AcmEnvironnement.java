/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 * The persistent class for the ACM_ENVIRONNEMENT database table. {@link AcmEnvironnement} class.
 * 
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Entity
@Table(name = "ACM_ENVIRONNEMENT")
@NamedQuery(name = "AcmEnvironnement.findAll", query = "SELECT l FROM AcmEnvironnement l")
public class AcmEnvironnement extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6506266749563955834L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

	/** The key. */
	@Column(name = "ACM_ENVIRONNEMENT_KEY", length = 256)
	private String key;

	/** The value. */
	@Column(name = "ACM_ENVIRONNEMENT_VALUE", length = 256)
	private String value;

	/** The description. */
	@Column(name = "DESCRIPTION", length = 512)
	private String description;

	/** The category. */
	@Column(name = "CATEGORY", length = 256)
	private String category;

	/** The type. */
	@Column(name = "TYPE_VALUE", length = 256)
	private String type;

	/**
	 * Instantiates a new acm environnement.
	 */
	public AcmEnvironnement() {

		/*
		 * 
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

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AcmEnvironnement [" + (id != null ? "id=" + id + ", " : "")
				+ (key != null ? "key=" + key + ", " : "")
				+ (value != null ? "value=" + value + ", " : "")
				+ (description != null ? "description=" + description + ", " : "")
				+ (category != null ? "category=" + category + ", " : "")
				+ (type != null ? "type=" + type : "") + "]";
	}

}
