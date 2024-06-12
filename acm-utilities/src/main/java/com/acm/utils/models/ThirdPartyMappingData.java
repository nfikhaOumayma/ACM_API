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
import javax.persistence.Table;

/**
 * {@link ThirdPartyMappingData } class.
 *
 * @author kouali
 * @since 0.1.0
 */
@Entity
@Table(name = "ACM_3RD_PARTY_MAPPING_DATA")
public class ThirdPartyMappingData extends GenericModel implements Serializable {

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_3RD_PARTY_MAPPING_DATA", unique = true, nullable = false)
	private Long id;

	/** The category. */
	@Column(name = "CATEGORY")
	private String category;

	/** The original data table name. */
	@Column(name = "ORIGINAL_DATA_TABLE_NAME")
	private String originalDataTableName;

	/** The original data id. */
	@Column(name = "ORIGINAL_DATA_ID")
	private Long originalDataId;

	/** The original data. */
	@Column(name = "ORIGINAL_DATA")
	private String originalData;

	/** The mapped data. */
	@Column(name = "MAPPED_DATA")
	private String mappedData;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

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
	 * @param category the category to set
	 */
	public void setCategory(String category) {

		this.category = category;
	}

	/**
	 * Gets the original data table name.
	 *
	 * @return the originalDataTableName
	 */
	public String getOriginalDataTableName() {

		return originalDataTableName;
	}

	/**
	 * Sets the original data table name.
	 *
	 * @param originalDataTableName the originalDataTableName to set
	 */
	public void setOriginalDataTableName(String originalDataTableName) {

		this.originalDataTableName = originalDataTableName;
	}

	/**
	 * Gets the original data.
	 *
	 * @return the originalData
	 */
	public String getOriginalData() {

		return originalData;
	}

	/**
	 * Gets the original data id.
	 *
	 * @return the originalDataId
	 */
	public Long getOriginalDataId() {

		return originalDataId;
	}

	/**
	 * Sets the original data id.
	 *
	 * @param originalDataId the originalDataId to set
	 */
	public void setOriginalDataId(Long originalDataId) {

		this.originalDataId = originalDataId;
	}

	/**
	 * Sets the original data.
	 *
	 * @param originalData the originalData to set
	 */
	public void setOriginalData(String originalData) {

		this.originalData = originalData;
	}

	/**
	 * Gets the mapped data.
	 *
	 * @return the mappedData
	 */
	public String getMappedData() {

		return mappedData;
	}

	/**
	 * Sets the mapped data.
	 *
	 * @param mappedData the mappedData to set
	 */
	public void setMappedData(String mappedData) {

		this.mappedData = mappedData;
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

}
