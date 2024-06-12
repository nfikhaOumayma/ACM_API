/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.acm.utils.models.UserDefinedFieldListValues;

/**
 * {@link UserDefinedFieldListValues} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class UserDefinedFieldListValuesDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -8428246320558277490L;

	/** The id. */
	private Long id;

	/** The table abacus name. */
	private String tableAbacusName;

	/** The id UDF list. */
	private Long idUDFList;

	/** The id UDF list value. */
	private Long idUDFListValue;

	/** The id UDF list link. */
	private Long idUDFListLink;

	/** The score. */
	private Integer score;

	/** The name. */
	private String name;

	/** The description. */
	private String description;

	/** The enabled. */
	private Boolean enabled;

	/** The parent UDF list value. */
	private Long parentUDFListValue;

	/**
	 * Instantiates a new user defined field list values.
	 */
	public UserDefinedFieldListValuesDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new user defined field list values DTO.
	 *
	 * @param idUDFListLink the idUDFListLink
	 * @param idUDFListValue the id UDF list value
	 */
	public UserDefinedFieldListValuesDTO(Long idUDFListLink, Long idUDFListValue) {

		this.idUDFListLink = idUDFListLink;
		this.idUDFListValue = idUDFListValue;
	}

	/**
	 * Instantiates a new user defined field list values DTO ==============================>
	 * Represent in ABACUS DB table : [UserDefinedFieldLists].
	 *
	 * @param tableAbacusName the table abacus name
	 * @param idUDFList the id UDF list
	 * @param name the name
	 * @param description the description
	 * @param enabled the enabled
	 */
	public UserDefinedFieldListValuesDTO(String tableAbacusName, Long idUDFList, String name,
			String description, Boolean enabled) {

		this.tableAbacusName = tableAbacusName;
		this.idUDFList = idUDFList;
		this.name = name;
		this.description = description;
		this.enabled = enabled;
		// default value 0 updated by SQL ACM 707
		this.parentUDFListValue = 0L;
	}

	/**
	 * Instantiates a new user defined field list values DTO ==============================>
	 * Represent in ABACUS DB table : [UserDefinedFieldListValues].
	 *
	 * @param tableAbacusName the table abacus name
	 * @param idUDFListValue the id UDF list value
	 * @param idUDFListLink the id UDF list link
	 * @param score the score
	 * @param name the name
	 * @param description the description
	 * @param enabled the enabled
	 */
	public UserDefinedFieldListValuesDTO(String tableAbacusName, Long idUDFListValue,
			Long idUDFListLink, Integer score, String name, String description, Boolean enabled) {

		this.tableAbacusName = tableAbacusName;
		this.idUDFListValue = idUDFListValue;
		this.idUDFListLink = idUDFListLink;
		this.score = score;
		this.name = name;
		this.description = description;
		this.enabled = enabled;
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
	 * Gets the table abacus name.
	 *
	 * @return the tableAbacusName
	 */
	public String getTableAbacusName() {

		return tableAbacusName;
	}

	/**
	 * Sets the table abacus name.
	 *
	 * @param tableAbacusName the tableAbacusName to set
	 */
	public void setTableAbacusName(String tableAbacusName) {

		this.tableAbacusName = tableAbacusName;
	}

	/**
	 * Gets the id UDF list.
	 *
	 * @return the idUDFList
	 */
	public Long getIdUDFList() {

		return idUDFList;
	}

	/**
	 * Sets the id UDF list.
	 *
	 * @param idUDFList the idUDFList to set
	 */
	public void setIdUDFList(Long idUDFList) {

		this.idUDFList = idUDFList;
	}

	/**
	 * Gets the id UDF list value.
	 *
	 * @return the idUDFListValue
	 */
	public Long getIdUDFListValue() {

		return idUDFListValue;
	}

	/**
	 * Sets the id UDF list value.
	 *
	 * @param idUDFListValue the idUDFListValue to set
	 */
	public void setIdUDFListValue(Long idUDFListValue) {

		this.idUDFListValue = idUDFListValue;
	}

	/**
	 * Gets the id UDF list link.
	 *
	 * @return the idUDFListLink
	 */
	public Long getIdUDFListLink() {

		return idUDFListLink;
	}

	/**
	 * Sets the id UDF list link.
	 *
	 * @param idUDFListLink the idUDFListLink to set
	 */
	public void setIdUDFListLink(Long idUDFListLink) {

		this.idUDFListLink = idUDFListLink;
	}

	/**
	 * Gets the score.
	 *
	 * @return the score
	 */
	public Integer getScore() {

		return score;
	}

	/**
	 * Sets the score.
	 *
	 * @param score the score to set
	 */
	public void setScore(Integer score) {

		this.score = score;
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
	 * Gets the parent UDF list value.
	 *
	 * @return the parentUDFListValue
	 */
	public Long getParentUDFListValue() {

		return parentUDFListValue;
	}

	/**
	 * Sets the parent UDF list value.
	 *
	 * @param parentUDFListValue the parentUDFListValue to set
	 */
	public void setParentUDFListValue(Long parentUDFListValue) {

		this.parentUDFListValue = parentUDFListValue;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "UserDefinedFieldListValuesDTO [id=" + id + ", tableAbacusName=" + tableAbacusName
				+ ", idUDFList=" + idUDFList + ", idUDFListValue=" + idUDFListValue
				+ ", idUDFListLink=" + idUDFListLink + ", score=" + score + ", name=" + name
				+ ", description=" + description + ", enabled=" + enabled + "]";
	}

}
